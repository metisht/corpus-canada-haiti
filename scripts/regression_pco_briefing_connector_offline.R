#!/usr/bin/env Rscript
# Focused offline regression for the bounded bilingual PCO briefing connector.

for (file in c("R/utils.R", "R/discover.R", "R/extract.R", "R/crawl.R",
               "R/spatial_coding.R")) source(file, encoding = "UTF-8")

index_en <- "https://www.canada.ca/en/privy-council/corporate/transparency/briefing-documents.html"
index_fr <- "https://www.canada.ca/fr/conseil-prive/organisation/transparence/documents-information.html"
collection_en <- "https://www.canada.ca/en/privy-council/corporate/transparency/briefing-documents/prime-minister-2025-b.html"
collection_fr <- "https://www.canada.ca/fr/conseil-prive/organisation/transparence/documents-information/premier-ministre-2025-b.html"
target_en <- "https://www.canada.ca/en/privy-council/corporate/transparency/briefing-documents/prime-minister-2025-b/state-world.html"
target_fr <- "https://www.canada.ca/fr/conseil-prive/organisation/transparence/documents-information/premier-ministre-2025-b/etat-monde.html"
unrelated_en <- "https://www.canada.ca/en/privy-council/corporate/transparency/briefing-documents/prime-minister-2025-b/cabinet-formation.html"
unrelated_fr <- "https://www.canada.ca/fr/conseil-prive/organisation/transparence/documents-information/premier-ministre-2025-b/formation-cabinet.html"

fixture_dir <- tempfile("pco-briefing-regression-")
dir.create(fixture_dir)
on.exit(unlink(fixture_dir, recursive = TRUE), add = TRUE)
fixtures <- setNames(file.path(fixture_dir, paste0(seq_len(8), ".html")), c(
  index_en, index_fr, collection_en, collection_fr, target_en, target_fr,
  unrelated_en, unrelated_fr
))
html <- c(
  sprintf('<html><body><main><a href="%s">May 13, 2025</a></main></body></html>', collection_en),
  sprintf('<html><body><main><a href="%s">13 mai 2025</a></main></body></html>', collection_fr),
  sprintf('<html><body><main><a href="%s">State of the World</a><a href="%s">Cabinet formation</a></main></body></html>', target_en, unrelated_en),
  sprintf('<html><body><main><a href="%s">État du monde</a><a href="%s">Formation du Cabinet</a></main></body></html>', target_fr, unrelated_fr),
  sprintf('<html><head><link rel="alternate" hreflang="fr" href="%s"></head><body><main><h1>State of the World</h1><h2>Haiti</h2><p>Canada’s significant security and humanitarian engagement in Haiti demonstrates a leadership role in our neighbourhood while responding to this regional priority and protecting regional stability.</p><h2>Page details</h2><p>2025-10-17</p></main></body></html>', target_fr),
  sprintf('<html><head><link rel="alternate" hreflang="en" href="%s"></head><body><main><h1>État du monde</h1><h2>Haïti</h2><p>L’important engagement du Canada en Haïti démontre un rôle de chef de file dans son voisinage en réalisant cette priorité régionale et en protégeant la stabilité régionale.</p><h2>Détails de la page</h2><p>2025-10-17</p></main></body></html>', target_en),
  '<html><body><main><h1>Cabinet formation</h1><p>This briefing concerns Cabinet machinery, appointments, committees, and administrative responsibilities in Canada.</p><h2>Page details</h2><p>2025-10-17</p></main></body></html>',
  '<html><body><main><h1>Formation du Cabinet</h1><p>Ce document porte sur les nominations, les comités et les responsabilités administratives du Cabinet au Canada.</p><h2>Détails de la page</h2><p>2025-10-17</p></main></body></html>'
)
for (i in seq_along(fixtures)) writeLines(html[i], fixtures[i], useBytes = TRUE)

polite_get <- function(url, cfg, force = FALSE) {
  path <- fixtures[[canonicalize_url(url)]]
  if (is.null(path) || !file.exists(path)) stop("Unexpected fixture URL: ", url)
  list(path = path, status = 200L, type = "text/html", cached = TRUE)
}

source <- data.frame(
  source_id = "pco_briefing_index",
  source_name = "Privy Council Office briefing documents",
  institution = "Privy Council Office",
  method = "pco_briefing_index",
  start_url = paste(index_en, index_fr, sep = "|"),
  allowed_domain = "canada.ca",
  default_families = "regional_strategy",
  language_hint = "bilingual",
  enabled = "TRUE",
  notes = "fixture",
  link_pattern = "",
  required_family = "regional_strategy",
  stringsAsFactors = FALSE
)
cfg <- yaml::read_yaml("config/case.yml")
cfg$start_year <- 1991L
cfg$end_year <- 2026L
cfg$max_discovered_per_source <- 100L
terms <- safe_read_csv("config/terms.csv")
families <- safe_read_csv("config/families.csv")

urls_first <- discover_source(source, cfg)
urls_second <- discover_source(source, cfg)
stopifnot(identical(urls_first, urls_second), setequal(
  urls_first, c(target_en, target_fr, unrelated_en, unrelated_fr)
))

target_rows <- do.call(rbind, lapply(c(target_en, target_fr), function(url) {
  process_url(url, source, cfg, terms, families)
}))
unrelated <- lapply(c(unrelated_en, unrelated_fr), function(url) {
  tryCatch(process_url(url, source, cfg, terms, families), error = identity)
})
stopifnot(
  nrow(target_rows) == 2L,
  length(unique(target_rows$document_id)) == 1L,
  setequal(target_rows$language, c("en", "fr")),
  !anyDuplicated(target_rows$version_id),
  all(target_rows$review_status == "pending"),
  all(target_rows$document_type == "briefing_book"),
  all(target_rows$families == "regional_strategy"),
  all(target_rows$event_start_date == "2025-05-13"),
  all(target_rows$publication_date == "2025-10-17"),
  all(target_rows$page_last_modified == "2025-10-17"),
  all(vapply(unrelated, inherits, logical(1), "crawler_condition")),
  all(vapply(unrelated, function(error) error$status == "excluded", logical(1)))
)

existing_rows <- target_rows
existing_rows$document_id <- "doc_bf16e0a427deba37"
rediscovered <- preserve_discovered_document_ids(target_rows, existing_rows)
stopifnot(
  all(rediscovered$document_id == "doc_bf16e0a427deba37"),
  nrow(deduplicate_candidates(rbind(existing_rows, rediscovered))) == 2L
)

spatial_rows <- target_rows
spatial_rows$review_status <- "retain"
spatial <- build_spatial_inventory(
  spatial_rows, safe_read_csv("config/spatial_terms.csv")
)
proposal <- build_spatial_queue(spatial)
proposal <- proposal[
  proposal$dimension == "relational_framing" &
    proposal$proposed_code == "neighbourhood", , drop = FALSE
]
stopifnot(
  nrow(proposal) == 1L,
  proposal$coding_decision == "pending",
  grepl("neighbourhood", proposal$matched_phrases, fixed = TRUE),
  grepl("voisinage", proposal$matched_phrases, fixed = TRUE)
)

twice <- deduplicate_candidates(rbind(rediscovered, rediscovered))
stopifnot(nrow(twice) == 2L, !anyDuplicated(twice$version_id))
diagnostic <- data.frame(
  run_id = "run_one", source_id = "pco_briefing_index",
  family = "regional_strategy", url = source$start_url,
  stage = "validation", status = "completed",
  message = "1 valid logical documents; 2 official-language records",
  http_status = NA_character_, occurred_at = "2026-01-01T00:00:00Z",
  stringsAsFactors = FALSE
)
rerun <- diagnostic
rerun$run_id <- "run_two"
rerun$occurred_at <- "2026-01-02T00:00:00Z"
stopifnot(nrow(merge_retrieval_diagnostics(diagnostic, rerun)) == 1L)

cat("PCO briefing connector offline regression passed\n")

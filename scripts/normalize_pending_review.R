#!/usr/bin/env Rscript
# Normalizes the current pending review set from stored candidate content only.
# It performs no retrieval. Retained candidates, retained review decisions, and
# corpus.csv are invariants; the output is an auditable document-level inventory.

for (file in c("R/utils.R", "R/extract.R", "R/crawl.R"))
  source(file, encoding = "UTF-8")

candidate_path <- "data/candidates.csv"
review_path <- "data/review.csv"
corpus_path <- "data/corpus.csv"
inventory_path <- "data/pending_review_inventory.csv"

file_hash <- function(path) digest::digest(file = path, algo = "sha256")
focused_excerpt <- function(text, pattern, before = 60L, after = 900L) {
  position <- regexpr(pattern, text, ignore.case = TRUE, perl = TRUE)[1]
  if (position < 1L) return("")
  clean_space(substr(text, max(1L, position - before), position + after))
}
sort_records <- function(x) {
  keys <- intersect(c("document_id", "version_id"), names(x))
  out <- x[do.call(order, x[keys]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

candidates <- safe_read_csv(candidate_path)
review <- safe_read_csv(review_path)
corpus_hash_before <- file_hash(corpus_path)
retained_before <- candidates[candidates$review_status == "retain", candidate_columns,
                              drop = FALSE]
retained_review_before <- review[review$decision == "retain", review_columns,
                                 drop = FALSE]

french_titles <- c(
  "https://www.canada.ca/fr/nouvelles/archive/2010/05/discours-ministre-cannon-conference-internationale-groupe-reflexion-action-haiti-nouvelle.html" =
    "Discours du ministre Cannon à la Conférence internationale du Groupe de réflexion et d’action pour une Haïti nouvelle",
  "https://www.canada.ca/fr/nouvelles/archive/2010/01/remarques-introduction-ministre-cannon-conference-ministerielle-preparatoire-haiti.html" =
    "Remarques d’introduction du ministre Cannon à la Conférence ministérielle préparatoire sur Haïti",
  "https://www.canada.ca/fr/nouvelles/archive/2010/07/ministres-cannon-kenney-oda-soulignent-passage-six-mois-depuis-seisme-haiti.html" =
    "Les ministres Cannon, Kenney et Oda soulignent le passage de six mois depuis le séisme en Haïti",
  "https://www.canada.ca/fr/nouvelles/archive/2010/07/declaration-ministre-cannon-haiti.html" =
    "Déclaration du ministre Cannon sur Haïti",
  "https://www.canada.ca/fr/affaires-mondiales/nouvelles/2021/07/le-ministre-garneau-sentretient-avec-le-premier-ministre-interimaire-et-ministre-des-affaires-etrangeres-dhaiti.html" =
    "Le ministre Garneau s’entretient avec le premier ministre intérimaire et ministre des Affaires étrangères d’Haïti",
  "https://www.canada.ca/fr/affaires-mondiales/nouvelles/2022/01/le-canada-annonce-une-aide-de-504-millions-de-dollars-pour-haiti.html" =
    "Le Canada annonce une aide de 50,4 millions de dollars pour Haïti",
  "https://www.canada.ca/fr/affaires-mondiales/nouvelles/2025/09/la-ministre-anand-rencontre-le-ministre-des-affaires-etrangeres-dhaiti.html" =
    "La ministre Anand rencontre le ministre des Affaires étrangères d’Haïti"
)

gac_ids <- unique(candidates$document_id[
  candidates$review_status == "pending" & candidates$source_id == "gac_news"
])
new_versions <- list()
for (document_id in gac_ids) {
  index <- which(candidates$document_id == document_id &
                   candidates$review_status == "pending")
  row <- candidates[index[1], , drop = FALSE]
  urls <- unique(c(row$document_url,
                   unlist(strsplit(row$official_alternate_urls, "\\|"))))
  english_url <- urls[grepl("/en/", urls)][1]
  french_url <- urls[grepl("/fr/", urls)][1]
  if (is.na(english_url) || is.na(french_url))
    stop("Pending GAC record lacks a verified English/French URL pair: ", document_id)
  shared_id <- stable_document_id(english_url, c(english_url, french_url))
  english_index <- index[canonicalize_url(candidates$document_url[index]) ==
                           canonicalize_url(english_url)][1]
  french_index <- index[canonicalize_url(candidates$document_url[index]) ==
                          canonicalize_url(french_url)][1]
  candidates$document_id[english_index] <- shared_id
  candidates$language[english_index] <- "en"
  candidates$version_id[english_index] <- stable_version_id(english_url, "en")
  candidates$official_alternate_urls[english_index] <- french_url

  if (is.na(french_index)) {
    french <- candidates[english_index, , drop = FALSE]
    french$document_id <- shared_id
    french$version_id <- stable_version_id(french_url, "fr")
    french$title <- unname(french_titles[canonicalize_url(french_url)])
    if (is.na(french$title) || !nzchar(french$title))
      stop("No stored-URL title normalization for: ", french_url)
    french$language <- "fr"
    french$source_page <- french_url
    french$document_url <- french_url
    french$canonical_url <- canonicalize_url(french_url)
    french$official_alternate_urls <- english_url
    french$relevance_excerpt <- ""
    french$extracted_text <- ""
    french$content_hash <- ""
    french$retrieval_method <- "stored_official_alternate_url"
    french$review_notes <- paste(
      "Official French counterpart URL was stored, but localized page content",
      "was not extracted; review the English stored excerpt and verify French content manually."
    )
    new_versions[[length(new_versions) + 1L]] <- french
  } else {
    candidates$document_id[french_index] <- shared_id
    candidates$language[french_index] <- "fr"
    candidates$version_id[french_index] <- stable_version_id(french_url, "fr")
    candidates$official_alternate_urls[french_index] <- english_url
  }
}
candidates <- bind_rows_base(c(list(candidates), new_versions))

finance <- candidates$review_status == "pending" &
  candidates$source_id == "public_finance_index" &
  candidates$publication_date == "2024-05-06"
if (sum(finance) != 2L || !setequal(candidates$language[finance], c("en", "fr")))
  stop("Expected one bilingual 2024-05-06 Supplementary Estimates briefing")
finance_id <- paste0("doc_", substr(digest::digest(
  "gac:supplementary-estimates-briefing:2024-05-06", "sha256"), 1, 16))
candidates$document_id[finance] <- finance_id

annual <- candidates$review_status == "pending" &
  candidates$source_id == "rcmp_publications" &
  candidates$document_id == "doc_d2d9aac6534a54ce"
candidates$title[annual & candidates$language == "en"] <-
  "Canadian Police Arrangement — International Police Peacekeeping and Peace Operations Program: Annual Report 2024–2025"
candidates$title[annual & candidates$language == "fr"] <-
  "Arrangement sur la police civile au Canada — Programme de police internationale de maintien de la paix et des opérations de paix : Rapport annuel 2024–2025"
candidates$publication_date[annual] <- ""
candidates$date_source[annual] <-
  "official 2024–2025 reporting period; exact publication date unavailable"

annual_en <- annual & candidates$language == "en"
annual_fr <- annual & candidates$language == "fr"
candidates$relevance_excerpt[annual_en] <- focused_excerpt(
  candidates$extracted_text[annual_en], "Haiti The CPA-IPP plays a vital role")
candidates$relevance_excerpt[annual_fr] <- focused_excerpt(
  candidates$extracted_text[annual_fr], "Haïti L’APCC-Programme PIP joue un rôle essentiel")

plan <- candidates$review_status == "pending" &
  candidates$source_id == "rcmp_publications" &
  candidates$document_id == "doc_9ec69e5108acfcad"
candidates$publication_date[plan] <- ""
candidates$year[plan] <- NA_integer_
candidates$date_source[plan] <-
  "publication date uncertain; 2023–2029 is the plan period"

candidates <- candidates[order(candidates$year, candidates$title,
                               candidates$language, na.last = TRUE),
                         candidate_columns, drop = FALSE]
pending_ids <- unique(candidates$document_id[candidates$review_status == "pending"])
pending_review <- lapply(pending_ids, function(document_id) {
  data.frame(
    document_id = document_id,
    version_id = candidates$version_id[match(document_id, candidates$document_id)],
    decision = "pending", notes = "", reviewer = "", reviewed_at = "",
    stringsAsFactors = FALSE
  )
})
review <- bind_rows_base(c(list(retained_review_before), pending_review))
review <- review[, review_columns, drop = FALSE]
candidates <- apply_review(candidates, review)

pending <- candidates[candidates$review_status == "pending", , drop = FALSE]
pending_ids <- unique(pending$document_id)
inventory <- lapply(pending_ids, function(document_id) {
  rows <- pending[pending$document_id == document_id, , drop = FALSE]
  urls <- unique(c(rows$document_url,
                   unlist(strsplit(rows$official_alternate_urls, "\\|"))))
  urls <- urls[nzchar(urls)]
  extracted <- rows[nzchar(rows$extracted_text), , drop = FALSE]
  excerpt <- extracted$relevance_excerpt[which.max(nchar(extracted$relevance_excerpt))]
  if (!length(excerpt) || is.na(excerpt)) excerpt <- ""
  reason <- if (all(rows$source_id == "rcmp_publications")) {
    "Official RCMP/Canadian Police Arrangement record with a substantive Haiti passage."
  } else if (all(rows$source_id == "public_finance_index")) {
    "Official expenditure briefing with substantive Haiti allocations or program information."
  } else {
    "Official ministerial speech, statement, meeting record, or funding announcement substantively concerning Haiti."
  }
  uncertainty <- character()
  if (any(rows$retrieval_method == "stored_official_alternate_url"))
    uncertainty <- c(uncertainty, "French official URL stored; French page text not extracted.")
  if (any(is.na(rows$year)))
    uncertainty <- c(uncertainty, "Publication date and year uncertain.")
  if (any(grepl("exact publication", rows$date_source)))
    uncertainty <- c(uncertainty, unique(rows$date_source[grepl("exact publication", rows$date_source)]))
  data.frame(
    document_id = document_id,
    english_title = paste(unique(rows$title[rows$language == "en"]), collapse = " | "),
    french_title = paste(unique(rows$title[rows$language == "fr"]), collapse = " | "),
    date = paste(unique(ifelse(nzchar(rows$publication_date), rows$publication_date,
                               ifelse(is.na(rows$year), "uncertain", rows$year))),
                 collapse = " | "),
    institution = paste(unique(rows$institution), collapse = " | "),
    family = paste(unique(rows$families), collapse = " | "),
    document_type = paste(unique(rows$document_type), collapse = " | "),
    languages = paste(sort(unique(rows$language)), collapse = "|"),
    official_urls = paste(urls, collapse = "|"),
    substantive_haiti_excerpt = excerpt,
    candidate_reason = reason,
    metadata_uncertainty = paste(unique(uncertainty), collapse = " "),
    decision = "pending",
    stringsAsFactors = FALSE
  )
})
inventory <- bind_rows_base(inventory)

plan_final <- candidates$review_status == "pending" &
  candidates$source_id == "rcmp_publications" &
  candidates$document_id == "doc_9ec69e5108acfcad"

stopifnot(
  !anyDuplicated(candidates$version_id),
  !anyDuplicated(candidates[c("document_id", "language")]),
  all(review$decision[review$document_id %in% pending_ids] == "pending"),
  identical(sort_records(retained_before),
            sort_records(candidates[candidates$review_status == "retain",
                                    candidate_columns, drop = FALSE])),
  identical(sort_records(retained_review_before),
            sort_records(review[review$decision == "retain", review_columns,
                                drop = FALSE])),
  identical(corpus_hash_before, file_hash(corpus_path)),
  all(candidates$publication_date[plan_final] == ""),
  all(is.na(candidates$year[plan_final]))
)

write_csv(candidates, candidate_path)
write_csv(review, review_path)
write_csv(inventory, inventory_path)

if (!identical(corpus_hash_before, file_hash(corpus_path)))
  stop("corpus.csv changed during pending reconciliation")
message(nrow(pending), " pending versions; ", length(pending_ids),
        " pending logical documents")

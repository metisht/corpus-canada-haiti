#!/usr/bin/env Rscript
# Offline fixtures for historical connector semantics. Temporary HTML fixtures
# exercise routing, dates, identity, classification, and UTF-8 behavior. No
# network or project-data writes occur.
for (f in c("R/utils.R", "R/http.R", "R/discover.R", "R/extract.R", "R/crawl.R"))
  source(f, encoding="UTF-8")

check <- function(name, ok) {
  if (!isTRUE(ok)) stop("FAIL: ", name, call.=FALSE)
  message("PASS ", name)
}
fixture <- function(text) {
  path <- tempfile(fileext=".html")
  writeLines(text, path, useBytes=TRUE)
  path
}

# Viewer links: the French route must be the localized chambre/seance/debats
# link exposed by the English viewer, and each localized viewer supplies its
# own direct PDF.
en_page <- "https://www.ourcommons.ca/DocumentViewer/en/35-2/house/sitting-2/hansard"
fr_page <- "https://www.noscommunes.ca/documentviewer/fr/35-2/chambre/seance-2/debats"
en_pdf <- "https://www.ourcommons.ca/Content/House/352/Debates/002/han002-e.pdf"
fr_pdf <- "https://www.ourcommons.ca/Content/House/352/Debates/002/han002-f.pdf"
pages <- setNames(c(
  fixture(sprintf('<html><a data-analytics-eventlabel="Language Switch" href="%s">Français</a><a class="btn-export-pdf" href="%s">PDF</a></html>', fr_page, en_pdf)),
  fixture(sprintf('<html><a data-analytics-eventlabel="Language Switch" href="%s">English</a><a class="btn-export-pdf" href="%s">PDF</a></html>', en_page, fr_pdf))
), c(en_page, fr_page))
polite_get <- function(url, cfg, force=FALSE) list(path=unname(pages[[url]]), type="text/html")
pair <- resolve_house_viewer_pair(en_page, list())
check("official localized French viewer routing", setequal(pair, c(en_pdf, fr_pdf)))
check("viewer source_page and direct PDF document_url",
      identical(discovered_source_page(en_pdf, ""), canonicalize_url(en_page)) &&
      identical(discovered_source_page(fr_pdf, ""), canonicalize_url(fr_page)))
check("PDF counterparts derive from localized viewers",
      identical(discovered_counterpart(en_pdf), canonicalize_url(fr_pdf)))

pm_en <- parliament_metadata("", "February 28, 1996 COMMONS DEBATES", en_pdf, "en")
pm_fr <- parliament_metadata("", "28 février 1996 DÉBATS DES COMMUNES", fr_pdf, "fr")
check("authoritative English Hansard title", identical(pm_en$title, "House of Commons Debates (Hansard) No. 2 — 1996-02-28"))
check("authoritative French Hansard title", identical(pm_fr$title, "Débats de la Chambre des communes no 2 — 1996-02-28"))

s <- sessions_in_range(list(start_year=1991L, end_year=1998L))
expected <- c("34-2","34-3","35-1","35-2","36-1")
check("complete 1991-1998 parliamentary sessions",
      setequal(paste(s$parliament, s$session, sep="-"), expected))

# The official Caribbean branch is below the global cap and contains every
# named Haiti operation without hard-coding those names in crawler logic.
root <- "https://www.canada.ca/en/department-national-defence/services/military-history/history-heritage/past-operations.html"
region <- "https://www.canada.ca/en/department-national-defence/services/military-history/history-heritage/past-operations/caribbean.html"
ops <- c("forward-action","cauldron","dialogue","pivot","standard-1996","standard-unsmih-1996")
op_urls <- paste0("https://www.canada.ca/en/department-national-defence/services/military-history/history-heritage/past-operations/caribbean/", ops, ".html")
archive_pages <- setNames(c(
  fixture(sprintf('<a href="%s">Caribbean</a>', region)),
  fixture(paste(sprintf('<a href="%s">Operation</a>', op_urls), collapse=""))
), c(root, region))
polite_get <- function(url, cfg, force=FALSE) list(path=unname(archive_pages[[url]]), type="text/html")
source <- data.frame(start_url=root, link_pattern="/caribbean/", stringsAsFactors=FALSE)
found <- discover_operations_archive(source, list())
check("all six Defence Past Operations routes", setequal(found, op_urls))

# Publications catalogue pagination emits only direct electronic documents on
# an allowed official domain; catalogue records and off-domain links are not
# crawler records.
search1 <- "https://publications.gc.ca/search?page=1"
search2 <- "https://publications.gc.ca/search?page=2"
record1 <- "https://publications.gc.ca/site/eng/1/publication.html"
record2 <- "https://publications.gc.ca/site/eng/2/publication.html"
catalogue_pages <- setNames(c(
  fixture(sprintf('<a href="%s">one</a><a rel="next" href="%s">Next</a>', record1, search2)),
  fixture(sprintf('<a href="%s">two</a>', record2)),
  fixture('<a href="https://publications.gc.ca/collections/one.pdf">View PDF</a><a href="https://evil.test/bad.pdf">View PDF</a>'),
  fixture('<a href="https://publications.gc.ca/collections/two.pdf">Voir PDF</a>')
), c(search1, search2, record1, record2))
polite_get <- function(url, cfg, force=FALSE) list(path=unname(catalogue_pages[[url]]), type="text/html")
source <- data.frame(start_url=search1, allowed_domain="publications.gc.ca", stringsAsFactors=FALSE)
docs <- discover_publications_catalogue(source, list(max_discovered_per_source=10L))
check("correct paginated historical catalogue routes",
      setequal(docs, c("https://publications.gc.ca/collections/one.pdf", "https://publications.gc.ca/collections/two.pdf")))
direct <- "https://publications.gc.ca/collections/collection_2010/acdi-cida/example.pdf"
source$start_url <- paste(search1, direct, sep="|")
docs <- discover_publications_catalogue(source, list(max_discovered_per_source=10L))
check("verified direct official documents supplement catalogue discovery",
      direct %in% docs && !any(grepl("publication\\.html$", docs)))
rcmp_en <- "https://open.canada.ca/data/dataset/x/resource/a/download/en-web-version-2024-25-cpa-ipp-annual-report.pdf"
rcmp_fr <- "https://open.canada.ca/data/dataset/x/resource/b/download/fr-web-version-2024-25-cpa-ipp-annual-report.pdf"
check("RCMP annual-report language versions share document identity",
      identical(source_document_id(rcmp_en, "rcmp_publications"),
                source_document_id(rcmp_fr, "rcmp_publications")))

archive_dates <- extract_date_metadata(
  "Archived ministerial statement without a machine-readable publication day",
  "https://www.canada.ca/en/news/archive/2010/07/statement-minister-cannon-haiti.html",
  "gac_news")
check("archived statements use the authoritative archive year",
      archive_dates$year == 2010L &&
        grepl("exact publication day unavailable", archive_dates$date_source))
check("observable administrative document types",
      observable_document_type("Canadian Police Arrangement Annual Report 2024-2025") == "annual_report" &&
      observable_document_type("RCMP Implementation Plan 2023 to 2029") == "implementation_report" &&
      observable_document_type("Evaluation of the International Police Program") == "program_evaluation" &&
      observable_document_type("Supplementary Estimates (C) 2023-24") == "supplementary_estimates" &&
      observable_document_type("Police training course in Haiti") == "training_record")
check("historical catalogue family classification",
      catalogue_record_relevant("historical_diplomacy", "Haiti — Department of Foreign Affairs") &&
      !catalogue_record_relevant("historical_diplomacy", "Haiti — Royal Canadian Mounted Police") &&
      catalogue_record_relevant("historical_development", "Haïti — Agence canadienne de développement international") &&
      catalogue_record_relevant("historical_sanctions", "Special Economic Measures regulations concerning Haiti") &&
      catalogue_record_relevant("historical_police", "RCMP civilian police mission in Haiti"))

check("bilingual 1992 Canadiana volume identity",
      historical_volume_pair_key("oop.com_HOC_3402_66_3") == historical_volume_pair_key("oop.com_CDC_3402_66_3") &&
      historical_volume_language("oop.com_HOC_3402_66_3") == "en" &&
      historical_volume_language("oop.com_CDC_3402_66_3") == "fr" &&
      setequal(historical_search_terms(), c("Haiti", "Haïti")))

excluded <- tryCatch(crawler_stop("outside_family", "wrong family"), error=identity)
failed <- simpleError("timeout")
check("diagnostic classification", diagnostic_status(excluded) == "outside_family" && diagnostic_status(failed) == "failed")
diags <- rbind(
  record_diagnostic("a","x","u","retrieval","failed",run_id="run_old"),
  record_diagnostic("a","x","u","retrieval","excluded",run_id="run_new"),
  record_diagnostic("a","x","u","validation","completed",run_id="run_new")
)
summary <- diagnostic_run_summary(diags, "run_new")
check("per-run diagnostic reporting", summary[["failed"]] == 0L && summary[["excluded"]] == 1L && summary[["completed"]] == 1L)

check("French ordinal sitting date", identical(parse_authoritative_house_date("vendredi 1er mars 1996"), "1996-03-01"))
check("French accents apostrophe and em dash remain UTF-8",
      identical(clean_space("Débats d’aujourd’hui — Haïti"), "Débats d’aujourd’hui — Haïti") &&
      !contains_mojibake("Débats d’aujourd’hui — Haïti"))
check("known mojibake signatures are rejected",
      contains_mojibake(c("FranÃ§ais", "dâ€™État", "D├⌐bats", "ΓÇö")))

html_route <- "https://www.ourcommons.ca/DocumentViewer/en/35-2/house/sitting-2/hansard"
pdf_route <- "https://www.ourcommons.ca/Content/House/352/Debates/002/han002-e.pdf"
check("stable Parliament identity ignores viewer versus PDF route",
      identical(stable_parliament_document_id(html_route), stable_parliament_document_id(pdf_route)))

make_candidate <- function(url, format, retrieved_at) {
  row <- empty_frame(candidate_columns)
  row[1, ] <- ""
  row$document_id <- stable_document_id(url)
  row$version_id <- stable_version_id(url, "en")
  row$language <- "en"; row$source_id <- "parliament"; row$document_url <- url
  row$canonical_url <- url; row$format <- format; row$retrieved_at <- retrieved_at
  row$review_status <- "pending"; row$content_hash <- paste0("hash-", format)
  row$official_alternate_urls <- ""
  row
}
routes <- bind_rows_base(list(make_candidate(html_route, "HTML", "2026-01-01T00:00:00Z"),
                              make_candidate(pdf_route, "PDF", "2026-01-02T00:00:00Z")))
once <- deduplicate_candidates(routes)
twice <- deduplicate_candidates(bind_rows_base(list(once, once)))
check("route repair replaces HTML and is idempotent",
      nrow(once) == 1L && once$format == "PDF" && nrow(twice) == 1L &&
      identical(once$document_id, twice$document_id))

historical_bill <- historical_parliament_metadata("BILL C-53 10-3-1992 INDEX", "https://parl.canadiana.ca/files/get/oop.com_HOC_3403_28_1/1")
historical_rights <- historical_parliament_metadata("DEVELOPMENT AND HUMAN RIGHTS 11-3-1992", "https://parl.canadiana.ca/files/get/oop.com_HOC_3403_25_2/2")
check("authoritative historical running-header titles",
      historical_bill$title == "Index — Bill C-53 / Projet de loi C-53" &&
      historical_rights$title == "Development and Human Rights / Développement et droits de la personne — 1992-03-11")

standard_unmih <- defence_operation_metadata("United Nations Mission in Haiti", "", "https://www.canada.ca/past-operations/caribbean/standard-1996.html")
standard_unsmih <- defence_operation_metadata("United Nations Support Mission in Haiti", "", "https://www.canada.ca/past-operations/caribbean/standard-unsmih-1996.html")
pivot <- defence_operation_metadata("UNMIH", "", "https://www.canada.ca/past-operations/caribbean/pivot.html")
check("Defence operation and mission identities remain distinct",
      standard_unmih$title == "Operation STANDARD (UNMIH)" &&
      standard_unsmih$title == "Operation STANDARD (UNSMIH)" &&
      pivot$operation_name == "Operation PIVOT" &&
      standard_unmih$international_mission_name == "United Nations Mission in Haiti")

gac_en <- "https://www.international.gc.ca/world-monde/international_relations-relations_internationales/sanctions/haiti.aspx?lang=eng"
gac_fr <- "https://www.international.gc.ca/world-monde/international_relations-relations_internationales/sanctions/haiti.aspx?lang=fra"
gac_news <- "https://www.canada.ca/en/global-affairs/news/2025/03/canada-imposes-additional-sanctions-against-individuals-linked-to-crisis-in-haiti.html"
gac_pages <- setNames(c(
  fixture(sprintf('<a href="%s">Canadian sanctions related to Haiti</a><a href="%s">Haiti sanctions announcement</a>', gac_fr, gac_news)),
  fixture(sprintf('<a href="%s">Les sanctions canadiennes liées à Haïti</a>', gac_en))
), c(gac_en, gac_fr))
polite_get <- function(url, cfg, force = FALSE) list(path = unname(gac_pages[[url]]), type = "text/html")
gac_source <- data.frame(source_id = "gac_sanctions", start_url = paste(gac_en, gac_fr, sep = "|"),
                         allowed_domain = "international.gc.ca|canada.ca",
                         link_pattern = "sanction|haiti|haïti", stringsAsFactors = FALSE)
gac_found <- discover_collection(gac_source, list())
check("dedicated GAC sanctions discovery includes policy languages and announcements",
      all(c(gac_en, gac_fr, gac_news) %in% gac_found))
check("GAC sanctions bilingual identity is separate from Justice and Gazette layers",
      stable_document_id(gac_en) == stable_document_id(gac_fr) &&
      stable_document_id(gac_en) != stable_document_id("https://laws-lois.justice.gc.ca/eng/regulations/SOR-2022-226/FullText.html") &&
      stable_document_id(gac_en) != stable_document_id("https://gazette.gc.ca/rp-pr/p2/2022/2022-11-23/html/sor-dors226-eng.html"))
review_en <- "https://international.canada.ca/en/global-affairs/corporate/transparency/acts-regulations/regulatory-reviews/sor-2022-226"
review_fr <- "https://international.canada.ca/fr/affaires-mondiales/organisation/transparence/lois-reglements/examens-reglementaires/dors-2022-226"
check("GAC regulatory reviews group bilingually but remain distinct from Justice consolidation",
      stable_document_id(review_en) == stable_document_id(review_fr) &&
      stable_document_id(review_en) != stable_document_id("https://laws-lois.justice.gc.ca/eng/regulations/SOR-2022-226/FullText.html"))
gac_meta <- derive_document_metadata("Canadian Sanctions Related to Haiti", "Haiti sanctions policy", gac_en,
                                     data.frame(source_id = "gac_sanctions", institution = "Global Affairs Canada"))
check("GAC policy pages are sanctions documents without being regulations",
      gac_meta$families == "sanctions" && gac_meta$document_type == "sanctions_policy")

review_fixture <- data.frame(document_id = "doc_shared", version_id = "ver_en", decision = "retain",
                             notes = "", reviewer = "reviewer", reviewed_at = "2026-01-01T00:00:00Z")
candidate_fixture <- data.frame(document_id = c("doc_shared", "doc_shared"),
                                version_id = c("ver_en", "ver_fr"),
                                retrieved_at = c("2025-01-01T00:00:00Z", "2026-02-01T00:00:00Z"),
                                review_status = c("retain", "pending"), review_notes = c("", ""))
reviewed_fixture <- apply_review(candidate_fixture, review_fixture)
check("document-level review propagates to every bilingual counterpart",
      all(reviewed_fixture$review_status == "retain"))

message("All offline historical regressions passed.")

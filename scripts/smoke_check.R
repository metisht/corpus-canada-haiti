required <- c("config/case.yml", "config/sources.csv", "data/candidates.csv", "data/review.csv", "data/corpus.csv", "data/concepts.csv", "data/retrieval_diagnostics.csv")
stopifnot(all(file.exists(required)))
cfg <- yaml::read_yaml("config/case.yml")
stopifnot(cfg$start_year == 1991, cfg$end_year == 2026, cfg$validation_max_valid_per_source == 5)
candidates <- read.csv("data/candidates.csv", stringsAsFactors = FALSE, check.names = FALSE)
needed <- c("document_id","version_id","title","publication_date","page_last_modified","event_start_date","event_end_date","year","date_source","language","institution","families","document_type","source_page","document_url","format","relevance_excerpt","extracted_text","matched_case_terms","matched_concepts","retrieval_method","retrieved_at","review_status")
stopifnot(all(needed %in% names(candidates)))
if (nrow(candidates)) {
  stopifnot(!anyDuplicated(candidates$version_id), !anyDuplicated(candidates$canonical_url), all(grepl("^https://", candidates$document_url)), all(candidates$review_status %in% c("pending","retain","include","exclude","needs_review")))
  gazette226 <- grepl("gazette.gc.ca/.*/sor-dors226-(eng|fra)", candidates$document_url, ignore.case=TRUE)
  justice226 <- grepl("laws-lois.justice.gc.ca/.*/SOR-2022-226", candidates$document_url, ignore.case=TRUE)
  if (sum(gazette226) == 2) stopifnot(length(unique(candidates$document_id[gazette226])) == 1)
  if (any(gazette226) && any(justice226)) stopifnot(!candidates$document_id[justice226][1] %in% candidates$document_id[gazette226])
  caf <- candidates$source_id == "forces_archive"
  if (any(caf)) stopifnot(all(candidates$families[caf] == "armed_forces"), all(candidates$document_type[caf] == "operation_record"), all(candidates$date_source[caf] == "event_start_date"))
  gazette <- grepl("gazette.gc.ca", candidates$document_url, fixed=TRUE)
  justice <- grepl("laws-lois.justice.gc.ca", candidates$document_url, fixed=TRUE)
  if (any(gazette | justice)) stopifnot(all(candidates$families[gazette | justice] == "sanctions"), all(candidates$document_type[gazette | justice] == "regulation"))
  stopifnot(!any(grepl("\\|", candidates$source_page[candidates$retrieval_method == "seed_list"])))
}
cat(sprintf("Smoke check passed (%d language versions; %d logical documents).\n", nrow(candidates), length(unique(candidates$document_id))))
if (nrow(candidates)) {
  groups <- split(candidates, candidates$document_id)
  for (z in groups) cat(sprintf("%s: %d version(s) | %s\n", z$document_id[1], nrow(z), paste(paste0(z$language, ": ", z$title), collapse = " || ")))
  review <- read.csv("data/review.csv", stringsAsFactors = FALSE, check.names = FALSE)
  cat(sprintf("Review check: %d rows, %d retained decisions, %d unique version IDs.\n", nrow(review), sum(review$decision == "retain", na.rm = TRUE), length(unique(review$version_id))))
  cat(sprintf("Extracted text present: %d/%d versions.\n", sum(!is.na(candidates$extracted_text) & nzchar(candidates$extracted_text)), nrow(candidates)))
}

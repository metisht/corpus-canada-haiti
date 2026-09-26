# Offline fixture for the completed 137-document review state.
# Reads project CSVs only and asserts retained membership, rejection audit history,
# bilingual grouping, uniqueness, UTF-8 validity, and the known 209-version fixture.

source("R/utils.R")
source("R/discover.R")
source("R/extract.R")
source("R/crawl.R")

rejected_ids <- c("doc_2517f95d9356f704", "doc_3cfa943a7a4225b8",
  "doc_fbb9f96e232bd24f", "doc_05b993ed424cca1c",
  "doc_99667ab611ac03ea", "doc_bde72e3958af0f51")
candidates <- safe_read_csv("data/candidates.csv")
review <- safe_read_csv("data/review.csv")
corpus <- safe_read_csv("data/corpus.csv")
diagnostics <- safe_read_csv("data/retrieval_diagnostics.csv")
changes <- safe_read_csv("data/validation_changes.csv")

stopifnot(nrow(review) == 137L, all(review$decision == "retain"),
          !any(review$decision %in% c("pending", "reject")),
          length(unique(review$document_id)) == 137L)
stopifnot(
  nrow(corpus) == 209L,
  dplyr::n_distinct(corpus$document_id) == 137L,
  all(corpus$review_status == "retain"),
  !any(corpus$review_status %in% c("pending", "reject")),
  setequal(unique(corpus$document_id),
           review$document_id[review$decision == "retain"])
)
stopifnot(!any(rejected_ids %in% candidates$document_id),
          !any(rejected_ids %in% corpus$document_id))
history_ids <- unique(changes$document_id[changes$change == "completed_review_rejection"])
stopifnot(setequal(history_ids, rejected_ids))
history <- diagnostics[diagnostics$stage == "completed_review_reconciliation", ]
stopifnot(nrow(history) == 7L, all(history$status %in% c("excluded", "non_document")),
          !any(history$status == "failed"), all(nzchar(history$message)))

dedicated <- corpus[corpus$document_id == "doc_7482c0e648fac8c5", ]
stopifnot(nrow(dedicated) == 2L, setequal(dedicated$language, c("en", "fr")),
          all(dedicated$families == "sanctions"),
          all(dedicated$document_type == "regulatory_review"))
stopifnot(pre_review_classification("Current sanctions imposed by Canada", "Haiti", "")$status == "non_document")
stopifnot(pre_review_classification("Regulatory stock review plan 2024 to 2034 and beyond", "Haiti", "")$status == "non_document")
stopifnot(pre_review_classification(
  "Development and Human Rights / Développement et droits de la personne — 1992-12-09",
  "Haiti appears in a country list.", "")$status == "excluded")
stopifnot(!any(duplicated(candidates$version_id)),
          !any(duplicated(paste(candidates$document_id, candidates$language))))
stopifnot(length(unique(corpus$version_id)) == 209L,
          !any(duplicated(paste(corpus$document_id, corpus$language))))
stopifnot(!any(vapply(candidates[c("title", "relevance_excerpt", "extracted_text")],
                      contains_mojibake, logical(1))))
stopifnot(all(review$document_id %in% corpus$document_id),
          all(corpus$document_id %in% review$document_id))
retained_candidates <- candidates[candidates$document_id %in%
  review$document_id[review$decision == "retain"], ]
stopifnot(setequal(corpus$version_id, retained_candidates$version_id))
paired <- split(corpus, corpus$document_id)
paired <- paired[vapply(paired, nrow, integer(1)) > 1L]
stopifnot(all(vapply(paired, function(rows) {
  length(unique(rows$publication_date[nzchar(rows$publication_date)])) <= 1L &&
    length(unique(rows$families)) == 1L &&
    length(unique(rows$document_type)) == 1L
}, logical(1))))
cat("Completed-review offline regression passed\n")

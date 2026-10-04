# Offline reconciliation regression for both historical-pending and completed-review states.
# Reads project data without writing. The completed state delegates to its focused
# fixture; the historical branch verifies consolidation and diagnostic invariants.

source("R/utils.R")
source("R/discover.R")
source("R/extract.R")
source("R/crawl.R")
source("R/reconcile.R")

candidates <- safe_read_csv("data/candidates.csv")
review <- safe_read_csv("data/review.csv")
if (all(review$decision == "retain") &&
    setequal(review$document_id, unique(candidates$document_id))) {
  source("scripts/regression_completed_review_offline.R")
  quit(save = "no", status = 0L)
}
historical <- candidates[candidates$source_id == "historical_parliament" &
  candidates$document_id %in% review$document_id[review$decision == "pending"], ]
stopifnot(nrow(historical) == 13L)
stopifnot(length(unique(historical$document_id)) == 13L)
stopifnot(all(historical$language == "bilingual"))
stopifnot(all(grepl("^https://", historical$document_url)))
stopifnot(all(nzchar(historical$component_urls)))
stopifnot(!any(grepl("Historical House publication|House of Commons Debates",
                    historical$title, fixed = FALSE)))
stopifnot(!any(grepl("(?:files/get|view)/[^/]+/[0-9]+$", historical$document_id,
                    perl = TRUE)))
stopifnot(!any(duplicated(historical$document_id)))
stopifnot(!any(duplicated(candidates$version_id)))
stopifnot(!any(duplicated(paste(candidates$document_id, candidates$language))))
stopifnot(identical(sort(historical_publication_spec()$publication_date),
                    sort(historical$publication_date)))
second <- consolidate_historical_candidates(historical)
stopifnot(identical(historical[, candidate_columns], second[, candidate_columns]))
stopifnot(identical(as.integer(table(review$decision)[c("retain", "pending")]),
                    c(112L, 28L)))
stopifnot(!"reject" %in% review$decision)
stopifnot(!any(vapply(candidates[c("title", "relevance_excerpt", "extracted_text")],
                      contains_mojibake, logical(1))))
diagnostics <- safe_read_csv("data/retrieval_diagnostics.csv")
recent <- diagnostics[diagnostics$stage == "pre_review_filter", ]
stopifnot(nrow(recent) >= 23L)
stopifnot(!any(recent$status == "failed"))
cat("Offline reconciliation regression passed\n")

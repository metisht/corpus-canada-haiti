#!/usr/bin/env Rscript
# Targeted recovery of the four 1994/2005 defence-policy PDFs exposed by
# official Government of Canada Publications records. This script does not run
# any other connector and never rebuilds the retained corpus.

for (file in c("R/utils.R", "R/http.R", "R/discover.R", "R/extract.R",
               "R/crawl.R"))
  source(file, encoding = "UTF-8")

cfg <- yaml::read_yaml("config/case.yml")
sources <- safe_read_csv("config/sources.csv")
source_row <- sources[sources$source_id == "defence_policy_index", , drop = FALSE]
stopifnot(nrow(source_row) == 1L)

configured_pages <- trimws(strsplit(source_row$start_url, "\\|")[[1]])
archive_pages <- configured_pages[
  grepl("publications\\.gc\\.ca/site/(eng|fra)/", configured_pages,
        ignore.case = TRUE)
]
stopifnot(length(archive_pages) == 4L)
source_row$start_url <- paste(archive_pages, collapse = "|")

document_urls <- discover_defence_policy_index(source_row, cfg, force = TRUE)
target_keys <- c(
  "defence_policy:white_paper:1994",
  "defence_policy:international_policy_statement_defence:2005"
)
document_urls <- document_urls[
  vapply(document_urls, defence_policy_document_key, character(1)) %in%
    target_keys
]
stopifnot(length(document_urls) == 4L)

terms <- safe_read_csv("config/terms.csv")
families <- safe_read_csv("config/families.csv")
records <- lapply(document_urls, function(url) {
  record <- process_url(url, source_row, cfg, terms, families, force = TRUE)
  bytes <- readBin(cache_path(url, "pdf"), "raw", n = 5L)
  stopifnot(is_pdf_bytes(bytes), identical(record$format, "PDF"))
  record
})
fresh <- bind_rows_base(records)

stopifnot(
  nrow(fresh) == 4L,
  length(unique(fresh$document_id)) == 2L,
  all(table(fresh$document_id) == 2L),
  all(vapply(split(fresh$language, fresh$document_id),
             function(x) setequal(x, c("en", "fr")), logical(1))),
  all(fresh$review_status == "pending"),
  all(grepl("Haiti|Haïti", fresh$extracted_text, ignore.case = TRUE))
)

old_candidates <- safe_read_csv("data/candidates.csv")
old_review <- safe_read_csv("data/review.csv")
old_corpus <- safe_read_csv("data/corpus.csv")
old_diagnostics <- safe_read_csv("data/retrieval_diagnostics.csv")
target_documents <- unique(fresh$document_id)
existing_target <- old_candidates[
  old_candidates$document_id %in% target_documents, , drop = FALSE
]
identity_mismatch <- nrow(existing_target) > 0L &&
  !setequal(existing_target$version_id, fresh$version_id)
preserved_old <- if (identity_mismatch) {
  old_candidates[!old_candidates$document_id %in% target_documents, ,
                 drop = FALSE]
} else {
  old_candidates
}
new_records <- fresh[!fresh$version_id %in% preserved_old$version_id, ,
                     drop = FALSE]
combined <- deduplicate_candidates(bind_rows_base(list(preserved_old,
                                                       new_records)))
fresh_match <- match(fresh$version_id, combined$version_id)
stopifnot(!anyNA(fresh_match))
combined$source_page[fresh_match] <- fresh$source_page
review <- sync_review(combined, old_review)
combined <- apply_review(combined, review)

old_match <- match(preserved_old$version_id, combined$version_id)
stopifnot(!anyNA(old_match))
for (column in setdiff(candidate_columns,
                       c("review_status", "review_notes", "source_page")))
  stopifnot(identical(preserved_old[[column]], combined[[column]][old_match]))
candidate_changed <- identity_mismatch || nrow(new_records) > 0L ||
  !identical(preserved_old$source_page,
             combined$source_page[old_match])

old_retain <- old_review[old_review$decision == "retain", review_columns,
                         drop = FALSE]
new_retain <- review[review$decision == "retain", review_columns, drop = FALSE]
stopifnot(identical(old_retain, new_retain))
stopifnot(all(review$decision[review$document_id %in% fresh$document_id] ==
              "pending"))

combined <- combined[
  order(combined$year, combined$title, na.last = TRUE),
  candidate_columns,
  drop = FALSE
]

diagnostic_message <- paste(
  "Official archive interstitial resolved with its session cookie and",
  "referrer; final response verified as PDF by MIME type and file signature"
)
run_id <- paste0(
  "run_defence_archive_",
  format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
)
new_diagnostics <- lapply(document_urls, function(url) {
  already_recorded <- nrow(old_diagnostics) && any(
    old_diagnostics$source_id == "defence_policy_index" &
      old_diagnostics$url == url &
      old_diagnostics$stage == "retrieval" &
      old_diagnostics$status == "completed" &
      old_diagnostics$message == diagnostic_message
  )
  if (already_recorded) return(NULL)
  record_diagnostic(
    source_id = "defence_policy_index",
    family = "defence_policy",
    url = url,
    stage = "retrieval",
    status = "completed",
    message = diagnostic_message,
    run_id = run_id
  )
})
new_diagnostics <- bind_rows_base(new_diagnostics)
diagnostics <- bind_rows_base(list(old_diagnostics, new_diagnostics))

if (candidate_changed) {
  write_csv(combined, "data/candidates.csv")
  write_csv(review, "data/review.csv")
}
if (nrow(new_diagnostics))
  write_csv(diagnostics, "data/retrieval_diagnostics.csv")

stopifnot(identical(old_corpus, safe_read_csv("data/corpus.csv")))
cat(sprintf(
  "Recovered %d official versions in %d logical documents; %d diagnostics added.\n",
  nrow(fresh), length(unique(fresh$document_id)), nrow(new_diagnostics)
))

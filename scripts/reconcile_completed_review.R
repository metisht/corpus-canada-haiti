#!/usr/bin/env Rscript
# Reconciles the authoritative completed review into candidates and corpus.
# Inputs are the active candidate, review, and validation-history CSV files.
# Outputs are deterministic candidates, corpus, and append-only audit records.
# No network access occurs. Human review fields and stable identities are invariants.

for (file in c("R/utils.R", "R/crawl.R"))
  source(file, encoding = "UTF-8")

candidate_path <- "data/candidates.csv"
review_path <- "data/review.csv"
corpus_path <- "data/corpus.csv"
changes_path <- "data/validation_changes.csv"

candidates <- safe_read_csv(candidate_path)
review <- safe_read_csv(review_path)
changes <- safe_read_csv(changes_path)
review_before <- review[, review_columns, drop = FALSE]

if (nrow(review) != length(unique(review$document_id)) ||
    any(review$decision != "retain"))
  stop("Expected one authoritative retain decision per logical document")
if (!setequal(review$document_id, unique(candidates$document_id)))
  stop("Candidate and authoritative review document identities differ")
expected_documents <- nrow(review)
expected_versions <- nrow(candidates)

metadata_before <- candidates

official_dates <- c(
  doc_c66aba07877243fe = "2010-05-21",
  doc_79aec294241bb2bc = "2010-07-12",
  doc_7cecb5ad0657869c = "2010-01-25",
  doc_5c4fd786da08dd80 = "2010-07-12",
  doc_3be7e12758d82756 = "2021-07-13",
  doc_f58312cd85500bb9 = "2025-09-25"
)
for (document_id in names(official_dates)) {
  index <- candidates$document_id == document_id
  if (sum(index) != 2L || !setequal(candidates$language[index], c("en", "fr")))
    stop("Expected a bilingual pair for metadata correction: ", document_id)
  candidates$publication_date[index] <- official_dates[[document_id]]
  candidates$year[index] <- as.integer(substr(official_dates[[document_id]], 1, 4))
  candidates$date_source[index] <- "explicit official document date"
  candidates$date_status[index] <- "in_range"
}

readouts <- candidates$document_id %in%
  c("doc_3be7e12758d82756", "doc_f58312cd85500bb9")
candidates$document_type[readouts] <- "readout"

briefing <- candidates$document_id == "doc_f97a96bc7941acb2"
if (sum(briefing) != 2L || !setequal(candidates$language[briefing], c("en", "fr")))
  stop("Expected the bilingual 2024-05-06 Supplementary Estimates briefing")
candidates$event_start_date[briefing] <- "2024-05-06"
candidates$event_end_date[briefing] <- ""
candidates$publication_date[briefing] <- "2024-09-03"
candidates$year[briefing] <- 2024L
candidates$date_source[briefing] <-
  "event_start_date; official page publication date 2024-09-03"
candidates$date_status[briefing] <- "in_range"

annual <- candidates$document_id == "doc_d2d9aac6534a54ce"
candidates$publication_date[annual] <- ""
candidates$event_start_date[annual] <- ""
candidates$event_end_date[annual] <- ""
candidates$year[annual] <- 2025L
candidates$date_source[annual] <-
  "official 2024–2025 reporting period; exact publication date unavailable"
candidates$date_status[annual] <- "in_range"

plan <- candidates$document_id == "doc_9ec69e5108acfcad"
candidates$publication_date[plan] <- ""
candidates$event_start_date[plan] <- ""
candidates$event_end_date[plan] <- ""
candidates$year[plan] <- NA_integer_
candidates$date_source[plan] <-
  "publication date uncertain; 2023–2029 is the plan period"
candidates$date_status[plan] <- "uncertain"

metadata_columns <- c(
  "publication_date", "page_last_modified", "event_start_date",
  "event_end_date", "year", "date_source", "date_status", "document_type"
)
changed <- vapply(seq_len(nrow(candidates)), function(index) {
  !identical(as.character(metadata_before[index, metadata_columns]),
             as.character(candidates[index, metadata_columns]))
}, logical(1))

review_index <- match(candidates$document_id, review$document_id)
if (any(changed)) {
  old_state <- apply(metadata_before[changed, metadata_columns, drop = FALSE], 1,
    function(row) paste(names(row), row, sep = "=", collapse = "; "))
  new_state <- apply(candidates[changed, metadata_columns, drop = FALSE], 1,
    function(row) paste(names(row), row, sep = "=", collapse = "; "))
  corrected <- candidates[changed, , drop = FALSE]
  corrected_review <- review[review_index[changed], , drop = FALSE]
  metadata_audit <- data.frame(
    change = "completed_review_metadata_correction",
    version_id = corrected$version_id,
    document_id = corrected$document_id,
    language = corrected$language,
    title = corrected$title,
    document_url = corrected$document_url,
    review_status = "retain",
    old_document_id = corrected$document_id,
    new_document_id = corrected$document_id,
    rule = "authoritative_manual_review_metadata",
    reason = paste0("Before: ", old_state, " | After: ", new_state),
    source = corrected$source_id,
    previous_state = old_state,
    resulting_state = new_state,
    manual_decision = corrected_review$decision,
    reviewer = corrected_review$reviewer,
    review_timestamp = corrected_review$reviewed_at,
    stringsAsFactors = FALSE
  )
  changes <- bind_rows_base(list(changes, metadata_audit))
}

target_ids <- c(names(official_dates), "doc_f97a96bc7941acb2",
                "doc_d2d9aac6534a54ce", "doc_9ec69e5108acfcad")
target_rows <- candidates[candidates$document_id %in% target_ids, , drop = FALSE]
target_review <- review[match(target_rows$document_id, review$document_id),
                        , drop = FALSE]
audit_descriptions <- c(
  doc_c66aba07877243fe = "Official document date set to 2010-05-21.",
  doc_79aec294241bb2bc = "Official document date set to 2010-07-12.",
  doc_7cecb5ad0657869c = "Official document date set to 2010-01-25.",
  doc_5c4fd786da08dd80 = "Official document date set to 2010-07-12.",
  doc_3be7e12758d82756 = "Official date set to 2021-07-13 and document type set to readout.",
  doc_f58312cd85500bb9 = "Meeting date set to 2025-09-25 and document type set to readout; page modification remains separate.",
  doc_f97a96bc7941acb2 = "Appearance date stored as event_start_date 2024-05-06; page publication stored as publication_date 2024-09-03; modification remains separate.",
  doc_d2d9aac6534a54ce = "2024–2025 retained as a reporting period; no exact publication date assigned.",
  doc_9ec69e5108acfcad = "2023–2029 retained as a plan period; publication date and year remain uncertain."
)
target_state <- apply(target_rows[metadata_columns], 1, function(row)
  paste(names(row), row, sep = "=", collapse = "; "))
target_audit <- data.frame(
  change = "completed_review_metadata_correction",
  version_id = target_rows$version_id,
  document_id = target_rows$document_id,
  language = target_rows$language,
  title = target_rows$title,
  document_url = target_rows$document_url,
  review_status = "retain",
  old_document_id = target_rows$document_id,
  new_document_id = target_rows$document_id,
  rule = "authoritative_manual_review_metadata",
  reason = unname(audit_descriptions[target_rows$document_id]),
  source = target_rows$source_id,
  previous_state = "candidate metadata before authoritative review correction",
  resulting_state = target_state,
  manual_decision = target_review$decision,
  reviewer = target_review$reviewer,
  review_timestamp = target_review$reviewed_at,
  stringsAsFactors = FALSE
)
changes <- bind_rows_base(list(changes, target_audit))

stale_status <- candidates$review_status != "retain"
if (any(stale_status)) {
  corrected <- candidates[stale_status, , drop = FALSE]
  corrected_review <- review[match(corrected$document_id, review$document_id),
                             , drop = FALSE]
  status_audit <- data.frame(
    change = "document_review_status_propagated",
    version_id = corrected$version_id,
    document_id = corrected$document_id,
    language = corrected$language,
    title = corrected$title,
    document_url = corrected$document_url,
    review_status = "retain",
    old_document_id = corrected$document_id,
    new_document_id = corrected$document_id,
    rule = "authoritative_document_level_review",
    reason = paste0("Review decision retain propagated to version; previous review_status=",
                    corrected$review_status),
    source = corrected$source_id,
    previous_state = corrected$review_status,
    resulting_state = "retain",
    manual_decision = corrected_review$decision,
    reviewer = corrected_review$reviewer,
    review_timestamp = corrected_review$reviewed_at,
    stringsAsFactors = FALSE
  )
  changes <- bind_rows_base(list(changes, status_audit))
}

completed_ids <- c(target_ids, "doc_01014bd134ebab17",
                   "doc_bf16e0a427deba37")
completed_rows <- candidates[candidates$document_id %in% completed_ids,
                             , drop = FALSE]
completed_review <- review[match(completed_rows$document_id, review$document_id),
                           , drop = FALSE]
completed_status_audit <- data.frame(
  change = "document_review_status_propagated",
  version_id = completed_rows$version_id,
  document_id = completed_rows$document_id,
  language = completed_rows$language,
  title = completed_rows$title,
  document_url = completed_rows$document_url,
  review_status = "retain",
  old_document_id = completed_rows$document_id,
  new_document_id = completed_rows$document_id,
  rule = "authoritative_document_level_review",
  reason = "Completed human-review decision retain propagated to active version.",
  source = completed_rows$source_id,
  previous_state = "pending",
  resulting_state = "retain",
  manual_decision = completed_review$decision,
  reviewer = completed_review$reviewer,
  review_timestamp = completed_review$reviewed_at,
  stringsAsFactors = FALSE
)
changes <- bind_rows_base(list(changes, completed_status_audit))

pco_id <- "doc_bf16e0a427deba37"
spatial_queue <- safe_read_csv("data/spatial_review_queue.csv")
spatial_decisions <- spatial_queue[
  spatial_queue$document_id == pco_id &
    spatial_queue$coding_decision == "confirm",
  , drop = FALSE
]
expected_spatial_codes <- c(
  "neighbourhood", "migration_mobility", "bilateral", "caribbean"
)
if (nrow(spatial_decisions) != length(expected_spatial_codes) ||
    !setequal(spatial_decisions$proposed_code, expected_spatial_codes))
  stop("Expected four confirmed PCO spatial decisions")
pco_rows <- candidates[candidates$document_id == pco_id, , drop = FALSE]
spatial_audit <- bind_rows_base(lapply(seq_len(nrow(spatial_decisions)),
  function(index) {
    decision <- spatial_decisions[index, , drop = FALSE]
    data.frame(
      change = "spatial_review_decision_propagated",
      version_id = pco_rows$version_id,
      document_id = pco_rows$document_id,
      language = pco_rows$language,
      title = pco_rows$title,
      document_url = pco_rows$document_url,
      review_status = "retain",
      old_document_id = pco_rows$document_id,
      new_document_id = pco_rows$document_id,
      rule = paste0(
        "authoritative_spatial_review:", decision$dimension, "/",
        decision$proposed_code
      ),
      reason = decision$review_notes,
      source = pco_rows$source_id,
      previous_state = "pending",
      resulting_state = "confirm",
      manual_decision = decision$coding_decision,
      reviewer = decision$reviewer,
      review_timestamp = decision$reviewed_at,
      stringsAsFactors = FALSE
    )
  }
))
changes <- bind_rows_base(list(changes, spatial_audit))

candidates <- apply_review(candidates, review)
candidates <- candidates[order(candidates$year, candidates$title,
                               candidates$language, na.last = TRUE),
                         candidate_columns, drop = FALSE]
corpus <- candidates[candidates$document_id %in%
  review$document_id[review$decision == "retain"], candidate_columns,
  drop = FALSE]

audit_key <- paste(changes$change, changes$version_id, changes$rule, sep = "|")
changes <- changes[!duplicated(audit_key), , drop = FALSE]
changes[] <- lapply(changes, as.character)

briefing_final <- candidates$document_id == "doc_f97a96bc7941acb2"
annual_final <- candidates$document_id == "doc_d2d9aac6534a54ce"
plan_final <- candidates$document_id == "doc_9ec69e5108acfcad"

stopifnot(
  identical(review_before, review[, review_columns, drop = FALSE]),
  nrow(candidates) == expected_versions,
  length(unique(candidates$document_id)) == expected_documents,
  nrow(corpus) == expected_versions,
  length(unique(corpus$document_id)) == expected_documents,
  all(candidates$review_status == "retain"),
  all(corpus$review_status == "retain"),
  setequal(unique(corpus$document_id), review$document_id),
  setequal(corpus$version_id, candidates$version_id),
  !anyDuplicated(candidates$version_id),
  !anyDuplicated(candidates[c("document_id", "language")]),
  all(candidates$publication_date[plan_final] == ""),
  all(is.na(candidates$year[plan_final])),
  all(candidates$publication_date[annual_final] == ""),
  all(candidates$event_start_date[briefing_final] == "2024-05-06"),
  all(candidates$publication_date[briefing_final] == "2024-09-03")
)

write_csv(candidates, candidate_path)
write_csv(corpus, corpus_path)
write_csv(changes, changes_path)

message("Completed-review reconciliation: ", expected_documents,
        " retained logical documents, ",
        nrow(corpus), " retained versions")

# Consolidates page-level historical publications and removes pre-review exclusions.
# Reads active project CSVs, writes candidates/review/diagnostics/change history,
# and protects retained decisions plus corpus bytes. No network access occurs.

source("R/utils.R")
source("R/discover.R")
source("R/extract.R")
source("R/crawl.R")
source("R/reconcile.R")

candidates_path <- "data/candidates.csv"
review_path <- "data/review.csv"
corpus_path <- "data/corpus.csv"
diagnostics_path <- "data/retrieval_diagnostics.csv"
changes_path <- "data/validation_changes.csv"

candidates <- safe_read_csv(candidates_path)
review <- safe_read_csv(review_path)
diagnostics <- safe_read_csv(diagnostics_path)
old_changes <- safe_read_csv(changes_path)
corpus_hash <- unname(tools::md5sum(corpus_path))

current_counts <- table(review$decision)
already_reconciled <- identical(as.integer(current_counts[c("retain", "pending")]),
                                c(112L, 28L)) &&
  !"reject" %in% names(current_counts)
if (already_reconciled) {
  historical <- candidates[candidates$source_id == "historical_parliament" &
    candidates$document_id %in% review$document_id[review$decision == "pending"], ]
  gac_ids <- unique(candidates$document_id[
    candidates$source_id == "gac_sanctions" &
      candidates$document_id %in% review$document_id[review$decision == "pending"]])
  if (nrow(historical) != 13L || length(unique(historical$document_id)) != 13L ||
      length(gac_ids) != 15L ||
      !identical(corpus_hash, unname(tools::md5sum(corpus_path)))) {
    stop("Existing reconciliation does not satisfy the validated target")
  }
  message("Already reconciled: 216 candidate rows; review 112 retain, 28 pending, 0 reject")
  quit(save = "no", status = 0L)
}

if (!identical(as.integer(table(review$decision)[c("retain", "reject", "pending")]),
               c(112L, 23L, 49L))) {
  stop("Unexpected starting review counts")
}

retain_review <- review[review$decision == "retain", , drop = FALSE]
retain_candidates <- candidates[candidates$document_id %in% retain_review$document_id,
                                , drop = FALSE]
gac_pending_ids <- review$document_id[
  review$decision == "pending" &
    review$document_id %in% candidates$document_id[candidates$source_id == "gac_sanctions"]]
if (length(unique(gac_pending_ids)) != 15L) stop("Expected 15 pending GAC documents")

historical_pending_ids <- review$document_id[
  review$decision == "pending" &
    review$document_id %in% candidates$document_id[
      candidates$source_id == "historical_parliament"]]
historical_rows <- candidates[candidates$document_id %in% historical_pending_ids,
                              , drop = FALSE]
if (length(unique(historical_pending_ids)) != 34L || nrow(historical_rows) != 53L) {
  stop("Expected 34 page-level historical identities and 53 rows")
}
consolidated <- consolidate_historical_candidates(historical_rows)
if (nrow(consolidated) != 13L) stop("Historical consolidation did not produce 13 documents")

rejected_review <- review[review$decision == "reject", , drop = FALSE]
if (nrow(rejected_review) != 23L) stop("Expected 23 rejected documents")
classifications <- lapply(seq_len(nrow(rejected_review)), function(i) {
  row <- candidates[candidates$document_id == rejected_review$document_id[i], , drop = FALSE][1, ]
  classify_rejected_record(row, rejected_review$notes[i])
})
statuses <- vapply(classifications, `[[`, character(1), "status")
rules <- vapply(classifications, `[[`, character(1), "rule")
reasons <- vapply(classifications, `[[`, character(1), "reason")
if (any(statuses == "failed") || sum(rules == "index_or_finding_aid") != 16L ||
    sum(rules != "index_or_finding_aid") != 7L) {
  stop("Rejected regression cases were not classified as 16 index and 7 other exclusions")
}

remove_ids <- unique(c(historical_pending_ids, rejected_review$document_id))
kept <- candidates[!candidates$document_id %in% remove_ids, , drop = FALSE]
for (column in setdiff(candidate_columns, names(kept))) kept[[column]] <- ""
new_candidates <- bind_rows_base(list(kept[, candidate_columns, drop = FALSE], consolidated))
new_candidates <- new_candidates[order(new_candidates$year, new_candidates$title,
                                       na.last = TRUE), candidate_columns, drop = FALSE]

historical_review <- data.frame(
  document_id = consolidated$document_id,
  version_id = consolidated$version_id,
  decision = "pending", notes = "", reviewer = "", reviewed_at = "",
  stringsAsFactors = FALSE
)
new_review <- review[!review$document_id %in% remove_ids, review_columns, drop = FALSE]
new_review <- bind_rows_base(list(new_review, historical_review))
new_review <- new_review[order(match(new_review$decision, c("retain", "pending")),
                               new_review$document_id), review_columns, drop = FALSE]

run_id <- paste0("reconciliation_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
rejected_candidates <- candidates[match(rejected_review$document_id,
                                        candidates$document_id), , drop = FALSE]
new_diagnostics <- bind_rows_base(lapply(seq_len(nrow(rejected_review)), function(i) {
  record_diagnostic(rejected_candidates$source_id[i],
                    rejected_candidates$families[i],
                    rejected_candidates$document_url[i], "pre_review_filter",
                    statuses[i], paste(rules[i], reasons[i], sep = ": "),
                    run_id = run_id)
}))
new_diagnostics$publication_date <- rejected_candidates$publication_date

historical_changes <- data.frame(
  change = "historical_page_identity_consolidated",
  version_id = historical_rows$version_id,
  document_id = historical_rows$document_id,
  language = historical_rows$language,
  title = historical_rows$title,
  document_url = historical_rows$document_url,
  review_status = "pending",
  old_document_id = historical_rows$document_id,
  new_document_id = consolidated$document_id[
    match(paste(vapply(seq_len(nrow(historical_rows)), function(i)
      historical_publication_type(historical_rows$document_url[i]), character(1)),
      vapply(seq_len(nrow(historical_rows)), function(i)
        historical_row_date(historical_rows[i, , drop = FALSE]), character(1))),
      paste(historical_publication_spec()$publication_type,
            historical_publication_spec()$publication_date))],
  rule = "publication_level_identity",
  reason = "Canadiana page identity replaced by authoritative publication and date identity.",
  stringsAsFactors = FALSE
)
rejected_changes <- data.frame(
  change = "removed_from_active_review_queue",
  version_id = rejected_candidates$version_id,
  document_id = rejected_candidates$document_id,
  language = rejected_candidates$language,
  title = rejected_candidates$title,
  document_url = rejected_candidates$document_url,
  review_status = "excluded",
  old_document_id = rejected_candidates$document_id,
  new_document_id = "", rule = rules, reason = reasons,
  stringsAsFactors = FALSE
)
new_changes <- bind_rows_base(list(old_changes, historical_changes, rejected_changes))

retained_review_check <- new_review[match(retain_review$document_id,
                                          new_review$document_id),
                                    review_columns, drop = FALSE]
rownames(retain_review) <- NULL
rownames(retained_review_check) <- NULL
if (!identical(retain_review, retained_review_check)) {
  stop("Retained review decisions changed")
}
retained_candidate_check <- new_candidates[match(retain_candidates$version_id,
                                                 new_candidates$version_id),
                                           names(retain_candidates), drop = FALSE]
rownames(retain_candidates) <- NULL
rownames(retained_candidate_check) <- NULL
retained_values_equal <- all(vapply(names(retain_candidates), function(column) {
  identical(as.character(retain_candidates[[column]]),
            as.character(retained_candidate_check[[column]]))
}, logical(1)))
if (!retained_values_equal) {
  stop("Retained candidate versions changed: ",
       paste(all.equal(retain_candidates, retained_candidate_check), collapse = "; "))
}
counts <- table(new_review$decision)
if (counts[["retain"]] != 112L || counts[["pending"]] != 28L ||
    "reject" %in% names(counts)) stop("Final review target was not met")
if (sum(new_candidates$document_id %in% gac_pending_ids) != 30L ||
    any(new_review$decision[new_review$document_id %in% gac_pending_ids] != "pending")) {
  stop("The 15 bilingual GAC documents were not preserved as pending")
}
if (any(duplicated(new_candidates$version_id)) ||
    any(duplicated(paste(new_candidates$document_id, new_candidates$language)))) {
  stop("Candidate identifiers are not unique")
}
if (any(vapply(new_candidates[c("title", "relevance_excerpt", "extracted_text")],
               contains_mojibake, logical(1)))) stop("Mojibake detected")
if (!identical(corpus_hash, unname(tools::md5sum(corpus_path)))) stop("corpus.csv changed")

write_csv(new_candidates, candidates_path)
write_csv(new_review, review_path)
write_csv(bind_rows_base(list(diagnostics, new_diagnostics)), diagnostics_path)
write_csv(new_changes, changes_path)

if (!identical(corpus_hash, unname(tools::md5sum(corpus_path)))) stop("corpus.csv changed")
message("Reconciled: ", nrow(candidates), " -> ", nrow(new_candidates),
        " candidate rows; review 112 retain, 28 pending, 0 reject")

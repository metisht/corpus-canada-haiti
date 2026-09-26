# Reconciles the completed manual review into active candidates and corpus data.
# Reads candidates/review/audit CSVs and rewrites them deterministically. It makes
# no network requests; rejection history, GAC corrections, and 137/209 invariants
# are checked before output. Called manually after the confirmed review fixture.

source("R/utils.R")
source("R/discover.R")
source("R/extract.R")
source("R/crawl.R")

rejected_ids <- c("doc_2517f95d9356f704", "doc_3cfa943a7a4225b8",
  "doc_fbb9f96e232bd24f", "doc_05b993ed424cca1c",
  "doc_99667ab611ac03ea", "doc_bde72e3958af0f51")
rejection_reasons <- c(
  "General worldwide sanctions index; the dedicated Haiti sanctions page is retained separately.",
  "Haiti appears only in a brief multilateral summary without an autonomous analytical passage.",
  "Haiti appears in a country list and a brief summary sentence without an autonomous analytical passage.",
  "Haiti appears only in a list of countries covered by human-rights resolutions; the developed discussion concerns Myanmar.",
  "General French regulatory-review index; Haiti is only one listed entry.",
  "English version of the same general regulatory-review index; it was also incorrectly represented as a separate logical document.")
names(rejection_reasons) <- rejected_ids

candidates <- safe_read_csv("data/candidates.csv")
review <- safe_read_csv("data/review.csv")
diagnostics <- safe_read_csv("data/retrieval_diagnostics.csv")
changes <- safe_read_csv("data/validation_changes.csv")
active_rejections <- review[review$document_id %in% rejected_ids &
  review$decision == "reject", , drop = FALSE]
already_reconciled <- !any(review$document_id %in% rejected_ids) &&
  nrow(review) == 137L && all(review$decision == "retain")

if (!already_reconciled) {
  counts <- table(review$decision)
  if (nrow(review) != 143L || counts[["retain"]] != 137L ||
      counts[["reject"]] != 6L || any(review$decision == "pending") ||
      !setequal(active_rejections$document_id, rejected_ids)) {
    stop("Completed-review starting state does not match the confirmed 137/6/0 state")
  }
  if (any(!review$document_id[review$decision == "retain"] %in% candidates$document_id))
    stop("A retained review decision has no matching candidate")
  if (any(candidates$review_status == "retain" &
          !candidates$document_id %in% review$document_id))
    stop("A retained candidate lacks a review decision")

  rejected_versions <- candidates[candidates$document_id %in% rejected_ids,
                                  , drop = FALSE]
  rejected_reviews <- active_rejections[match(rejected_versions$document_id,
                                               active_rejections$document_id), ]
  rejected_status <- ifelse(rejected_versions$document_id %in%
    c("doc_2517f95d9356f704", "doc_99667ab611ac03ea", "doc_bde72e3958af0f51"),
    "non_document", "excluded")
  audit_rejections <- data.frame(
    change = "completed_review_rejection", version_id = rejected_versions$version_id,
    document_id = rejected_versions$document_id, language = rejected_versions$language,
    title = rejected_versions$title, document_url = rejected_versions$document_url,
    review_status = "removed_from_active_queue",
    old_document_id = rejected_versions$document_id, new_document_id = "",
    rule = ifelse(rejected_status == "non_document", "general_index",
                  "below_substantive_threshold"),
    reason = unname(rejection_reasons[rejected_versions$document_id]),
    source = rejected_versions$source_id,
    previous_state = "active_candidate; reviewed reject",
    resulting_state = paste0("removed; diagnostic ", rejected_status),
    manual_decision = rejected_reviews$decision, reviewer = rejected_reviews$reviewer,
    review_timestamp = rejected_reviews$reviewed_at, stringsAsFactors = FALSE)
  diagnostics_rejections <- data.frame(
    source_id = rejected_versions$source_id, family = rejected_versions$families,
    url = rejected_versions$document_url, stage = "completed_review_reconciliation",
    status = rejected_status,
    message = paste("manual_reject:", unname(rejection_reasons[rejected_versions$document_id])),
    http_status = NA_integer_, occurred_at = rejected_reviews$reviewed_at,
    run_id = "manual_review_history", publication_date = rejected_versions$publication_date,
    stringsAsFactors = FALSE)
  existing_diagnostic_keys <- paste(diagnostics$source_id, diagnostics$url,
    diagnostics$stage, diagnostics$status, diagnostics$message, sep="|")
  new_diagnostic_keys <- paste(diagnostics_rejections$source_id,
    diagnostics_rejections$url, diagnostics_rejections$stage,
    diagnostics_rejections$status, diagnostics_rejections$message, sep="|")
  diagnostics_rejections <- diagnostics_rejections[
    !new_diagnostic_keys %in% existing_diagnostic_keys, , drop=FALSE]
  candidates <- candidates[!candidates$document_id %in% rejected_ids, , drop = FALSE]
  review <- review[!review$document_id %in% rejected_ids, , drop = FALSE]
  changes <- bind_rows_base(list(changes, audit_rejections))
  diagnostics <- bind_rows_base(list(diagnostics, diagnostics_rejections))
}

stale_review_status <- candidates$document_id %in%
  review$document_id[review$decision == "retain"] &
  candidates$review_status != "retain"
if (any(stale_review_status)) {
  corrected_status <- candidates[stale_review_status, , drop=FALSE]
  status_audit <- data.frame(
    change="document_review_status_propagated",
    version_id=corrected_status$version_id,
    document_id=corrected_status$document_id,
    language=corrected_status$language,
    title=corrected_status$title,
    document_url=corrected_status$document_url,
    review_status="retain",
    old_document_id=corrected_status$document_id,
    new_document_id=corrected_status$document_id,
    rule="authoritative_document_level_review",
    reason=paste0("Review decision retain propagated to version; previous review_status=",
                  corrected_status$review_status),
    source=corrected_status$source_id,
    previous_state=corrected_status$review_status,
    resulting_state="retain", manual_decision="retain",
    reviewer="", review_timestamp="", stringsAsFactors=FALSE)
  changes <- bind_rows_base(list(changes, status_audit))
}
candidates <- apply_review(candidates, review)

retained_gac <- candidates$source_id %in% c("gac_news", "gac_sanctions") &
  candidates$document_id %in% review$document_id[review$decision == "retain"]
before_gac <- candidates[retained_gac, , drop = FALSE]
dated_documents <- c(doc_8beb617c16e0a8f8="2022-11-04",
  doc_d4ba178c32fff813="2022-11-04", doc_98ab02dc33b997a7="2022-11-14",
  doc_a440cfd066679311="2022-11-19", doc_ed6882b9667e1c7a="2022-12-05",
  doc_69d48fb2875b3537="2022-12-20", doc_446ed6e0029a3c15="2023-01-13",
  doc_951a3585c8cc0883="2023-06-15", doc_efb13f470393d5e5="2023-09-21",
  doc_6c20483be6a4fdae="2024-06-21", doc_2d57be6a22f9f43b="2024-06-21",
  doc_83e9a22aa6a888b4="2025-03-21", doc_f5a69ac74f7e5499="2025-03-21")
for (document_id in names(dated_documents)) {
  index <- candidates$document_id == document_id
  candidates$publication_date[index] <- dated_documents[[document_id]]
  candidates$year[index] <- as.integer(substr(dated_documents[[document_id]], 1, 4))
  candidates$date_source[index] <- "explicit official release date"
  candidates$date_status[index] <- "in_range"
}
evergreen <- candidates$document_id %in% c("doc_2535ac242fc24e1b",
  "doc_7482c0e648fac8c5", "doc_7ca740d5aef72797")
candidates$publication_date[evergreen] <- ""
candidates$year[evergreen] <- NA_integer_
candidates$date_source[evergreen] <- "page modification date only"
candidates$date_status[evergreen] <- "uncertain"
relations <- candidates$document_id == "doc_7ca740d5aef72797"
candidates$families[relations] <- "development"
candidates$document_type[relations] <- "country_relations"
meeting <- candidates$document_id == "doc_951a3585c8cc0883"
candidates$families[meeting] <- "statements"
candidates$document_type[meeting] <- "news_release"

for (document_id in unique(candidates$document_id[retained_gac])) {
  index <- which(candidates$document_id == document_id)
  if (length(index) < 2L) next
  preferred <- index[match("en", candidates$language[index])]
  if (is.na(preferred)) preferred <- index[1]
  candidates$families[index] <- candidates$families[preferred]
  candidates$document_type[index] <- candidates$document_type[preferred]
}

after_gac <- candidates[match(before_gac$version_id, candidates$version_id), , drop = FALSE]
metadata_columns <- c("publication_date", "year", "date_source", "date_status",
                      "families", "document_type")
metadata_changed <- vapply(seq_len(nrow(before_gac)), function(i) {
  !identical(as.character(before_gac[i, metadata_columns]),
             as.character(after_gac[i, metadata_columns]))
}, logical(1))
if (any(metadata_changed)) {
  old_values <- apply(before_gac[metadata_changed, metadata_columns, drop=FALSE], 1,
    function(row) paste(names(row), row, sep="=", collapse="; "))
  new_values <- apply(after_gac[metadata_changed, metadata_columns, drop=FALSE], 1,
    function(row) paste(names(row), row, sep="=", collapse="; "))
  corrected <- after_gac[metadata_changed, , drop=FALSE]
  audit_metadata <- data.frame(change="authoritative_gac_metadata_correction",
    version_id=corrected$version_id, document_id=corrected$document_id,
    language=corrected$language, title=corrected$title,
    document_url=corrected$document_url, review_status="retain",
    old_document_id=corrected$document_id, new_document_id=corrected$document_id,
    rule="bilingual_authoritative_metadata",
    reason=paste0("Before: ", old_values, " | After: ", new_values),
    source=corrected$source_id, previous_state=old_values, resulting_state=new_values,
    manual_decision="retain", reviewer="", review_timestamp="",
    stringsAsFactors=FALSE)
  changes <- bind_rows_base(list(changes, audit_metadata))
}

audit_key <- paste(changes$change, changes$version_id, changes$rule, changes$reason, sep="|")
changes <- changes[!duplicated(audit_key), , drop=FALSE]
review <- review[order(review$document_id), review_columns, drop=FALSE]
review[] <- lapply(review, as.character)
candidates <- candidates[order(candidates$year, candidates$title, na.last=TRUE),
                         candidate_columns, drop=FALSE]
corpus <- candidates[candidates$document_id %in%
  review$document_id[review$decision == "retain"], , drop=FALSE]

if (nrow(review) != 137L || any(review$decision != "retain") ||
    length(unique(review$document_id)) != 137L) stop("Active review is not 137 retain")
if (any(rejected_ids %in% candidates$document_id) || any(rejected_ids %in% corpus$document_id))
  stop("Rejected identity remains active")
if (any(!review$document_id %in% corpus$document_id) ||
    any(!corpus$document_id %in% review$document_id)) stop("Corpus/review mismatch")
if (nrow(corpus) != 209L || length(unique(corpus$document_id)) != 137L ||
    any(corpus$review_status != "retain")) stop("Retained corpus status propagation failed")
if (any(duplicated(candidates$version_id)) ||
    any(duplicated(paste(candidates$document_id, candidates$language))))
  stop("Duplicate candidate identity")
if (any(vapply(candidates[c("title","relevance_excerpt","extracted_text")],
               contains_mojibake, logical(1)))) stop("Mojibake remains")

changes[] <- lapply(changes, as.character)

write_csv(candidates, "data/candidates.csv")
write_csv(review, "data/review.csv")
write_csv(corpus, "data/corpus.csv")
write_csv(diagnostics, "data/retrieval_diagnostics.csv")
write_csv(changes, "data/validation_changes.csv")
message("Completed-review reconciliation: 137 retained logical documents, ",
        nrow(corpus), " retained versions")

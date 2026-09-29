#!/usr/bin/env Rscript
# Reconcile completed concept decisions into active candidate/corpus versions.
# This is offline, deterministic, conflict-intolerant, and idempotent.

source("R/utils.R", encoding = "UTF-8")
source("R/extract.R", encoding = "UTF-8")

queue_path <- "data/concept_review_queue.csv"
audit_path <- "data/concept_existing_assignment_audit.csv"
decision_path <- "data/concept_decisions.csv"
candidate_path <- "data/candidates.csv"
corpus_path <- "data/corpus.csv"
changes_path <- "data/validation_changes.csv"

queue <- readr::read_csv(queue_path, show_col_types = FALSE)
audit <- readr::read_csv(audit_path, show_col_types = FALSE)
candidates <- readr::read_csv(candidate_path, show_col_types = FALSE)
corpus <- readr::read_csv(corpus_path, show_col_types = FALSE)
changes <- readr::read_csv(changes_path, show_col_types = FALSE)
terms <- readr::read_csv("config/terms.csv", show_col_types = FALSE)

present <- function(value) !is.na(value) & nzchar(trimws(as.character(value)))
collapse_unique <- function(value, separator = " | ") {
  value <- unique(trimws(as.character(value[present(value)])))
  paste(value, collapse = separator)
}
split_codes <- function(value) {
  value <- value[present(value)]
  if (!length(value)) return(character())
  codes <- trimws(unlist(strsplit(value, "\\|")))
  sort(unique(codes[nzchar(codes)]))
}
normalize_excerpt <- function(value, limit = 520L) {
  value <- trimws(gsub("\\s+", " ", ifelse(is.na(value), "", value), perl = TRUE))
  if (nchar(value) > limit) paste0(substr(value, 1L, limit - 1L), "…") else value
}

expected_decisions <- c("confirm", "dismiss")
if (any(!queue$coding_decision %in% expected_decisions) ||
    any(!audit$coding_decision %in% expected_decisions)) {
  stop("Concept review is incomplete: pending or invalid decisions remain")
}
if (nrow(queue) != 139L || sum(queue$coding_decision == "confirm") != 126L ||
    sum(queue$coding_decision == "dismiss") != 13L) {
  stop("Unexpected completed new-proposal decision totals")
}
if (nrow(audit) != 101L || sum(audit$coding_decision == "confirm") != 58L ||
    sum(audit$coding_decision == "dismiss") != 43L) {
  stop("Unexpected completed existing-assignment audit totals")
}
if (anyDuplicated(paste(queue$document_id, queue$proposed_concept))) {
  stop("Duplicate document_id + concept in new-proposal decisions")
}
if (anyDuplicated(paste(audit$document_id, audit$concept))) {
  stop("Duplicate document_id + concept in existing-assignment audit")
}

queue_evidence <- function(row, language) {
  phrase <- if (language == "en") row$matched_english_phrase else row$matched_french_phrase
  excerpts <- unlist(row[c("evidence_excerpt_1", "evidence_excerpt_2", "evidence_excerpt_3")])
  prefix <- if (language == "en") "^\\[(en|bilingual)\\]" else "^\\[(fr|bilingual)\\]"
  excerpts <- excerpts[present(excerpts) & grepl(prefix, excerpts)]
  collapse_unique(c(
    if (present(phrase)) paste0("Matched phrase: ", phrase) else character(),
    vapply(excerpts, normalize_excerpt, character(1))
  ), " || ")
}

nearest_term_evidence <- function(document_id, concept, language) {
  rows <- corpus[corpus$document_id == document_id &
    (corpus$language == language | corpus$language == "bilingual"), , drop = FALSE]
  if (!nrow(rows)) return("")
  term <- terms[terms$term_id == concept, , drop = FALSE]
  if (nrow(term) != 1L) stop("Unknown configured concept: ", concept)
  contextual <- lapply(seq_len(nrow(rows)), function(index) {
    match_concepts_contextually(
      rows$extracted_text[index], term,
      relevance_text = rows$relevance_excerpt[index],
      title = rows$title[index],
      document_type = rows$document_type[index],
      language = rows$language[index]
    )
  })
  contextual <- Filter(function(value) nrow(value), contextual)
  if (length(contextual)) {
    evidence <- do.call(rbind, contextual)
    return(collapse_unique(vapply(
      head(unique(evidence$context_excerpt), 3L), normalize_excerpt, character(1)
    ), " || "))
  }
  pattern <- normalize_concept_pattern(term$pattern)
  candidates_evidence <- list()
  evidence_index <- 0L
  for (index in seq_len(nrow(rows))) {
    text <- ifelse(is.na(rows$extracted_text[index]), "", rows$extracted_text[index])
    matches <- gregexpr(pattern, text, ignore.case = TRUE, perl = TRUE)[[1]]
    if (matches[1] == -1L) next
    haiti <- gregexpr(
      haiti_reference_pattern(), text, ignore.case = TRUE, perl = TRUE
    )[[1]]
    distances <- if (haiti[1] == -1L) rep(Inf, length(matches)) else
      vapply(matches, function(position) min(abs(position - haiti)), numeric(1))
    selected <- which.min(distances)
    position <- matches[selected]
    evidence_index <- evidence_index + 1L
    candidates_evidence[[evidence_index]] <- data.frame(
      distance = distances[selected],
      excerpt = normalize_excerpt(substr(
        text, max(1L, position - 240L), min(nchar(text), position + 420L)
      )),
      stringsAsFactors = FALSE
    )
  }
  if (!length(candidates_evidence)) return("")
  evidence <- do.call(rbind, candidates_evidence)
  evidence$excerpt[order(evidence$distance)][1]
}

proposal_rows <- lapply(seq_len(nrow(queue)), function(index) {
  row <- queue[index, , drop = FALSE]
  data.frame(
    document_id = row$document_id,
    concept = row$proposed_concept,
    decision = row$coding_decision,
    decision_origin = "new_proposal",
    reviewer = row$reviewer,
    review_notes = row$review_notes,
    reviewed_at = row$reviewed_at,
    english_evidence = queue_evidence(row, "en"),
    french_evidence = queue_evidence(row, "fr"),
    matching_rule = row$matching_rule,
    stringsAsFactors = FALSE
  )
})
proposal_decisions <- do.call(rbind, proposal_rows)

audit_rows <- lapply(seq_len(nrow(audit)), function(index) {
  row <- audit[index, , drop = FALSE]
  data.frame(
    document_id = row$document_id,
    concept = row$concept,
    decision = row$coding_decision,
    decision_origin = "existing_assignment_audit",
    reviewer = row$reviewer,
    review_notes = row$review_notes,
    reviewed_at = row$reviewed_at,
    english_evidence = nearest_term_evidence(row$document_id, row$concept, "en"),
    french_evidence = nearest_term_evidence(row$document_id, row$concept, "fr"),
    matching_rule = row$matching_rules,
    stringsAsFactors = FALSE
  )
})
audit_decisions <- do.call(rbind, audit_rows)

proposal_key <- paste(proposal_decisions$document_id, proposal_decisions$concept)
audit_key <- paste(audit_decisions$document_id, audit_decisions$concept)
overlap <- intersect(proposal_key, audit_key)
if (length(overlap)) {
  proposal_overlap <- proposal_decisions$decision[match(overlap, proposal_key)]
  audit_overlap <- audit_decisions$decision[match(overlap, audit_key)]
  conflicts <- overlap[proposal_overlap != audit_overlap]
  if (length(conflicts)) {
    stop("Conflicting concept decisions: ", paste(conflicts, collapse = ", "))
  }
  # A compatible overlap is an audited prior assignment, so the audit row is
  # authoritative and the duplicate proposal row is omitted.
  proposal_decisions <- proposal_decisions[!proposal_key %in% overlap, , drop = FALSE]
}

decisions <- rbind(proposal_decisions, audit_decisions)
decisions <- decisions[order(decisions$document_id, decisions$concept), , drop = FALSE]
decision_key <- paste(decisions$document_id, decisions$concept)
if (anyDuplicated(decision_key)) stop("Duplicate authoritative concept decision key")
if (any(!decisions$decision %in% expected_decisions)) stop("Invalid authoritative decision")
readr::write_csv(decisions, decision_path, na = "")

allowed_states <- c(
  "confirmed", "pending_review", "reviewed_no_confirmed_concept",
  "no_configured_match"
)
document_ids <- sort(unique(candidates$document_id))
decision_documents <- unique(decisions$document_id)
if (!all(decision_documents %in% document_ids)) {
  stop("Concept decision refers to a non-active document")
}

confirmed_by_document <- split(
  decisions$concept[decisions$decision == "confirm"],
  decisions$document_id[decisions$decision == "confirm"]
)
pending_by_document <- split(
  decisions$concept[decisions$decision == "pending_review"],
  decisions$document_id[decisions$decision == "pending_review"]
)
document_concepts <- setNames(rep("", length(document_ids)), document_ids)
for (document_id in names(confirmed_by_document)) {
  document_concepts[[document_id]] <- paste(
    sort(unique(confirmed_by_document[[document_id]])), collapse = "|"
  )
}
document_states <- setNames(rep("no_configured_match", length(document_ids)), document_ids)
for (document_id in document_ids) {
  if (document_id %in% names(confirmed_by_document)) {
    document_states[[document_id]] <- "confirmed"
  } else if (document_id %in% names(pending_by_document)) {
    document_states[[document_id]] <- "pending_review"
  } else if (document_id %in% decision_documents) {
    document_states[[document_id]] <- "reviewed_no_confirmed_concept"
  }
}

reconcile_versions <- function(data) {
  before <- data
  if (!"concept_coding_state" %in% names(data)) data$concept_coding_state <- ""
  data$matched_concepts <- unname(document_concepts[data$document_id])
  data$concept_coding_state <- unname(document_states[data$document_id])
  list(before = before, after = data)
}
candidate_result <- reconcile_versions(candidates)
corpus_result <- reconcile_versions(corpus)
candidates <- candidate_result$after
corpus <- corpus_result$after

# Expand the shared audit schema without disturbing historical records.
concept_change_columns <- c(
  "affected_version_id", "previous_concepts", "new_concepts",
  "previous_coding_state", "new_coding_state"
)
for (column in setdiff(concept_change_columns, names(changes))) changes[[column]] <- ""

build_change_rows <- function(before, after) {
  previous_concepts <- if ("matched_concepts" %in% names(before))
    ifelse(is.na(before$matched_concepts), "", before$matched_concepts) else rep("", nrow(before))
  previous_state <- if ("concept_coding_state" %in% names(before))
    ifelse(is.na(before$concept_coding_state), "", before$concept_coding_state) else rep("not_recorded", nrow(before))
  new_concepts <- ifelse(is.na(after$matched_concepts), "", after$matched_concepts)
  new_state <- after$concept_coding_state
  changed <- previous_concepts != new_concepts | previous_state != new_state
  if (!any(changed)) return(data.frame())
  indices <- which(changed)
  rows <- lapply(indices, function(index) {
    document_decisions <- decisions[decisions$document_id == after$document_id[index], , drop = FALSE]
    added <- setdiff(split_codes(new_concepts[index]), split_codes(previous_concepts[index]))
    removed <- setdiff(split_codes(previous_concepts[index]), split_codes(new_concepts[index]))
    reason <- paste0(
      "Completed concept review reconciled at document level; added=",
      if (length(added)) paste(added, collapse = "|") else "none",
      "; removed=", if (length(removed)) paste(removed, collapse = "|") else "none",
      "; state=", previous_state[index], " -> ", new_state[index], "."
    )
    data.frame(
      change = "concept_review_reconciled",
      version_id = after$version_id[index],
      document_id = after$document_id[index],
      language = after$language[index],
      title = after$title[index],
      document_url = after$document_url[index],
      review_status = after$review_status[index],
      old_document_id = after$document_id[index],
      new_document_id = after$document_id[index],
      rule = "authoritative_concept_decisions",
      reason = reason,
      source = after$source_id[index],
      previous_state = paste0("concepts=", previous_concepts[index], "; coding_state=", previous_state[index]),
      resulting_state = paste0("concepts=", new_concepts[index], "; coding_state=", new_state[index]),
      manual_decision = collapse_unique(document_decisions$decision),
      reviewer = collapse_unique(document_decisions$reviewer),
      review_timestamp = collapse_unique(document_decisions$reviewed_at),
      affected_version_id = after$version_id[index],
      previous_concepts = previous_concepts[index],
      new_concepts = new_concepts[index],
      previous_coding_state = previous_state[index],
      new_coding_state = new_state[index],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

new_changes <- build_change_rows(candidate_result$before, candidates)
if (nrow(new_changes)) {
  for (column in setdiff(names(changes), names(new_changes))) new_changes[[column]] <- ""
  for (column in setdiff(names(new_changes), names(changes))) changes[[column]] <- ""
  new_changes <- new_changes[, names(changes), drop = FALSE]
  changes <- rbind(changes, new_changes)
}
change_key <- paste(
  changes$change, changes$version_id, changes$rule,
  changes$previous_concepts, changes$new_concepts,
  changes$previous_coding_state, changes$new_coding_state,
  sep = "|"
)
changes <- changes[!duplicated(change_key), , drop = FALSE]

stopifnot(
  nrow(candidates) == 235L,
  nrow(corpus) == 235L,
  length(unique(candidates$document_id)) == 150L,
  length(unique(corpus$document_id)) == 150L,
  !anyDuplicated(candidates$version_id),
  !anyDuplicated(candidates[c("document_id", "language")]),
  all(candidates$concept_coding_state %in% allowed_states),
  all(corpus$concept_coding_state %in% allowed_states)
)

# Preserve column order while placing the state next to confirmed concepts.
order_columns <- function(data) {
  base <- setdiff(names(data), "concept_coding_state")
  position <- match("matched_concepts", base)
  ordered <- append(base, "concept_coding_state", after = position)
  data[, ordered, drop = FALSE]
}
candidates <- order_columns(candidates)
corpus <- order_columns(corpus)

readr::write_csv(candidates, candidate_path, na = "")
readr::write_csv(corpus, corpus_path, na = "")
readr::write_csv(changes, changes_path, na = "")

cat("decisions=", nrow(decisions), " confirm=",
    sum(decisions$decision == "confirm"), " dismiss=",
    sum(decisions$decision == "dismiss"), " overlap=", length(overlap), "\n", sep = "")
cat("states:\n")
print(table(document_states))

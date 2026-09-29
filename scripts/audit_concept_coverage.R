#!/usr/bin/env Rscript
# Builds concept-review evidence without changing active corpus assignments.
# Inputs: data/corpus.csv and config/terms.csv.
# Outputs: consolidated, grouped, individual, and existing-assignment audits.

# Dependencies and active evidence ------------------------------------------

source("R/utils.R", encoding = "UTF-8")
source("R/extract.R", encoding = "UTF-8")

corpus <- readr::read_csv("data/corpus.csv", show_col_types = FALSE)
terms <- readr::read_csv("config/terms.csv", show_col_types = FALSE)
present <- function(value) {
  !is.na(value) & nzchar(trimws(value))
}

collapse_values <- function(value, separator = " | ") {
  value <- sort(unique(value[present(value)]))
  paste(value, collapse = separator)
}

split_codes <- function(value) {
  value <- value[present(value)]
  if (!length(value)) return(character())
  codes <- trimws(unlist(strsplit(value, "\\|")))
  sort(unique(codes[nzchar(codes)]))
}

period_from_year <- function(year) {
  if (is.na(year)) return("uncertain")
  cut(
    year,
    breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2026),
    labels = c(
      "1991–1995", "1996–2000", "2001–2005", "2006–2010",
      "2011–2015", "2016–2020", "2021–2026"
    )
  ) |>
    as.character()
}

document_groups <- split(corpus, corpus$document_id)

# Comparison baseline for the former full-document behavior. These matches use
# the repaired PCRE patterns but are never admitted directly to the new queue.
full_document_keys <- unique(do.call(rbind, lapply(
  seq_len(nrow(corpus)),
  function(index) {
    row <- corpus[index, , drop = FALSE]
    patterns <- vapply(terms$pattern, normalize_concept_pattern, character(1))
    matched <- vapply(patterns, grepl, logical(1), x = row$extracted_text,
                      ignore.case = TRUE, perl = TRUE)
    if (!any(matched)) return(NULL)
    data.frame(
      document_id = row$document_id,
      proposed_concept = terms$term_id[matched],
      stringsAsFactors = FALSE
    )
  }
)))

version_evidence <- do.call(rbind, lapply(seq_len(nrow(corpus)), function(index) {
  row <- corpus[index, , drop = FALSE]
  evidence <- match_concepts_contextually(
    row$extracted_text,
    terms,
    relevance_text = row$relevance_excerpt,
    title = row$title,
    document_type = row$document_type,
    language = row$language
  )
  if (!nrow(evidence)) return(NULL)
  evidence$document_id <- row$document_id
  evidence$version_id <- row$version_id
  evidence[, c(
    "document_id", "version_id", "language", "proposed_concept",
    "matched_phrase", "context_excerpt", "matching_rule"
  )]
}))

if (is.null(version_evidence)) {
  version_evidence <- data.frame(
    document_id = character(), version_id = character(), language = character(),
    proposed_concept = character(), matched_phrase = character(),
    context_excerpt = character(), matching_rule = character()
  )
}

# Proposal triage -----------------------------------------------------------

rule_priority <- c(
  haiti_paragraph = 1L,
  stored_relevance_passage = 2L,
  bounded_haiti_context = 3L,
  dedicated_haiti_document = 4L
)
explicit_concepts <- c(
  "human_security", "fragile_state", "organized_crime",
  "maritime_security", "regional_security", "neighbourhood"
)

classify_proposal <- function(concept, evidence) {
  ambiguous_idiom <-
    (concept == "arms" && any(grepl(
      "arms of (government|the state)|open arms|arm.?s[- ]length",
      evidence$context_excerpt,
      ignore.case = TRUE,
      perl = TRUE
    ))) ||
    (concept == "drugs" && any(grepl(
      "food and drug|drug administration",
      evidence$context_excerpt,
      ignore.case = TRUE,
      perl = TRUE
    )))
  if (ambiguous_idiom) {
    return(c(
      proposal_class = "likely_false_positive",
      suggested_decision = "dismiss",
      confidence = "high",
      common_ambiguity = "Configured word is used in a non-conceptual idiom."
    ))
  }
  if (concept %in% explicit_concepts ||
      any(evidence$matching_rule == "dedicated_haiti_document")) {
    return(c(
      proposal_class = "high_confidence_contextual_match",
      suggested_decision = "confirm",
      confidence = "high",
      common_ambiguity = "Explicit concept phrase or publication wholly devoted to Haiti."
    ))
  }
  c(
    proposal_class = "individual_review",
    suggested_decision = "manual_review",
    confidence = "medium",
    common_ambiguity = "Generic term shares Haiti context but its conceptual use requires review."
  )
}

uncoded_documents <- names(document_groups)[vapply(document_groups, function(rows) {
  !any(present(rows$matched_concepts))
}, logical(1))]
full_document_keys <- full_document_keys[
  full_document_keys$document_id %in% uncoded_documents,
  ,
  drop = FALSE
]
proposal_keys <- unique(version_evidence[
  version_evidence$document_id %in% uncoded_documents,
  c("document_id", "proposed_concept"),
  drop = FALSE
])

proposal_rows <- lapply(seq_len(nrow(proposal_keys)), function(index) {
  key <- proposal_keys[index, ]
  rows <- document_groups[[key$document_id]]
  evidence <- version_evidence[
    version_evidence$document_id == key$document_id &
      version_evidence$proposed_concept == key$proposed_concept,
    ,
    drop = FALSE
  ]
  evidence$priority <- unname(rule_priority[evidence$matching_rule])
  evidence <- evidence[order(evidence$priority, evidence$language), , drop = FALSE]
  all_evidence <- evidence
  classification <- classify_proposal(key$proposed_concept, all_evidence)
  evidence <- evidence[!duplicated(paste(
    evidence$language,
    evidence$context_excerpt
  )), , drop = FALSE]
  selected <- head(evidence, 3L)
  excerpts <- paste0("[", selected$language, "] ", selected$context_excerpt)
  excerpts <- c(excerpts, rep("", 3L - length(excerpts)))
  english_title <- rows$title[rows$language == "en" & present(rows$title)]
  titles <- rows$title[present(rows$title)]
  year <- rows$year[which(!is.na(rows$year))[1]]
  family_codes <- split_codes(rows$families)
  data.frame(
    document_id = key$document_id,
    title = if (length(english_title)) english_title[1] else titles[1],
    year_or_period = if (length(year)) as.character(year) else "uncertain",
    period = period_from_year(if (length(year)) year else NA_real_),
    institution = collapse_values(rows$institution),
    family = paste(family_codes, collapse = " | "),
    document_type = collapse_values(rows$document_type),
    available_languages = collapse_values(rows$language),
    proposed_concept = key$proposed_concept,
    matched_english_phrase = collapse_values(
      evidence$matched_phrase[evidence$language == "en"]
    ),
    matched_french_phrase = collapse_values(
      evidence$matched_phrase[evidence$language == "fr"]
    ),
    evidence_excerpt_1 = excerpts[1],
    evidence_excerpt_2 = excerpts[2],
    evidence_excerpt_3 = excerpts[3],
    matching_rule = paste(unique(all_evidence$matching_rule), collapse = " | "),
    suggested_decision = classification[["suggested_decision"]],
    confidence = classification[["confidence"]],
    proposal_class = classification[["proposal_class"]],
    common_ambiguity = classification[["common_ambiguity"]],
    coding_decision = "pending",
    reviewer = "",
    review_notes = "",
    reviewed_at = "",
    stringsAsFactors = FALSE
  )
})

queue <- if (length(proposal_rows)) do.call(rbind, proposal_rows) else data.frame()
queue <- queue[order(
  queue$proposal_class,
  queue$proposed_concept,
  queue$year_or_period,
  queue$document_id
), , drop = FALSE]
stopifnot(!anyDuplicated(paste(queue$document_id, queue$proposed_concept)))
stopifnot(all(queue$coding_decision == "pending"))
readr::write_csv(queue, "data/concept_review_queue.csv", na = "")

# Grouped and individual review outputs ------------------------------------

group_keys <- unique(queue[, c(
  "proposed_concept", "matched_english_phrase", "matched_french_phrase",
  "matching_rule", "suggested_decision", "common_ambiguity", "proposal_class"
)])
group_rows <- lapply(seq_len(nrow(group_keys)), function(index) {
  key <- group_keys[index, ]
  matches <- rep(TRUE, nrow(queue))
  for (field in names(key)) {
    matches <- matches & ifelse(
      is.na(queue[[field]]) & is.na(key[[field]]),
      TRUE,
      !is.na(queue[[field]]) & !is.na(key[[field]]) & queue[[field]] == key[[field]]
    )
  }
  rows <- queue[matches, , drop = FALSE]
  data.frame(
    group_id = sprintf("concept_group_%03d", index),
    proposed_concept = key$proposed_concept,
    matched_english_phrase = key$matched_english_phrase,
    matched_french_phrase = key$matched_french_phrase,
    matching_rule = key$matching_rule,
    suggested_decision = key$suggested_decision,
    common_ambiguity = key$common_ambiguity,
    case_count = nrow(rows),
    document_ids = paste(rows$document_id, collapse = " | "),
    representative_examples = paste(head(rows$evidence_excerpt_1, 3L), collapse = " || "),
    recommendation = if (key$proposal_class == "high_confidence_contextual_match") {
      "bulk_confirm_review"
    } else if (key$proposal_class == "likely_false_positive") {
      "bulk_dismiss_review"
    } else {
      "individual_review"
    },
    stringsAsFactors = FALSE
  )
})
grouped <- if (length(group_rows)) do.call(rbind, group_rows) else data.frame()
readr::write_csv(grouped, "data/concept_grouped_review.csv", na = "")

individual <- queue[queue$proposal_class == "individual_review", , drop = FALSE]
readr::write_csv(individual, "data/concept_individual_review.csv", na = "")

# Existing-assignment audit -------------------------------------------------

assignment_rows <- list()
assignment_index <- 0L
for (document_id in names(document_groups)) {
  rows <- document_groups[[document_id]]
  assigned <- split_codes(rows$matched_concepts)
  if (!length(assigned)) next
  available_languages <- sort(unique(rows$language))
  for (concept in assigned) {
    assigned_languages <- sort(unique(rows$language[vapply(
      rows$matched_concepts,
      function(value) concept %in% split_codes(value),
      logical(1)
    )]))
    evidence <- version_evidence[
      version_evidence$document_id == document_id &
        version_evidence$proposed_concept == concept,
      ,
      drop = FALSE
    ]
    supported_languages <- sort(unique(evidence$language))
    bilingual_pair <- all(c("en", "fr") %in% available_languages)
    language_discrepancy <- bilingual_pair &&
      !all(c("en", "fr") %in% supported_languages)
    audit_status <- if (!nrow(evidence)) {
      "unsupported_by_haiti_context"
    } else if (language_discrepancy) {
      "supported_with_language_discrepancy"
    } else {
      "supported_by_haiti_context"
    }
    assignment_index <- assignment_index + 1L
    assignment_rows[[assignment_index]] <- data.frame(
      document_id = document_id,
      concept = concept,
      available_languages = paste(available_languages, collapse = " | "),
      assigned_languages = paste(assigned_languages, collapse = " | "),
      supported_languages = paste(supported_languages, collapse = " | "),
      matching_rules = collapse_values(evidence$matching_rule),
      audit_status = audit_status,
      requires_human_review = audit_status != "supported_by_haiti_context",
      stringsAsFactors = FALSE
    )
  }
}
assignment_audit <- do.call(rbind, assignment_rows)
readr::write_csv(
  assignment_audit,
  "data/concept_existing_assignment_audit.csv",
  na = ""
)

# Summary ------------------------------------------------------------------

cat("proposals_before_context_filter=", nrow(full_document_keys), "\n", sep = "")
cat("proposals_after=", nrow(queue), "\n", sep = "")
cat("proposal_documents=", length(unique(queue$document_id)), "\n", sep = "")
cat("proposal_classes:\n")
print(table(queue$proposal_class))
cat("proposals_by_concept:\n")
print(sort(table(queue$proposed_concept), decreasing = TRUE))
cat("group_recommendations:\n")
print(table(grouped$recommendation))
cat("individual_review=", nrow(individual), "\n", sep = "")
cat("existing_assignments:\n")
print(table(assignment_audit$audit_status))
cat(
  "existing_documents=",
  length(unique(assignment_audit$document_id)),
  "\n",
  sep = ""
)

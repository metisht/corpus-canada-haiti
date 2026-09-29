#!/usr/bin/env Rscript
# Focused regression for contextual bilingual concept matching.
# It exercises only stored patterns and synthetic offline text.

source("R/utils.R", encoding = "UTF-8")
source("R/extract.R", encoding = "UTF-8")

terms <- readr::read_csv("config/terms.csv", show_col_types = FALSE)
affected_examples <- data.frame(
  term_id = c(
    "violence", "instability", "democracy", "gangs", "migration",
    "borders", "arms", "drugs", "neighbourhood"
  ),
  stringsAsFactors = FALSE
)

for (index in seq_len(nrow(affected_examples))) {
  term_id <- affected_examples$term_id[index]
  term <- terms[terms$term_id == term_id, , drop = FALSE]
  stopifnot(nrow(term) == 1L)
  english <- match_concepts_contextually(
    paste("Haiti policy addresses", term$english),
    term,
    language = "en"
  )
  french <- match_concepts_contextually(
    paste("Document", intToUtf8(c(72L, 97L, 239L, 116L, 105L)), term$french),
    term,
    language = "fr"
  )
  if (!nrow(english)) stop("English regex example failed: ", term_id)
  if (!nrow(french)) stop("French regex example failed: ", term_id)
  stopifnot(
    all(english$proposed_concept == term_id),
    all(french$proposed_concept == term_id)
  )
}

# Confirm that the doubled CSV escape becomes a PCRE word boundary rather
# than two literal backslash characters.
violence_pattern <- normalize_concept_pattern(
  terms$pattern[terms$term_id == "violence"]
)
stopifnot(
  grepl(violence_pattern, "violence", perl = TRUE),
  !grepl(violence_pattern, "nonviolence", perl = TRUE)
)

long_hansard <- paste(
  "Haiti and democratic institutions were discussed.",
  paste(rep("unrelated parliamentary material", 350L), collapse = " "),
  "A separate debate mentioned narcotics."
)
long_matches <- match_concepts_contextually(
  long_hansard,
  terms,
  title = "House of Commons Debates",
  document_type = "house_debate",
  language = "en"
)
stopifnot(
  "democracy" %in% long_matches$proposed_concept,
  !"drugs" %in% long_matches$proposed_concept,
  !any(long_matches$matching_rule == "dedicated_haiti_document")
)

dedicated_text <- paste(
  "Haiti is the exclusive subject of this official publication.",
  paste(rep("program implementation detail", 100L), collapse = " "),
  "The publication addresses national security."
)
dedicated_matches <- match_concepts_contextually(
  dedicated_text,
  terms,
  title = "Canada-Haiti relations",
  document_type = "statement",
  language = "en"
)
stopifnot(
  "national_security" %in% dedicated_matches$proposed_concept,
  any(dedicated_matches$matching_rule == "dedicated_haiti_document")
)

title_only <- match_concepts_contextually(
  "This body text contains neither a case reference nor a configured term.",
  terms,
  title = "Haiti and violence",
  document_type = "statement",
  language = "en"
)
stopifnot(!nrow(title_only))

cat("Contextual concept-coding regression passed\n")

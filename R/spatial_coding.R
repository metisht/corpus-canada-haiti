# Builds a review inventory of explicit spatial formulations in retained texts.
# This module reads corpus rows and a controlled bilingual vocabulary, returns
# version-level evidence, and writes only the separate spatial-review file.
# It never changes corpus membership, review decisions, or dashboard data.
# Existing manual spatial decisions are preserved by stable occurrence_id.

# Controlled vocabulary -----------------------------------------------------

spatial_dimensions <- c(
  "spatial_scale",
  "spatial_practice",
  "relational_framing"
)

spatial_codes <- list(
  spatial_scale = c(
    "bilateral", "caribbean", "americas_hemisphere",
    "north_american_continental", "canada_domestic", "global_multilateral"
  ),
  spatial_practice = c(
    "border", "maritime", "migration_mobility", "transnational",
    "regional_security"
  ),
  relational_framing = c(
    "peripheral", "priority_country", "special_relationship",
    "neighbourhood", "regional_partner", "shared_security_space"
  )
)

validate_spatial_terms <- function(terms) {
  required <- c("dimension", "code", "language", "pattern", "match_type", "notes")
  missing <- setdiff(required, names(terms))
  if (length(missing)) {
    stop("Spatial vocabulary is missing columns: ", paste(missing, collapse = ", "))
  }
  if (!setequal(unique(terms$dimension), spatial_dimensions)) {
    stop("Spatial vocabulary must contain exactly the three controlled dimensions.")
  }
  valid_code <- vapply(seq_len(nrow(terms)), function(index) {
    terms$code[index] %in% spatial_codes[[terms$dimension[index]]]
  }, logical(1))
  if (!all(valid_code)) stop("Spatial vocabulary contains an uncontrolled code.")
  if (!all(terms$language %in% c("en", "fr"))) {
    stop("Spatial vocabulary languages must be en or fr.")
  }
  if (!all(terms$match_type == "regex")) {
    stop("The first-pass vocabulary currently supports regex matches only.")
  }
  invisible(terms)
}

read_spatial_terms <- function(path) {
  terms <- readr::read_csv(path, show_col_types = FALSE, na = character())
  validate_spatial_terms(terms)
  terms
}

# Official-text preparation -------------------------------------------------

spatial_present <- function(value) {
  !is.na(value) & nzchar(trimws(as.character(value)))
}

remove_spatial_boilerplate <- function(text) {
  if (!spatial_present(text)) return("")
  boilerplate <- paste0(
    "(?i)^(?:skip to main content|skip to .*about this site.*|search canada\\.ca|",
    "about government|contact us|report a problem|share this page|date modified|",
    "passer au contenu principal|rechercher dans canada\\.ca|gouvernement du canada|",
    "signaler un probl[eè]me|partager cette page|date de modification|avis)$"
  )
  lines <- unlist(strsplit(as.character(text), "\\r?\\n", perl = TRUE))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines) & !grepl(boilerplate, lines, perl = TRUE)]
  paste(lines, collapse = "\n")
}

spatial_context <- function(text, start, end, radius = 400L) {
  left <- max(1L, start - radius)
  right <- min(nchar(text), end + radius)
  excerpt <- substr(text, left, right)
  excerpt <- gsub("[[:space:]]+", " ", excerpt)
  excerpt <- trimws(excerpt)
  if (left > 1L) excerpt <- paste0("…", excerpt)
  if (right < nchar(text)) excerpt <- paste0(excerpt, "…")
  excerpt
}

context_mentions_haiti <- function(context) {
  grepl("(?i)\\b(?:Haiti|Ha[iï]ti|Haitian|Ha[iï]tien(?:ne)?s?)\\b", context, perl = TRUE)
}

qualifies_spatial_match <- function(code, phrase, context) {
  phrase <- tolower(phrase)
  action_or_institution <- paste0(
    "(?i)\\b(?:action|assistance|cooperat|coordinat|engagement|framework|initiative|",
    "institution|mission|organization|partners?|policy|program|response|security|strategy|support|",
    "aide|appui|cadre|coop[eé]rat|coord|engagement|institution|mission|organisation|partenaires?|",
    "politique|programme|r[eé]ponse|s[eé]curit[eé]|strat[eé]gie)"
  )
  canada_actor <- grepl("(?i)\\b(?:Canada|Canadian|canadien(?:ne)?s?)\\b", context, perl = TRUE)
  movement_relation <- grepl(
    paste0(
      "(?i)\\b(?:across|between|cross|deport|destination|displac|flow|from|into|return|route|",
      "smuggl|traffic|travel|to\\s+Canada|toward|weapons?|arms|Dominican|United\\s+States|",
      "entre|franchi|depuis|vers|destination|d[eé]plac|flux|retour|route|trafic|circulation|",
      "armes?|R[eé]publique\\s+dominicaine|[EÉ]tats-Unis)\\b"
    ),
    context,
    perl = TRUE
  )
  haitian_population <- grepl(
    "(?i)\\b(?:Haitian|Ha[iï]tien(?:ne)?s?)\\s+(?:migrants?|refugees?|population)|(?:migrants?|r[eé]fugi[eé]s?)\\s+ha[iï]tiens?\\b",
    context,
    perl = TRUE
  )
  regional_framework <- grepl(
    "(?i)\\b(?:CARICOM|Caribbean\\s+Community|OAS|Organization\\s+of\\s+American\\s+States|regional\\s+(?:alliance|framework|partners?|security)|communaut[eé]\\s+carib[eé]enne|OEA|organisation\\s+des\\s+[EÉ]tats\\s+am[eé]ricains|alliance\\s+r[eé]gionale)\\b",
    context,
    perl = TRUE
  )
  if (code == "bilateral") return(canada_actor && grepl(action_or_institution, context, perl = TRUE))
  if (code == "caribbean") return(regional_framework || grepl(action_or_institution, context, perl = TRUE))
  if (code == "global_multilateral") return(grepl(action_or_institution, context, perl = TRUE))
  if (code == "border") return(movement_relation)
  if (code == "migration_mobility") return(haitian_population || movement_relation)
  TRUE
}

spatial_occurrence_id <- function(row) {
  key <- paste(
    row$version_id, row$source_field, row$dimension, row$proposed_code,
    tolower(trimws(row$matched_phrase)), row$context_excerpt,
    sep = "|"
  )
  paste0("spatial_", digest::digest(key, algo = "xxhash64", serialize = FALSE))
}

# Evidence extraction -------------------------------------------------------

extract_spatial_matches <- function(version, field, terms) {
  text <- remove_spatial_boilerplate(version[[field]])
  if (!spatial_present(text)) return(NULL)
  version_language <- tolower(ifelse(is.na(version$language), "", version$language))
  eligible_languages <- if (version_language %in% c("en", "fr")) {
    version_language
  } else {
    c("en", "fr")
  }
  terms <- terms[terms$language %in% eligible_languages, , drop = FALSE]
  matches <- lapply(seq_len(nrow(terms)), function(term_index) {
    term <- terms[term_index, , drop = FALSE]
    locations <- stringr::str_locate_all(
      text,
      stringr::regex(term$pattern, ignore_case = TRUE)
    )[[1]]
    if (!nrow(locations)) return(NULL)
    rows <- lapply(seq_len(nrow(locations)), function(location_index) {
      start <- locations[location_index, "start"]
      end <- locations[location_index, "end"]
      context <- spatial_context(text, start, end)
      # A spatial term elsewhere in a long retained publication is not evidence
      # about Haiti. Requiring Haiti in the local window removes incidental
      # debates about other countries while leaving the decision pending.
      if (!context_mentions_haiti(context)) return(NULL)
      if (!qualifies_spatial_match(term$code, substr(text, start, end), context)) return(NULL)
      data.frame(
        document_id = as.character(version$document_id),
        version_id = as.character(version$version_id),
        title = as.character(version$title),
        year = suppressWarnings(as.integer(version$year)),
        language = as.character(version$language),
        dimension = as.character(term$dimension),
        proposed_code = as.character(term$code),
        matched_phrase = substr(text, start, end),
        context_excerpt = context,
        source_field = field,
        coding_decision = "pending",
        reviewer = "",
        review_notes = "",
        reviewed_at = "",
        stringsAsFactors = FALSE
      )
    })
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) return(NULL)
    do.call(rbind, rows)
  })
  matches <- Filter(Negate(is.null), matches)
  if (!length(matches)) return(NULL)
  do.call(rbind, matches)
}

build_spatial_inventory <- function(corpus, terms) {
  required <- c(
    "document_id", "version_id", "title", "year", "language",
    "title", "extracted_text", "relevance_excerpt", "review_status"
  )
  missing <- setdiff(required, names(corpus))
  if (length(missing)) {
    stop("Corpus is missing columns: ", paste(missing, collapse = ", "))
  }
  retained <- corpus[corpus$review_status == "retain", , drop = FALSE]
  # Prefer the fullest stored official text. Shorter derived fields are used
  # only when extraction is unavailable, preventing the same passage from
  # appearing once as an excerpt and again as full text.
  inventory <- lapply(seq_len(nrow(retained)), function(version_index) {
    version <- retained[version_index, , drop = FALSE]
    fields <- if (spatial_present(version$extracted_text)) {
      "extracted_text"
    } else if (spatial_present(version$relevance_excerpt)) {
      "relevance_excerpt"
    } else {
      "title"
    }
    version_matches <- lapply(fields, function(field) {
      extract_spatial_matches(version, field, terms)
    })
    version_matches <- Filter(Negate(is.null), version_matches)
    if (!length(version_matches)) return(NULL)
    do.call(rbind, version_matches)
  })
  inventory <- Filter(Negate(is.null), inventory)
  if (!length(inventory)) {
    return(data.frame(
      occurrence_id = character(), document_id = character(),
      version_id = character(), title = character(), year = integer(),
      language = character(), dimension = character(), proposed_code = character(),
      matched_phrase = character(), context_excerpt = character(),
      source_field = character(), coding_decision = character(),
      reviewer = character(), review_notes = character(), reviewed_at = character()
    ))
  }
  inventory <- do.call(rbind, inventory)
  inventory <- unique(inventory)
  inventory$occurrence_id <- vapply(
    seq_len(nrow(inventory)),
    function(index) spatial_occurrence_id(inventory[index, , drop = FALSE]),
    character(1)
  )
  inventory <- inventory[c(
    "occurrence_id", "document_id", "version_id", "title", "year", "language",
    "dimension", "proposed_code", "matched_phrase", "context_excerpt",
    "source_field", "coding_decision", "reviewer", "review_notes", "reviewed_at"
  )]
  inventory[order(
    inventory$document_id, inventory$version_id, inventory$dimension,
    inventory$proposed_code, inventory$source_field, inventory$occurrence_id
  ), , drop = FALSE]
}

# Reconciliation and output -------------------------------------------------

preserve_spatial_decisions <- function(inventory, existing) {
  if (is.null(existing) || !nrow(existing) || !"occurrence_id" %in% names(existing)) {
    return(inventory)
  }
  decision_columns <- c("coding_decision", "reviewer", "review_notes", "reviewed_at")
  matched <- match(inventory$occurrence_id, existing$occurrence_id)
  for (column in decision_columns) {
    if (!column %in% names(existing)) next
    preserved <- existing[[column]][matched]
    use_preserved <- !is.na(preserved) & nzchar(as.character(preserved))
    inventory[[column]][use_preserved] <- preserved[use_preserved]
  }
  reviewed_existing <- existing[
    !existing$occurrence_id %in% inventory$occurrence_id &
      spatial_present(existing$coding_decision) &
      existing$coding_decision != "pending",
    names(inventory),
    drop = FALSE
  ]
  if (nrow(reviewed_existing)) inventory <- rbind(inventory, reviewed_existing)
  inventory[order(
    inventory$document_id, inventory$version_id, inventory$dimension,
    inventory$proposed_code, inventory$source_field, inventory$occurrence_id
  ), , drop = FALSE]
}

spatial_review_priority <- function(dimension, code) {
  if (dimension == "relational_framing") return("1_explicit_relational_framing")
  if (dimension == "spatial_practice") return("2_spatial_practice")
  if (code == "global_multilateral") return("4_global_multilateral")
  "3_specific_spatial_scale"
}

spatial_evidence_score <- function(dimension, context) {
  score <- ifelse(dimension == "relational_framing", 300, 0)
  score <- score + ifelse(grepl(
    "(?i)\\b(?:Canada|Canadian|CARICOM|United Nations|Nations Unies|mission|policy|politique|program|programme|support|appui|security|s[eé]curit[eé])\\b",
    context,
    perl = TRUE
  ), 100, 0)
  score + pmin(nchar(context), 800) / 1000
}

select_representative_evidence <- function(group, maximum = 3L) {
  group <- group[!duplicated(group$context_excerpt), , drop = FALSE]
  group$score <- spatial_evidence_score(group$dimension, group$context_excerpt)
  group <- group[order(-group$score, group$occurrence_id), , drop = FALSE]
  selected <- integer()
  languages <- unique(group$language)
  if (length(languages) > 1L) {
    for (language in languages) {
      candidate <- which(group$language == language & !seq_len(nrow(group)) %in% selected)
      if (length(candidate)) selected <- c(selected, candidate[1])
      if (length(selected) >= maximum) break
    }
  }
  remaining <- setdiff(seq_len(nrow(group)), selected)
  selected <- c(selected, utils::head(remaining, maximum - length(selected)))
  group[selected, , drop = FALSE]
}

build_spatial_queue <- function(inventory) {
  if (!nrow(inventory)) return(data.frame())
  group_key <- paste(inventory$document_id, inventory$dimension, inventory$proposed_code, sep = "|")
  groups <- split(inventory, group_key)
  queue <- lapply(groups, function(group) {
    evidence <- select_representative_evidence(group)
    language_order <- c("en", "fr", "bilingual", "bilingual_or_uncertain")
    languages <- unique(as.character(group$language))
    languages <- c(intersect(language_order, languages), sort(setdiff(languages, language_order)))
    title_order <- match(group$language, language_order)
    title_order[is.na(title_order)] <- length(language_order) + 1L
    title <- group$title[order(title_order)][1]
    years <- suppressWarnings(as.integer(group$year))
    year <- if (any(!is.na(years))) years[which(!is.na(years))[1]] else NA_integer_
    excerpts <- paste0("[", evidence$language, "] ", evidence$context_excerpt)
    excerpts <- c(excerpts, rep("", 3L - length(excerpts)))
    phrases <- unique(trimws(as.character(group$matched_phrase)))
    phrases <- phrases[nzchar(phrases)]
    data.frame(
      document_id = group$document_id[1],
      title = title,
      year = year,
      languages = paste(languages, collapse = " | "),
      dimension = group$dimension[1],
      proposed_code = group$proposed_code[1],
      matched_phrases = paste(phrases, collapse = " | "),
      evidence_count = nrow(group),
      representative_excerpt_1 = excerpts[1],
      representative_excerpt_2 = excerpts[2],
      representative_excerpt_3 = excerpts[3],
      review_priority = spatial_review_priority(group$dimension[1], group$proposed_code[1]),
      coding_decision = "pending",
      reviewer = "",
      review_notes = "",
      reviewed_at = "",
      stringsAsFactors = FALSE
    )
  })
  queue <- do.call(rbind, queue)
  rownames(queue) <- NULL
  year_order <- ifelse(is.na(queue$year), Inf, queue$year)
  queue[order(
    queue$review_priority, year_order, queue$document_id, queue$proposed_code
  ), , drop = FALSE]
}

preserve_spatial_queue_decisions <- function(queue, existing) {
  if (is.null(existing) || !nrow(existing)) return(queue)
  key <- paste(queue$document_id, queue$dimension, queue$proposed_code, sep = "|")
  existing_key <- paste(existing$document_id, existing$dimension, existing$proposed_code, sep = "|")
  matched <- match(key, existing_key)
  decision_columns <- c("coding_decision", "reviewer", "review_notes", "reviewed_at")
  for (column in decision_columns) {
    if (!column %in% names(existing)) next
    preserved <- existing[[column]][matched]
    use_preserved <- !is.na(preserved) & nzchar(as.character(preserved))
    queue[[column]][use_preserved] <- preserved[use_preserved]
  }
  reviewed_existing <- existing[
    !existing_key %in% key & spatial_present(existing$coding_decision) &
      existing$coding_decision != "pending",
    names(queue),
    drop = FALSE
  ]
  if (nrow(reviewed_existing)) queue <- rbind(queue, reviewed_existing)
  year_order <- ifelse(is.na(queue$year), Inf, queue$year)
  queue[order(
    queue$review_priority, year_order, queue$document_id, queue$proposed_code
  ), , drop = FALSE]
}

write_spatial_queue <- function(
  inventory,
  output_path = "data/spatial_review_queue.csv"
) {
  queue <- build_spatial_queue(inventory)
  existing <- if (file.exists(output_path)) {
    readr::read_csv(
      output_path,
      col_types = readr::cols(
        .default = readr::col_character(),
        year = readr::col_integer(),
        evidence_count = readr::col_integer()
      ),
      na = c("", "NA")
    )
  } else {
    NULL
  }
  queue <- preserve_spatial_queue_decisions(queue, existing)
  readr::write_csv(queue, output_path, na = "")
  invisible(queue)
}

write_spatial_review <- function(
  corpus_path = "data/corpus.csv",
  terms_path = "config/spatial_terms.csv",
  output_path = "data/spatial_review.csv"
) {
  corpus <- readr::read_csv(corpus_path, show_col_types = FALSE)
  terms <- read_spatial_terms(terms_path)
  inventory <- build_spatial_inventory(corpus, terms)
  existing <- if (file.exists(output_path)) {
    readr::read_csv(
      output_path,
      col_types = readr::cols(.default = readr::col_character(), year = readr::col_integer()),
      na = c("", "NA")
    )
  } else {
    NULL
  }
  inventory <- preserve_spatial_decisions(inventory, existing)
  readr::write_csv(inventory, output_path, na = "")
  invisible(inventory)
}

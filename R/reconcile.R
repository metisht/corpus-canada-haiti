# Pure consolidation helpers for page-level historical Parliament records.
# Reconciliation scripts supply candidate rows and receive publication-level rows.
# Stable identity, complete component provenance, and idempotence are invariants.

historical_publication_spec <- function() {
  data.frame(
    publication_type = c(rep("bill_c_53", 5), rep("development_human_rights", 8)),
    publication_date = c("1992-03-10", "1992-03-17", "1992-03-19", "1992-03-26",
                         "1992-04-29", "1992-06-08", "1992-11-17", "1992-11-26",
                         "1992-12-08", "1992-12-09", "1993-03-24", "1993-05-12",
                         "1993-06-02"),
    stringsAsFactors = FALSE
  )
}

historical_publication_type <- function(url) {
  if (grepl("_28_1", url, fixed = TRUE)) "bill_c_53" else
    if (grepl("_25_2", url, fixed = TRUE)) "development_human_rights" else NA_character_
}

historical_row_date <- function(row) {
  date <- row$publication_date
  if (nzchar(date) && grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) return(date)
  raw <- first_match("[0-3]?[0-9][-/][01]?[0-9][-/](?:19|20)\\d{2}",
                     substr(paste(row$title, row$extracted_text), 1, 5000))
  if (is.na(raw)) return(NA_character_)
  bits <- as.integer(strsplit(gsub("-", "/", raw), "/", fixed = TRUE)[[1]])
  sprintf("%04d-%02d-%02d", bits[3], bits[2], bits[1])
}

consolidate_historical_candidates <- function(rows) {
  if (!nrow(rows)) return(rows)
  already_consolidated <- nrow(rows) == 13L &&
    length(unique(rows$document_id)) == 13L &&
    all(rows$language == "bilingual") &&
    all(nzchar(rows$component_urls))
  if (already_consolidated) return(rows[, candidate_columns, drop = FALSE])
  rows$publication_type <- vapply(rows$document_url, historical_publication_type,
                                  character(1))
  rows$authoritative_date <- vapply(seq_len(nrow(rows)), function(i) {
    historical_row_date(rows[i, , drop = FALSE])
  }, character(1))
  expected <- historical_publication_spec()
  key <- paste(rows$publication_type, rows$authoritative_date)
  expected_key <- paste(expected$publication_type, expected$publication_date)
  if (any(is.na(rows$authoritative_date)) || any(!key %in% expected_key)) {
    stop("Historical page could not be assigned to one of the 13 validated publications")
  }
  groups <- split(seq_len(nrow(rows)), factor(key, levels = expected_key))
  out <- lapply(seq_along(groups), function(i) {
    g <- rows[groups[[i]], , drop = FALSE]
    spec <- expected[i, ]
    urls <- sort(unique(c(g$document_url, unlist(strsplit(g$official_alternate_urls,
                                                          "\\|")))))
    urls <- urls[nzchar(urls)]
    collection <- if (spec$publication_type == "bill_c_53")
      "oop.com_HOC_3403_28_1" else "oop.com_HOC_3403_25_2"
    document_id <- stable_historical_document_id(collection,
                                                  spec$publication_type,
                                                  spec$publication_date)
    label <- if (spec$publication_type == "bill_c_53")
      "Bill C-53 / Projet de loi C-53" else
      "Development and Human Rights / Développement et droits de la personne"
    primary <- sort(unique(g$document_url))[1]
    text_blocks <- vapply(seq_len(nrow(g)), function(j) {
      paste0("[Source page: ", g$document_url[j], "]\n", g$extracted_text[j])
    }, character(1))
    combined_text <- paste(unique(text_blocks), collapse = "\n\n")
    row <- g[1, setdiff(names(g), c("publication_type", "authoritative_date")),
             drop = FALSE]
    row$document_id <- document_id
    row$version_id <- stable_historical_version_id(document_id, "bilingual")
    row$title <- paste(label, spec$publication_date, sep = " — ")
    row$publication_date <- spec$publication_date
    row$year <- as.integer(substr(spec$publication_date, 1, 4))
    row$date_source <- "authoritative running header"
    row$language <- "bilingual"
    row$document_type <- "committee_evidence"
    row$source_page <- primary
    row$document_url <- primary
    row$official_alternate_urls <- paste(setdiff(urls, primary), collapse = "|")
    row$component_urls <- paste(sort(unique(g$document_url)), collapse = "|")
    row$relevance_excerpt <- paste(unique(g$relevance_excerpt[nzchar(g$relevance_excerpt)]),
                                   collapse = " […] ")
    row$extracted_text <- combined_text
    row$content_hash <- digest::digest(combined_text, algo = "sha256", serialize = FALSE)
    row$review_status <- "pending"
    row$review_notes <- ""
    row$canonical_url <- canonicalize_url(primary)
    row
  })
  result <- bind_rows_base(out)
  result[, candidate_columns, drop = FALSE]
}

classify_rejected_record <- function(candidate, review_note = "") {
  classification <- pre_review_classification(candidate$title,
                                                candidate$extracted_text,
                                                candidate$document_url)
  if (grepl("index|finding aid", review_note, ignore.case = TRUE)) {
    classification <- list(include = FALSE, status = "non_document",
                           rule = "index_or_finding_aid", reason = review_note)
  } else if (grepl("incidental|illustrative comparison", review_note,
                   ignore.case = TRUE)) {
    classification <- list(include = FALSE, status = "excluded",
                           rule = "incidental_comparison", reason = review_note)
  } else if (grepl("adoption|procedural|grammar|editorial", review_note,
                   ignore.case = TRUE)) {
    classification <- list(include = FALSE, status = "excluded",
                           rule = "procedural_reference", reason = review_note)
  }
  classification
}

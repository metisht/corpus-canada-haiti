candidate_columns <- c("document_id","version_id","title","publication_date","page_last_modified","event_start_date","event_end_date","year","date_source","language","institution","families","document_type","source_page","document_url","official_alternate_urls","format","relevance_excerpt","extracted_text","matched_case_terms","matched_concepts","retrieval_method","retrieved_at","review_status","review_notes","date_status","canonical_url","content_hash","source_id")
review_columns <- c("document_id","version_id","decision","notes","reviewer","reviewed_at")
diagnostic_columns <- c("source_id","family","url","stage","status","message","http_status","occurred_at")

empty_frame <- function(cols) as.data.frame(setNames(replicate(length(cols), character(), simplify = FALSE), cols))

process_url <- function(url, source, cfg, terms, families, force = FALSE) {
  fetched <- polite_get(url, cfg, force)
  content <- extract_content(fetched$path, fetched$type, url)
  if (!nzchar(content$title) || nchar(content$text) < 100) stop("Page has no substantive document text")
  hits <- case_matches(paste(content$title, content$text), unlist(cfg$case_labels))
  if (!length(hits)) return(NULL)
  dates <- extract_date_metadata(content$text, url, source$source_id)
  year <- dates$year
  date_status <- if (is.na(year)) "uncertain" else if (year >= cfg$start_year && year <= cfg$end_year) "in_range" else "out_of_range"
  if (date_status == "out_of_range") return(NULL)
  hash <- digest::digest(content$text, algo = "sha256", serialize = FALSE)
  meta <- derive_document_metadata(content$title, content$text, url, source)
  lang <- language_of(content$text)
  data.frame(document_id = stable_document_id(url, content$alternate_urls), version_id = stable_version_id(url, lang), title = content$title,
    publication_date = dates$publication_date, page_last_modified = dates$page_last_modified, event_start_date = dates$event_start_date, event_end_date = dates$event_end_date,
    year = year, date_source = dates$date_source, language = lang, institution = meta$institution,
    families = meta$families, document_type = meta$document_type, source_page = if (source$method == "seed_list") url else source$start_url, document_url = url,
    official_alternate_urls = paste(content$alternate_urls, collapse = "|"), format = content$format, relevance_excerpt = relevance_excerpt(content$text, hits, cfg$excerpt_characters), extracted_text = content$text,
    matched_case_terms = paste(hits, collapse = "|"), matched_concepts = code_terms(content$text, terms),
    retrieval_method = source$method, retrieved_at = now_utc(), review_status = "pending", review_notes = "",
    date_status = date_status, canonical_url = canonicalize_url(url), content_hash = hash,
    source_id = source$source_id, stringsAsFactors = FALSE)
}

deduplicate_candidates <- function(x) {
  if (!nrow(x)) return(x)
  x[!duplicated(x$canonical_url), , drop = FALSE]
}

sync_review <- function(candidates, old_review) {
  review <- if (nrow(old_review)) old_review else empty_frame(review_columns)
  new_versions <- setdiff(candidates$version_id, review$version_id)
  if (length(new_versions)) {
    m <- match(new_versions, candidates$version_id)
    review <- bind_rows_base(list(review, data.frame(document_id = candidates$document_id[m], version_id = new_versions, decision = "pending", notes = "", reviewer = "", reviewed_at = "", stringsAsFactors = FALSE)))
  }
  review[!duplicated(review$version_id), review_columns, drop = FALSE]
}

apply_review <- function(candidates, review) {
  if (!nrow(candidates)) return(candidates)
  i <- match(candidates$version_id, review$version_id)
  candidates$review_status <- ifelse(!is.na(i) & nzchar(review$decision[i]), review$decision[i], "pending")
  candidates$review_notes <- ifelse(!is.na(i), review$notes[i], "")
  candidates
}

run_crawl <- function(validation = TRUE, force = FALSE) {
  cfg <- yaml::read_yaml("config/case.yml")
  sources <- safe_read_csv("config/sources.csv"); sources <- sources[tolower(sources$enabled) == "true", , drop = FALSE]
  terms <- safe_read_csv("config/terms.csv"); families <- safe_read_csv("config/families.csv")
  old <- safe_read_csv("data/candidates.csv"); old_review <- safe_read_csv("data/review.csv")
  if (!nrow(old_review)) old_review <- safe_read_csv("data/manual_review.csv")
  records <- list(); diagnostics <- list(); k <- 0L; d <- 0L
  add_diag <- function(...) { d <<- d + 1L; diagnostics[[d]] <<- record_diagnostic(...) }
  for (s in seq_len(nrow(sources))) {
    source <- sources[s, ]; message("[", source$source_id, "] discover")
    urls <- tryCatch(discover_source(source, cfg, force), error = function(e) { add_diag(source$source_id, source$default_families, source$start_url, "discovery", "failed", conditionMessage(e)); character() })
    add_diag(source$source_id, source$default_families, source$start_url, "discovery", "completed", paste(length(urls), "document URLs discovered"))
    valid <- 0L
    for (url in urls) {
      if (validation && valid >= as.integer(cfg$validation_max_valid_per_source)) break
      rec <- tryCatch(process_url(url, source, cfg, terms, families, force), error = function(e) { add_diag(source$source_id, source$default_families, url, "retrieval", "failed", conditionMessage(e)); NULL })
      if (!is.null(rec)) { k <- k + 1L; records[[k]] <- rec; valid <- valid + 1L; message("  candidate: ", rec$title) }
    }
    add_diag(source$source_id, source$default_families, source$start_url, "validation", "completed", paste(valid, "valid candidates"))
  }
  fresh <- bind_rows_base(records); combined <- deduplicate_candidates(bind_rows_base(list(old, fresh)))
  if (!nrow(combined)) combined <- empty_frame(candidate_columns)
  review <- sync_review(combined, old_review); combined <- apply_review(combined, review)
  combined <- combined[order(combined$year, combined$title, na.last = TRUE), candidate_columns, drop = FALSE]
  corpus <- combined[combined$review_status %in% c("retain", "include"), , drop = FALSE]
  diag <- bind_rows_base(diagnostics); if (!nrow(diag)) diag <- empty_frame(diagnostic_columns)
  write_csv(combined, "data/candidates.csv"); write_csv(review, "data/review.csv")
  write_csv(corpus, "data/corpus.csv"); write_csv(terms, "data/concepts.csv"); write_csv(diag, "data/retrieval_diagnostics.csv")
  message(sprintf("Done: %d candidates, %d retained, %d failed retrieval/discovery events.", nrow(combined), nrow(corpus), sum(diag$status == "failed")))
  invisible(list(candidates = combined, corpus = corpus, diagnostics = diag))
}

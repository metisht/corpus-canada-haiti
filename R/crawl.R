candidate_columns <- c("document_id","title","publication_date","year","language","institution","families","document_type","source_page","document_url","format","relevance_excerpt","matched_case_terms","matched_concepts","retrieval_method","retrieved_at","review_status","review_notes","date_status","canonical_url","content_hash","source_id")
review_columns <- c("document_id","decision","notes","reviewer","reviewed_at")
diagnostic_columns <- c("source_id","family","url","stage","status","message","http_status","occurred_at")

empty_frame <- function(cols) as.data.frame(setNames(replicate(length(cols), character(), simplify = FALSE), cols))

process_url <- function(url, source, cfg, terms, families, force = FALSE) {
  fetched <- polite_get(url, cfg, force)
  content <- extract_content(fetched$path, fetched$type, url)
  if (!nzchar(content$title) || nchar(content$text) < 100) stop("Page has no substantive document text")
  hits <- case_matches(paste(content$title, content$text), unlist(cfg$case_labels))
  if (!length(hits)) return(NULL)
  date <- extract_date(content$text, content$title, url)
  year <- suppressWarnings(as.integer(substr(date, 1, 4)))
  date_status <- if (is.na(year)) "uncertain" else if (year >= cfg$start_year && year <= cfg$end_year) "in_range" else "out_of_range"
  if (date_status == "out_of_range") return(NULL)
  hash <- digest::digest(content$text, algo = "sha256", serialize = FALSE)
  fam <- classify_families(paste(content$title, url, substr(content$text, 1, 10000)), source$default_families, families)
  type <- if (grepl("speech|statement|news release|communiqué|déclaration", content$title, ignore.case = TRUE)) "statement" else if (content$format == "PDF") "publication" else "web document"
  data.frame(document_id = paste0("doc_", substr(hash, 1, 16)), title = content$title,
    publication_date = date, year = year, language = language_of(content$text), institution = source$institution,
    families = fam, document_type = type, source_page = source$start_url, document_url = url,
    format = content$format, relevance_excerpt = relevance_excerpt(content$text, hits, cfg$excerpt_characters),
    matched_case_terms = paste(hits, collapse = "|"), matched_concepts = code_terms(content$text, terms),
    retrieval_method = source$method, retrieved_at = now_utc(), review_status = "pending", review_notes = "",
    date_status = date_status, canonical_url = canonicalize_url(url), content_hash = hash,
    source_id = source$source_id, stringsAsFactors = FALSE)
}

deduplicate_candidates <- function(x) {
  if (!nrow(x)) return(x)
  x <- x[!duplicated(x$canonical_url), , drop = FALSE]
  dup <- duplicated(x$content_hash)
  if (any(dup)) for (i in which(dup)) {
    j <- match(x$content_hash[i], x$content_hash)
    x$families[j] <- paste(unique(c(strsplit(x$families[j], "\\|")[[1]], strsplit(x$families[i], "\\|")[[1]])), collapse = "|")
  }
  x[!dup, , drop = FALSE]
}

sync_review <- function(candidates, old_review) {
  review <- if (nrow(old_review)) old_review else empty_frame(review_columns)
  new_ids <- setdiff(candidates$document_id, review$document_id)
  if (length(new_ids)) review <- bind_rows_base(list(review, data.frame(document_id = new_ids, decision = "pending", notes = "", reviewer = "", reviewed_at = "", stringsAsFactors = FALSE)))
  review[!duplicated(review$document_id), review_columns, drop = FALSE]
}

apply_review <- function(candidates, review) {
  if (!nrow(candidates)) return(candidates)
  i <- match(candidates$document_id, review$document_id)
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

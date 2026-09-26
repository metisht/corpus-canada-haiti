# Crawler orchestration, schemas, review propagation, and diagnostics.
# Loaded by run.R after utility/discovery modules. run_crawl() owns project CSV
# side effects; review decisions are authoritative per document_id and all versions.

candidate_columns <- c("document_id","version_id","title","publication_date","page_last_modified","event_start_date","event_end_date","year","date_source","language","institution","families","document_type","operation_name","international_mission_name","source_page","document_url","official_alternate_urls","component_urls","format","relevance_excerpt","extracted_text","matched_case_terms","matched_concepts","retrieval_method","retrieved_at","review_status","review_notes","date_status","canonical_url","content_hash","source_id")
review_columns <- c("document_id","version_id","decision","notes","reviewer","reviewed_at")
diagnostic_columns <- c("run_id","source_id","family","url","stage","status","message","http_status","occurred_at")
validation_cap_scope <- "per_source"

empty_frame <- function(cols) as.data.frame(setNames(replicate(length(cols), character(), simplify = FALSE), cols))

crawler_stop <- function(status, message) {
  cnd <- structure(list(message = message, call = NULL, status = status),
                   class = c("crawler_condition", "error", "condition"))
  stop(cnd)
}

diagnostic_status <- function(condition) {
  if (inherits(condition, "crawler_condition")) condition$status else "failed"
}

diagnostic_run_summary <- function(diagnostics, run_id) {
  x <- diagnostics[diagnostics$run_id == run_id, , drop=FALSE]
  statuses <- c("completed", "excluded", "non_document", "outside_family",
                "missing_counterpart", "failed")
  counts <- setNames(integer(length(statuses)), statuses)
  if (nrow(x)) {
    found <- table(x$status)
    counts[intersect(names(found), names(counts))] <- as.integer(found[intersect(names(found), names(counts))])
  }
  counts
}

substantive_haiti <- function(text, url = "") {
  loc <- gregexpr("(?i)\\b(ha[iï]ti|haitian|haïtien(?:ne)?)\\w*", text, perl = TRUE)[[1]]
  count <- if (loc[1] == -1) 0L else length(loc)
  if (!count) return(FALSE)
  if (grepl("/(journals|Journals)/", url, ignore.case = TRUE)) {
    snippets <- vapply(loc, function(p) substr(text, max(1, p - 220), p + 450), character(1))
    procedural_only <- all(grepl("adopt(ed|ion)|concurred|presented|déposé|adopté|rapport", snippets, ignore.case = TRUE) &
                           !grepl("motion|sanction|intervention|government|democracy|military|police|aid|assistance|peace|embargo|résolution", snippets, ignore.case = TRUE))
    if (procedural_only) return(FALSE)
  }
  count >= 2L || grepl("haiti|haïti", substr(text, 1, 2500), ignore.case = TRUE)
}

pre_review_classification <- function(title, text, url = "") {
  heading <- clean_space(substr(paste(title, text), 1, 3500))
  general_index <- grepl(
    "Current sanctions imposed by Canada|Sanctions actuelles imposées par le Canada|"
      |>
      paste0("Regulatory stock review plan|Plan d.examen des règlements"),
    title, ignore.case = TRUE, perl = TRUE)
  if (general_index) {
    return(list(include = FALSE, status = "non_document",
                rule = "general_index",
                reason = "General index in which Haiti is only one listed entry."))
  }
  below_threshold_historical <- grepl(
    "Development and Human Rights / Développement et droits de la personne — "
      |>
      paste0("(1992-12-08|1992-12-09|1993-03-24)$"),
    title, ignore.case = TRUE, perl = TRUE)
  if (below_threshold_historical) {
    return(list(include = FALSE, status = "excluded",
                rule = "below_substantive_threshold",
                reason = "Haiti appears only in a list or brief multilateral summary without an autonomous analytical passage."))
  }
  if (grepl("\\b(index|finding aid|table of contents|table des mati[eè]res)\\b",
            heading, ignore.case = TRUE, perl = TRUE)) {
    return(list(include = FALSE, status = "non_document",
                rule = "index_or_finding_aid",
                reason = "Index or finding aid without independently substantive document content."))
  }
  contexts <- relevance_excerpt(text, "Haiti|Haïti", 2500)
  procedural <- grepl(
    "adopt(?:ed|ion)|concurrence|correction (?:grammatical|editorial)|corrigendum|"
      |>
      paste0("discussion (?:later|ultérieure)|order of business|ordre du jour"),
    contexts, ignore.case = TRUE, perl = TRUE)
  substantive <- grepl(
    "Canada|Canadian|canadien|sanction|operation|opération|military|militaire|police|"
      |>
      paste0("mission|aid|aide|assistance|diploma|question|answer|réponse|testimony|",
             "témoignage|recommend|recommand|assessment|évaluation"),
    contexts, ignore.case = TRUE, perl = TRUE)
  if (procedural && !substantive) {
    return(list(include = FALSE, status = "excluded", rule = "procedural_reference",
                reason = "Procedural reference without an autonomous substantive Haiti passage."))
  }
  comparison <- grepl("comparison|compared|for example|illustrat|comparaison|par exemple|à l'instar",
                      contexts, ignore.case = TRUE, perl = TRUE)
  if (comparison && !substantive) {
    return(list(include = FALSE, status = "excluded", rule = "incidental_comparison",
                reason = "Incidental comparison in a document primarily about another subject."))
  }
  list(include = TRUE, status = "completed", rule = "substantive_passage", reason = "")
}

process_url <- function(url, source, cfg, terms, families, force = FALSE) {
  fetched <- polite_get(url, cfg, force)
  content <- extract_content(fetched$path, fetched$type, url)
  if (!nzchar(content$title) || nchar(content$text) < 100) crawler_stop("non_document", "Page has no substantive document text")
  hits <- case_matches(paste(content$title, content$text), unlist(cfg$case_labels))
  if (!length(hits) || !substantive_haiti(content$text, url)) crawler_stop("excluded", "Retrieved successfully but Haiti is not substantive")
  dates <- extract_date_metadata(content$text, url, source$source_id)
  year <- dates$year
  date_status <- if (is.na(year)) "uncertain" else if (year >= cfg$start_year && year <= cfg$end_year) "in_range" else "out_of_range"
  if (date_status == "out_of_range") crawler_stop("excluded", paste("Document year", year, "is outside the bounded range"))
  hash <- digest::digest(content$text, algo = "sha256", serialize = FALSE)
  lang <- discovered_language(url)
  if (!length(lang)) lang <- language_of(content$text)
  if (source$source_id %in% c("parliament", "historical_parliament")) {
    pm <- parliament_metadata(content$title, content$text, url, lang)
    content$title <- pm$title
    routed <- discovered_counterpart(url)
    house_pdf <- grepl("/Content/(Committee|House)/.*\\.pdf($|[?#])", url, ignore.case=TRUE)
    if (length(routed) && !identical(canonicalize_url(routed), canonicalize_url(url))) content$alternate_urls <- unique(c(content$alternate_urls, routed))
  }
  operation_name <- international_mission_name <- ""
  if (source$source_id == "historical_parliament") {
    historical <- historical_parliament_metadata(content$text, url)
    classification <- pre_review_classification(historical$title, content$text, url)
    if (!classification$include) {
      crawler_stop(classification$status,
                   paste(classification$rule, classification$reason, sep = ": "))
    }
    content$title <- historical$title
    if (!is.na(historical$publication_date)) {
      dates$publication_date <- historical$publication_date
      dates$year <- as.integer(substr(historical$publication_date, 1, 4))
      dates$date_source <- "authoritative running header"
      year <- dates$year
      date_status <- if (year >= cfg$start_year && year <= cfg$end_year) "in_range" else "out_of_range"
    }
  }
  if (source$source_id == "forces_archive") {
    operation <- defence_operation_metadata(content$title, content$text, url)
    content$title <- operation$title
    operation_name <- operation$operation_name
    international_mission_name <- operation$international_mission_name
  }
  meta <- derive_document_metadata(content$title, content$text, url, source)
  required <- source$required_family %||% ""
  assigned <- strsplit(meta$families %||% "", "\\|")[[1]]
  if (nzchar(required) && !required %in% assigned) crawler_stop("outside_family", paste0("Classified outside required family: ", meta$families %||% "none"))
  if (source$source_id == "historical_parliament" &&
      !is.na(historical$publication_date)) {
    publication_type <- historical_publication_type(url)
    collection <- if (publication_type == "bill_c_53")
      "oop.com_HOC_3403_28_1" else "oop.com_HOC_3403_25_2"
    document_id <- stable_historical_document_id(collection, publication_type,
                                                 historical$publication_date)
    version_id <- stable_historical_version_id(document_id, "bilingual")
    lang <- "bilingual"
  } else {
    document_id <- if (source$source_id == "parliament")
      stable_parliament_document_id(url, meta$document_type) else
      stable_document_id(url, content$alternate_urls)
    version_id <- stable_version_id(url, lang)
  }
  data.frame(document_id = document_id, version_id = version_id, title = content$title,
    publication_date = dates$publication_date, page_last_modified = dates$page_last_modified, event_start_date = dates$event_start_date, event_end_date = dates$event_end_date,
    year = year, date_source = dates$date_source, language = lang, institution = meta$institution,
    families = meta$families, document_type = meta$document_type,
    operation_name = operation_name, international_mission_name = international_mission_name,
    source_page = discovered_source_page(url, if (source$method == "seed_list") url else source$start_url), document_url = url,
    official_alternate_urls = paste(content$alternate_urls, collapse = "|"), component_urls = if (source$source_id == "historical_parliament") url else "", format = content$format, relevance_excerpt = relevance_excerpt(content$text, hits, cfg$excerpt_characters), extracted_text = content$text,
    matched_case_terms = paste(hits, collapse = "|"), matched_concepts = code_terms(content$text, terms),
    retrieval_method = source$method, retrieved_at = now_utc(), review_status = "pending", review_notes = "",
    date_status = date_status, canonical_url = canonicalize_url(url), content_hash = hash,
    source_id = source$source_id, stringsAsFactors = FALSE)
}

deduplicate_candidates <- function(x) {
  if (!nrow(x)) return(x)
  pending_parliament <- x$source_id == "parliament" &
    !x$review_status %in% c("retain", "include")
  x$document_id[pending_parliament] <- vapply(
    x$document_url[pending_parliament], stable_parliament_document_id, character(1))
  priority <- ifelse(x$review_status %in% c("retain", "include"), 3L,
                     ifelse(toupper(x$format) == "PDF", 2L, 1L))
  x <- x[order(priority, x$retrieved_at, decreasing = TRUE), , drop = FALSE]
  # Prefer retained records, then direct PDFs, while attaching alternate routes.
  keys <- ifelse(nzchar(x$content_hash), paste(x$content_hash, x$language), x$canonical_url)
  keep <- !duplicated(keys)
  for (i in which(!keep)) {
    j <- match(keys[i], keys)
    alts <- unique(c(strsplit(x$official_alternate_urls[j] %||% "", "\\|")[[1]],
                     x$document_url[i], strsplit(x$official_alternate_urls[i] %||% "", "\\|")[[1]]))
    x$official_alternate_urls[j] <- paste(alts[nzchar(alts)], collapse = "|")
  }
  x <- x[keep, , drop = FALSE]
  # A duplicate official route discovered later inherits the already-reviewed
  # logical document id whenever its content hash matches an existing record.
  hash_doc <- setNames(x$document_id, x$content_hash)
  mapped <- unname(hash_doc[x$content_hash])
  x$document_id[!is.na(mapped)] <- mapped[!is.na(mapped)]
  # Search hits can point to several matching pages in the same daily House
  # publication. Keep one language record and retain all matched page URLs.
  work_lang <- paste(x$document_id, x$language)
  keep2 <- !duplicated(work_lang)
  for (i in which(!keep2)) {
    j <- match(work_lang[i], work_lang)
    alts <- unique(c(strsplit(x$official_alternate_urls[j] %||% "", "\\|")[[1]], x$document_url[i]))
    x$official_alternate_urls[j] <- paste(alts[nzchar(alts)], collapse="|")
    if (x$source_id[j] == "historical_parliament") {
      components <- unique(c(strsplit(x$component_urls[j] %||% "", "\\|")[[1]],
                             strsplit(x$component_urls[i] %||% "", "\\|")[[1]],
                             x$document_url[i]))
      x$component_urls[j] <- paste(components[nzchar(components)], collapse = "|")
      x$extracted_text[j] <- paste(unique(c(x$extracted_text[j], x$extracted_text[i])),
                                  collapse = "\n\n")
    }
  }
  x[keep2 & !duplicated(x$canonical_url), , drop = FALSE]
}

sync_review <- function(candidates, old_review) {
  review <- if (nrow(old_review)) old_review else empty_frame(review_columns)
  documents <- unique(candidates$document_id)
  rows <- vector("list", length(documents))
  for (j in seq_along(documents)) {
    id <- documents[j]
    old <- review[review$document_id == id, , drop = FALSE]
    decided <- old[old$decision != "pending" & nzchar(old$decision), , drop = FALSE]
    decisions <- unique(decided$decision)
    if (length(decisions) > 1) stop("Conflicting review decisions for ", id)
    chosen <- if (nrow(decided)) decided[which.max(nzchar(decided$reviewed_at)), , drop = FALSE] else if (nrow(old)) old[1, , drop = FALSE] else empty_frame(review_columns)
    representative <- candidates$version_id[match(id, candidates$document_id)]
    rows[[j]] <- data.frame(document_id = id, version_id = representative,
      decision = if (nrow(chosen)) chosen$decision[1] else "pending",
      notes = if (nrow(chosen)) chosen$notes[1] else "",
      reviewer = if (nrow(chosen)) chosen$reviewer[1] else "",
      reviewed_at = if (nrow(chosen)) chosen$reviewed_at[1] else "", stringsAsFactors = FALSE)
  }
  bind_rows_base(rows)[, review_columns, drop = FALSE]
}

apply_review <- function(candidates, review) {
  if (!nrow(candidates)) return(candidates)
  i <- match(candidates$document_id, review$document_id)
  apply_decision <- !is.na(i) & nzchar(review$decision[i])
  candidates$review_status <- ifelse(apply_decision, review$decision[i], "pending")
  candidates$review_notes <- ifelse(apply_decision, review$notes[i], "")
  candidates
}

run_crawl <- function(validation = TRUE, force = FALSE, source_ids = NULL,
                      from_year = NA_integer_, to_year = NA_integer_, max_valid = NA_integer_) {
  cfg <- yaml::read_yaml("config/case.yml")
  scalar_integer <- function(x, flag) {
    if (length(x) != 1L || is.na(x)) return(NA_integer_)
    if (x != as.integer(x)) stop(flag, " must be an integer")
    as.integer(x)
  }
  from_year <- scalar_integer(from_year, "--from-year")
  to_year <- scalar_integer(to_year, "--to-year")
  max_valid <- scalar_integer(max_valid, "--max-valid-per-source")
  if (!is.na(from_year)) cfg$start_year <- from_year
  if (!is.na(to_year)) cfg$end_year <- to_year
  if (!is.na(max_valid)) cfg$validation_max_valid_per_source <- max_valid
  if (cfg$start_year > cfg$end_year) stop("--from-year must not exceed --to-year")
  if (validation && (!is.numeric(cfg$validation_max_valid_per_source) ||
      cfg$validation_max_valid_per_source < 1L)) stop("--max-valid-per-source must be at least 1")
  sources <- safe_read_csv("config/sources.csv"); sources <- sources[tolower(sources$enabled) == "true", , drop = FALSE]
  if (length(source_ids)) {
    unknown <- setdiff(source_ids, sources$source_id)
    if (length(unknown)) stop("Unknown or disabled source id(s): ", paste(unknown, collapse = ", "))
    sources <- sources[sources$source_id %in% source_ids, , drop = FALSE]
  }
  terms <- safe_read_csv("config/terms.csv"); families <- safe_read_csv("config/families.csv")
  old <- safe_read_csv("data/candidates.csv"); old_review <- safe_read_csv("data/review.csv")
  old_corpus <- safe_read_csv("data/corpus.csv")
  old_diagnostics <- safe_read_csv("data/retrieval_diagnostics.csv")
  if (!nrow(old_review)) old_review <- safe_read_csv("data/manual_review.csv")
  records <- list(); diagnostics <- list(); k <- 0L; d <- 0L
  run_id <- paste0("run_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz="UTC"), "_", substr(digest::digest(runif(1), "sha256"), 1, 8))
  add_diag <- function(...) {
    args <- list(...); args$run_id <- run_id
    d <<- d + 1L; diagnostics[[d]] <<- do.call(record_diagnostic, args)
  }
  for (s in seq_len(nrow(sources))) {
    source <- sources[s, ]; message("[", source$source_id, "] discover")
    discovery_error <- NULL
    urls <- tryCatch(discover_source(source, cfg, force), error = function(e) {
      discovery_error <<- conditionMessage(e)
      character()
    })
    if (is.null(discovery_error))
      add_diag(source$source_id, source$default_families, source$start_url, "discovery", "completed", paste(length(urls), "document URLs discovered"))
    else
      add_diag(source$source_id, source$default_families, source$start_url, "discovery", "failed", discovery_error)
    valid <- 0L; valid_documents <- character(); queue <- urls; seen_urls <- character(); q <- 1L
    while (q <= length(queue)) {
      url <- queue[q]; q <- q + 1L
      if (canonicalize_url(url) %in% seen_urls) next
      seen_urls <- c(seen_urls, canonicalize_url(url))
      if (validation && length(valid_documents) >= as.integer(cfg$validation_max_valid_per_source)) break
      rec <- tryCatch(process_url(url, source, cfg, terms, families, force), error = function(e) {
        status <- diagnostic_status(e)
        add_diag(source$source_id, source$default_families, url, "retrieval", status, conditionMessage(e)); NULL
      })
      if (!is.null(rec)) {
        k <- k + 1L; records[[k]] <- rec; valid <- valid + 1L
        valid_documents <- unique(c(valid_documents, rec$document_id)); message("  candidate: ", rec$title)
        alts <- strsplit(rec$official_alternate_urls %||% "", "\\|")[[1]]
        domains <- source_domains(source)
        alts <- alts[nzchar(alts) & vapply(alts, function(z) any(vapply(domains, grepl, logical(1), x=z, fixed=TRUE)), logical(1))]
        queue <- unique(c(queue, alts))
      }
    }
    add_diag(source$source_id, source$required_family %||% source$default_families, source$start_url, "validation", "completed", paste(length(valid_documents), "valid logical documents;", valid, "official-language records"))
  }
  fresh <- bind_rows_base(records)
  if (nrow(fresh)) {
    for (id in unique(fresh$document_id)) {
      z <- fresh[fresh$document_id == id, , drop=FALSE]
      langs <- unique(z$language[z$language %in% c("en","fr")])
      if (length(langs) == 1L) add_diag(z$source_id[1], z$families[1], z$document_url[1],
        "counterpart", "missing_counterpart", paste("No retrievable official", if (langs == "en") "French" else "English", "counterpart"))
    }
  }
  combined <- deduplicate_candidates(bind_rows_base(list(old, fresh)))
  if (!nrow(combined)) combined <- empty_frame(candidate_columns)
  review <- sync_review(combined, old_review); combined <- apply_review(combined, review)
  if (nrow(old_review)) {
    oi <- old_review[nzchar(old_review$decision) & old_review$decision != "pending", c("document_id","decision"), drop=FALSE]
    ri <- match(oi$document_id, review$document_id)
    if (any(is.na(ri)) || any(review$decision[ri] != oi$decision)) stop("Existing review decision preservation check failed")
    decided <- review[nzchar(review$decision) & review$decision != "pending", c("document_id", "decision"), drop = FALSE]
    authorized <- paste(oi$document_id, oi$decision)
    if (any(!paste(decided$document_id, decided$decision) %in% authorized))
      stop("Unexpected review decision propagation check failed")
  }
  combined <- combined[order(combined$year, combined$title, na.last = TRUE), candidate_columns, drop = FALSE]
  corpus <- combined[combined$review_status %in% c("retain", "include"), , drop = FALSE]
  if (any(vapply(combined[c("title", "relevance_excerpt", "extracted_text")],
                 contains_mojibake, logical(1))))
    stop("Candidate serialization blocked: mojibake remains in title, excerpt, or extracted text")
  new_diag <- bind_rows_base(diagnostics)
  if (nrow(new_diag)) new_diag <- new_diag[!duplicated(new_diag[setdiff(diagnostic_columns, "occurred_at")]), , drop=FALSE]
  diag <- bind_rows_base(list(old_diagnostics, new_diag)); if (!nrow(diag)) diag <- empty_frame(diagnostic_columns)
  run_summary <- diagnostic_run_summary(new_diag, run_id)
  # New discoveries are pending, so a bounded validation must not change the
  # retained corpus. Fail before writing if an existing retained record moved.
  if (nrow(old_corpus)) {
    old_ids <- sort(old_corpus$version_id); new_old <- corpus[corpus$version_id %in% old_ids, , drop=FALSE]
    if (!identical(old_ids, sort(new_old$version_id))) stop("Corpus preservation check failed")
  }
  write_csv(combined, "data/candidates.csv"); write_csv(review, "data/review.csv")
  if (!validation) write_csv(corpus, "data/corpus.csv")
  write_csv(terms, "data/concepts.csv"); write_csv(diag, "data/retrieval_diagnostics.csv")
  message(sprintf("Done: %d candidates, %d retained; this run: %d completed, %d excluded, %d failed events.",
                  nrow(combined), nrow(corpus), run_summary[["completed"]],
                  sum(run_summary[c("excluded","non_document","outside_family")]), run_summary[["failed"]]))
  invisible(list(candidates = combined, corpus = corpus, diagnostics = diag,
                 current_run_diagnostics = new_diag, diagnostic_summary = run_summary,
                 run_id = run_id))
}

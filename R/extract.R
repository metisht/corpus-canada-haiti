extract_content <- function(path, content_type, url) {
  magic <- tryCatch(rawToChar(readBin(path, "raw", n = 5)), error = function(e) "")
  is_pdf <- grepl("pdf", content_type, ignore.case = TRUE) && identical(magic, "%PDF-")
  if (is_pdf) {
    pages <- pdftools::pdf_text(path)
    txt <- clean_space(paste(pages, collapse = "\n"))
    info <- tryCatch(pdftools::pdf_info(path), error = function(e) list(keys = list()))
    pdf_title <- tryCatch(info$keys[["Title"]], error = function(e) NULL)
    title <- clean_space(pdf_title %||% basename(httr2::url_parse(url)$path))
    return(list(title = title, text = txt, format = "PDF", alternate_urls = character()))
  }
  doc <- rvest::read_html(path)
  alt_nodes <- rvest::html_elements(doc, "link[rel='alternate'][href], a[hreflang][href], a[lang][href]")
  alt_href <- rvest::html_attr(alt_nodes, "href")
  alt_lang <- paste(rvest::html_attr(alt_nodes, "hreflang"), rvest::html_attr(alt_nodes, "lang"))
  keep_alt <- !is.na(alt_href) & grepl("(^|[^a-z])(en|fr|eng|fra)([^a-z]|$)", alt_lang, ignore.case = TRUE)
  alternate_urls <- unique(xml2::url_absolute(alt_href[keep_alt], url))
  rvest::html_elements(doc, "script,style,nav,footer,header,form,aside") |> xml2::xml_remove()
  title <- clean_space(rvest::html_text2(rvest::html_element(doc, "h1")))
  if (!nzchar(title)) title <- clean_space(rvest::html_text2(rvest::html_element(doc, "title")))
  main <- rvest::html_element(doc, "main, article, [role='main']")
  if (inherits(main, "xml_missing")) main <- rvest::html_element(doc, "body")
  list(title = title, text = clean_space(rvest::html_text2(main)), format = "HTML", alternate_urls = alternate_urls)
}

case_matches <- function(text, labels) {
  hit <- labels[vapply(labels, function(z) grepl(paste0("(?i)\\b", z, "\\w*"), text, perl = TRUE), logical(1))]
  unique(hit)
}

extract_date <- function(text, title = "", url = "") {
  x <- paste(title, substr(text, 1, 12000), url)
  iso <- regmatches(x, regexpr("\\b(19|20)\\d{2}[-/](0[1-9]|1[0-2])[-/]([0-2]\\d|3[01])\\b", x, perl = TRUE))
  if (nzchar(iso)) return(gsub("/", "-", iso))
  y <- regmatches(x, regexpr("\\b(19|20)\\d{2}\\b", x, perl = TRUE))
  if (nzchar(y)) paste0(y, "-01-01") else NA_character_
}

parse_english_date <- function(x) {
  x <- sub("^(January|February|March|April|May|June|July|August|September|October|November|December) ([0-9]),", "\\1 0\\2,", x)
  as.character(suppressWarnings(as.Date(x, format = "%B %d, %Y")))
}

first_match <- function(pattern, text) {
  m <- regmatches(text, regexpr(pattern, text, ignore.case = TRUE, perl = TRUE))
  if (length(m) && nzchar(m)) m else NA_character_
}

extract_date_metadata <- function(text, url, source_id) {
  page_raw <- first_match("(?:Page details|Page Details )?(?:Date modified: ?|Date de modification: ?)?[^0-9]*(20\\d{2}-\\d{2}-\\d{2})$", text)
  if (is.na(page_raw)) page_raw <- first_match("(?:Page details|Page Details Date modified:|Date modified:|Date de modification:)[^0-9]*(20\\d{2}-\\d{2}-\\d{2})", text)
  page_modified <- if (!is.na(page_raw)) first_match("20\\d{2}-\\d{2}-\\d{2}", page_raw) else NA_character_
  event_start <- event_end <- publication <- NA_character_

  if (source_id == "forces_archive") {
    range <- regexec("(?:Canadian Operation Dates|Mission Date):? +([0-9]{1,4}[-/ ][A-Za-z0-9]+[-/ ,][0-9]{2,4}) *[–-] *([0-9]{1,4}[-/ ][A-Za-z0-9]+[-/ ,][0-9]{2,4})", text, ignore.case = TRUE, perl = TRUE)
    bits <- regmatches(text, range)[[1]]
    if (length(bits) == 3) {
      norm <- function(z) {
        z <- gsub("/", "-", z)
        d <- suppressWarnings(as.Date(z, tryFormats = c("%Y-%m-%d", "%d %B %Y")))
        if (is.na(d)) NA_character_ else as.character(d)
      }
      event_start <- norm(bits[2]); event_end <- norm(bits[3])
    }
  } else if (grepl("gazette.gc.ca", url, fixed = TRUE)) {
    publication <- first_match("20\\d{2}-\\d{2}-\\d{2}", url)
  } else if (grepl("laws-lois.justice.gc.ca", url, fixed = TRUE)) {
    publication <- first_match("Registration +(20\\d{2}-\\d{2}-\\d{2})", text)
    publication <- first_match("20\\d{2}-\\d{2}-\\d{2}", publication)
  } else if (source_id == "gac_news") {
    raw <- first_match("(January|February|March|April|May|June|July|August|September|October|November|December) +[0-9]{1,2}, +20\\d{2} *(?:[–-])", substr(text, 1, 2000))
    if (!is.na(raw)) publication <- parse_english_date(trimws(sub("[–-].*$", "", raw)))
  }
  analysis_date <- if (!is.na(event_start)) event_start else if (!is.na(publication)) publication else page_modified
  list(publication_date = publication, page_last_modified = page_modified,
       event_start_date = event_start, event_end_date = event_end,
       year = suppressWarnings(as.integer(substr(analysis_date, 1, 4))),
       date_source = if (!is.na(event_start)) "event_start_date" else if (!is.na(publication)) "publication_date" else if (!is.na(page_modified)) "page_last_modified; publication date uncertain" else "uncertain")
}

derive_document_metadata <- function(title, text, url, source) {
  host <- tolower(httr2::url_parse(url)$hostname %||% "")
  if (source$source_id == "forces_archive")
    return(list(institution = "National Defence", families = "armed_forces", document_type = "operation_record"))
  if (host == "gazette.gc.ca")
    return(list(institution = "Canada Gazette", families = "sanctions", document_type = "regulation"))
  if (host == "laws-lois.justice.gc.ca")
    return(list(institution = "Department of Justice Canada", families = "sanctions", document_type = "regulation"))
  if (grepl("international.gc.ca$", host) && grepl("stories-histoires", url))
    return(list(institution = "Global Affairs Canada", families = "development", document_type = "project_story"))
  if (source$source_id == "gac_news") {
    kind <- if (grepl("\\bStatement\\b", substr(text, 1, 500), ignore.case = TRUE)) "statement" else "news_release"
    substantive <- if (grepl("sanction", title, ignore.case = TRUE)) "sanctions" else if (grepl("funding|assistance|aid", title, ignore.case = TRUE)) "development" else character()
    return(list(institution = "Global Affairs Canada", families = paste(c("statements", substantive), collapse = "|"), document_type = kind))
  }
  list(institution = source$institution, families = source$default_families, document_type = if (grepl("report", title, ignore.case=TRUE)) "report" else if (grepl("regulation", title, ignore.case=TRUE)) "regulation" else "other")
}

language_of <- function(text) {
  low <- tolower(substr(text, 1, 20000))
  fr <- sum(lengths(regmatches(low, gregexpr("\\b(le|la|les|des|une|pour|avec|dans|haïti)\\b", low, perl = TRUE))))
  en <- sum(lengths(regmatches(low, gregexpr("\\b(the|and|of|for|with|in|haiti)\\b", low, perl = TRUE))))
  if (fr > en * 1.25) "fr" else if (en > fr * 1.25) "en" else "bilingual_or_uncertain"
}

code_terms <- function(text, terms) {
  n <- vapply(terms$pattern, function(p) { m <- gregexpr(p, text, ignore.case = TRUE, perl = TRUE)[[1]]; if (m[1] == -1) 0L else length(m) }, integer(1))
  paste(terms$term_id[n > 0], collapse = "|")
}

classify_families <- function(text, defaults, families) {
  hit <- families$family_id[vapply(families$patterns, grepl, logical(1), x = text, ignore.case = TRUE, perl = TRUE)]
  paste(unique(c(strsplit(defaults %||% "", "\\|" )[[1]], hit))[nzchar(unique(c(strsplit(defaults %||% "", "\\|")[[1]], hit)))], collapse = "|")
}

relevance_excerpt <- function(text, labels, n = 500) {
  positions <- unlist(lapply(labels, function(z) regexpr(tolower(z), tolower(text), fixed = TRUE)[1]))
  positions <- positions[positions > 0]
  start <- if (length(positions)) max(1, min(positions) - floor(n / 3)) else 1
  substr(text, start, start + n - 1)
}

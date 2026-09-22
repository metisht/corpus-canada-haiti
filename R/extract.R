extract_content <- function(path, content_type, url) {
  is_pdf <- grepl("pdf", content_type, ignore.case = TRUE) || grepl("\\.pdf($|[?#])", path, ignore.case = TRUE)
  if (is_pdf) {
    pages <- pdftools::pdf_text(path)
    txt <- clean_space(paste(pages, collapse = "\n"))
    info <- tryCatch(pdftools::pdf_info(path), error = function(e) list(keys = list()))
    pdf_title <- tryCatch(info$keys[["Title"]], error = function(e) NULL)
    title <- clean_space(pdf_title %||% basename(httr2::url_parse(url)$path))
    return(list(title = title, text = txt, format = "PDF"))
  }
  doc <- rvest::read_html(path)
  rvest::html_elements(doc, "script,style,nav,footer,header,form,aside") |> xml2::xml_remove()
  title <- clean_space(rvest::html_text2(rvest::html_element(doc, "h1")))
  if (!nzchar(title)) title <- clean_space(rvest::html_text2(rvest::html_element(doc, "title")))
  main <- rvest::html_element(doc, "main, article, [role='main']")
  if (inherits(main, "xml_missing")) main <- rvest::html_element(doc, "body")
  list(title = title, text = clean_space(rvest::html_text2(main)), format = "HTML")
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

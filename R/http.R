# Polite HTTP retrieval and local response caching for crawler runs.
# Called with official URLs and case configuration; returns cached-file metadata.
# Cache writes occur only on misses, with per-host delays and explicit timeouts.

.last_request_time <- new.env(parent = emptyenv())

cache_path <- function(url, ext = "bin") file.path("data", "cache", paste0(digest::digest(url, "sha256"), ".", ext))

is_pdf_bytes <- function(raw) {
  length(raw) >= 5L && identical(raw[seq_len(5L)], charToRaw("%PDF-"))
}

is_publications_archive_pdf <- function(url) {
  parsed <- httr2::url_parse(url)
  identical(tolower(parsed$hostname %||% ""), "publications.gc.ca") &&
    grepl("/collections/.*\\.pdf$", parsed$path %||% "", ignore.case = TRUE)
}

perform_publications_archive_request <- function(req) {
  cookie_file <- tempfile("publications-archive-", fileext = ".cookies")
  on.exit(unlink(cookie_file), add = TRUE)
  first <- req |>
    httr2::req_cookie_preserve(cookie_file) |>
    httr2::req_perform()
  first_type <- httr2::resp_header(first, "content-type") %||% ""
  if (!grepl("html", first_type, ignore.case = TRUE)) return(first)
  first_body <- httr2::resp_body_string(first)
  if (!grepl("Information Archived on the Web|Information archivée dans le Web",
             first_body, ignore.case = TRUE)) return(first)
  req |>
    httr2::req_headers(Referer = httr2::resp_url(first)) |>
    httr2::req_cookie_preserve(cookie_file) |>
    httr2::req_perform()
}

polite_get <- function(url, cfg, force = FALSE) {
  dir.create("data/cache", recursive = TRUE, showWarnings = FALSE)
  guessed <- if (grepl("\\.pdf($|[?#])", url, ignore.case = TRUE)) "pdf" else "html"
  path <- cache_path(url, guessed)
  if (!force && file.exists(path) && file.info(path)$size > 0)
    return(list(path = path, status = 200L, type = if (guessed == "pdf") "application/pdf" else "text/html", cached = TRUE))
  host <- httr2::url_parse(url)$hostname %||% "unknown"
  last <- .last_request_time[[host]] %||% as.POSIXct(0, origin = "1970-01-01", tz = "UTC")
  pause <- as.numeric(cfg$request_delay_seconds) - as.numeric(difftime(Sys.time(), last, units = "secs"))
  if (pause > 0) Sys.sleep(pause)
  req <- httr2::request(url) |>
    httr2::req_user_agent(cfg$user_agent) |>
    httr2::req_timeout(as.numeric(cfg$request_timeout_seconds)) |>
    httr2::req_retry(max_tries = as.integer(cfg$request_retries), backoff = function(...) 2)
  resp <- if (is_publications_archive_pdf(url)) {
    perform_publications_archive_request(req)
  } else {
    httr2::req_perform(req)
  }
  .last_request_time[[host]] <- Sys.time()
  type <- httr2::resp_header(resp, "content-type") %||% ""
  if (!grepl("html|pdf|text/plain|json|xml", type, ignore.case = TRUE)) stop("Unsupported content type: ", type)
  raw <- httr2::resp_body_raw(resp)
  if (!length(raw)) stop("Empty response")
  if (is_publications_archive_pdf(url) &&
      (!grepl("application/pdf", type, ignore.case = TRUE) ||
       !is_pdf_bytes(raw)))
    stop("Official archive destination did not return verified PDF content")
  if (grepl("pdf", type, ignore.case = TRUE) && guessed != "pdf") {
    path <- cache_path(url, "pdf")
  }
  writeBin(raw, path)
  list(path = path, status = httr2::resp_status(resp), type = type, cached = FALSE)
}

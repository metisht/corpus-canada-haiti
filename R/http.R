.last_request_time <- new.env(parent = emptyenv())

cache_path <- function(url, ext = "bin") file.path("data", "cache", paste0(digest::digest(url, "sha256"), ".", ext))

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
  resp <- httr2::req_perform(req)
  .last_request_time[[host]] <- Sys.time()
  type <- httr2::resp_header(resp, "content-type") %||% ""
  if (!grepl("html|pdf|text/plain|json|xml", type, ignore.case = TRUE)) stop("Unsupported content type: ", type)
  raw <- httr2::resp_body_raw(resp)
  if (!length(raw)) stop("Empty response")
  if (grepl("pdf", type, ignore.case = TRUE) && guessed != "pdf") {
    path <- cache_path(url, "pdf")
  }
  writeBin(raw, path)
  list(path = path, status = httr2::resp_status(resp), type = type, cached = FALSE)
}

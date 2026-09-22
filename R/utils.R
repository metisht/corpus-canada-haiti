`%||%` <- function(x, y) if (is.null(x) || !length(x) || all(is.na(x))) y else x

now_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
clean_space <- function(x) trimws(gsub("\\s+", " ", iconv(x %||% "", from = "", to = "UTF-8", sub = "")))

canonicalize_url <- function(x) {
  x <- trimws(x)
  x <- sub("#.*$", "", x)
  x <- gsub("([?&])(utm_[^=&]+|wbdisable|pedisable|_ga)=[^&]*", "", x, ignore.case = TRUE)
  x <- gsub("[?&]+$", "", x)
  sub("/$", "", x)
}

official_work_key <- function(url, alternate_urls = character()) {
  u <- canonicalize_url(c(url, alternate_urls))
  host <- tolower(vapply(u, function(z) httr2::url_parse(z)$hostname %||% "", character(1)))
  # Canada Gazette English/French files share the exact SOR/DORS identifier.
  if (any(host == "gazette.gc.ca")) {
    z <- u[host == "gazette.gc.ca"][1]
    id <- regmatches(z, regexpr("(?i)sor-dors[0-9]+", z, perl = TRUE))
    issue <- regmatches(z, regexpr("/p[12]/[0-9]{4}/[0-9]{4}-[0-9]{2}-[0-9]{2}/", z, perl = TRUE))
    if (length(id) && nzchar(id)) return(paste0("gazette:", tolower(issue), tolower(id)))
  }
  # Consolidated Justice Laws are a distinct publication from Gazette originals.
  if (any(host == "laws-lois.justice.gc.ca")) {
    z <- u[host == "laws-lois.justice.gc.ca"][1]
    id <- regmatches(z, regexpr("(?i)(SOR|DORS)-[0-9]{4}-[0-9]+", z, perl = TRUE))
    if (length(id) && nzchar(id)) return(paste0("justice:consolidated:", toupper(id)))
  }
  # International Canada uses an explicit language query on the same page path.
  if (any(grepl("international.gc.ca$", host))) {
    z <- u[grepl("international.gc.ca$", host)][1]
    return(paste0("international:", sub("([?&])lang=(eng|fra)(&|$)", "", z, ignore.case = TRUE)))
  }
  # For Canada.ca, only an official alternate-language link authorizes pairing.
  if (length(alternate_urls)) {
    normalized <- sort(unique(sub("https://www.canada.ca/(en|fr)/", "https://www.canada.ca/{lang}/", u, ignore.case = TRUE)))
    return(paste0("official-alternates:", paste(normalized, collapse = "|")))
  }
  paste0("official-version:", canonicalize_url(url))
}

stable_document_id <- function(url, alternate_urls = character()) paste0("doc_", substr(digest::digest(official_work_key(url, alternate_urls), "sha256"), 1, 16))
stable_version_id <- function(url, language) paste0("ver_", substr(digest::digest(paste(canonicalize_url(url), language, sep = "|"), "sha256"), 1, 16))

safe_read_csv <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0) return(data.frame())
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("NA"), encoding = "UTF-8")
}

bind_rows_base <- function(xs) {
  xs <- Filter(function(x) !is.null(x) && nrow(x), xs)
  if (!length(xs)) return(data.frame())
  nms <- unique(unlist(lapply(xs, names)))
  xs <- lapply(xs, function(x) { for (nm in setdiff(nms, names(x))) x[[nm]] <- NA; x[nms] })
  out <- do.call(rbind, xs)
  rownames(out) <- NULL
  out
}

write_csv <- function(x, path) write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")

record_diagnostic <- function(source_id, family, url, stage, status, message = "", http_status = NA_integer_) {
  data.frame(source_id, family, url, stage, status, message = substr(message, 1, 1000),
             http_status, occurred_at = now_utc(), stringsAsFactors = FALSE)
}

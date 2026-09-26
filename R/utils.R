# Shared utilities for identifiers, UTF-8 normalization, CSV I/O, and diagnostics.
# Loaded by crawler, reconciliation, migration, and tests. File writes occur only
# through write_csv(); stable IDs and lossless UTF-8 are core invariants.

`%||%` <- function(x, y) if (is.null(x) || !length(x) || all(is.na(x))) y else x

now_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
normalize_utf8 <- function(x) {
  x <- enc2utf8(x %||% "")
  iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
}

clean_space <- function(x) trimws(gsub("\\s+", " ", normalize_utf8(x)))

contains_mojibake <- function(x) {
  any(grepl("Ã|â€™|D├⌐|ΓÇö", x %||% ""), na.rm = TRUE)
}

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
  if (any(grepl("international\\.(?:gc|canada)\\.ca$", host))) {
    z <- u[grepl("international\\.(?:gc|canada)\\.ca$", host)][1]
    review_id <- regmatches(z, regexpr("(?i)(?:sor|dors)-[0-9]{4}-[0-9]+", z, perl = TRUE))
    if (length(review_id) && nzchar(review_id))
      return(paste0("international:regulatory-review:", sub("^(sor|dors)-", "", tolower(review_id))))
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

parliament_document_key <- function(url, document_type = "") {
  normalized <- canonicalize_url(url)
  modern <- regexec("/(?:en|fr)/(\\d+)-(\\d+)/(?:house|chambre)/(?:sitting|seance)-(\\d+)/(hansard|debats|journals|journaux)", normalized, ignore.case = TRUE, perl = TRUE)
  bits <- regmatches(normalized, modern)[[1]]
  if (length(bits) == 5L) {
    kind <- if (tolower(bits[5]) %in% c("hansard", "debats")) "house_debate" else "house_journal"
    return(paste("parliament", bits[2], bits[3], kind, as.integer(bits[4]), sep = ":"))
  }
  pdf <- regexec("/Content/House/(\\d{2})(\\d)/(Debates|Journals)/(\\d+)/", normalized, ignore.case = TRUE, perl = TRUE)
  bits <- regmatches(normalized, pdf)[[1]]
  if (length(bits) == 5L) {
    kind <- if (tolower(bits[4]) == "debates") "house_debate" else "house_journal"
    return(paste("parliament", bits[2], bits[3], kind, as.integer(bits[5]), sep = ":"))
  }
  historical <- regexec("/(?:files/get|view)/([^/]+)/(\\d+)$", normalized, ignore.case = TRUE, perl = TRUE)
  bits <- regmatches(normalized, historical)[[1]]
  if (length(bits) == 3L) {
    key <- historical_volume_pair_key(bits[2])
    return(paste("historical_parliament", key, as.integer(bits[3]), sep = ":"))
  }
  official_work_key(normalized)
}

stable_parliament_document_id <- function(url, document_type = "") {
  paste0("doc_", substr(digest::digest(parliament_document_key(url, document_type), "sha256"), 1, 16))
}

historical_publication_key <- function(collection, publication_type, publication_date,
                                       sitting_number = "") {
  paste("historical_parliament", collection, publication_type,
        publication_date, sitting_number, sep = ":")
}

stable_historical_document_id <- function(collection, publication_type,
                                          publication_date, sitting_number = "") {
  key <- historical_publication_key(collection, publication_type,
                                    publication_date, sitting_number)
  paste0("doc_", substr(digest::digest(key, "sha256"), 1, 16))
}

stable_historical_version_id <- function(document_id, language = "bilingual") {
  paste0("ver_", substr(digest::digest(paste(document_id, language, sep = "|"),
                                        "sha256"), 1, 16))
}

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

record_diagnostic <- function(source_id, family, url, stage, status, message = "", http_status = NA_integer_, run_id = "") {
  data.frame(run_id, source_id, family, url, stage, status, message = substr(message, 1, 1000),
             http_status, occurred_at = now_utc(), stringsAsFactors = FALSE)
}

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

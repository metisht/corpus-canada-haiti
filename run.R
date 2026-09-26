#!/usr/bin/env Rscript
# CLI entry point for bounded validation and explicitly requested full crawls.
# Loads crawler modules, validates options, then delegates all data/cache effects
# to run_crawl(). Validation limits apply per source unless --full is supplied.

options(warn = 1)
required <- c("yaml", "httr2", "rvest", "xml2", "jsonlite", "pdftools", "digest")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Run: Rscript scripts/install_packages.R")
for (f in c("R/utils.R", "R/http.R", "R/discover.R", "R/extract.R", "R/crawl.R")) source(f, encoding = "UTF-8")
args <- commandArgs(trailingOnly = TRUE)
full <- "--full" %in% args
force <- "--force" %in% args
source_arg <- args[grepl("^--sources=", args)]
source_ids <- if (length(source_arg)) strsplit(sub("^--sources=", "", source_arg[1]), ",", fixed = TRUE)[[1]] else NULL
arg_value <- function(name, default = NA_character_) {
  hit <- args[grepl(paste0("^--", name, "="), args)]
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
}
from_year <- suppressWarnings(as.integer(arg_value("from-year")))
to_year <- suppressWarnings(as.integer(arg_value("to-year")))
max_valid_per_source_arg <- arg_value("max-valid-per-source")
legacy_max_valid_arg <- arg_value("max-valid")
if (!is.na(max_valid_per_source_arg) && !is.na(legacy_max_valid_arg))
  stop("Use either --max-valid-per-source or the legacy --max-valid alias, not both")
max_valid <- suppressWarnings(as.integer(if (!is.na(max_valid_per_source_arg)) max_valid_per_source_arg else legacy_max_valid_arg))
message(if (full) "FULL live crawl explicitly requested." else
  paste0("LIMITED live validation; --max-valid is a per-source logical-document cap",
         if (!is.na(max_valid)) paste0(" (", max_valid, " per source).") else "."))
run_crawl(validation = !full, force = force, source_ids = source_ids,
          from_year = from_year, to_year = to_year, max_valid = max_valid)

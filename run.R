#!/usr/bin/env Rscript
options(warn = 1)
required <- c("yaml", "httr2", "rvest", "xml2", "jsonlite", "pdftools", "digest")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Run: Rscript scripts/install_packages.R")
for (f in c("R/utils.R", "R/http.R", "R/discover.R", "R/extract.R", "R/crawl.R")) source(f, encoding = "UTF-8")
args <- commandArgs(trailingOnly = TRUE)
full <- "--full" %in% args
force <- "--force" %in% args
message(if (full) "FULL live crawl explicitly requested." else "LIMITED live validation: at most five valid candidates per source.")
run_crawl(validation = !full, force = force)

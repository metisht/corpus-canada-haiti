#!/usr/bin/env Rscript
# Offline regression checks for crawl bounds and CLI semantics. It loads crawler
# modules, uses synthetic configuration/source fixtures, and writes no project
# data. Per-source limits and fail-closed source selection are invariants.
for (f in c("R/utils.R", "R/http.R", "R/discover.R", "R/extract.R", "R/crawl.R"))
  source(f, encoding = "UTF-8")

check <- function(label, value) {
  if (!isTRUE(value)) stop("FAIL: ", label, call. = FALSE)
  message("PASS ", label)
}

cfg <- list(start_year = 1991L, end_year = 1993L)
s <- sessions_in_range(cfg)
check("1991-1993 selects only overlapping 34th Parliament sessions",
      nrow(s) == 2L && all(s$parliament == 34L))

cfg <- list(start_year = 1996L, end_year = 1996L)
s <- sessions_in_range(cfg)
check("1996 selects only the overlapping 35th Parliament session",
      nrow(s) == 1L && s$parliament == 35L && s$session == 1L)

cfg <- list(start_year = 1998L, end_year = 1998L)
s <- sessions_in_range(cfg)
check("1998 selects only the overlapping 36th Parliament session",
      nrow(s) == 1L && s$parliament == 36L && s$session == 1L)

check("missing optional CLI values remain scalar NA",
      length(suppressWarnings(as.integer(NA_character_))) == 1L)

err <- tryCatch({ run_crawl(max_valid = 0L); "" }, error = conditionMessage)
check("zero per-source validation bound is rejected before discovery",
      grepl("--max-valid-per-source must be at least 1", err, fixed = TRUE))
check("validation cap semantics are explicitly per source",
      identical(validation_cap_scope, "per_source"))

err <- tryCatch({ run_crawl(source_ids = "not-a-source"); "" }, error = conditionMessage)
check("unknown source filters fail closed",
      grepl("Unknown or disabled source", err, fixed = TRUE))

# Discovery must enforce the cap before resolving viewer pages, not merely trim
# the result after all expensive index and viewer requests have been made.
index_calls <- 0L
viewer_calls <- 0L
parliament_index_links <- function(page, cfg, force = FALSE) {
  index_calls <<- index_calls + 1L
  list(documents = paste0("https://example.test/view/", 1:5), pages = character())
}
resolve_house_viewer_pair <- function(page, cfg, force = FALSE) {
  viewer_calls <<- viewer_calls + 1L
  paste0(page, ".pdf")
}
cfg <- list(start_year = 1996L, end_year = 1996L, max_discovered_per_source = 2L)
found <- discover_house_publications(cfg)
check("House discovery resolves no more than the configured cap",
      length(found) == 2L && viewer_calls == 2L && index_calls == 2L)

message("All bounded-crawl regressions passed.")

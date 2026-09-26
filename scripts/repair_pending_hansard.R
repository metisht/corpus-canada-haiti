#!/usr/bin/env Rscript
# Narrow maintenance command for pending House debate viewer records.
# Reads candidates/review plus official viewer/PDF routes, then rewrites candidates
# and review only after preservation checks. Intended for explicit targeted use.
for (file in c("R/utils.R", "R/http.R", "R/discover.R", "R/extract.R", "R/crawl.R"))
  source(file, encoding = "UTF-8")

cfg <- yaml::read_yaml("config/case.yml")
cfg$start_year <- 1991L
cfg$end_year <- 1998L
sources <- safe_read_csv("config/sources.csv")
source_row <- sources[sources$source_id == "parliament", , drop = FALSE][1, ]
terms <- safe_read_csv("config/terms.csv")
families <- safe_read_csv("config/families.csv")
old <- safe_read_csv("data/candidates.csv")
review <- safe_read_csv("data/review.csv")
viewers <- old$document_url[
  old$source_id == "parliament" & old$document_type == "house_debate" &
    old$review_status == "pending" & toupper(old$format) == "HTML"
]
urls <- unique(unlist(lapply(viewers, resolve_house_viewer_pair, cfg = cfg, force = TRUE), use.names = FALSE))
fresh <- bind_rows_base(lapply(urls, function(url) {
  tryCatch(process_url(url, source_row, cfg, terms, families, TRUE), error = function(error) NULL)
}))
combined <- deduplicate_candidates(bind_rows_base(list(old, fresh)))
review <- sync_review(combined, review)
combined <- apply_review(combined, review)
combined <- combined[order(combined$year, combined$title, na.last = TRUE), candidate_columns, drop = FALSE]
stopifnot(!any(combined$review_status != "pending" & !combined$version_id %in% old$version_id))
stopifnot(!any(vapply(combined[c("title", "relevance_excerpt", "extracted_text")], contains_mojibake, logical(1))))
write_csv(combined, "data/candidates.csv")
write_csv(review, "data/review.csv")
message("Repaired ", length(viewers), " pending viewers from ", length(urls),
        " localized PDFs; ", nrow(combined), " candidates remain.")

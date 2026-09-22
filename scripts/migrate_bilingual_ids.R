# One-time local migration from the original one-row/one-document schema.
# It reads only existing CSVs and cached responses; it performs no HTTP requests.
for (f in c("R/utils.R", "R/extract.R")) source(f, encoding = "UTF-8")

candidates <- safe_read_csv("data/candidates.csv")
review <- safe_read_csv("data/review.csv")
old_ids <- candidates$document_id
old_versions <- if ("version_id" %in% names(candidates)) candidates$version_id else rep(NA_character_, nrow(candidates))

cached_content <- function(url) {
  stem <- digest::digest(url, "sha256")
  paths <- file.path("data", "cache", paste0(stem, c(".html", ".pdf", ".bin")))
  path <- paths[file.exists(paths)][1]
  if (is.na(path)) return(list(text = "", alternate_urls = character()))
  magic <- tryCatch(rawToChar(readBin(path, "raw", n=5)), error=function(e) "")
  type <- if (identical(magic, "%PDF-")) "application/pdf" else "text/html"
  tryCatch(extract_content(path, type, url), error=function(e) list(text = "", alternate_urls = character()))
}

cdata <- lapply(candidates$document_url, cached_content)
candidates$version_id <- mapply(stable_version_id, candidates$document_url, candidates$language, USE.NAMES = FALSE)
candidates$official_alternate_urls <- vapply(cdata, function(z) paste(z$alternate_urls, collapse="|"), character(1))
candidates$document_id <- mapply(stable_document_id, candidates$document_url, lapply(cdata, `[[`, "alternate_urls"), USE.NAMES = FALSE)
candidates$extracted_text <- vapply(cdata, `[[`, character(1), "text")

sources <- safe_read_csv("config/sources.csv")
for (i in seq_len(nrow(candidates))) {
  source <- sources[match(candidates$source_id[i], sources$source_id), , drop = FALSE]
  meta <- derive_document_metadata(candidates$title[i], candidates$extracted_text[i], candidates$document_url[i], source)
  dates <- extract_date_metadata(candidates$extracted_text[i], candidates$document_url[i], candidates$source_id[i])
  candidates$institution[i] <- meta$institution
  candidates$families[i] <- meta$families
  candidates$document_type[i] <- meta$document_type
  candidates$publication_date[i] <- dates$publication_date
  candidates$page_last_modified[i] <- dates$page_last_modified
  candidates$event_start_date[i] <- dates$event_start_date
  candidates$event_end_date[i] <- dates$event_end_date
  candidates$year[i] <- dates$year
  candidates$date_source[i] <- dates$date_source
  candidates$date_status[i] <- if (is.na(dates$year)) "uncertain" else "in_range"
  candidates$source_page[i] <- if (source$method == "seed_list") candidates$document_url[i] else source$start_url
}

# Reapply every prior version-level decision through the old-ID to new-ID map.
idx <- if ("version_id" %in% names(review) && any(!is.na(old_versions))) match(old_versions, review$version_id) else match(old_ids, review$document_id)
new_review <- data.frame(
  document_id = candidates$document_id,
  version_id = candidates$version_id,
  decision = ifelse(!is.na(idx), review$decision[idx], "pending"),
  notes = ifelse(!is.na(idx), review$notes[idx], ""),
  reviewer = ifelse(!is.na(idx), review$reviewer[idx], ""),
  reviewed_at = ifelse(!is.na(idx), review$reviewed_at[idx], ""),
  stringsAsFactors = FALSE
)

source("R/crawl.R", encoding = "UTF-8")
candidates <- candidates[candidate_columns]
candidates <- apply_review(candidates, new_review)
corpus <- candidates[candidates$review_status %in% c("retain", "include"), , drop = FALSE]
write_csv(candidates, "data/candidates.csv")
write_csv(new_review[review_columns], "data/review.csv")
write_csv(corpus, "data/corpus.csv")

cat(sprintf("Migrated %d versions into %d logical documents; preserved %d non-pending decisions.\n",
            nrow(candidates), length(unique(candidates$document_id)), sum(new_review$decision != "pending")))

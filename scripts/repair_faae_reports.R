# Repairs retained FAAE report metadata from authoritative report covers.
# The viewer remains source_page and its exposed PDF becomes document_url.
# Review decisions and document/version identities are never changed.

source("R/utils.R", encoding = "UTF-8")

repairs <- data.frame(
  document_id = c("doc_9ed93e8a637fe724", "doc_36f22a42bdeb158b"),
  title = c("The Human Rights Situation in Haiti", "Canada's International Policy Put to the Test in Haiti"),
  publication_date = c("2023-05-01", "2006-12-01"),
  year = c(2023L, 2006L),
  source_page = c(
    "https://www.ourcommons.ca/DocumentViewer/en/44-1/FAAE/report-15",
    "https://www.ourcommons.ca/DocumentViewer/en/39-1/FAAE/report-4"
  ),
  document_url = c(
    "https://www.ourcommons.ca/Content/Committee/441/FAAE/Reports/RP12363360/faaerp15/faaerp15-e.pdf",
    "https://www.ourcommons.ca/Content/Committee/391/FAAE/Reports/RP2593086/faaerp04/faaerp04-e.pdf"
  ),
  stringsAsFactors = FALSE
)

repair_file <- function(path) {
  data <- readr::read_csv(path, show_col_types = FALSE)
  for (index in seq_len(nrow(repairs))) {
    rows <- data$document_id == repairs$document_id[index]
    stopifnot(any(rows), all(data$review_status[rows] == "retain"))
    data$title[rows] <- repairs$title[index]
    data$publication_date[rows] <- repairs$publication_date[index]
    data$year[rows] <- repairs$year[index]
    data$date_source[rows] <- "authoritative committee-report cover month"
    data$date_status[rows] <- "in_range"
    data$document_type[rows] <- "committee_report"
    data$source_page[rows] <- repairs$source_page[index]
    data$document_url[rows] <- repairs$document_url[index]
    data$canonical_url[rows] <- repairs$document_url[index]
    data$format[rows] <- "PDF"
  }
  readr::write_csv(data, path, na = "")
}

repair_file("data/candidates.csv")
repair_file("data/corpus.csv")

provenance_repairs <- data.frame(
  version_id = c("ver_e691a4be321909cf", "ver_60ebf8ec2b2bd592"),
  document_id = "doc_d555c489f6b3563c",
  language = c("en", "fr"),
  source_page = c(
    "https://www.ourcommons.ca/DocumentViewer/en/45-1/FAAE/report-4",
    "https://www.noscommunes.ca/documentviewer/fr/45-1/FAAE/rapport-4"
  ),
  document_url = c(
    "https://www.ourcommons.ca/content/Committee/451/FAAE/Reports/RP13790973/451_FAAE_Rpt4_PDF/451_FAAE_Rpt4-e.pdf",
    "https://www.ourcommons.ca/content/Committee/451/FAAE/Reports/RP13790973/451_FAAE_Rpt4_PDF/451_FAAE_Rpt4-f.pdf"
  ),
  stringsAsFactors = FALSE
)

repair_provenance_file <- function(path) {
  data <- readr::read_csv(path, show_col_types = FALSE)
  for (index in seq_len(nrow(provenance_repairs))) {
    rows <- data$version_id == provenance_repairs$version_id[index]
    stopifnot(sum(rows) == 1L,
              data$document_id[rows] == provenance_repairs$document_id[index],
              data$language[rows] == provenance_repairs$language[index],
              canonicalize_url(data$document_url[rows]) ==
                canonicalize_url(provenance_repairs$document_url[index]))
    data$source_page[rows] <- provenance_repairs$source_page[index]
  }
  readr::write_csv(data, path, na = "")
}

repair_provenance_file("data/candidates.csv")
repair_provenance_file("data/corpus.csv")

changes_path <- "data/validation_changes.csv"
changes <- readr::read_csv(changes_path, show_col_types = FALSE)
for (index in seq_len(nrow(repairs))) {
  document_id <- repairs$document_id[index]
  candidate <- readr::read_csv("data/candidates.csv", show_col_types = FALSE)
  row <- candidate[candidate$document_id == document_id, , drop = FALSE][1, ]
  audit_key <- paste("faae_report_metadata_repaired", document_id, repairs$document_url[index], sep = "|")
  existing_key <- paste(changes$change, changes$document_id, changes$document_url, sep = "|")
  if (!audit_key %in% existing_key) {
    addition <- as.list(rep("", length(names(changes))))
    names(addition) <- names(changes)
    addition$change <- "faae_report_metadata_repaired"
    addition$version_id <- row$version_id
    addition$document_id <- document_id
    addition$language <- row$language
    addition$title <- repairs$title[index]
    addition$document_url <- repairs$document_url[index]
    addition$review_status <- row$review_status
    addition$old_document_id <- document_id
    addition$new_document_id <- document_id
    addition$rule <- "authoritative_report_cover_and_viewer_pdf"
    addition$reason <- "Replaced generic report title and viewer-as-document metadata with the authoritative title, cover month, viewer source page, and exposed PDF."
    addition$source <- "parliament"
    addition$previous_state <- "generic title; uncertain date; viewer document_url"
    addition$resulting_state <- paste(repairs$title[index], repairs$publication_date[index], "direct PDF", sep = "; ")
    addition$manual_decision <- "retain"
    addition$reviewer <- "metadata correction only"
    addition$review_timestamp <- now_utc()
    changes <- rbind(changes, as.data.frame(addition, stringsAsFactors = FALSE))
  }
}
for (index in seq_len(nrow(provenance_repairs))) {
  repair <- provenance_repairs[index, ]
  audit_key <- paste("faae_report_provenance_repaired", repair$version_id,
                     repair$source_page, sep = "|")
  existing_key <- paste(changes$change, changes$version_id,
                        changes$resulting_state, sep = "|")
  if (!audit_key %in% existing_key) {
    candidate <- readr::read_csv("data/candidates.csv", show_col_types = FALSE)
    row <- candidate[candidate$version_id == repair$version_id, , drop = FALSE][1, ]
    addition <- as.list(rep("", length(names(changes))))
    names(addition) <- names(changes)
    addition$change <- "faae_report_provenance_repaired"
    addition$version_id <- repair$version_id
    addition$affected_version_id <- repair$version_id
    addition$document_id <- repair$document_id
    addition$language <- repair$language
    addition$title <- row$title
    addition$document_url <- repair$document_url
    addition$review_status <- row$review_status
    addition$old_document_id <- repair$document_id
    addition$new_document_id <- repair$document_id
    addition$rule <- "verified_localized_viewer_and_direct_pdf"
    addition$reason <- "Recorded the verified localized House of Commons report viewer as source_page while preserving the direct official PDF as document_url."
    addition$source <- "parliament"
    addition$previous_state <- paste0("source_page=", repair$document_url)
    addition$resulting_state <- repair$source_page
    addition$manual_decision <- row$review_status
    addition$reviewer <- "metadata correction only"
    addition$review_timestamp <- now_utc()
    changes <- rbind(changes, as.data.frame(addition, stringsAsFactors = FALSE))
  }
}
readr::write_csv(changes, changes_path, na = "")

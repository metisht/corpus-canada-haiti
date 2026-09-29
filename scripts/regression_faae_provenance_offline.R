# Focused offline regression for the verified bilingual FAAE report provenance.

source("R/utils.R", encoding = "UTF-8")
source("R/discover.R", encoding = "UTF-8")

expected <- data.frame(
  version_id = c("ver_e691a4be321909cf", "ver_60ebf8ec2b2bd592"),
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

for (path in c("data/candidates.csv", "data/corpus.csv")) {
  data <- readr::read_csv(path, show_col_types = FALSE)
  rows <- data[data$document_id == "doc_d555c489f6b3563c", , drop = FALSE]
  rows <- rows[match(expected$version_id, rows$version_id), , drop = FALSE]
  stopifnot(
    nrow(rows) == 2L,
    identical(rows$version_id, expected$version_id),
    identical(rows$language, expected$language),
    identical(rows$source_page, expected$source_page),
    identical(rows$document_url, expected$document_url),
    all(rows$year == 2025L),
    length(unique(rows$document_id)) == 1L
  )
}

stopifnot(identical(
  unname(vapply(expected$document_url, discovered_source_page, character(1), fallback = "")),
  expected$source_page
))

changes <- readr::read_csv("data/validation_changes.csv", show_col_types = FALSE)
audit <- changes[
  changes$change == "faae_report_provenance_repaired" &
    changes$version_id %in% expected$version_id,
  ,
  drop = FALSE
]
stopifnot(nrow(audit) == 2L, !anyDuplicated(audit$version_id))

cat("Targeted FAAE provenance regression passed\n")

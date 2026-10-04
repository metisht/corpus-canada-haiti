source("R/site_i18n.R", encoding = "UTF-8")
source("R/spatial_constellation.R", encoding = "UTF-8")

spatial <- read.csv("data/spatial_review_queue.csv", stringsAsFactors = FALSE, check.names = FALSE)
corpus <- read.csv("data/corpus.csv", stringsAsFactors = FALSE, check.names = FALSE)
doc_years <- unique(corpus[c("document_id", "year")])
confirmed <- confirmed_spatial_documents(spatial, doc_years)

document_groups <- lapply(split(corpus, corpus$document_id), function(rows) {
  confirmed_concepts <- rows$matched_concepts[
    rows$concept_coding_state == "confirmed" & !is.na(rows$matched_concepts)
  ]
  data.frame(
    document_id = rows$document_id[1],
    year = paste(unique(na.omit(rows$year)), collapse = "|"),
    institution = paste(unique(rows$institution[nzchar(rows$institution)]), collapse = "|"),
    family = paste(unique(rows$families[nzchar(rows$families)]), collapse = "|"),
    concept = paste(unique(unlist(strsplit(paste(confirmed_concepts, collapse = "|"), "\\|"))), collapse = "|"),
    search = tolower(paste(rows$title, rows$institution, rows$families, confirmed_concepts, collapse = " ")),
    stringsAsFactors = FALSE
  )
})
document_groups <- do.call(rbind, document_groups)
visible_documents <- function(query = "", filters = list(year = "", institution = "", family = "", concept = "")) {
  visible <- grepl(tolower(query), document_groups$search, fixed = TRUE)
  for (name in names(filters)) {
    selected <- tolower(filters[[name]])
    if (!nzchar(selected)) next
    values <- strsplit(tolower(document_groups[[name]]), "\\|")
    visible <- visible & vapply(values, function(value) selected %in% trimws(value), logical(1))
  }
  sum(visible)
}
first_group <- document_groups[which(nzchar(document_groups$concept))[1], , drop = FALSE]
first_title <- corpus$title[which(!is.na(corpus$title) & nzchar(corpus$title))[1]]
first_query <- strsplit(tolower(first_title), "[[:space:]]+")[[1]][1]
combined_filters <- lapply(first_group[c("year", "institution", "family", "concept")], function(value) strsplit(value, "\\|")[[1]][1])

stopifnot(
  length(unique(corpus$document_id)) == 151L,
  visible_documents() == 151L,
  visible_documents(filters = list(year = "", institution = "", family = "", concept = "")) == 151L,
  visible_documents(first_query) >= 1L,
  all(vapply(names(combined_filters), function(name) {
    filters <- list(year = "", institution = "", family = "", concept = "")
    filters[[name]] <- combined_filters[[name]]
    visible_documents(filters = filters) >= 1L
  }, logical(1))),
  visible_documents(filters = combined_filters) >= 1L,
  nrow(spatial[spatial$coding_decision == "confirm", , drop = FALSE]) == 160L,
  !anyDuplicated(confirmed[c("document_id", "dimension", "proposed_code")]),
  !"peripheral" %in% confirmed$proposed_code,
  all(confirmed$document_id %in% corpus$document_id),
  all(na.omit(unique(confirmed$period)) %in% spatial_constellation_periods),
  !"2016–2020" %in% na.omit(unique(confirmed$period))
)

pco_spatial <- confirmed[
  confirmed$document_id == "doc_bf16e0a427deba37" &
    confirmed$dimension == "relational_framing" &
    confirmed$proposed_code == "neighbourhood",
  , drop = FALSE
]
stopifnot(nrow(pco_spatial) == 1L, pco_spatial$year == 2025L,
          pco_spatial$period == spatial_constellation_periods[7])
pco_confirmed <- confirmed[confirmed$document_id == "doc_bf16e0a427deba37",
                           , drop = FALSE]
stopifnot(
  nrow(pco_confirmed) == 4L,
  setequal(pco_confirmed$proposed_code,
           c("neighbourhood", "migration_mobility", "bilateral", "caribbean")),
  all(pco_confirmed$period == spatial_constellation_periods[7])
)

english <- render_spatial_constellation(spatial, doc_years, "en")
french <- render_spatial_constellation(spatial, doc_years, "fr")
count_matches <- function(pattern, text) {
  matches <- gregexpr(pattern, text, fixed = TRUE)[[1]]
  if (identical(matches, -1L)) 0L else length(matches)
}
stopifnot(
  count_matches('data-spatial-period-panel', english) == 7L,
  count_matches('class="spatial-connection', english) == 35L,
  count_matches('class="spatial-centre"', english) == 7L,
  count_matches('<option value="spatial-period-', english) == 7L,
  grepl("Spatial configurations", english, fixed = TRUE),
  !grepl("Configurations spatiales", english, fixed = TRUE),
  grepl("Configurations spatiales", french, fixed = TRUE),
  !grepl("Spatial configurations", french, fixed = TRUE),
  grepl(spatial_constellation_labels$en$note, english, fixed = TRUE),
  grepl(spatial_constellation_labels$fr$note, french, fixed = TRUE),
  !grepl("Peripheral", english, fixed = TRUE),
  !grepl("Périph", french, fixed = TRUE)
)

dashboard <- paste(readLines("_includes/dashboard.qmd", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
styles <- paste(readLines("styles.css", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
positions <- c(
  engagement_panel = regexpr("#lens-panel-engagements", dashboard, fixed = TRUE),
  engagement_component = regexpr("render_operational_board(", dashboard, fixed = TRUE),
  spatial_panel = regexpr("#lens-panel-spatial", dashboard, fixed = TRUE),
  spatial_component = regexpr("render_spatial_constellation(", dashboard, fixed = TRUE),
  discourse_panel = regexpr("#lens-panel-discourses", dashboard, fixed = TRUE),
  discourse_component = regexpr("## `r L$problematizations_heading`", dashboard, fixed = TRUE),
  institution_panel = regexpr("#lens-panel-institutions", dashboard, fixed = TRUE),
  institution_component = regexpr('data-instrument-matrix', dashboard, fixed = TRUE),
  about = regexpr("::: {.about-corpus}", dashboard, fixed = TRUE),
  explorer = regexpr("## `r L$explore`", dashboard, fixed = TRUE)
)
stopifnot(
  all(positions > 0L),
  all(diff(positions) > 0L),
  count_matches("render_operational_board(", dashboard) == 1L,
  count_matches("render_spatial_constellation(", dashboard) == 1L,
  count_matches("## `r L$problematizations_heading`", dashboard) == 1L,
  count_matches('data-instrument-matrix', dashboard) == 2L,
  count_matches("::: {#lens-panel-", dashboard) == 4L,
  grepl('role="tablist"', dashboard, fixed = TRUE),
  grepl('role="tab"', dashboard, fixed = TRUE),
  grepl('role="tabpanel"', dashboard, fixed = TRUE),
  grepl("aria-selected", dashboard, fixed = TRUE),
  grepl("ArrowRight", dashboard, fixed = TRUE),
  grepl("ArrowLeft", dashboard, fixed = TRUE),
  grepl("Home", dashboard, fixed = TRUE),
  grepl("End", dashboard, fixed = TRUE),
  grepl("event.key === 'Enter' || event.key === ' '", dashboard, fixed = TRUE),
  grepl(".lens-tabs > [role=\"tab\"]", dashboard, fixed = TRUE),
  grepl("requestAnimationFrame(() => requestAnimationFrame(() =>", dashboard, fixed = TRUE),
  grepl("refreshSpatialPanel(activePanel)", dashboard, fixed = TRUE),
  grepl("Math.max(periodPanel.clientWidth, 1)", dashboard, fixed = TRUE),
  grepl("svg.style.height", dashboard, fixed = TRUE),
  grepl("window.dispatchEvent(new Event('resize'))", dashboard, fixed = TRUE),
  grepl("window.addEventListener('corpus-spatial-filter', apply)", dashboard, fixed = TRUE),
  grepl("spatialIds.has(group.dataset.documentId)", dashboard, fixed = TRUE),
  grepl("data-document-explorer-controls", dashboard, fixed = TRUE),
  grepl("explorerControls?.querySelectorAll('#q, [id^=\"f-\"]')", dashboard, fixed = TRUE),
  grepl("window.corpusSpatialDocumentIds = null", dashboard, fixed = TRUE),
  grepl("filterSelects.forEach(select =>", dashboard, fixed = TRUE),
  grepl("window.addEventListener('corpus-spatial-filter', apply);\n  apply();", dashboard, fixed = TRUE),
  grepl('.lens-panel[hidden] { display: none; }', styles, fixed = TRUE),
  grepl('.lens-tabs [role="tab"]:focus-visible', styles, fixed = TRUE)
)

cat("Dashboard lenses and confirmed spatial constellation regression passed\n")

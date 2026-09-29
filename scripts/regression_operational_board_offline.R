#!/usr/bin/env Rscript
# Focused offline regression for the curated engagement architecture.
# It verifies identities, evidence relations, instruments, dates, localization,
# and rendering without changing project data.

source("R/utils.R", encoding = "UTF-8")
source("R/operational_board.R", encoding = "UTF-8")

corpus <- safe_read_csv("data/corpus.csv")
engagements <- safe_read_csv("data/engagements.csv")
engagement_documents <- safe_read_csv("data/engagement_documents.csv")
engagement_instruments <- safe_read_csv("data/engagement_instruments.csv")

stopifnot(
  !anyDuplicated(engagements$engagement_id),
  all(engagement_documents$engagement_id %in% engagements$engagement_id),
  all(engagement_instruments$engagement_id %in% engagements$engagement_id),
  all(engagement_documents$document_id %in% corpus$document_id[corpus$review_status == "retain"]),
  all(engagements$engagement_id %in% engagement_documents$engagement_id)
)

expected_operations <- c(
  "eng_escort", "eng_cauldron", "eng_forward_action", "eng_cadence",
  "eng_dialogue", "eng_pivot", "eng_standard_unmih",
  "eng_standard_unsmih", "eng_stable", "eng_constable", "eng_compliment"
)
stopifnot(all(expected_operations %in% engagements$engagement_id))
stopifnot(
  engagements$start_date[engagements$engagement_id == "eng_pivot"] == "1995-04-01",
  engagements$end_date[engagements$engagement_id == "eng_pivot"] == "1996-04-15",
  sum(grepl("^eng_standard_", engagements$engagement_id)) == 2L,
  "eng_unmih_advance_team_1994" %in% engagements$engagement_id,
  "eng_police_training_initiative_1994" %in% engagements$engagement_id
)

english <- build_timeline_engagements(
  engagements, engagement_documents, engagement_instruments, corpus, "en"
)
french <- build_timeline_engagements(
  engagements, engagement_documents, engagement_instruments, corpus, "fr"
)
stopifnot(
  identical(english$engagement_id, french$engagement_id),
  identical(english$start, french$start),
  all(english$engagement_id %in% engagements$engagement_id),
  !all(english$engagement_id %in% corpus$document_id)
)

engagements_for_category <- function(records, category) {
  records$engagement_id[vapply(
    strsplit(records$categories, "\\|"),
    function(categories) category %in% categories,
    logical(1)
  )]
}
stopifnot(
  length(engagements_for_category(english, "sanctions")) > 0L,
  length(engagements_for_category(english, "police")) > 0L,
  length(engagements_for_category(english, "military")) > 0L,
  "eng_pivot" %in% engagements_for_category(english, "military"),
  "eng_pivot" %in% engagements_for_category(english, "police"),
  setequal(engagements_for_category(english, "sanctions"), engagements_for_category(french, "sanctions")),
  setequal(engagements_for_category(english, "police"), engagements_for_category(french, "police")),
  setequal(engagements_for_category(english, "military"), engagements_for_category(french, "military"))
)

english_html <- render_operational_board(
  engagements, engagement_documents, engagement_instruments, corpus, "en"
)
french_html <- render_operational_board(
  engagements, engagement_documents, engagement_instruments, corpus, "fr"
)
stopifnot(
  length(gregexpr("data-operation-target", english_html, fixed = TRUE)[[1]]) == sum(lengths(strsplit(english$categories, "\\|"))),
  length(gregexpr("data-operation-target", french_html, fixed = TRUE)[[1]]) == sum(lengths(strsplit(french$categories, "\\|"))),
  length(gregexpr("class=\"engagement-layer ", english_html, fixed = TRUE)[[1]]) == 6L,
  length(gregexpr("class=\"engagement-layer ", french_html, fixed = TRUE)[[1]]) == 6L,
  length(gregexpr("class=\"engagement-layer-track\"", english_html, fixed = TRUE)[[1]]) == 6L,
  length(gregexpr("data-layer-index=", english_html, fixed = TRUE)[[1]]) == 6L,
  all(vapply(names(engagement_categories), function(category) {
    grepl(sprintf('data-engagement-layer="%s"', category), english_html, fixed = TRUE)
  }, logical(1))),
  all(vapply(names(engagement_categories), function(category) {
    grepl(engagement_categories[[category]][["fr"]], french_html, fixed = TRUE)
  }, logical(1))),
  length(gregexpr("class=\"operation-brief\"", english_html, fixed = TRUE)[[1]]) == nrow(english),
  length(gregexpr("class=\"operation-brief\"", french_html, fixed = TRUE)[[1]]) == nrow(french),
  length(gregexpr("data-engagement-filter", english_html, fixed = TRUE)[[1]]) == 7L,
  length(gregexpr("data-engagement-filter", french_html, fixed = TRUE)[[1]]) == 7L,
  grepl('data-engagement-filter="all" aria-pressed="true">All</button>', english_html, fixed = TRUE),
  grepl('data-engagement-filter="all" aria-pressed="true">Tous</button>', french_html, fixed = TRUE),
  grepl("Military interventions and operations", english_html, fixed = TRUE),
  grepl("Police missions and training", english_html, fixed = TRUE),
  grepl("Sanctions and regulatory measures", english_html, fixed = TRUE),
  grepl("Interventions et opérations militaires", french_html, fixed = TRUE),
  grepl("Missions et formation policières", french_html, fixed = TRUE),
  grepl("Sanctions et mesures réglementaires", french_html, fixed = TRUE),
  !grepl("Interventions et opérations militaires", english_html, fixed = TRUE),
  !grepl("Military interventions and operations", french_html, fixed = TRUE),
  grepl('data-instrument-categories="military|development|police"', english_html, fixed = TRUE),
  grepl('data-instrument-categories="military|development|police"', french_html, fixed = TRUE),
  grepl("overlap and accumulation", english_html, fixed = TRUE),
  grepl("chevauchement et leur sédimentation", french_html, fixed = TRUE),
  grepl("neither hierarchy nor causal dependence", english_html, fixed = TRUE),
  grepl("ni hiérarchie ni dépendance causale", french_html, fixed = TRUE),
  grepl("The layers show the sedimentation, coexistence, and modular combination of documented instruments. Their vertical order implies neither hierarchy nor causal dependence.", english_html, fixed = TRUE),
  grepl("Les couches montrent la sédimentation, la coexistence et la combinaison modulaire des instruments documentés. Leur ordre vertical n’implique ni hiérarchie ni dépendance causale.", french_html, fixed = TRUE),
  !grepl(paste("sequence", "of phases"), english_html, fixed = TRUE),
  !grepl(paste("succession", "de phases"), french_html, fixed = TRUE),
  !grepl('class="simple-timeline"', english_html, fixed = TRUE),
  !grepl('class="simple-timeline"', french_html, fixed = TRUE),
  grepl("instrument-segment", english_html, fixed = TRUE),
  !grepl("Source officielle", english_html, fixed = TRUE),
  !grepl("Official source", french_html, fixed = TRUE)
)

dashboard_source <- paste(readLines("_includes/dashboard.qmd", warn = FALSE), collapse = "\n")
styles_source <- paste(readLines("styles.css", warn = FALSE), collapse = "\n")
brief_placement <- function(anchor_top, card_height, plot_top, gap = 14, edge = 14) {
  if (anchor_top - gap - card_height >= plot_top + edge) "above" else "below"
}
stopifnot(
  grepl("filter.getAttribute('aria-pressed') !== 'true'", dashboard_source, fixed = TRUE),
  grepl("category === 'all'", dashboard_source, fixed = TRUE),
  grepl("activeCategories.includes(layer.dataset.engagementLayer)", dashboard_source, fixed = TRUE),
  grepl("activeCategories.length === categoryFilters.length", dashboard_source, fixed = TRUE),
  grepl("openCategories.some(value => activeCategories.includes(value))", dashboard_source, fixed = TRUE),
  grepl("positionBrief(target, trigger)", dashboard_source, fixed = TRUE),
  grepl("document.body.appendChild(target)", dashboard_source, fixed = TRUE),
  grepl("window.innerWidth - edge - briefRect.width", dashboard_source, fixed = TRUE),
  grepl("window.innerHeight - edge - briefRect.height", dashboard_source, fixed = TRUE),
  grepl("const plotRect = visiblePlotRect()", dashboard_source, fixed = TRUE),
  grepl("above >= plotRect.top + edge", dashboard_source, fixed = TRUE),
  identical(brief_placement(anchor_top = 250, card_height = 320, plot_top = 200), "below"),
  grepl("window.addEventListener('scroll', repositionOpenBrief, true)", dashboard_source, fixed = TRUE),
  grepl("event.key !== 'Escape'", dashboard_source, fixed = TRUE),
  grepl(".operation-brief.is-viewport-overlay", styles_source, fixed = TRUE),
  grepl("position: fixed", styles_source, fixed = TRUE),
  grepl("z-index: 2147483000", styles_source, fixed = TRUE)
)

police_1994 <- engagement_documents[
  engagement_documents$engagement_id == "eng_police_training_initiative_1994",
  ,
  drop = FALSE
]
stopifnot(
  nrow(police_1994) == 1L,
  grepl("train Haitian police forces", police_1994$evidence_excerpt, fixed = TRUE),
  grepl("put forward by Canada", police_1994$evidence_excerpt, fixed = TRUE),
  !grepl("off the south-west coast of Haiti", police_1994$evidence_excerpt, fixed = TRUE)
)

faae_reports <- corpus[
  corpus$document_id %in% c("doc_9ed93e8a637fe724", "doc_36f22a42bdeb158b"),
  ,
  drop = FALSE
]
stopifnot(
  identical(
    sort(faae_reports$title),
    sort(c(
      "Canada's International Policy Put to the Test in Haiti",
      "The Human Rights Situation in Haiti"
    ))
  ),
  setequal(faae_reports$year, c(2006L, 2023L)),
  all(grepl("/DocumentViewer/", faae_reports$source_page, fixed = TRUE)),
  all(grepl("\\.pdf$", faae_reports$document_url, ignore.case = TRUE)),
  all(faae_reports$document_type == "committee_report")
)

cat("Curated engagement architecture regression passed\n")

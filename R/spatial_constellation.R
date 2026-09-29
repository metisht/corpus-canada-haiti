# Renders the dashboard's document-level view of confirmed spatial coding.
# The authoritative review table is read by the caller; this helper never
# modifies decisions or active corpus data.

spatial_constellation_labels <- list(
  en = list(
    title = "Spatial configurations",
    period = "Corpus period",
    reset = "Reset spatial filter",
    haiti = "Haiti",
    nodes = c(
      canada = "Canada",
      caribbean = "Caribbean",
      americas = "Americas / hemisphere",
      north_america = "North America / continental space",
      global = "Global / multilateral space"
    ),
    relations = "Relational framings",
    relation = c(
      special_relationship = "Special relationship",
      regional_partner = "Regional partner",
      neighbourhood = "Neighbourhood"
    ),
    practices = "Spatial practices",
    practice = c(
      migration_mobility = "Migration / mobility",
      border = "Borders",
      maritime = "Maritime",
      transnational = "Transnational",
      regional_security = "Regional security"
    ),
    documents = "confirmed documents",
    no_data = "No confirmed documentary evidence in this period",
    default_info = "Activate a node or relation to filter the document explorer.",
    note = "This is a schematic representation of documented spatial configurations. Position and distance do not represent geographic location or causal importance."
  ),
  fr = list(
    title = "Configurations spatiales",
    period = "Période du corpus",
    reset = "Réinitialiser le filtre spatial",
    haiti = "Haïti",
    nodes = c(
      canada = "Canada",
      caribbean = "Caraïbes",
      americas = "Amériques / hémisphère",
      north_america = "Amérique du Nord / espace continental",
      global = "Espace mondial / multilatéral"
    ),
    relations = "Cadrages relationnels",
    relation = c(
      special_relationship = "Relation spéciale",
      regional_partner = "Partenaire régional",
      neighbourhood = "Voisinage"
    ),
    practices = "Pratiques spatiales",
    practice = c(
      migration_mobility = "Migration / mobilité",
      border = "Frontières",
      maritime = "Maritime",
      transnational = "Transnational",
      regional_security = "Sécurité régionale"
    ),
    documents = "documents confirmés",
    no_data = "Aucune preuve documentaire confirmée pour cette période",
    default_info = "Activez un nœud ou une relation pour filtrer l’exploration documentaire.",
    note = "Il s’agit d’une représentation schématique des configurations spatiales documentées. La position et la distance ne représentent ni la localisation géographique ni l’importance causale."
  )
)

spatial_constellation_periods <- c(
  "1991–1995", "1996–2000", "2001–2005", "2006–2010",
  "2011–2015", "2016–2020", "2021–2026"
)

spatial_period_from_year <- function(year) {
  cut(
    year,
    breaks = c(1991, 1996, 2001, 2006, 2011, 2016, 2021, 2027),
    right = FALSE,
    labels = spatial_constellation_periods
  )
}

confirmed_spatial_documents <- function(spatial, doc_years) {
  required <- c("document_id", "dimension", "proposed_code", "coding_decision")
  if (!all(required %in% names(spatial))) stop("Spatial review data are missing required columns")
  confirmed <- spatial[spatial$coding_decision == "confirm", required, drop = FALSE]
  confirmed <- unique(confirmed)
  years <- unique(doc_years[c("document_id", "year")])
  confirmed <- merge(confirmed, years, by = "document_id", all.x = TRUE)
  confirmed$period <- as.character(spatial_period_from_year(confirmed$year))
  confirmed
}

spatial_html_escape <- function(value) {
  value <- ifelse(is.na(value), "", as.character(value))
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub('"', "&quot;", value, fixed = TRUE)
  value
}

spatial_ids_for <- function(records, dimension = NULL, codes = NULL) {
  selected <- records
  if (!is.null(dimension)) selected <- selected[selected$dimension == dimension, , drop = FALSE]
  if (!is.null(codes)) selected <- selected[selected$proposed_code %in% codes, , drop = FALSE]
  sort(unique(selected$document_id))
}

spatial_filter_attributes <- function(ids, label, count, labels) {
  no_data <- count == 0L
  info <- if (no_data) labels$no_data else sprintf("%s: %d %s", label, count, labels$documents)
  sprintf(
    'data-spatial-filter data-spatial-documents="%s" data-spatial-label="%s" data-spatial-info="%s"%s',
    spatial_html_escape(paste(ids, collapse = "|")),
    spatial_html_escape(label),
    spatial_html_escape(info),
    if (no_data) ' data-no-spatial-data="true"' else ""
  )
}

render_spatial_constellation <- function(spatial, doc_years, lang = "en") {
  labels <- spatial_constellation_labels[[lang]]
  records <- confirmed_spatial_documents(spatial, doc_years)
  node_definitions <- list(
    list(key = "canada", codes = c("bilateral", "canada_domestic"), x = 175L, y = 105L),
    list(key = "caribbean", codes = "caribbean", x = 170L, y = 405L),
    list(key = "americas", codes = "americas_hemisphere", x = 450L, y = 465L),
    list(key = "north_america", codes = "north_american_continental", x = 725L, y = 105L),
    list(key = "global", codes = "global_multilateral", x = 730L, y = 405L)
  )
  relation_codes <- c("special_relationship", "regional_partner", "neighbourhood")
  practice_codes <- c("migration_mobility", "border", "maritime", "transnational", "regional_security")
  html <- c(
    sprintf('<div class="spatial-constellation" data-spatial-constellation data-lang="%s">', lang),
    sprintf('<label class="spatial-period-control"><span>%s</span><select class="form-select" data-spatial-period>', spatial_html_escape(labels$period)),
    paste(sprintf('<option value="spatial-period-%d">%s</option>', seq_along(spatial_constellation_periods), spatial_constellation_periods), collapse = ""),
    '</select></label>'
  )
  for (period_index in seq_along(spatial_constellation_periods)) {
    period <- spatial_constellation_periods[period_index]
    period_records <- records[!is.na(records$period) & records$period == period, , drop = FALSE]
    period_ids <- spatial_ids_for(period_records)
    node_counts <- vapply(node_definitions, function(node) {
      length(spatial_ids_for(period_records, "spatial_scale", node$codes))
    }, integer(1))
    maximum <- max(c(node_counts, 1L))
    panel <- c(sprintf(
      '<section id="spatial-period-%d" class="spatial-period-panel" data-spatial-period-panel aria-label="%s"%s>',
      period_index, period, if (period_index == 1L) "" else " hidden"
    ))
    panel <- c(panel, sprintf(
      '<svg class="spatial-schematic" viewBox="0 0 900 540" role="img" aria-labelledby="spatial-title-%d spatial-desc-%d"><title id="spatial-title-%d">%s — %s</title><desc id="spatial-desc-%d">%s</desc>',
      period_index, period_index, period_index, spatial_html_escape(labels$title), period,
      period_index, spatial_html_escape(labels$note)
    ))
    for (node_index in seq_along(node_definitions)) {
      node <- node_definitions[[node_index]]
      ids <- spatial_ids_for(period_records, "spatial_scale", node$codes)
      count <- length(ids)
      count_display <- if (count) as.character(count) else "—"
      radius <- 25 + 17 * count / maximum
      width <- 1.5 + 7 * count / maximum
      node_label <- unname(labels$nodes[node$key])
      attrs <- spatial_filter_attributes(ids, node_label, count, labels)
      tooltip <- if (count) sprintf("%s: %d %s", node_label, count, labels$documents) else sprintf("%s: %s", node_label, labels$no_data)
      panel <- c(panel, sprintf(
        paste0('<g class="spatial-connection%s" role="button" tabindex="0" %s aria-label="%s">',
          '<title>%s</title><line x1="450" y1="270" x2="%d" y2="%d" style="stroke-width:%.2f"/>',
          '<circle cx="%d" cy="%d" r="%.2f"/><text class="spatial-node-label" x="%d" y="%.2f">%s</text>',
          '<text class="spatial-node-count" x="%d" y="%d">%s</text></g>'),
        if (count) "" else " is-no-data", attrs, spatial_html_escape(tooltip), spatial_html_escape(tooltip),
        node$x, node$y, width, node$x, node$y, radius, node$x, node$y - radius - 12,
        spatial_html_escape(node_label), node$x, node$y + 5, count_display
      ))
    }
    haiti_attrs <- spatial_filter_attributes(period_ids, labels$haiti, length(period_ids), labels)
    panel <- c(panel, sprintf(
      '<g class="spatial-centre" role="button" tabindex="0" %s aria-label="%s"><title>%s: %d %s</title><circle cx="450" cy="270" r="55"/><text x="450" y="266">%s</text><text class="spatial-node-count" x="450" y="291">%s</text></g>',
      haiti_attrs, spatial_html_escape(labels$haiti), spatial_html_escape(labels$haiti), length(period_ids),
      spatial_html_escape(labels$documents), spatial_html_escape(labels$haiti), if (length(period_ids)) length(period_ids) else "—"
    ))
    panel <- c(panel, '</svg>')
    panel <- c(panel, sprintf('<div class="spatial-relations" aria-label="%s"><h3>%s</h3>', spatial_html_escape(labels$relations), spatial_html_escape(labels$relations)))
    for (code in relation_codes) {
      ids <- spatial_ids_for(period_records, "relational_framing", code)
      label <- unname(labels$relation[code])
      attrs <- spatial_filter_attributes(ids, label, length(ids), labels)
      panel <- c(panel, sprintf('<button type="button" class="spatial-relation%s" %s><span>%s</span><strong>%s</strong></button>', if (length(ids)) "" else " is-no-data", attrs, spatial_html_escape(label), if (length(ids)) length(ids) else "—"))
    }
    panel <- c(panel, '</div>', sprintf('<div class="spatial-practices" aria-label="%s"><h3>%s</h3>', spatial_html_escape(labels$practices), spatial_html_escape(labels$practices)))
    for (code in practice_codes) {
      ids <- spatial_ids_for(period_records, "spatial_practice", code)
      label <- unname(labels$practice[code])
      attrs <- spatial_filter_attributes(ids, label, length(ids), labels)
      panel <- c(panel, sprintf('<button type="button" class="spatial-practice practice-%s%s" %s><i aria-hidden="true"></i><span>%s</span><strong>%s</strong></button>', code, if (length(ids)) "" else " is-no-data", attrs, spatial_html_escape(label), if (length(ids)) length(ids) else "—"))
    }
    panel <- c(panel, '</div></section>')
    html <- c(html, panel)
  }
  html <- c(
    html,
    sprintf('<div class="spatial-selection" data-spatial-selection role="status" aria-live="polite">%s</div>', spatial_html_escape(labels$default_info)),
    sprintf('<button type="button" class="spatial-reset" data-spatial-reset>%s</button>', spatial_html_escape(labels$reset)),
    sprintf('<p class="spatial-note">%s</p>', spatial_html_escape(labels$note)),
    '</div>'
  )
  paste(html, collapse = "")
}

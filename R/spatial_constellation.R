# Renders the dashboard's document-level view of confirmed spatial coding.
# The authoritative review table is read by the caller; this helper never
# modifies decisions or active corpus data.

spatial_constellation_labels <- list(
  en = list(
    title = "Spatial configurations",
    period = "Corpus period", scales = "Scales",
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
    default_info = "Select a cell to read the supporting passages and open official sources.",
    evidence = "Supporting passages", source = "Official source", close = "Return to the overview",
    denominator = "dated documents in the period",
    note = "Each cell reports confirmed distinct documents (n) over all dated documents in the period (N). A dash means that the period has no dated documents."
  ),
  fr = list(
    title = "Configurations spatiales",
    period = "Période du corpus", scales = "Échelles",
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
    default_info = "Sélectionnez une cellule pour lire les passages justificatifs et ouvrir les sources officielles.",
    evidence = "Passages justificatifs", source = "Source officielle", close = "Revenir à la vue d’ensemble",
    denominator = "documents datés de la période",
    note = "Chaque cellule indique les documents distincts confirmés (n) sur tous les documents datés de la période (N). Un tiret signifie que la période ne contient aucun document daté."
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

render_spatial_constellation <- function(spatial, doc_years, docs, lang = "en") {
  labels <- spatial_constellation_labels[[lang]]
  records <- confirmed_spatial_documents(spatial, doc_years)
  node_definitions <- list(
    list(key = "canada", codes = c("bilateral", "canada_domestic")),
    list(key = "caribbean", codes = "caribbean"),
    list(key = "americas", codes = "americas_hemisphere"),
    list(key = "north_america", codes = "north_american_continental"),
    list(key = "global", codes = "global_multilateral")
  )
  relation_codes <- c("special_relationship", "regional_partner", "neighbourhood")
  practice_codes <- c("migration_mobility", "border", "maritime", "transnational", "regional_security")
  row_definitions <- c(
    lapply(node_definitions, function(x) list(group = labels$scales, dimension = "spatial_scale", code = x$codes, label = unname(labels$nodes[x$key]))),
    lapply(practice_codes, function(x) list(group = labels$practices, dimension = "spatial_practice", code = x, label = unname(labels$practice[x]))),
    lapply(relation_codes, function(x) list(group = labels$relations, dimension = "relational_framing", code = x, label = unname(labels$relation[x])))
  )
  denominators <- vapply(spatial_constellation_periods, function(period) {
    document_periods <- as.character(spatial_period_from_year(doc_years$year))
    length(unique(doc_years$document_id[!is.na(document_periods) & document_periods == period]))
  }, integer(1))
  html <- c(sprintf('<div class="spatial-constellation" data-spatial-constellation data-lang="%s">', lang),
    '<div class="matrix-scroll"><table class="spatial-matrix"><thead><tr><th scope="col"></th>',
    paste(sprintf('<th scope="col">%s<br><small>N = %d</small></th>', spatial_constellation_periods, denominators), collapse = ""),
    '</tr></thead><tbody>')
  previous_group <- ""
  for (definition in row_definitions) {
    if (!identical(definition$group, previous_group)) {
      html <- c(html, sprintf('<tr class="spatial-group"><th colspan="8" scope="rowgroup">%s</th></tr>', spatial_html_escape(definition$group)))
      previous_group <- definition$group
    }
    html <- c(html, sprintf('<tr><th scope="row">%s</th>', spatial_html_escape(definition$label)))
    for (period_index in seq_along(spatial_constellation_periods)) {
      period <- spatial_constellation_periods[period_index]
      period_records <- records[records$period == period & !is.na(records$period), , drop = FALSE]
      ids <- spatial_ids_for(period_records, definition$dimension, definition$code)
      n <- length(ids); N <- denominators[period_index]
      display <- if (N) sprintf("%d/%d", n, N) else "—"
      info <- if (N) sprintf("%s — %s: %s", definition$label, period, display) else sprintf("%s — %s: %s", definition$label, period, labels$no_data)
      html <- c(html, sprintf('<td><button type="button" class="spatial-cell%s" style="--cell-level:%.3f" data-spatial-filter data-spatial-documents="%s" data-spatial-info="%s" data-spatial-period-label="%s" data-spatial-code-label="%s" aria-pressed="false"%s><span>%s</span></button></td>',
        if (!N) " is-no-data" else "", if (N) n / max(denominators, 1L) else 0,
        spatial_html_escape(paste(ids, collapse = "|")), spatial_html_escape(info),
        spatial_html_escape(period), spatial_html_escape(definition$label), if (!N) " disabled" else "", display))
    }
    html <- c(html, '</tr>')
  }
  html <- c(html, '</tbody></table></div>')
  evidence_rows <- spatial[spatial$coding_decision == "confirm", , drop = FALSE]
  html <- c(html, sprintf('<section class="spatial-evidence" data-spatial-evidence hidden tabindex="-1"><div class="spatial-evidence-header"><h3>%s</h3><button type="button" data-spatial-reset>%s</button></div>', spatial_html_escape(labels$evidence), spatial_html_escape(labels$close)))
  for (id in unique(evidence_rows$document_id)) {
    excerpts <- unique(unlist(evidence_rows[evidence_rows$document_id == id, grep("^representative_excerpt_", names(evidence_rows)), drop = FALSE], use.names = FALSE))
    excerpts <- excerpts[!is.na(excerpts) & nzchar(trimws(excerpts))]
    doc <- docs[docs$document_id == id, , drop = FALSE]
    title <- if (nrow(doc) && any(nzchar(ifelse(is.na(doc$title), "", doc$title)))) doc$title[which(nzchar(ifelse(is.na(doc$title), "", doc$title)))[1]] else id
    url <- if (nrow(doc) && any(nzchar(ifelse(is.na(doc$document_url), "", doc$document_url)))) doc$document_url[which(nzchar(ifelse(is.na(doc$document_url), "", doc$document_url)))[1]] else ""
    source <- if (nzchar(url)) sprintf('<a href="%s" target="_blank" rel="noopener noreferrer">%s ↗</a>', spatial_html_escape(url), spatial_html_escape(labels$source)) else ""
    html <- c(html, sprintf('<article data-spatial-evidence-document="%s" hidden><h4>%s</h4>%s%s</article>', spatial_html_escape(id), spatial_html_escape(title), paste(sprintf('<blockquote>%s</blockquote>', spatial_html_escape(excerpts)), collapse = ""), source))
  }
  html <- c(html, '</section>')
  html <- c(
    html,
    sprintf('<div class="spatial-selection" data-spatial-selection role="status" aria-live="polite">%s</div>', spatial_html_escape(labels$default_info)),
    sprintf('<button type="button" class="spatial-reset" data-spatial-reset>%s</button>', spatial_html_escape(labels$reset)),
    sprintf('<p class="spatial-note">%s</p>', spatial_html_escape(labels$note)),
    '</div>'
  )
  paste(html, collapse = "")
}

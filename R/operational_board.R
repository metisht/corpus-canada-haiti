# Builds the bilingual operational timeline from retained corpus rows.
# The dashboard supplies corpus.csv and receives language-specific semantic HTML.
# No data is written: operation identities, dates, missions, links, and the
# operation-specific descriptions remain grounded in retained official records.

operational_mission_labels <- list(
  en = c(
    "Operation ESCORT" = "Operation ESCORT",
    "United Nations Mission in Haiti - UNMIH" = "United Nations Mission in Haiti (UNMIH)",
    "Multinational Force" = "Multinational Force (MNF)",
    "Military Observer Group Dominican Republic" = "Military Observer Group in the Dominican Republic",
    "DIALOGUE" = "Canadian contingency operation",
    "United Nations Support Mission in Haiti" = "United Nations Support Mission in Haiti (UNSMIH)",
    "United Nations Support Mission in Haiti (UNSMIH)" = "United Nations Support Mission in Haiti (UNSMIH)",
    "United Nations Transition Mission in Haiti (UNTMIH)" = "United Nations Transition Mission in Haiti (UNTMIH)",
    "United Nations Civilian Police Mission in Haiti" = "United Nations Civilian Police Mission in Haiti (MIPONUH)"
  ),
  fr = c(
    "Operation ESCORT" = "Opération ESCORT",
    "United Nations Mission in Haiti - UNMIH" = "Mission des Nations Unies en Haïti (MINUHA)",
    "Multinational Force" = "Force multinationale (FMN)",
    "Military Observer Group Dominican Republic" = "Groupe d’observateurs militaires en République dominicaine",
    "DIALOGUE" = "Opération canadienne de contingence",
    "United Nations Support Mission in Haiti" = "Mission d’appui des Nations Unies en Haïti (MANUH)",
    "United Nations Support Mission in Haiti (UNSMIH)" = "Mission d’appui des Nations Unies en Haïti (MANUH)",
    "United Nations Transition Mission in Haiti (UNTMIH)" = "Mission de transition des Nations Unies en Haïti (MITNUH)",
    "United Nations Civilian Police Mission in Haiti" = "Mission de police civile des Nations Unies en Haïti (MIPONUH)"
  )
)

# Descriptions are keyed by the stable engagement label. English and French
# prose is authored separately from the same retained National Defence record.
operational_descriptions <- list(
  en = c(
    "ESCORT" = paste("ESCORT was a contingency plan to evacuate as many as 1,600 Canadian nationals.", "The planned force combined CC-137 and CC-130 aircraft, air-movement and evacuee-processing teams, naval vessels, airfield security, and medical and security screening; the evacuation was never activated."),
    "CAULDRON" = paste("Canada's contribution to the United Nations Mission in Haiti focused primarily on military engineering and infrastructure restoration.", "Canadian personnel also performed patrol, transport, reconnaissance, evacuation, logistics, military-police and public-information functions as part of the multinational force."),
    "FORWARD ACTION" = paste("Canadian warships enforced the United Nations oil and arms embargo through maritime interception operations off Haiti.", "Successive Canadian deployments made 9,424 hailings, conducted 1,388 armed boardings and diverted 119 ships attempting deliveries to Haiti."),
    "CADENCE" = paste("Canadian military observers monitored traffic across the Dominican Republic-Haiti border and reported on compliance with the United Nations embargo.", "The contribution grew from individual Spanish-speaking officers in the advance team to a group of sixteen Canadian personnel."),
    "DIALOGUE" = paste("DIALOGUE prepared for a non-combatant evacuation of Canadian personnel if conditions deteriorated.", "Canada deployed HMC Ships Preserver, Fraser and Gatineau offshore, coordinated plans with United States forces and placed 1st Commando of the Canadian Airborne Regiment on alert; the extraction was not required."),
    "PIVOT" = paste("Roughly 500 Canadian personnel supplied UNMIH's aviation, military ground transport, airfield engineering, security, communications and logistical support.", "Eight Twin Huey helicopters moved personnel, cargo and election materials, while Canadian engineers built mission camps and the transport flight connected sites across Haiti."),
    "STANDARD — UNMIH" = paste("Canada replaced PIVOT's air-command support contingent with a predominantly army force, including two infantry companies for armed patrols and security in Port-au-Prince.", "Engineers, helicopters, logistics, transport, military police, public-information personnel and headquarters staff supported the final UNMIH phase, which was commanded by Canadian Brigadier-General Pierre Daigle."),
    "STANDARD — UNSMIH" = paste("STANDARD continued under UNSMIH with about 760 Canadian personnel, retaining reconnaissance patrols, engineers, six Twin Huey helicopters, logistics, military police and public-information capacity.", "The contingent supported civilian and Haitian police, refurbished fifteen police stations, improved roads and the airport, and provided transport and medical evacuation before the Canadian operation became STABLE."),
    "STABLE" = paste("Canada provided 750 military personnel and civilian police to support patrols, police training, peace-support duties and humanitarian assistance.", "The force combined a reconnaissance battalion, engineers, six Twin Huey helicopters, logistics, military police and public-information personnel for the longer Canadian contribution under UNSMIH."),
    "CONSTABLE" = paste("Canada contributed up to about 650 military personnel and 50 civilian police to UNTMIH.", "An infantry battalion, five CH-146 Griffon helicopters and logistics, engineering, medical and military-police personnel carried out patrol and protection duties while also building schools, operating clinics and delivering humanitarian aid."),
    "COMPLIMENT" = paste("Canada supplied between six Bison armoured personnel carriers and up to eleven driving instructors and maintenance personnel to MIPONUH.", "Argentine civilian police operated the vehicles as the mission's quick-reaction force while the mission continued supervisory and specialized training support for the Haitian National Police.")
  ),
  fr = c(
    "ESCORT" = paste("ESCORT était un plan de contingence visant l'évacuation d'au plus 1 600 ressortissants canadiens.", "La force prévue réunissait des aéronefs CC-137 et CC-130, des équipes de contrôle des mouvements aériens et de traitement des évacués, des bâtiments navals, des éléments de sécurité d'aérodrome ainsi que des capacités de contrôle médical et de sécurité; l'évacuation n'a jamais été déclenchée."),
    "CAULDRON" = paste("La contribution canadienne à la Mission des Nations Unies en Haïti reposait principalement sur le génie militaire et la restauration des infrastructures.", "Le personnel canadien assurait également des fonctions de patrouille, de transport, de reconnaissance, d'évacuation, de logistique, de police militaire et d'information publique au sein de la force multinationale."),
    "FORWARD ACTION" = paste("Des bâtiments de guerre canadiens ont fait respecter l'embargo des Nations Unies sur le pétrole et les armes au moyen d'opérations d'interception maritime au large d'Haïti.", "Les déploiements canadiens successifs ont effectué 9 424 interrogations, 1 388 arraisonnements armés et détourné 119 navires qui tentaient d'effectuer des livraisons en Haïti."),
    "CADENCE" = paste("Des observateurs militaires canadiens surveillaient la circulation à la frontière entre la République dominicaine et Haïti et rendaient compte du respect de l'embargo des Nations Unies.", "La contribution est passée d'un officier hispanophone au sein de l'équipe avancée à un groupe de seize militaires canadiens."),
    "DIALOGUE" = paste("DIALOGUE préparait une évacuation de non-combattants canadiens au cas où la situation se détériorerait.", "Le Canada a déployé au large les NCSM Preserver, Fraser et Gatineau, coordonné sa planification avec les forces américaines et placé le 1er Commando du Régiment aéroporté du Canada en alerte; l'extraction n'a pas été nécessaire."),
    "PIVOT" = paste("Environ 500 militaires canadiens fournissaient à la MINUHA les capacités aériennes, le transport militaire terrestre, le génie d'aérodrome, la sécurité, les communications et le soutien logistique.", "Huit hélicoptères Twin Huey transportaient personnel, fret et matériel électoral, tandis que les ingénieurs canadiens construisaient les camps de la mission et que l'unité de transport reliait les sites répartis en Haïti."),
    "STANDARD — UNMIH" = paste("Le Canada a remplacé le contingent de soutien aérien de PIVOT par une force principalement terrestre, comprenant deux compagnies d'infanterie chargées des patrouilles armées et de la sécurité à Port-au-Prince.", "Ingénieurs, hélicoptères, logistique, transport, police militaire, personnel d'information publique et officiers d'état-major ont soutenu la phase finale de la MINUHA, commandée par le brigadier-général canadien Pierre Daigle."),
    "STANDARD — UNSMIH" = paste("STANDARD s'est poursuivie sous la MANUH avec environ 760 militaires canadiens et les mêmes capacités de reconnaissance, de génie, de transport par six Twin Huey, de logistique, de police militaire et d'information publique.", "Le contingent appuyait les policiers civils et haïtiens, a remis en état quinze postes de police, amélioré des routes et l'aéroport et assuré transport et évacuations médicales avant que l'opération canadienne ne devienne STABLE."),
    "STABLE" = paste("Le Canada a fourni 750 militaires ainsi que des policiers civils pour soutenir les patrouilles, la formation policière, les tâches de soutien de la paix et l'aide humanitaire.", "La force réunissait un bataillon de reconnaissance, des ingénieurs, six hélicoptères Twin Huey, des éléments logistiques, la police militaire et du personnel d'information publique pour la contribution canadienne de plus longue durée sous la MANUH."),
    "CONSTABLE" = paste("Le Canada a affecté jusqu'à environ 650 militaires et 50 policiers civils à la MITNUH.", "Un bataillon d'infanterie, cinq hélicoptères CH-146 Griffon et du personnel de logistique, de génie, médical et de police militaire assuraient patrouilles et protection tout en construisant des écoles, exploitant des cliniques et livrant de l'aide humanitaire."),
    "COMPLIMENT" = paste("Le Canada a fourni à la MIPONUH de six véhicules blindés de transport de troupes Bison et jusqu'à onze instructeurs de conduite et techniciens d'entretien.", "Des policiers civils argentins utilisaient ces véhicules comme force de réaction rapide de la mission, pendant que celle-ci poursuivait l'encadrement et la formation spécialisée de la Police nationale d'Haïti.")
  )
)

simple_timeline_labels <- list(
  en = list(
    eyebrow = "Operational briefing board",
    title = "Canada’s engagement architecture in Haiti",
    subtitle = "Layers of documented Canadian engagement show their overlap and accumulation, 1991–2026.",
    axis = "Documented Canadian engagements from 1991 to 2026",
    legend_dot = "Dot: dated official engagement record",
    legend_segment = "Line: documented duration",
    filter_all = "All",
    filter_label = "Filter engagements by instrument category",
    category = "Category",
    record_date = "Date or period",
    operation_dates = "Canadian operation dates",
    mission_dates = "International mission dates",
    mission = "International mission",
    institution = "Institution",
    contribution = "Documented engagement",
    source = "Official source",
    close = "Close briefing",
    open = "Open briefing",
    unavailable = "No English official version identified in the retained corpus.",
    fallback = "The retained official record identifies the Canadian contribution but provides no further operational detail.",
    layer_note = "The layers show the sedimentation, coexistence, and modular combination of documented instruments. Their vertical order implies neither hierarchy nor causal dependence."
  ),
  fr = list(
    eyebrow = "Tableau de situation opérationnelle",
    title = "Architecture de l’engagement canadien en Haïti",
    subtitle = "Les couches de l’engagement canadien documenté montrent leur chevauchement et leur sédimentation, 1991–2026.",
    axis = "Engagements canadiens documentés de 1991 à 2026",
    legend_dot = "Point : document officiel daté relatif à l’engagement",
    legend_segment = "Trait : durée documentée",
    filter_all = "Tous",
    filter_label = "Filtrer les engagements par catégorie d’instrument",
    category = "Catégorie",
    record_date = "Date ou période",
    operation_dates = "Dates de l’opération canadienne",
    mission_dates = "Dates de la mission internationale",
    mission = "Mission internationale",
    institution = "Institution",
    contribution = "Engagement documenté",
    source = "Source officielle",
    close = "Fermer la fiche",
    open = "Ouvrir la fiche",
    unavailable = "Aucune version officielle française repérée dans le corpus retenu.",
    fallback = "La fiche officielle retenue identifie la contribution canadienne sans fournir davantage de détails opérationnels.",
    layer_note = "Les couches montrent la sédimentation, la coexistence et la combinaison modulaire des instruments documentés. Leur ordre vertical n’implique ni hiérarchie ni dépendance causale."
  )
)

operational_mission_periods <- list(
  "FORWARD ACTION" = c("1993-10-16", "1994-09-29"),
  "PIVOT" = c("1993-09-23", "1996-06-30"),
  "STANDARD — UNMIH" = c("1993-09-23", "1996-06-30"),
  "STANDARD — UNSMIH" = c("1996-06-28", "1997-07-31"),
  "STABLE" = c("1996-06-28", "1997-07-31"),
  "COMPLIMENT" = c("1997-11-28", "2000-03-16")
)

operational_present <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

operational_escape <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

localized_mission <- function(mission, lang) {
  translated <- unname(operational_mission_labels[[lang]][mission])
  ifelse(is.na(translated), mission, translated)
}

localized_operational_description <- function(phase_name, lang) {
  description <- unname(operational_descriptions[[lang]][phase_name])
  ifelse(is.na(description), simple_timeline_labels[[lang]]$fallback, description)
}

localized_institution <- function(institution, lang) {
  if (lang == "en") return(institution)
  translations <- c(
    "National Defence" = "Défense nationale",
    "Global Affairs Canada" = "Affaires mondiales Canada",
    "Department of Justice Canada" = "Ministère de la Justice Canada",
    "Canada Gazette" = "Gazette du Canada",
    "Royal Canadian Mounted Police" = "Gendarmerie royale du Canada",
    "Department of Finance Canada" = "Ministère des Finances Canada",
    "Public Safety Canada" = "Sécurité publique Canada"
  )
  translated <- unname(translations[institution])
  ifelse(is.na(translated), institution, translated)
}

localized_operation_source <- function(row, lang) {
  alternate <- ifelse(is.na(row$official_alternate_urls), "",
    row$official_alternate_urls)
  urls <- unique(c(row$document_url,
    unlist(strsplit(alternate, "\\|"))))
  urls <- trimws(urls[operational_present(urls)])
  pattern <- if (lang == "fr") {
    "/fr/|lang=fra|[-_]f\\.pdf"
  } else {
    "/en/|lang=eng|[-_]e\\.pdf"
  }
  matched <- urls[grepl(pattern, urls, ignore.case = TRUE)]
  if (length(matched)) matched[1] else ""
}

operational_phase_name <- function(operation, mission) {
  name <- sub("^Operation\\s+", "", operation, ignore.case = TRUE)
  if (toupper(name) != "STANDARD") return(toupper(name))
  if (grepl("Support|UNSMIH", mission, ignore.case = TRUE)) {
    "STANDARD — UNSMIH"
  } else {
    "STANDARD — UNMIH"
  }
}

build_operational_engagements <- function(docs, lang) {
  required <- c("document_id", "operation_name", "international_mission_name",
    "event_start_date", "event_end_date", "document_url",
    "official_alternate_urls", "institution", "review_status")
  for (column in setdiff(required, names(docs))) docs[[column]] <- ""
  operations <- docs[
    operational_present(docs$operation_name) &
      docs$review_status == "retain",
    ,
    drop = FALSE
  ]
  operations <- operations[!duplicated(operations$document_id), , drop = FALSE]
  operations$start <- as.Date(operations$event_start_date)
  operations$end <- as.Date(operations$event_end_date)
  operations <- operations[
    !is.na(operations$start) & !is.na(operations$end),
    ,
    drop = FALSE
  ]
  operations <- operations[
    order(operations$start, operations$end, operations$document_id),
    ,
    drop = FALSE
  ]
  operations$phase_name <- mapply(
    operational_phase_name,
    operations$operation_name,
    operations$international_mission_name,
    USE.NAMES = FALSE
  )
  operations$mission_label <- localized_mission(
    operations$international_mission_name,
    lang
  )
  operations$source_url <- vapply(seq_len(nrow(operations)), function(index) {
    localized_operation_source(operations[index, , drop = FALSE], lang)
  }, character(1))
  operations
}

timeline_date_position <- function(date) {
  axis_start <- as.Date("1991-01-01")
  axis_end <- as.Date("2026-12-31")
  as.numeric(as.Date(date) - axis_start) /
    as.numeric(axis_end - axis_start) * 100
}

engagement_categories <- list(
  military = c(en = "Military interventions and operations", fr = "Interventions et opérations militaires"),
  police = c(en = "Police missions and training", fr = "Missions et formation policières"),
  development = c(en = "Development and institutional support", fr = "Développement et soutien institutionnel"),
  diplomacy = c(en = "Diplomatic and multilateral action", fr = "Action diplomatique et multilatérale"),
  sanctions = c(en = "Sanctions and regulatory measures", fr = "Sanctions et mesures réglementaires"),
  finance = c(en = "Financial and humanitarian assistance", fr = "Assistance financière et humanitaire")
)

engagement_instrument_labels <- list(
  military_operation = c(en = "Military operation", fr = "Opération militaire"),
  police_training = c(en = "Police training", fr = "Formation policière"),
  police_deployment = c(en = "Police deployment", fr = "Déploiement policier"),
  institutional_support = c(en = "Institutional support", fr = "Appui institutionnel"),
  diplomatic_multilateral = c(en = "Diplomatic and multilateral action", fr = "Action diplomatique et multilatérale"),
  sanctions_regulatory = c(en = "Sanctions and regulatory measures", fr = "Sanctions et mesures réglementaires"),
  development_program = c(en = "Development program", fr = "Programme de développement"),
  financial_humanitarian = c(en = "Financial and humanitarian assistance", fr = "Aide financière et humanitaire")
)

instrument_category <- function(instrument_code) {
  if (instrument_code == "military_operation") return("military")
  if (instrument_code %in% c("police_training", "police_deployment")) return("police")
  if (instrument_code %in% c("institutional_support", "development_program")) return("development")
  if (instrument_code == "diplomatic_multilateral") return("diplomacy")
  if (instrument_code == "sanctions_regulatory") return("sanctions")
  "finance"
}

classify_engagement <- function(families, operation_name) {
  values <- tolower(unlist(strsplit(ifelse(is.na(families), "", families), "\\|")))
  if (operational_present(operation_name) || any(values %in% c("armed_forces", "defence_policy"))) return("military")
  if ("rcmp" %in% values) return("police")
  if ("sanctions" %in% values) return("sanctions")
  if ("development" %in% values) return("development")
  if ("public_finance" %in% values) return("finance")
  if (any(values %in% c("statements", "regional_strategy"))) return("diplomacy")
  ""
}

select_localized_record <- function(rows, lang) {
  language <- tolower(ifelse(is.na(rows$language), "", rows$language))
  preferred <- which(language == lang)
  if (!length(preferred)) preferred <- which(language %in% c("bilingual", "bilingual_or_uncertain"))
  rows[if (length(preferred)) preferred[1] else 1, , drop = FALSE]
}

operational_date <- function(value) {
  if (!operational_present(value)) return(as.Date(NA))
  value <- as.character(value)[1]
  match <- regmatches(value, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", value))
  if (!length(match) || !nzchar(match)) return(as.Date(NA))
  as.Date(match)
}

build_timeline_engagements <- function(engagements, engagement_documents,
                                       engagement_instruments, docs, lang) {
  records <- engagements[engagements$coding_status == "confirmed", , drop = FALSE]
  records$start <- as.Date(records$start_date)
  records$end <- as.Date(as.character(records$end_date))
  records <- records[!is.na(records$start), , drop = FALSE]
  records$title <- records[[paste0("label_", lang)]]
  records$description <- records[[paste0("description_", lang)]]
  records$institution <- localized_institution(records$lead_institution, lang)
  records$date_label <- ifelse(
    !is.na(records$end),
    paste(records$start, records$end, sep = " – "),
    as.character(records$start)
  )
  records$instrument_codes <- vapply(records$engagement_id, function(id) {
    paste(engagement_instruments$instrument_code[engagement_instruments$engagement_id == id], collapse = "|")
  }, character(1))
  records$instrument_labels <- vapply(strsplit(records$instrument_codes, "\\|"), function(codes) {
    paste(vapply(codes, function(code) engagement_instrument_labels[[code]][[lang]], character(1)), collapse = " · ")
  }, character(1))
  records$categories <- vapply(strsplit(records$instrument_codes, "\\|"), function(codes) {
    paste(unique(vapply(codes, instrument_category, character(1))), collapse = "|")
  }, character(1))
  records$category <- vapply(strsplit(records$categories, "\\|"), `[`, character(1), 1L)
  records$source_links <- vapply(records$engagement_id, function(id) {
    document_ids <- engagement_documents$document_id[engagement_documents$engagement_id == id]
    links <- vapply(document_ids, function(document_id) {
      rows <- docs[docs$document_id == document_id, , drop = FALSE]
      if (!nrow(rows)) return("")
      row <- select_localized_record(rows, lang)
      row_language <- tolower(ifelse(is.na(row$language), "", row$language))
      if (!row_language %in% c(lang, "bilingual", "bilingual_or_uncertain")) return("")
      url <- as.character(row$document_url)
      if (!operational_present(url)) return("")
      paste(document_id, as.character(row$title), url, sep = "\t")
    }, character(1))
    paste(unique(links[nzchar(links)]), collapse = "\n")
  }, character(1))
  records[order(records$start, records$engagement_id), , drop = FALSE]
}

assign_engagement_offsets <- function(records) {
  offsets <- c(-44L, 44L, -72L, 72L, -100L, 100L, -128L, 128L, -156L, 156L, -184L, 184L)
  date_key <- as.character(records$start)
  sequence_on_date <- ave(seq_along(date_key), date_key, FUN = seq_along)
  offsets[(sequence_on_date - 1L) %% length(offsets) + 1L]
}

render_operational_board <- function(engagements, engagement_documents,
                                     engagement_instruments, docs, lang) {
  labels <- simple_timeline_labels[[lang]]
  records <- build_timeline_engagements(
    engagements, engagement_documents, engagement_instruments, docs, lang
  )
  esc <- operational_escape
  if (!nrow(records)) return(sprintf('<p class="empty-state">%s</p>', esc(labels$axis)))
  records$position <- vapply(as.character(records$start), timeline_date_position, numeric(1))
  records$end_position <- ifelse(is.na(records$end), records$position, vapply(as.character(records$end), timeline_date_position, numeric(1)))
  ticks <- unique(c(1991L, seq(1995L, 2025L, 5L), 2026L))
  tick_html <- vapply(ticks, function(year) sprintf('<span class="simple-year-tick" style="--tick-position:%.4f%%">%d</span>', timeline_date_position(sprintf("%d-01-01", year)), year), character(1))
  category_filters <- c(
    sprintf(
      '<button type="button" class="engagement-filter is-active" data-engagement-filter="all" aria-pressed="true">%s</button>',
      esc(labels$filter_all)
    ),
    vapply(names(engagement_categories), function(category) sprintf(
      '<button type="button" class="engagement-filter category-%s" data-engagement-filter="%s" aria-pressed="false"><i class="category-swatch" aria-hidden="true"></i><span>%s</span></button>',
      category, category, esc(engagement_categories[[category]][[lang]])
    ), character(1))
  )
  html <- c(
    sprintf('<section class="operational-board simple-operational-board" data-operational-board data-lang="%s" aria-labelledby="operational-board-title">', lang),
    sprintf('<header class="operational-header"><p class="operational-eyebrow">%s</p><h2 id="operational-board-title">%s</h2><p class="engagement-subtitle">%s</p></header>', esc(labels$eyebrow), esc(labels$title), esc(labels$subtitle)),
    sprintf('<div class="simple-timeline-legend"><span><i class="legend-dot" aria-hidden="true"></i>%s</span><span><i class="legend-segment" aria-hidden="true"></i>%s</span></div>', esc(labels$legend_dot), esc(labels$legend_segment)),
    sprintf('<div class="engagement-filters" role="group" aria-label="%s">%s</div>', esc(labels$filter_label), paste(category_filters, collapse = "")),
    sprintf('<div class="simple-timeline-scroll" role="region" aria-label="%s" tabindex="0"><div class="layered-timeline"><div class="layered-year-axis">%s</div><div class="layered-timeline-body">', esc(labels$axis), paste(tick_html, collapse = ""))
  )
  category_names <- names(engagement_categories)
  multi_layer_records <- records[vapply(strsplit(records$categories, "\\|"), length, integer(1)) > 1L, , drop = FALSE]
  connectors <- vapply(seq_len(nrow(multi_layer_records)), function(index) {
    record <- multi_layer_records[index, ]
    categories <- strsplit(record$categories, "\\|")[[1]]
    rows <- match(categories, category_names)
    sprintf(
      '<span class="engagement-layer-link" data-engagement-id="%s" data-instrument-categories="%s" style="--link-position:%.4f%%;--link-top:%dpx;--link-height:%dpx"></span>',
      esc(record$engagement_id), esc(record$categories), record$position,
      round((min(rows) - 0.5) * 88), round((max(rows) - min(rows)) * 88)
    )
  }, character(1))
  html <- c(html, sprintf('<div class="engagement-layer-links" aria-hidden="true">%s</div>', paste(connectors, collapse = "")))
  for (category_index in seq_along(category_names)) {
    category <- category_names[category_index]
    category_records <- records[vapply(
      strsplit(records$categories, "\\|"),
      function(categories) category %in% categories,
      logical(1)
    ), , drop = FALSE]
    date_key <- as.character(category_records$start)
    sequence_on_date <- ave(seq_along(date_key), date_key, FUN = seq_along)
    offsets <- c(0L, -12L, 12L, -20L, 20L)
    category_records$layer_offset <- offsets[(sequence_on_date - 1L) %% length(offsets) + 1L]
    layer_label_id <- sprintf("engagement-layer-label-%s-%s", lang, category)
    html <- c(html, sprintf(
      '<section class="engagement-layer category-%s" data-engagement-layer="%s" data-layer-index="%d" role="group" aria-labelledby="%s"><h3 id="%s">%s</h3><ol class="engagement-layer-track">',
      category, category, category_index, layer_label_id, layer_label_id,
      esc(engagement_categories[[category]][[lang]])
    ))
    for (index in seq_len(nrow(category_records))) {
      record <- category_records[index, ]
      duration <- if (!is.na(record$end)) '<span class="operation-duration" aria-hidden="true"></span>' else ""
      html <- c(html, sprintf(paste0('<li class="simple-operation category-%s" data-engagement-id="%s" data-instrument-categories="%s" style="--operation-position:%.4f%%;--operation-width:%.4f%%;--operation-offset:%dpx">%s',
        '<button type="button" class="operation-dot" aria-expanded="false" aria-controls="operation-brief-%s" data-operation-target="operation-brief-%s">',
        '<span class="instrument-marker" aria-hidden="true"><i class="instrument-segment category-%s"></i></span><span class="operation-tooltip" role="tooltip"><strong>%s</strong><span>%s</span><span>%s</span><span>%s</span></span><span class="visually-hidden">%s</span></button></li>'),
        category, esc(record$engagement_id), esc(record$categories), record$position, max(0.25, record$end_position - record$position), record$layer_offset, duration,
        esc(record$engagement_id), esc(record$engagement_id), category, esc(record$title), esc(record$institution), esc(record$instrument_labels), esc(record$date_label), esc(labels$open)))
    }
    html <- c(html, '</ol></section>')
  }
  html <- c(html, '</div></div></div><div class="operation-briefings">')
  for (index in seq_len(nrow(records))) {
    record <- records[index, ]
    source_rows <- strsplit(record$source_links, "\n", fixed = TRUE)[[1]]
    source <- if (length(source_rows) && nzchar(source_rows[1])) paste(vapply(source_rows, function(value) {
      fields <- strsplit(value, "\t", fixed = TRUE)[[1]]
      sprintf('<a href="%s" target="_blank" rel="noopener noreferrer">%s ↗</a>', esc(fields[3]), esc(fields[2]))
    }, character(1)), collapse = " ") else sprintf('<span class="source-unavailable">%s</span>', esc(labels$unavailable))
    html <- c(html, sprintf(paste0('<article id="operation-brief-%s" class="operation-brief" data-engagement-id="%s" hidden tabindex="-1"><div class="brief-sequence">%02d</div><div><h3>%s</h3><dl>',
      '<div><dt>%s</dt><dd>%s</dd></div><div><dt>%s</dt><dd>%s</dd></div><div><dt>%s</dt><dd>%s</dd></div>',
      '<div class="brief-wide"><dt>%s</dt><dd>%s</dd></div></dl><p class="brief-source">%s</p></div><button class="brief-close" type="button" aria-label="%s" data-close-brief>×</button></article>'),
      esc(record$engagement_id), esc(record$engagement_id), index, esc(record$title), esc(labels$category), esc(record$instrument_labels), esc(labels$record_date), esc(record$date_label), esc(labels$institution), esc(record$institution), esc(labels$contribution), esc(record$description), source, esc(labels$close)))
  }
  paste(c(
    html,
    sprintf('</div><p class="operational-note">%s</p></section>', esc(labels$layer_note))
  ), collapse = "")
}

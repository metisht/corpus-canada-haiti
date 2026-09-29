# Shared English/French labels and render-time dashboard consistency checks.
# Sourced by the dashboard include; performs no I/O. Both language renders must
# receive identical logical-document structures and substantively equivalent labels.

site_labels <- list(
  en = list(
    corpus_summary = "Corpus summary", documents = "Documents", date_range = "Date range",
    institutions = "Institutions", document_families = "Document families", year = "Year",
    family = "Family", concept = "Concept", document_family = "Document family",
    analytical_concept = "Analytical concept", documents_over_time = "Documents over time",
    families_over_time = "Families over time", documents_by_institution = "Documents by institution",
    language_availability = "Language availability", concepts_over_time = "Concepts over time",
    engagement_heading = "Canadian engagement in Haiti, 1991–2026",
    analytical_lenses = "Analytical lenses",
    analytical_lenses_intro = "Four analytical lenses organize the dashboard: engagements, spatial configurations, discourses and concepts, and institutions and instruments.",
    lens_engagements = "Engagements", lens_spatial = "Spatial configurations",
    lens_discourses = "Discourses and concepts", lens_institutions = "Institutions and instruments",
    problematizations_heading = "Problematizations over time",
    institutions_instruments_heading = "Institutions and instruments",
    about_corpus = "About the corpus", show_about_corpus = "Show or hide corpus-description charts",
    concept_percentage_note = "Percentages describe the retained corpus and should not be interpreted as external measures of policy prevalence. A concept not matched in a document is not proof that the concept is absent.",
    no_dated_period = "No dated documents in the retained corpus",
    instrument = "Instrument", selected_period = "Selected period", full_period = "All periods (1991–2026)",
    corpus_composition_note = "Institutions identify the public bodies that issued the documents. Document families group records by the principal official domains or instruments through which Canadian security and intervention in Haiti are documented. Families are not mutually exclusive.",
    family_by_concept = "Family by concept", english = "English", french = "French",
    available_english = "Documents available in English", available_french = "Documents available in French",
    language_note = "A document available in both official languages is represented once in each language.",
    explore = "Document explorer",
    search_intro = "Search across the table or narrow the corpus by year, institution, document family, or analytical concept. Document titles link to the available official texts.",
    search_documents = "Search documents", search_placeholder = "Search title, institution, family or concept",
    filter = c(year = "Year", institution = "Institution", family = "Family", concept = "Concept"),
    all = "All", title = "Title", institution = "Institution", document_type = "Document type",
    concepts = "Concepts", format = "Format", publication_modification = "Publication / modification",
    official_links = "Official links", published = "Published", modified = "Modified",
    official_text = "Official text", official_english = "English ↗", official_french = "French ↗",
    official_english_aria = "English official document (opens in a new tab)",
    official_french_aria = "French official document (opens in a new tab)",
    official_text_aria = "Official document (opens in a new tab)", untitled = "Untitled document",
    no_dated = "No dated documents are currently available.",
    no_families = "No dated document-family records are currently available.",
    no_institutions = "No institutions are currently available.",
    no_languages = "No language information is currently available.",
    no_concepts = "No dated concept coding is currently available.",
    no_pairs = "No family–concept co-occurrences are currently available.",
    no_matches = "No documents match the current filters.",
    empty_corpus = "The corpus is currently empty. Documents will appear here after collection and review."
  ),
  fr = list(
    corpus_summary = "Résumé du corpus", documents = "Documents", date_range = "Période",
    institutions = "Institutions", document_families = "Familles documentaires", year = "Année",
    family = "Famille", concept = "Concept", document_family = "Famille documentaire",
    analytical_concept = "Concept analytique", documents_over_time = "Documents au fil du temps",
    families_over_time = "Familles documentaires au fil du temps", documents_by_institution = "Documents par institution",
    language_availability = "Disponibilité linguistique", concepts_over_time = "Concepts analytiques au fil du temps",
    engagement_heading = "Engagement du Canada en Haïti, 1991–2026",
    analytical_lenses = "Perspectives analytiques",
    analytical_lenses_intro = "Quatre perspectives analytiques structurent le tableau de bord : les engagements, les configurations spatiales, les discours et concepts, ainsi que les institutions et instruments.",
    lens_engagements = "Engagements", lens_spatial = "Configurations spatiales",
    lens_discourses = "Discours et concepts", lens_institutions = "Institutions et instruments",
    problematizations_heading = "Problématisations au fil du temps",
    institutions_instruments_heading = "Institutions et instruments",
    about_corpus = "À propos du corpus", show_about_corpus = "Afficher ou masquer les graphiques descriptifs du corpus",
    concept_percentage_note = "Les pourcentages décrivent le corpus retenu et ne constituent pas une mesure externe de la prévalence des politiques. L’absence de correspondance avec un concept dans un document ne prouve pas que ce concept en est absent.",
    no_dated_period = "Aucun document daté dans le corpus retenu",
    instrument = "Instrument", selected_period = "Période sélectionnée", full_period = "Toutes les périodes (1991–2026)",
    corpus_composition_note = "Les institutions désignent les organismes publics qui ont produit les documents. Les familles documentaires regroupent les publications selon les principaux domaines ou instruments officiels par lesquels la sécurité et l’intervention canadiennes en Haïti sont documentées. Ces familles ne sont pas mutuellement exclusives.",
    family_by_concept = "Famille documentaire par concept", english = "Anglais", french = "Français",
    available_english = "Documents disponibles en anglais", available_french = "Documents disponibles en français",
    language_note = "Un document disponible dans les deux langues officielles est représenté une fois dans chaque langue.",
    explore = "Exploration documentaire",
    search_intro = "Effectuez une recherche dans le tableau ou filtrez le corpus par année, institution, famille documentaire ou concept analytique. Les titres donnent accès aux textes officiels disponibles.",
    search_documents = "Rechercher des documents", search_placeholder = "Rechercher un titre, une institution, une famille ou un concept",
    filter = c(year = "Année", institution = "Institution", family = "Famille", concept = "Concept"),
    all = "Tous", title = "Titre", institution = "Institution", document_type = "Type de document",
    concepts = "Concepts", format = "Format", publication_modification = "Publication / modification",
    official_links = "Liens officiels", published = "Publié", modified = "Modifié",
    official_text = "Texte officiel", official_english = "Anglais ↗", official_french = "Français ↗",
    official_english_aria = "Document officiel en anglais (s’ouvre dans un nouvel onglet)",
    official_french_aria = "Document officiel en français (s’ouvre dans un nouvel onglet)",
    official_text_aria = "Document officiel (s’ouvre dans un nouvel onglet)", untitled = "Document sans titre",
    no_dated = "Aucun document daté n’est actuellement disponible.",
    no_families = "Aucun document daté et classé par famille n’est actuellement disponible.",
    no_institutions = "Aucune institution n’est actuellement disponible.",
    no_languages = "Aucune information linguistique n’est actuellement disponible.",
    no_concepts = "Aucun codage conceptuel daté n’est actuellement disponible.",
    no_pairs = "Aucune cooccurrence entre famille et concept n’est actuellement disponible.",
    no_matches = "Aucun document ne correspond aux filtres sélectionnés.",
    empty_corpus = "Le corpus est actuellement vide. Les documents apparaîtront ici après la collecte et l’examen."
  )
)

site_terms <- list(
  en = c(armed_forces = "Armed Forces", defence_policy = "Defence Policy", development = "Development",
    parliament = "Parliament", public_finance = "Public Finance", rcmp = "RCMP", regional_strategy = "Regional Strategy",
    sanctions = "Sanctions", statements = "Statements", fragile_state = "Fragile State", human_security = "Human Security",
    maritime_security = "Maritime Security", national_security = "National Security", organized_crime = "Organized Crime", regional_security = "Regional Security",
    news_release = "News Release", operation_record = "Operation Record", project_story = "Project Story",
    regulation = "Regulation", statement = "Statement"),
  fr = c(armed_forces = "Forces armées", defence_policy = "Politique de défense", development = "Développement",
    parliament = "Parlement", public_finance = "Finances publiques", rcmp = "GRC", regional_strategy = "Stratégie régionale",
    sanctions = "Sanctions", statements = "Déclarations", fragile_state = "État fragile", human_security = "Sécurité humaine",
    maritime_security = "Sécurité maritime", national_security = "Sécurité nationale", organized_crime = "Crime organisé", regional_security = "Sécurité régionale",
    news_release = "Communiqué", operation_record = "Fiche d’opération", project_story = "Récit de projet",
    regulation = "Règlement", statement = "Déclaration")
)

site_term <- function(x, lang) {
  x <- as.character(x)
  translated <- unname(site_terms[[lang]][x])
  fallback <- tools::toTitleCase(gsub("_", " ", x, fixed = TRUE))
  ifelse(is.na(translated), fallback, translated)
}

check_bilingual_consistency <- function(analytical_docs, doc_years, doc_institutions, fam, con, docs) {
  signatures <- lapply(c("en", "fr"), function(lang) list(
    documents = sort(unique(analytical_docs$document_id)),
    years = doc_years[order(doc_years$document_id, doc_years$year), , drop = FALSE],
    institutions = doc_institutions[order(doc_institutions$document_id, doc_institutions$institution), , drop = FALSE],
    families = fam[order(fam$id, fam$value), , drop = FALSE],
    concepts = con[order(con$id, con$value), , drop = FALSE],
    table_rows = sort(unique(analytical_docs$document_id)),
    links = sort(docs$document_url[!is.na(docs$document_url) & nzchar(docs$document_url)]),
    graphics = c("time", "institution", "family_time", "concept_time", "family_concept", "language"),
    filters = c("year", "institution", "family", "concept")
  ))
  stopifnot(identical(signatures[[1]], signatures[[2]]))
  invisible(TRUE)
}

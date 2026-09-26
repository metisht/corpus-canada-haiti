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
    family_by_concept = "Family by concept", english = "English", french = "French",
    available_english = "Documents available in English", available_french = "Documents available in French",
    language_note = "A document available in both official languages is represented once in each language.",
    explore = "Explore the documents",
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
    family_by_concept = "Famille documentaire par concept", english = "English", french = "Français",
    available_english = "Documents disponibles en anglais", available_french = "Documents disponibles en français",
    language_note = "Un document disponible dans les deux langues officielles est représenté une fois dans chaque langue.",
    explore = "Explorer les documents",
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
  en = c(armed_forces = "Armed Forces", development = "Development", sanctions = "Sanctions",
    statements = "Statements", national_security = "National Security", regional_security = "Regional Security",
    news_release = "News Release", operation_record = "Operation Record", project_story = "Project Story",
    regulation = "Regulation", statement = "Statement"),
  fr = c(armed_forces = "Forces armées", development = "Développement", sanctions = "Sanctions",
    statements = "Déclarations", national_security = "Sécurité nationale", regional_security = "Sécurité régionale",
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

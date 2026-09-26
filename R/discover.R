# Official-source discovery and route metadata registry.
# Called by run_crawl() and focused regressions; returns official document URLs
# while remembering provenance and localized counterparts. It never decides inclusion.

absolute_url <- function(href, base) {
  if (is.na(href) || !nzchar(href) || grepl("^(mailto:|javascript:|tel:|#)", href)) return(NA_character_)
  xml2::url_absolute(href, base)
}

.discovery_source_pages <- new.env(parent = emptyenv())
.discovery_counterparts <- new.env(parent = emptyenv())
.discovery_document_types <- new.env(parent = emptyenv())
.discovery_languages <- new.env(parent = emptyenv())
.discovery_dates <- new.env(parent = emptyenv())

remember_discovery_source <- function(document_url, source_page) {
  .discovery_source_pages[[canonicalize_url(document_url)]] <- canonicalize_url(source_page)
  invisible(document_url)
}

discovered_source_page <- function(document_url, fallback) {
  .discovery_source_pages[[canonicalize_url(document_url)]] %||% fallback
}

remember_discovered_counterparts <- function(a, b) {
  if (nzchar(a) && nzchar(b)) {
    .discovery_counterparts[[canonicalize_url(a)]] <- canonicalize_url(b)
    .discovery_counterparts[[canonicalize_url(b)]] <- canonicalize_url(a)
  }
  invisible(NULL)
}

discovered_counterpart <- function(document_url) {
  .discovery_counterparts[[canonicalize_url(document_url)]] %||% character()
}

remember_discovery_type <- function(document_url, document_type) {
  .discovery_document_types[[canonicalize_url(document_url)]] <- document_type
  invisible(document_url)
}

discovered_document_type <- function(document_url) {
  .discovery_document_types[[canonicalize_url(document_url)]] %||% character()
}

remember_discovery_language <- function(document_url, language) {
  .discovery_languages[[canonicalize_url(document_url)]] <- language
  invisible(document_url)
}

discovered_language <- function(document_url) {
  .discovery_languages[[canonicalize_url(document_url)]] %||% character()
}

remember_discovery_date <- function(document_url, publication_date) {
  if (length(publication_date) && !is.na(publication_date) && nzchar(publication_date))
    .discovery_dates[[canonicalize_url(document_url)]] <- publication_date
  invisible(document_url)
}

discovered_date <- function(document_url) {
  .discovery_dates[[canonicalize_url(document_url)]] %||% NA_character_
}

historical_volume_language <- function(key) {
  if (grepl("(?:^|[._])CDC", key, ignore.case=TRUE)) "fr" else
    if (grepl("(?:^|[._])HOC", key, ignore.case=TRUE)) "en" else "bilingual_or_uncertain"
}

historical_volume_pair_key <- function(key) {
  sub("((?:^|[._]))(?:HOC|CDC)", "\\1{chamber}", key, ignore.case=TRUE)
}

historical_search_terms <- function() c("Haiti", "Haïti")

discover_historical_volume <- function(volume, cfg) {
  key <- sub(".*/view/([^/?#]+).*$", "\\1", volume)
  seqs <- character()
  for (term in historical_search_terms()) {
    req <- httr2::request("https://parl.canadiana.ca/search/post") |>
      httr2::req_user_agent(cfg$user_agent) |>
      httr2::req_timeout(as.numeric(cfg$request_timeout_seconds)) |>
      httr2::req_body_form(handler="page", q=term, pkey=key, fmt="ajax")
    matches <- tryCatch(
      httr2::req_perform(req) |>
        httr2::resp_body_string(),
      error = function(e) ""
    )
    if (nzchar(matches)) {
      md <- rvest::read_html(matches)
      seqs <- unique(c(seqs, rvest::html_attr(rvest::html_elements(md, "[data-seq]"), "data-seq")))
    }
  }
  seqs <- unique(seqs[grepl("^[0-9]+$", seqs)])
  out <- character()
  for (seq in seqs) {
    viewer <- paste0(volume, "/", seq)
    download <- sprintf("https://parl.canadiana.ca/files/get/%s/%s", key, seq)
    remember_discovery_source(download, viewer)
    remember_discovery_language(download, historical_volume_language(key))
    dtype <- if (grepl("debates", key, ignore.case=TRUE)) "house_debate" else if (grepl("journals", key, ignore.case=TRUE)) "house_journal" else "committee_evidence"
    remember_discovery_type(download, dtype)
    out <- c(out, download)
  }
  unique(out)
}

house_viewer_links <- function(page, cfg, force = FALSE) {
  fetched <- polite_get(page, cfg, force)
  doc <- rvest::read_html(fetched$path)
  nodes <- rvest::html_elements(doc, "a[href]")
  href <- xml2::url_absolute(rvest::html_attr(nodes, "href"), page)
  label <- clean_space(rvest::html_text2(nodes))
  classes <- rvest::html_attr(nodes, "class"); classes[is.na(classes)] <- ""
  switch_labels <- rvest::html_attr(nodes, "data-analytics-eventlabel"); switch_labels[is.na(switch_labels)] <- ""
  pdf <- unique(href[grepl("btn-export-pdf", classes, ignore.case=TRUE) |
                     (grepl("^PDF$", label, ignore.case=TRUE) & grepl("\\.pdf($|[?#])", href, ignore.case=TRUE))])
  pdf <- canonicalize_url(pdf[grepl("ourcommons\\.ca|noscommunes\\.ca", pdf, ignore.case=TRUE)])
  if (length(pdf) > 1L) {
    lang <- if (grepl("/(fr|fra)/|noscommunes\\.ca", page, ignore.case=TRUE)) "f" else "e"
    pdf <- pdf[grepl(paste0("-", lang, "(?:ng|ra)?\\.pdf$"), pdf, ignore.case=TRUE)]
  }
  # The localized route is authoritative only when it is exposed by the
  # viewer.  In particular, French House routes use chambre/seance/debats and
  # cannot be obtained safely by replacing `/en/` with `/fr/`.
  hreflang <- rvest::html_attr(nodes, "hreflang"); hreflang[is.na(hreflang)] <- ""
  switch_link <- unique(href[
    grepl("Language Switch", switch_labels, ignore.case=TRUE) |
    grepl("^(en|fr|eng|fra)$", hreflang, ignore.case=TRUE) |
    grepl("^(English|Français|Francais)$", label, ignore.case=TRUE)
  ])
  switch_link <- canonicalize_url(switch_link[grepl("ourcommons\\.ca|noscommunes\\.ca", switch_link, ignore.case=TRUE)])
  heading <- clean_space(paste(rvest::html_text2(rvest::html_elements(doc, "h1, title")), collapse = " "))
  list(pdf = if (length(pdf) == 1L) pdf else character(),
       counterpart_page = if (length(switch_link) == 1L) switch_link else character(),
       publication_date = parse_authoritative_house_date(heading))
}

resolve_house_viewer_pair <- function(page, cfg, force = FALSE) {
  primary <- house_viewer_links(page, cfg, force)
  pages <- unique(c(canonicalize_url(page), primary$counterpart_page))
  pdfs <- character()
  for (viewer in pages) {
    links <- if (identical(viewer, canonicalize_url(page))) primary else house_viewer_links(viewer, cfg, force)
    if (length(links$pdf)) {
      pdfs <- c(pdfs, links$pdf)
      remember_discovery_source(links$pdf, viewer)
      remember_discovery_date(links$pdf, links$publication_date)
    }
  }
  pdfs <- unique(pdfs)
  known_dates <- unique(vapply(pdfs, discovered_date, character(1)))
  known_dates <- known_dates[!is.na(known_dates) & nzchar(known_dates)]
  if (length(known_dates) == 1L)
    for (pdf in pdfs) remember_discovery_date(pdf, known_dates)
  if (length(pdfs) == 2L) remember_discovered_counterparts(pdfs[1], pdfs[2])
  pdfs
}

house_report_pdf <- function(page, cfg, force = FALSE) {
  candidates <- house_viewer_links(page, cfg, force)$pdf
  if (!length(candidates)) stop("No committee-report PDF link found on DocumentViewer page")
  page_language <- if (grepl("/fr/|noscommunes\\.ca", page, ignore.case = TRUE)) "fr" else "en"
  language_match <- if (page_language == "fr") {
    grepl("-f(?:ra)?\\.pdf($|[?#])", candidates, ignore.case = TRUE)
  } else {
    grepl("-e(?:ng)?\\.pdf($|[?#])", candidates, ignore.case = TRUE)
  }
  if (sum(language_match) == 1L) candidates <- candidates[language_match]
  if (length(candidates) != 1L) stop("DocumentViewer page did not identify one unambiguous localized report PDF")
  pdf <- canonicalize_url(candidates[1])
  remember_discovery_source(pdf, page)
  pdf
}

is_document_url <- function(url, label = "") {
  x <- paste(url, label)
  bad <- grepl("(search|recherche|sitemap|site-map|contact|accessibility|accessibilite|login|sign-in)([/?#=&]|$)", url, ignore.case = TRUE)
  plausible <- grepl("\\.pdf($|[?#])|/news/|/department-national-defence/|/content/committee/|/documentviewer/|/publication/|/publications/|/regulations/|/dataset/", x, ignore.case = TRUE)
  !bad && plausible
}

discover_html_listing <- function(source, cfg, force = FALSE) {
  fetched <- polite_get(source$start_url, cfg, force)
  doc <- rvest::read_html(fetched$path)
  nodes <- rvest::html_elements(doc, "a[href]")
  href <- vapply(rvest::html_attr(nodes, "href"), absolute_url, character(1), base = source$start_url)
  label <- clean_space(rvest::html_text2(nodes))
  keep <- !is.na(href) & grepl(source$allowed_domain, href, fixed = TRUE) &
    vapply(seq_along(href), function(i) is_document_url(href[i], label[i]), logical(1)) &
    (grepl("ha[iï]ti|haitian|haït", paste(label, href), ignore.case = TRUE) | grepl("\\.pdf($|[?#])", href, ignore.case = TRUE))
  unique(canonicalize_url(href[keep]))
}

discover_operations_archive <- function(source, cfg, force = FALSE) {
  starts <- trimws(strsplit(source$start_url, "\\|")[[1]])
  seen <- character(); frontier <- starts; documents <- character()
  # The archive is hierarchical (archive -> region -> operation). Traverse it
  # without requiring Haiti to occur in link text; relevance is checked only
  # after the operation page itself has been retrieved.
  for (depth in 0:2) {
    next_frontier <- character()
    for (page in setdiff(frontier, seen)) {
      seen <- c(seen, page)
      links <- tryCatch({
        fetched <- polite_get(page, cfg, force)
        doc <- rvest::read_html(fetched$path)
        href <- xml2::url_absolute(rvest::html_attr(rvest::html_elements(doc, "a[href]"), "href"), page)
        unique(canonicalize_url(href[grepl("canada\\.ca/.*/past-operations/", href, ignore.case = TRUE)]))
      }, error = function(e) character())
      is_leaf <- grepl("/past-operations/[^/?]+/[^/?]+\\.html$", links, ignore.case = TRUE)
      documents <- unique(c(documents, links[is_leaf]))
      next_frontier <- unique(c(next_frontier, links[!is_leaf]))
    }
    frontier <- next_frontier
  }
  documents <- setdiff(documents, canonicalize_url(starts))
  pattern <- source$link_pattern %||% ""
  if (nzchar(pattern)) documents <- documents[grepl(pattern, documents, ignore.case=TRUE, perl=TRUE)]
  documents
}

source_domains <- function(source) trimws(strsplit(source$allowed_domain, "\\|")[[1]])

catalogue_record_relevant <- function(source_id, text) {
  if (!grepl("^historical_", source_id %||% "")) return(TRUE)
  case <- grepl("ha[iï]ti|haitian|haïtien", text, ignore.case=TRUE)
  family <- switch(source_id,
    historical_diplomacy = grepl("External Affairs|Foreign Affairs|Affaires extérieures|Affaires étrangères|DFAIT|MAECI", text, ignore.case=TRUE),
    historical_development = grepl("Canadian International Development Agency|Agence canadienne de développement international|\\bCIDA\\b|\\bACDI\\b", text, ignore.case=TRUE),
    historical_sanctions = grepl("sanction|embargo|Special Economic Measures|mesures économiques spéciales|Haiti Regulations|Règlement.*Haïti", text, ignore.case=TRUE),
    historical_police = grepl("Royal Canadian Mounted Police|Gendarmerie royale du Canada|\\bRCMP\\b|\\bGRC\\b|civilian police|police civile", text, ignore.case=TRUE),
    TRUE)
  case && family
}

discover_collection <- function(source, cfg, force = FALSE) {
  starts <- trimws(strsplit(source$start_url, "\\|")[[1]])
  pattern <- source$link_pattern %||% ""
  out <- character()
  for (start in starts) {
    fetched <- polite_get(start, cfg, force)
    doc <- rvest::read_html(fetched$path)
    nodes <- rvest::html_elements(doc, "a[href]")
    href <- vapply(rvest::html_attr(nodes, "href"), absolute_url, character(1), base = start)
    label <- clean_space(rvest::html_text2(nodes))
    domains <- source_domains(source)
    official <- !is.na(href) & vapply(href, function(x) any(vapply(domains, grepl, logical(1), x = x, fixed = TRUE)), logical(1))
    matches_collection <- !nzchar(pattern) | grepl(pattern, paste(label, href), ignore.case = TRUE, perl = TRUE)
    bad <- grepl("(accessibility|accessibilite|contact|login|sign-in)([/?#=&]|$)", href, ignore.case = TRUE)
    out <- c(out, href[official & matches_collection & !bad])
  }
  if (source$source_id == "public_finance_index") {
    master <- grepl("budget\\.canada\\.ca/.*/home-accueil-(en|fr)\\.html$|/[0-9]{4}-[0-9]{2}-estimates\\.html$|/supplementary-estimates-[a-z]-[0-9]{4}-[0-9]{2}\\.html$|/recgen/cpc-pac/[0-9]{4}/.*(eng|fra)\\.(html|pdf)$", out, ignore.case = TRUE)
    out <- out[master]
  }
  if (source$source_id == "gac_sanctions") {
    out <- unique(c(starts, out))
    out <- out[
      grepl("haiti|ha[iï]ti", out, ignore.case = TRUE) |
        grepl("sanction|permit|authorization|autorisation|regulatory-reviews|examens-reglementaires", out, ignore.case = TRUE)
    ]
  }
  unique(canonicalize_url(out))
}

discover_ckan <- function(source, cfg, force = FALSE) {
  fetched <- polite_get(source$start_url, cfg, force)
  obj <- jsonlite::fromJSON(fetched$path, simplifyDataFrame = FALSE)
  packs <- obj$result$results %||% list()
  urls <- unlist(lapply(packs, function(p) c(p$url %||% NA_character_,
    unlist(lapply(p$resources %||% list(), function(r) r$url %||% NA_character_)))))
  unique(canonicalize_url(urls[!is.na(urls) & grepl("^https?://", urls)]))
}

discover_publications_catalogue <- function(source, cfg, force = FALSE) {
  searches <- trimws(strsplit(source$start_url, "\\|")[[1]])
  records <- character()
  for (start in searches) {
    pages <- start; seen <- character(); p <- 1L
    while (p <= length(pages) && length(records) < as.integer(cfg$max_discovered_per_source)) {
      search <- pages[p]; p <- p + 1L
      if (canonicalize_url(search) %in% seen) next
      seen <- c(seen, canonicalize_url(search))
      fetched <- polite_get(search, cfg, force)
      doc <- rvest::read_html(fetched$path)
      nodes <- rvest::html_elements(doc, "a[href]")
      href <- xml2::url_absolute(rvest::html_attr(nodes, "href"), search)
      records <- unique(c(records, href[grepl("/site/(eng|fra)/[0-9.]+/publication\\.html$", href, ignore.case = TRUE)]))
      rel <- rvest::html_attr(nodes, "rel"); rel[is.na(rel)] <- ""
      label <- clean_space(rvest::html_text2(nodes))
      next_links <- href[grepl("(^| )next( |$)", rel, ignore.case=TRUE) |
                         grepl("^(Next|Suivant)", label, ignore.case=TRUE)]
      pages <- unique(c(pages, next_links[grepl("publications\\.gc\\.ca", next_links, ignore.case=TRUE)]))
    }
  }
  documents <- character()
  record_limit <- min(length(records), as.integer(cfg$max_discovered_per_source))
  for (record in head(records, record_limit)) {
    links <- tryCatch({
      fetched <- polite_get(record, cfg, force)
      doc <- rvest::read_html(fetched$path)
      record_text <- clean_space(rvest::html_text2(rvest::html_element(doc, "main, [role='main'], body")))
      if (!catalogue_record_relevant(source$source_id %||% "", record_text))
        stop("Catalogue record is outside the configured historical family")
      nodes <- rvest::html_elements(doc, "a[href]")
      href <- xml2::url_absolute(rvest::html_attr(nodes, "href"), record)
      label <- clean_space(rvest::html_text2(nodes))
      domains <- source_domains(source)
      official <- !is.na(href) & vapply(href, function(x)
        any(vapply(domains, grepl, logical(1), x=x, fixed=TRUE)), logical(1))
      electronic <- official & grepl("\\.(pdf|html?)($|[?#])", href, ignore.case = TRUE) &
        !grepl("marcXml|publication\\.html|search|accessibility|contact", href, ignore.case = TRUE) &
        grepl("View|Voir|PDF|HTML", label, ignore.case = TRUE)
      if (grepl("^historical_", source$source_id %||% "")) {
        evidence <- paste(href, label)
        year_hits <- gregexpr("(19|20)\\d{2}", evidence, perl=TRUE)
        in_range <- vapply(seq_along(evidence), function(i) {
          years <- suppressWarnings(as.integer(regmatches(evidence[i], year_hits[i])[[1]]))
          length(years) && any(years >= cfg$start_year & years <= cfg$end_year, na.rm=TRUE)
        }, logical(1))
        electronic <- electronic & in_range
      }
      href[electronic]
    }, error = function(e) character())
    documents <- unique(c(documents, links))
    if (length(documents) >= as.integer(cfg$max_discovered_per_source)) break
  }
  head(unique(canonicalize_url(documents)), as.integer(cfg$max_discovered_per_source))
}

parliament_sessions <- function() {
  data.frame(
    parliament = c(34,34,35,35,36,36,37,37,37,38,39,39,40,40,40,41,41,42,43,43,44,45),
    session = c(2,3,1,2,1,2,1,2,3,1,1,2,1,2,3,1,2,1,1,2,1,1),
    start_year = c(1989,1991,1994,1997,1997,1999,2001,2002,2004,2004,2006,2007,2008,2009,2010,2011,2013,2015,2019,2020,2021,2025),
    end_year = c(1991,1993,1997,1997,1999,2000,2002,2003,2004,2005,2007,2008,2008,2009,2011,2013,2015,2019,2020,2021,2025,2026)
  )
}

sessions_in_range <- function(cfg, sessions = parliament_sessions()) {
  sessions[sessions$start_year <= cfg$end_year & sessions$end_year >= cfg$start_year, , drop = FALSE]
}

parliament_relevant_links <- function(doc, base) {
  nodes <- rvest::html_elements(doc, "a[href]")
  href <- vapply(rvest::html_attr(nodes, "href"), absolute_url, character(1), base = base)
  context <- vapply(nodes, function(node) {
    parent <- xml2::xml_find_first(node, "ancestor::*[self::li or self::tr or self::article or self::p][1]")
    if (inherits(parent, "xml_missing")) parent <- node
    clean_space(rvest::html_text2(parent))
  }, character(1))
  publication <- grepl("/(evidence|temoignages)(/|$)|/report-[0-9]+/|/rapport-[0-9]+/|/Reports/|/reports/", href, ignore.case = TRUE)
  relevant <- grepl("Haiti|Haïti", context, ignore.case = TRUE)
  unique(canonicalize_url(href[!is.na(href) & publication & relevant]))
}

parliament_index_links <- function(page, cfg, force = FALSE) {
  fetched <- polite_get(page, cfg, force)
  doc <- rvest::read_html(fetched$path)
  nodes <- rvest::html_elements(doc, "a[href]")
  href <- xml2::url_absolute(rvest::html_attr(nodes, "href"), page)
  # Index pages can contain thousands of anchors. Restrict context extraction
  # to containers whose own text contains the case term; walking ancestors for
  # every anchor is quadratic on historical sessions.
  hits <- rvest::html_elements(doc, "li, tr, article, p, dd")
  hit_text <- clean_space(rvest::html_text2(hits))
  hits <- hits[grepl("Haiti|Haïti|Haitian|Haïtien", hit_text, ignore.case=TRUE)]
  relevant_href <- unique(unlist(lapply(hits, function(node) {
    xml2::url_absolute(rvest::html_attr(rvest::html_elements(node, "a[href]"), "href"), page)
  }), use.names=FALSE))
  document <- grepl("/house/sitting-[0-9]+/(hansard|journals)|/Content/House/[0-9]+/(Debates|Journals)/", relevant_href, ignore.case = TRUE)
  page_link <- grepl("/(hansard|journals)-index/page-[0-9]+", href, ignore.case = TRUE)
  # The table of contents labels alphabetical ranges (for example Hab · Har).
  # Haiti/Haïti is confined to the Ha range, so there is no reason to fetch
  # every one of the hundred-plus index pages.
  pages <- href[page_link & grepl("^Ha", clean_space(rvest::html_text2(nodes)), ignore.case=TRUE)]
  list(documents = unique(canonicalize_url(relevant_href[document])), pages = unique(canonicalize_url(pages)))
}

discover_house_publications <- function(cfg, force = FALSE) {
  sessions <- sessions_in_range(cfg)
  sessions <- sessions[sessions$parliament >= 35, , drop = FALSE]
  out <- character()
  for (i in seq_len(nrow(sessions))) {
    stem <- sprintf("https://www.ourcommons.ca/DocumentViewer/en/%d-%d/house/", sessions$parliament[i], sessions$session[i])
    for (kind in c("hansard", "journals")) {
      root <- paste0(stem, kind, "-index")
      first <- tryCatch(parliament_index_links(root, cfg, force), error = function(e) list(documents=character(), pages=character()))
      out <- unique(c(out, first$documents))
      for (page in setdiff(first$pages, root)) {
        found <- tryCatch(parliament_index_links(page, cfg, force), error = function(e) list(documents=character()))
        out <- unique(c(out, found$documents))
      }
    }
  }
  out <- head(out, as.integer(cfg$max_discovered_per_source))
  resolved <- unlist(lapply(out, function(page) tryCatch(resolve_house_viewer_pair(page, cfg, force), error=function(e) character())), use.names=FALSE)
  unique(canonicalize_url(resolved))
}

discover_historical_parliament <- function(source, cfg, force = FALSE) {
  # Library of Parliament/LAC's official pre-1994 collection. Its searchable
  # issue pages are collection records; only direct issue/PDF content is emitted.
  starts <- trimws(strsplit(source$start_url, "\\|")[[1]])
  limit <- as.integer(cfg$max_discovered_per_source)
  out <- character()
  for (start in starts) {
    pages <- start; done <- character(); p <- 1L
    while (p <= length(pages)) {
      if (length(out) >= limit) break
      page <- pages[p]; p <- p + 1L
      if (page %in% done) next
      done <- c(done, page)
      fetched <- polite_get(page, cfg, force)
      doc <- rvest::read_html(fetched$path)
      nodes <- rvest::html_elements(doc, "a[href]")
      href <- xml2::url_absolute(rvest::html_attr(nodes, "href"), page)
      pages <- unique(c(pages, href[grepl("/search/general/[0-9]+", href)]))
      volumes <- unique(href[grepl("parl\\.canadiana\\.ca/view/", href, ignore.case = TRUE)])
      for (volume in volumes) {
        out <- unique(c(out, discover_historical_volume(volume, cfg)))
        if (length(out) >= limit) break
      }
    }
  }
  head(canonicalize_url(out), limit)
}

discover_parliament_collection <- function(source, cfg, force = FALSE) {
  sessions <- sessions_in_range(cfg)
  committees <- c("FAAE", "FAIT", "SDIR")
  out <- character()
  for (i in seq_len(nrow(sessions))) {
    p <- sessions$parliament[i]; s <- sessions$session[i]
    pages <- unlist(lapply(committees, function(committee) c(
      sprintf("https://www.ourcommons.ca/Committees/en/%s/Meetings?parl=%d&session=%d", committee, p, s),
      sprintf("https://www.ourcommons.ca/Committees/en/%s/Work?parl=%d&session=%d&show=reports", committee, p, s)
    )))
    for (page in pages) {
      links <- tryCatch({
        fetched <- polite_get(page, cfg, force)
        parliament_relevant_links(rvest::read_html(fetched$path), page)
      }, error = function(e) character())
      out <- unique(c(out, links))
    }
  }
  report_pages <- out[grepl("/(report|rapport)-[0-9]+/?$", out, ignore.case = TRUE)]
  report_pdfs <- unique(unlist(lapply(report_pages, function(page) tryCatch(
    resolve_house_viewer_pair(page, cfg, force), error=function(e) character())), use.names=FALSE))
  out <- unique(c(out[!grepl("/(report|rapport)-[0-9]+/?($|[?#])", out, ignore.case = TRUE)], report_pdfs))
  house <- discover_house_publications(cfg, force)
  out <- unique(c(out, house))
  keys <- vapply(out, parliament_document_key, character(1))
  allowed <- head(unique(keys), as.integer(cfg$max_discovered_per_source))
  out[keys %in% allowed]
}

discover_source <- function(source, cfg, force = FALSE) {
  urls <- switch(source$method,
    html_listing = discover_html_listing(source, cfg, force),
    operations_archive = discover_operations_archive(source, cfg, force),
    collection_listing = discover_collection(source, cfg, force),
    parliament_collection = discover_parliament_collection(source, cfg, force),
    historical_parliament = discover_historical_parliament(source, cfg, force),
    ckan_api = discover_ckan(source, cfg, force),
    publications_catalogue = discover_publications_catalogue(source, cfg, force),
    seed_list = strsplit(source$start_url, "\\|")[[1]],
    stop("Unknown discovery method: ", source$method))
  if (source$source_id == "parliament" && source$method != "parliament_collection") urls <- unique(c(urls, vapply(urls, parliament_counterpart_url, character(1))))
  urls <- unique(urls)
  if (source$source_id == "parliament") {
    keys <- vapply(urls, parliament_document_key, character(1))
    allowed <- head(unique(keys), as.integer(cfg$max_discovered_per_source))
    return(urls[keys %in% allowed])
  }
  head(urls, as.integer(cfg$max_discovered_per_source))
}

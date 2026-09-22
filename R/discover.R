absolute_url <- function(href, base) {
  if (is.na(href) || !nzchar(href) || grepl("^(mailto:|javascript:|tel:|#)", href)) return(NA_character_)
  xml2::url_absolute(href, base)
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

discover_ckan <- function(source, cfg, force = FALSE) {
  fetched <- polite_get(source$start_url, cfg, force)
  obj <- jsonlite::fromJSON(fetched$path, simplifyDataFrame = FALSE)
  packs <- obj$result$results %||% list()
  urls <- unlist(lapply(packs, function(p) c(p$url %||% NA_character_,
    unlist(lapply(p$resources %||% list(), function(r) r$url %||% NA_character_)))))
  unique(canonicalize_url(urls[!is.na(urls) & grepl("^https?://", urls)]))
}

discover_source <- function(source, cfg, force = FALSE) {
  urls <- switch(source$method,
    html_listing = discover_html_listing(source, cfg, force),
    ckan_api = discover_ckan(source, cfg, force),
    seed_list = strsplit(source$start_url, "\\|")[[1]],
    stop("Unknown discovery method: ", source$method))
  head(unique(urls), as.integer(cfg$max_discovered_per_source))
}

# Content extraction and controlled metadata classification.
# Converts cached HTML/PDF responses into normalized text and metadata. Historical
# Canadiana extraction may fetch its advertised PDF; date concepts remain distinct.

extract_content <- function(path, content_type, url) {
  if (grepl("parl\\.canadiana\\.ca/files/get/", url, ignore.case = TRUE)) {
    obj <- jsonlite::fromJSON(path, simplifyVector=TRUE)
    pdf_url <- obj$download_uri %||% ""
    if (!nzchar(pdf_url)) stop("Historical Parliament PDF download was unavailable")
    tmp <- tempfile(fileext=".pdf"); on.exit(unlink(tmp), add=TRUE)
    raw <- httr2::request(pdf_url) |>
      httr2::req_timeout(60) |>
      httr2::req_perform() |>
      httr2::resp_body_raw()
    writeBin(raw, tmp)
    txt <- clean_space(paste(pdftools::pdf_text(tmp), collapse="\n"))
    return(list(title="Historical House publication", text=txt, format="PDF", alternate_urls=character()))
  }
  if (grepl("parl\\.canadiana\\.ca/view/[^/]+/[0-9]+$", url, ignore.case = TRUE)) {
    key <- sub(".*/view/([^/]+)/[0-9]+$", "\\1", url)
    seq <- sub(".*/", "", url)
    endpoint <- sprintf("https://parl.canadiana.ca/files/get/%s/%s", key, seq)
    obj <- httr2::request(endpoint) |>
      httr2::req_user_agent("CanadaHaitiAcademicCorpus/1.0") |>
      httr2::req_timeout(30) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE)
    pdf_url <- obj$download_uri %||% ""
    if (!nzchar(pdf_url)) stop("Historical Parliament page PDF was unavailable")
    tmp <- tempfile(fileext=".pdf"); on.exit(unlink(tmp), add=TRUE)
    raw <- httr2::request(pdf_url) |>
      httr2::req_timeout(60) |>
      httr2::req_perform() |>
      httr2::resp_body_raw()
    writeBin(raw, tmp)
    pages <- pdftools::pdf_text(tmp); txt <- clean_space(paste(pages, collapse="\n"))
    title <- clean_space(strsplit(txt, "\\n")[[1]][1] %||% paste("Historical House publication", seq))
    return(list(title=title, text=txt, format="PDF", alternate_urls=character()))
  }
  magic <- tryCatch(rawToChar(readBin(path, "raw", n = 5)), error = function(e) "")
  is_pdf <- grepl("pdf", content_type, ignore.case = TRUE) && identical(magic, "%PDF-")
  if (is_pdf) {
    pages <- pdftools::pdf_text(path)
    txt <- clean_space(paste(pages, collapse = "\n"))
    info <- tryCatch(pdftools::pdf_info(path), error = function(e) list(keys = list()))
    pdf_title <- tryCatch(info$keys[["Title"]], error = function(e) NULL)
    title <- clean_space(pdf_title %||% basename(httr2::url_parse(url)$path))
    return(list(title = title, text = txt, format = "PDF", alternate_urls = character()))
  }
  doc <- rvest::read_html(path)
  alt_nodes <- rvest::html_elements(doc, "link[rel='alternate'][href], a[hreflang][href], a[lang][href]")
  alt_href <- rvest::html_attr(alt_nodes, "href")
  alt_lang <- paste(rvest::html_attr(alt_nodes, "hreflang"), rvest::html_attr(alt_nodes, "lang"))
  keep_alt <- !is.na(alt_href) & grepl("(^|[^a-z])(en|fr|eng|fra)([^a-z]|$)", alt_lang, ignore.case = TRUE)
  alternate_urls <- unique(xml2::url_absolute(alt_href[keep_alt], url))
  rvest::html_elements(doc, "script,style,nav,footer,header,form,aside") |>
    xml2::xml_remove()
  title <- clean_space(rvest::html_text2(rvest::html_element(doc, "h1")))
  if (!nzchar(title)) title <- clean_space(rvest::html_text2(rvest::html_element(doc, "title")))
  main <- rvest::html_element(doc, "main, article, [role='main']")
  if (inherits(main, "xml_missing")) main <- rvest::html_element(doc, "body")
  list(title = title, text = clean_space(rvest::html_text2(main)), format = "HTML", alternate_urls = alternate_urls)
}

case_matches <- function(text, labels) {
  hit <- labels[vapply(labels, function(z) grepl(paste0("(?i)\\b", z, "\\w*"), text, perl = TRUE), logical(1))]
  unique(hit)
}

extract_date <- function(text, title = "", url = "") {
  x <- paste(title, substr(text, 1, 12000), url)
  iso <- regmatches(x, regexpr("\\b(19|20)\\d{2}[-/](0[1-9]|1[0-2])[-/]([0-2]\\d|3[01])\\b", x, perl = TRUE))
  if (nzchar(iso)) return(gsub("/", "-", iso))
  y <- regmatches(x, regexpr("\\b(19|20)\\d{2}\\b", x, perl = TRUE))
  if (nzchar(y)) paste0(y, "-01-01") else NA_character_
}

parse_english_date <- function(x) {
  x <- sub("^(January|February|March|April|May|June|July|August|September|October|November|December) ([0-9]),", "\\1 0\\2,", x)
  as.character(suppressWarnings(as.Date(x, format = "%B %d, %Y")))
}

parse_authoritative_house_date <- function(x) {
  english <- first_match("(January|February|March|April|May|June|July|August|September|October|November|December) +[0-9]{1,2}, +(19|20)\\d{2}", x)
  if (!is.na(english)) return(parse_english_date(english))
  french <- first_match("[0-9]{1,2}(?:er)? +(janvier|février|mars|avril|mai|juin|juillet|août|septembre|octobre|novembre|décembre) +(19|20)\\d{2}", x)
  if (is.na(french)) return(NA_character_)
  months <- c(janvier = "01", février = "02", mars = "03", avril = "04", mai = "05", juin = "06",
              juillet = "07", août = "08", septembre = "09", octobre = "10", novembre = "11", décembre = "12")
  bits <- strsplit(tolower(french), " +")[[1]]
  sprintf("%s-%s-%02d", bits[3], months[[bits[2]]], as.integer(sub("er$", "", bits[1])))
}

first_match <- function(pattern, text) {
  m <- regmatches(text, regexpr(pattern, text, ignore.case = TRUE, perl = TRUE))
  if (length(m) && nzchar(m)) m else NA_character_
}

parse_month_year <- function(value) {
  if (is.na(value) || !nzchar(value)) return(NA_character_)
  normalized <- tolower(iconv(value, from = "UTF-8", to = "ASCII//TRANSLIT"))
  month <- c(
    january = 1L, february = 2L, march = 3L, april = 4L, may = 5L,
    june = 6L, july = 7L, august = 8L, september = 9L, october = 10L,
    november = 11L, december = 12L, janvier = 1L, fevrier = 2L,
    mars = 3L, avril = 4L, mai = 5L, juin = 6L, juillet = 7L,
    aout = 8L, septembre = 9L, octobre = 10L, novembre = 11L,
    decembre = 12L
  )
  name <- first_match("[a-z]+", normalized)
  year <- first_match("(?:19|20)\\d{2}", normalized)
  if (is.na(name) || is.na(year) || is.na(month[[name]])) return(NA_character_)
  sprintf("%s-%02d-01", year, month[[name]])
}

extract_date_metadata <- function(text, url, source_id) {
  page_raw <- first_match("(?:Page details|Page Details )?(?:Date modified: ?|Date de modification: ?)?[^0-9]*(20\\d{2}-\\d{2}-\\d{2})$", text)
  if (is.na(page_raw)) page_raw <- first_match("(?:Page details|Page Details Date modified:|Date modified:|Date de modification:)[^0-9]*(20\\d{2}-\\d{2}-\\d{2})", text)
  page_modified <- if (!is.na(page_raw)) first_match("20\\d{2}-\\d{2}-\\d{2}", page_raw) else NA_character_
  event_start <- event_end <- publication <- NA_character_

  if (source_id %in% c("parliament", "historical_parliament")) {
    is_report <- grepl("/Reports/|_Rpt[0-9]+_", url, ignore.case = TRUE)
    if (!is_report) {
      raw <- first_match("(Monday|Tuesday|Wednesday|Thursday|Friday), +(January|February|March|April|May|June|July|August|September|October|November|December) +[0-9]{1,2}, +(19|20)\\d{2}", text)
      if (!is.na(raw)) publication <- parse_english_date(sub("^(Monday|Tuesday|Wednesday|Thursday|Friday), +", "", raw))
      if (is.na(publication)) {
        raw <- first_match("(January|February|March|April|May|June|July|August|September|October|November|December) +[0-9]{1,2}, +(19|20)\\d{2}", substr(text, 1, 6000))
        if (!is.na(raw)) publication <- parse_english_date(raw)
      }
    }
    if (!is_report && is.na(publication)) {
      raw_fr <- first_match("(lundi|mardi|mercredi|jeudi|vendredi) +[0-9]{1,2} +(janvier|février|mars|avril|mai|juin|juillet|août|septembre|octobre|novembre|décembre) +(19|20)\\d{2}", text)
      if (!is.na(raw_fr)) {
        months <- c(janvier="01", février="02", mars="03", avril="04", mai="05", juin="06", juillet="07", août="08", septembre="09", octobre="10", novembre="11", décembre="12")
        bits <- strsplit(tolower(raw_fr), " +")[[1]]
        publication <- sprintf("%s-%s-%02d", tail(bits, 1), months[[bits[3]]], as.integer(bits[2]))
      }
      if (is.na(publication)) {
        raw_fr <- first_match("[0-9]{1,2} +(janvier|février|mars|avril|mai|juin|juillet|août|septembre|octobre|novembre|décembre) +(19|20)\\d{2}", substr(text, 1, 6000))
        if (!is.na(raw_fr)) {
          months <- c(janvier="01", février="02", mars="03", avril="04", mai="05", juin="06", juillet="07", août="08", septembre="09", octobre="10", novembre="11", décembre="12")
          bits <- strsplit(tolower(raw_fr), " +")[[1]]
          publication <- sprintf("%s-%s-%02d", bits[3], months[[bits[2]]], as.integer(bits[1]))
        }
      }
    }
    if (is_report) {
      ym <- first_match("(JANUARY|FEBRUARY|MARCH|APRIL|MAY|JUNE|JULY|AUGUST|SEPTEMBER|OCTOBER|NOVEMBER|DECEMBER|JANVIER|FÉVRIER|MARS|AVRIL|MAI|JUIN|JUILLET|AOÛT|SEPTEMBRE|OCTOBRE|NOVEMBRE|DÉCEMBRE) +(19|20)\\d{2}", substr(text, 1, 1000))
      if (!is.na(ym)) {
        # A month-only report date is authoritative but not precise enough to invent a day.
        year_month <- ym
      }
    }
  } else if (source_id == "forces_archive") {
    date_value <- "([0-9]{1,2} +[A-Za-z]+ +[0-9]{4})"
    canadian_prefixes <- c(
      "(?:CAF Operation Dates|Canadian Operation Dates|CF Mission/Operation Dates?)",
      "(?:Canadian Operation Name:[^:]{0,100}Date|Canadian Forces \\(CF\\) Information \\([^)]*\\) Name:[^:]{0,100}(?:CF )?Date|\\([A-Z -]+\\) Name:[^:]{0,100}(?:CF )?Date)"
    )
    bits <- character()
    for (canadian_prefix in canadian_prefixes) {
      range <- regexec(paste0(canadian_prefix, " *:? *", date_value, " *[–—-] *", date_value), text, ignore.case = TRUE, perl = TRUE)
      bits <- regmatches(text, range)[[1]]
      if (length(bits) == 3L) break
    }
    if (length(bits) != 3L) {
      range <- regexec(paste0("Mission Date:? *", date_value, " *[–—-] *", date_value), text, ignore.case = TRUE, perl = TRUE)
      bits <- regmatches(text, range)[[1]]
    }
    if (length(bits) == 3) {
      norm <- function(z) {
        z <- gsub("/", "-", z)
        d <- suppressWarnings(as.Date(z, tryFormats = c("%Y-%m-%d", "%d %B %Y")))
        if (is.na(d)) NA_character_ else as.character(d)
      }
      event_start <- norm(bits[2]); event_end <- norm(bits[3])
    }
  } else if (grepl("gazette.gc.ca", url, fixed = TRUE)) {
    publication <- first_match("20\\d{2}-\\d{2}-\\d{2}", url)
  } else if (grepl("laws-lois.justice.gc.ca", url, fixed = TRUE)) {
    publication <- first_match("Registration +(20\\d{2}-\\d{2}-\\d{2})", text)
    publication <- first_match("20\\d{2}-\\d{2}-\\d{2}", publication)
  } else if (source_id %in% c("gac_news", "gac_sanctions")) {
    raw <- first_match("(January|February|March|April|May|June|July|August|September|October|November|December) +[0-9]{1,2}, +20\\d{2} *(?:[–-])", substr(text, 1, 2000))
    if (!is.na(raw)) publication <- parse_english_date(trimws(sub("[–-].*$", "", raw)))
    if (is.na(publication)) {
      raw <- first_match("(January|February|March|April|May|June|July|August|September|October|November|December) +[0-9]{1,2}, +20\\d{2}",
                         substr(text, 1, 4000))
      if (!is.na(raw)) publication <- parse_english_date(raw)
    }
  } else if (grepl("^historical_", source_id)) {
    candidate <- extract_date(text, url = url)
    if (!is.na(candidate)) publication <- candidate
  }
  if (source_id == "parliament" && exists("discovered_date", mode = "function")) {
    authoritative <- discovered_date(url)
    if (!is.na(authoritative) && nzchar(authoritative)) publication <- authoritative
  }
  analysis_date <- if (!is.na(event_start)) event_start else if (!is.na(publication)) publication else page_modified
  date_source_override <- NA_character_
  if (source_id == "gac_news" && is.na(publication) &&
      grepl("/news/archive/20\\d{2}/[01]\\d/", url)) {
    archive_year <- first_match("20\\d{2}",
                                first_match("/news/archive/20\\d{2}/", url))
    analysis_date <- paste0(archive_year, "-01-01")
    date_source_override <- "official archive year; exact publication day unavailable"
  }
  if (source_id == "public_finance_index" && is.na(publication)) {
    url_date <- first_match("20\\d{2}-[01]\\d-[0-3]\\d",
                            first_match("/20\\d{2}-[01]\\d-[0-3]\\d-", url))
    if (!is.na(url_date)) {
      publication <- url_date
      analysis_date <- publication
      date_source_override <- "official document URL date"
    }
  }
  if (source_id == "defence_policy_index" && is.na(publication)) {
    policy_year <- first_match(
      "1994|2005|2024",
      defence_policy_document_key(url)
    )
    if (!is.na(policy_year)) {
      analysis_date <- paste0(policy_year, "-01-01")
      date_source_override <- paste(
        "official policy year; exact publication day unavailable"
      )
    }
  }
  if (source_id == "rcmp_publications" && is.na(publication) &&
      grepl("2024-25-cpa-ipp-annual-report", url, ignore.case = TRUE)) {
    analysis_date <- "2025-01-01"
    date_source_override <- "official report period ending 2025; exact publication day unavailable"
  }
  if (source_id %in% c("parliament", "historical_parliament") && exists("year_month", inherits = FALSE) && is.na(analysis_date)) {
    yr <- first_match("20\\d{2}", year_month)
    analysis_date <- paste0(yr, "-01-01")
  }
  list(publication_date = publication, page_last_modified = page_modified,
       event_start_date = event_start, event_end_date = event_end,
       year = suppressWarnings(as.integer(substr(analysis_date, 1, 4))),
       date_source = if (!is.na(date_source_override)) date_source_override else if (!is.na(event_start)) "event_start_date" else if (!is.na(publication)) "publication_date" else if (source_id %in% c("parliament", "historical_parliament") && exists("year_month", inherits = FALSE)) paste0("official report month: ", year_month, "; exact day uncertain") else if (!is.na(page_modified)) "page_last_modified; publication date uncertain" else "uncertain")
}

parliament_metadata <- function(title, text, url, language) {
  report <- grepl("/Reports/|_Rpt[0-9]+_|/(report|rapport)-[0-9]+/", url, ignore.case = TRUE)
  evidence <- grepl("/(evidence|temoignages)(/|$)", url, ignore.case = TRUE) || grepl("\\bEVIDENCE\\b|\\bTÉMOIGNAGES\\b", substr(text, 1, 12000))
  known_type <- discovered_document_type(url)
  debate <- identical(known_type, "house_debate") || grepl("/hansard|/debats|/Debates/|debates_", url, ignore.case = TRUE)
  journal <- identical(known_type, "house_journal") || grepl("/journals|/journaux|/Journals/|journals_", url, ignore.case = TRUE)
  if (report) {
    front <- clean_space(substr(text, 1, 3000))
    match <- regexec(
      "(?is)(?:Next Page|Page suivante)\\s+(?:HOUSE OF COMMONS\\s+CANADA\\s+|CHAMBRE DES COMMUNES\\s+CANADA\\s+)?(.+?)\\s+(?:Report of|Rapport du)",
      front,
      perl = TRUE
    )
    parts <- regmatches(front, match)[[1]]
    if (length(parts) >= 2L) title <- clean_space(parts[2])
    title <- gsub("^(?:HOUSE OF COMMONS|CHAMBRE DES COMMUNES)\\s+CANADA\\s+", "", title, ignore.case = TRUE)
    title <- tools::toTitleCase(tolower(title))
    report_month <- first_match(
      "(?:JANUARY|FEBRUARY|MARCH|APRIL|MAY|JUNE|JULY|AUGUST|SEPTEMBER|OCTOBER|NOVEMBER|DECEMBER|JANVIER|FÉVRIER|MARS|AVRIL|MAI|JUIN|JUILLET|AOÛT|SEPTEMBRE|OCTOBRE|NOVEMBRE|DÉCEMBRE) +(?:19|20)\\d{2}",
      front
    )
    return(list(title = title, document_type = "committee_report", report_month = report_month))
  }
  if (evidence) {
    committee <- first_match("(?:Standing Committee on|Comité permanent (?:des|de la|du|de l’|de l')) [A-Za-zÀ-ÿ ’'–—-]+?(?= +(?:NUMBER|NUMÉRO|EVIDENCE|TÉMOIGNAGES))", text)
    number <- first_match("(?:NUMBER|Meeting|Réunion|NUMÉRO) +0*[0-9]+", text)
    if (!is.na(committee) && !is.na(number)) {
      kind <- if (language == "fr") "Témoignages" else "Evidence"
      title <- paste(kind, "—", committee, "—", number)
    }
    return(list(title = title, document_type = "committee_evidence", report_month = NA_character_))
  }
  publication_title <- function(kind) {
    sitting <- first_match("(?:sitting-|seance-|/(?:Debates|Journals)/)([0-9]+)", url)
    sitting <- first_match("[0-9]+", sitting)
    date <- extract_date_metadata(text, url, "parliament")$publication_date
    if (is.na(date)) date <- "date uncertain"
    label <- if (kind == "house_debate") {
      if (language == "fr") "Débats de la Chambre des communes" else "House of Commons Debates (Hansard)"
    } else if (language == "fr") "Journaux de la Chambre des communes" else "House of Commons Journals"
    number_label <- if (language == "fr") " no " else " No. "
    paste0(label, if (!is.na(sitting)) paste0(number_label, as.integer(sitting)) else "", " — ", date)
  }
  if (debate) return(list(title = publication_title("house_debate"), document_type = "house_debate", report_month = NA_character_))
  if (journal) return(list(title = publication_title("house_journal"), document_type = "house_journal", report_month = NA_character_))
  list(title = title, document_type = "parliamentary_document", report_month = NA_character_)
}

historical_parliament_metadata <- function(text, url) {
  key <- sub(".*/(?:files/get|view)/([^/]+)/[0-9]+$", "\\1", url, perl = TRUE)
  index <- grepl("\\bINDEX\\b", substr(text, 1, 2500), ignore.case = TRUE)
  if (grepl("_28_1$", key)) work <- "Bill C-53 / Projet de loi C-53" else
    if (grepl("_25_2$", key)) work <- "Development and Human Rights / Développement et droits de la personne" else work <- "Historical House publication"
  raw_date <- first_match("[0-3]?[0-9][-/][01]?[0-9][-/](?:19|20)\\d{2}", substr(text, 1, 4000))
  publication_date <- NA_character_
  if (!is.na(raw_date)) {
    bits <- as.integer(strsplit(gsub("-", "/", raw_date), "/", fixed = TRUE)[[1]])
    publication_date <- sprintf("%04d-%02d-%02d", bits[3], bits[2], bits[1])
  }
  title <- if (index) paste("Index", work, sep = " — ") else if (!is.na(publication_date)) paste(work, publication_date, sep = " — ") else work
  list(title = title, publication_date = publication_date,
       document_type = if (index) "committee_index" else "committee_evidence")
}

defence_operation_metadata <- function(title, text, url) {
  slug <- sub("\\.html$", "", basename(httr2::url_parse(url)$path), ignore.case = TRUE)
  operation <- toupper(gsub("-(?:(?:unmih|unsmih)-)?1996$", "", slug, perl = TRUE))
  operation <- gsub("-", " ", operation)
  mission <- first_match("(?:International (?:Mission|Operation) Name|Associated Mission|UN Mission):? +.+?(?= +International Mission Name:| +Mandating Organization:| +Region Name:| +Location:| +Mission Date:)", text)
  if (!is.na(mission)) mission <- trimws(sub("^.*?: +", "", mission)) else mission <- title
  phase <- if (grepl("unsmih", slug, ignore.case = TRUE)) "UNSMIH" else if (grepl("standard", slug, ignore.case = TRUE)) "UNMIH" else ""
  operation_name <- paste("Operation", operation)
  list(operation_name = operation_name, international_mission_name = mission,
       title = paste0(operation_name, if (nzchar(phase)) paste0(" (", phase, ")") else ""))
}

observable_document_type <- function(title, url = "", fallback = "official_publication") {
  evidence <- paste(title, url)
  if (grepl("evaluation|évaluation", evidence, ignore.case = TRUE)) return("program_evaluation")
  if (grepl("implementation (plan|report)|plan de mise en œuvre|rapport de mise en œuvre",
            evidence, ignore.case = TRUE)) return("implementation_report")
  if (grepl("annual report|rapport annuel", evidence, ignore.case = TRUE)) return("annual_report")
  if (grepl("departmental (results|performance) report|rapport (sur les résultats|ministériel sur le rendement)",
            evidence, ignore.case = TRUE)) return("departmental_report")
  if (grepl("\\b(training|formation|course|cours)\\b", evidence,
            ignore.case = TRUE, perl = TRUE)) return("training_record")
  if (grepl("public accounts|comptes publics", evidence, ignore.case = TRUE)) return("public_accounts")
  if (grepl("supplementary estimates|budget supplémentaire des dépenses", evidence,
            ignore.case = TRUE)) return("supplementary_estimates")
  fallback
}

derive_document_metadata <- function(title, text, url, source) {
  host <- tolower(httr2::url_parse(url)$hostname %||% "")
  if (source$source_id %in% c("parliament", "historical_parliament")) {
    pm <- parliament_metadata(title, text, url, language_of(text))
    return(list(institution = "Parliament of Canada", families = "parliament", document_type = pm$document_type))
  }
  if (source$source_id == "forces_archive")
    return(list(institution = "National Defence", families = "armed_forces", document_type = "operation_record"))
  if (host == "gazette.gc.ca")
    return(list(institution = "Canada Gazette", families = "sanctions", document_type = "regulation"))
  if (host == "laws-lois.justice.gc.ca")
    return(list(institution = "Department of Justice Canada", families = "sanctions", document_type = "regulation"))
  if (grepl("international.gc.ca$", host) && grepl("stories-histoires", url))
    return(list(institution = "Global Affairs Canada", families = "development", document_type = "project_story"))
  if (source$source_id == "gac_news") {
    kind <- if (grepl("\\bStatement\\b", title, ignore.case = TRUE)) "statement" else
      if (grepl("address|remarks|discours|allocution", title, ignore.case = TRUE)) "speech" else "news_release"
    substantive <- if (grepl("sanction", title, ignore.case = TRUE)) "sanctions" else if (grepl("funding|assistance|aid", title, ignore.case = TRUE)) "development" else character()
    return(list(institution = "Global Affairs Canada", families = paste(c("statements", substantive), collapse = "|"), document_type = kind))
  }
  if (source$source_id == "gac_sanctions") {
    if (grepl("country-pays/haiti/relations", url, ignore.case = TRUE)) {
      return(list(institution = "Global Affairs Canada", families = "development",
                  document_type = "country_relations"))
    }
    if (grepl("ministerial-meeting-on-haiti|reunion-ministerielle-sur-haiti",
              url, ignore.case = TRUE)) {
      return(list(institution = "Global Affairs Canada", families = "statements",
                  document_type = "news_release"))
    }
    kind <- if (grepl("/(news|nouvelles)/", url, ignore.case = TRUE)) {
      if (grepl("backgrounder|document-d-information", url, ignore.case = TRUE)) "backgrounder" else "news_release"
    } else if (grepl("regulatory-reviews|examens-reglementaires", url, ignore.case = TRUE)) {
      "regulatory_review"
    } else if (grepl("permit|authorization|autorisation", paste(title, url), ignore.case = TRUE)) {
      "permit_authorization"
    } else {
      "sanctions_policy"
    }
    return(list(institution = "Global Affairs Canada", families = "sanctions", document_type = kind))
  }
  if (source$source_id == "historical_diplomacy")
    return(list(institution = "External Affairs / Global Affairs Canada", families = classify_families(paste(title, text), "", safe_read_csv("config/families.csv")), document_type = if (grepl("speech|address|statement|discours|déclaration", title, ignore.case=TRUE)) "statement" else "official_publication"))
  if (source$source_id == "historical_development")
    return(list(institution = "Canadian International Development Agency", families = classify_families(paste(title, text), "", safe_read_csv("config/families.csv")), document_type = observable_document_type(title, url)))
  if (source$source_id == "historical_police")
    return(list(institution = "Royal Canadian Mounted Police / Government of Canada", families = classify_families(paste(title, text), "", safe_read_csv("config/families.csv")), document_type = observable_document_type(title, url, "mission_record")))
  if (source$source_id == "historical_sanctions")
    return(list(institution = source$institution, families = classify_families(paste(title, text), "", safe_read_csv("config/families.csv")), document_type = "regulation_or_notice"))
  if (source$source_id == "defence_policy_index") {
    implementation <- grepl("defen[cs]e plan|plan de la défense|implementation",
                            title, ignore.case = TRUE)
    policy <- grepl(
      "defen[cs]e policy|white paper|livre blanc|politique de défense|our north,? strong and free|notre nord,? fort et libre|strong,? secure,? engaged|protection,? sécurité,? engagement",
      title, ignore.case = TRUE
    )
    policy <- policy || grepl(
      "white_paper:1994|international_policy_statement_defence:2005|our_north_strong_and_free:2024",
      defence_policy_document_key(url)
    )
    ok <- policy || implementation
    kind <- if (implementation) {
      "defence_policy_implementation_plan"
    } else if (policy) {
      "defence_policy"
    } else {
      "other"
    }
    return(list(institution = "National Defence",
                families = if (ok) "defence_policy" else "",
                document_type = kind))
  }
  if (source$source_id == "rcmp_publications") {
    ok <- grepl("international police|police peacekeeping|peace operations|police mission|missions? de police|maintien de la paix", paste(title, substr(text, 1, 12000)), ignore.case = TRUE)
    return(list(institution = "Royal Canadian Mounted Police", families = if (ok) "rcmp" else "", document_type = if (ok) observable_document_type(title, url, "mission_record") else "other"))
  }
  if (source$source_id == "regional_strategy_index") {
    ok <- grepl("strategy|strategic partnership|stratégie|partenariat stratégique", title, ignore.case = TRUE)
    return(list(institution = "Global Affairs Canada", families = if (ok) "regional_strategy" else "", document_type = if (ok) "regional_strategy" else "other"))
  }
  if (source$source_id == "public_finance_index") {
    ok <- grepl("budget|estimates|public accounts|budget des dépenses|comptes publics", title, ignore.case = TRUE)
    issuer <- if (grepl("international\\.(gc|canada)\\.ca$", host)) "Global Affairs Canada" else source$institution
    return(list(institution = issuer, families = if (ok) "public_finance" else "", document_type = if (ok) observable_document_type(title, url, "expenditure_document") else "other"))
  }
  detected <- classify_families(paste(title, text), "", safe_read_csv("config/families.csv"))
  list(institution = source$institution, families = detected, document_type = if (grepl("report", title, ignore.case=TRUE)) "report" else if (grepl("policy|politique", title, ignore.case=TRUE)) "policy" else if (grepl("budget|estimates|comptes publics", title, ignore.case=TRUE)) "public_finance_document" else "other")
}

parliament_counterpart_url <- function(url) {
  if (grepl("-e\\.pdf($|[?#])", url, ignore.case = TRUE)) return(sub("-e\\.pdf($|[?#])", "-f.pdf\\1", url, ignore.case = TRUE))
  if (grepl("-f\\.pdf($|[?#])", url, ignore.case = TRUE)) return(sub("-f\\.pdf($|[?#])", "-e.pdf\\1", url, ignore.case = TRUE))
  if (grepl("/en/", url, ignore.case = TRUE)) {
    out <- sub("/en/", "/fr/", url, ignore.case = TRUE)
    out <- sub("/meeting-([0-9]+)/evidence", "/reunion-\\1/temoignages", out, ignore.case = TRUE)
    out <- sub("/report-([0-9]+)(/|$)", "/rapport-\\1\\2", out, ignore.case = TRUE)
  } else {
    out <- sub("/fr/", "/en/", url, ignore.case = TRUE)
    out <- sub("/reunion-([0-9]+)/temoignages", "/meeting-\\1/evidence", out, ignore.case = TRUE)
    out <- sub("/rapport-([0-9]+)(/|$)", "/report-\\1\\2", out, ignore.case = TRUE)
  }
  out
}

language_of <- function(text) {
  low <- tolower(substr(text, 1, 20000))
  fr <- sum(lengths(regmatches(low, gregexpr("\\b(le|la|les|des|une|pour|avec|dans|haïti)\\b", low, perl = TRUE))))
  en <- sum(lengths(regmatches(low, gregexpr("\\b(the|and|of|for|with|in|haiti)\\b", low, perl = TRUE))))
  if (fr > en * 1.25) "fr" else if (en > fr * 1.25) "en" else "bilingual_or_uncertain"
}

normalize_concept_pattern <- function(pattern) {
  # CSV preserves the two characters in `\\b`; PCRE needs a single
  # backslash for word boundaries and other escaped operators.
  gsub("\\\\\\\\", "\\\\", pattern)
}

haiti_reference_pattern <- function() {
  "\\b(Ha[iï]ti|Haitians?|Ha[iï]tiens?|Ha[iï]tiennes?)\\b"
}

normalize_concept_text <- function(value) {
  value <- ifelse(is.na(value), "", value)
  trimws(gsub("\\s+", " ", value, perl = TRUE))
}

dedicated_haiti_document <- function(title, text, document_type) {
  dedicated_types <- c(
    "backgrounder", "committee_report", "country_relations",
    "implementation_report", "news_release", "project_story",
    "sanctions_policy", "speech", "statement"
  )
  grepl(haiti_reference_pattern(), title %||% "", ignore.case = TRUE, perl = TRUE) &&
    grepl(haiti_reference_pattern(), text %||% "", ignore.case = TRUE, perl = TRUE) &&
    (document_type %||% "") %in% dedicated_types
}

haiti_context_units <- function(text, relevance_text = "", title = "",
                                document_type = "", context_characters = 650L) {
  text <- ifelse(is.na(text), "", text)
  relevance_text <- ifelse(is.na(relevance_text), "", relevance_text)
  haiti_pattern <- haiti_reference_pattern()
  units <- list()
  unit_index <- 0L
  add_unit <- function(value, rule) {
    value <- normalize_concept_text(value)
    if (!nzchar(value)) return(invisible(NULL))
    unit_index <<- unit_index + 1L
    units[[unit_index]] <<- data.frame(
      context_text = value,
      matching_rule = rule,
      stringsAsFactors = FALSE
    )
  }

  paragraphs <- unlist(strsplit(text, "(?:\\r?\\n){2,}|\\f", perl = TRUE))
  paragraphs <- paragraphs[
    nchar(paragraphs) <= 2500L &
      grepl(haiti_pattern, paragraphs, ignore.case = TRUE, perl = TRUE)
  ]
  for (paragraph in paragraphs) add_unit(paragraph, "haiti_paragraph")

  if (grepl(haiti_pattern, relevance_text, ignore.case = TRUE, perl = TRUE)) {
    add_unit(relevance_text, "stored_relevance_passage")
  }

  references <- gregexpr(haiti_pattern, text, ignore.case = TRUE, perl = TRUE)[[1]]
  if (references[1] != -1L) {
    for (position in references) {
      add_unit(
        substr(
          text,
          max(1L, position - context_characters),
          min(nchar(text), position + context_characters)
        ),
        "bounded_haiti_context"
      )
    }
  }

  # Whole-text matching is restricted to publication forms that can establish
  # a single-document subject, and whose title and body explicitly name Haiti.
  # Multi-topic forms (for example debates, evidence, annual reports, policies,
  # and estimates) never qualify merely because Haiti appears in their title.
  if (dedicated_haiti_document(title, text, document_type)) {
    add_unit(text, "dedicated_haiti_document")
  }

  if (!length(units)) {
    return(data.frame(context_text = character(), matching_rule = character()))
  }
  unique(do.call(rbind, units))
}

match_concepts_contextually <- function(text, terms, relevance_text = "",
                                        title = "", document_type = "",
                                        language = "") {
  units <- haiti_context_units(
    text,
    relevance_text = relevance_text,
    title = title,
    document_type = document_type
  )
  if (!nrow(units)) return(data.frame())
  patterns <- vapply(terms$pattern, normalize_concept_pattern, character(1))
  evidence <- list()
  evidence_index <- 0L
  for (term_index in seq_len(nrow(terms))) {
    for (unit_index in seq_len(nrow(units))) {
      context <- units$context_text[unit_index]
      matches <- gregexpr(
        patterns[term_index],
        context,
        ignore.case = TRUE,
        perl = TRUE
      )[[1]]
      if (matches[1] == -1L) next
      lengths <- attr(matches, "match.length")
      for (match_index in seq_along(matches)) {
        position <- matches[match_index]
        evidence_index <- evidence_index + 1L
        evidence[[evidence_index]] <- data.frame(
          proposed_concept = terms$term_id[term_index],
          language = language,
          matched_phrase = substr(
            context,
            position,
            position + lengths[match_index] - 1L
          ),
          context_excerpt = normalize_concept_text(substr(
            context,
            max(1L, position - 220L),
            min(nchar(context), position + lengths[match_index] + 220L)
          )),
          matching_rule = units$matching_rule[unit_index],
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(evidence)) return(data.frame())
  unique(do.call(rbind, evidence))
}

code_terms <- function(text, terms, relevance_text = "", title = "",
                       document_type = "") {
  evidence <- match_concepts_contextually(
    text,
    terms,
    relevance_text = relevance_text,
    title = title,
    document_type = document_type
  )
  if (!nrow(evidence)) return("")
  paste(unique(evidence$proposed_concept), collapse = "|")
}

classify_families <- function(text, defaults, families) {
  hit <- families$family_id[vapply(families$patterns, grepl, logical(1), x = text, ignore.case = TRUE, perl = TRUE)]
  paste(unique(c(strsplit(defaults %||% "", "\\|" )[[1]], hit))[nzchar(unique(c(strsplit(defaults %||% "", "\\|")[[1]], hit)))], collapse = "|")
}

relevance_excerpt <- function(text, labels, n = 500) {
  positions <- unlist(lapply(labels, function(z) regexpr(tolower(z), tolower(text), fixed = TRUE)[1]))
  positions <- positions[positions > 0]
  start <- if (length(positions)) max(1, min(positions) - floor(n / 3)) else 1
  substr(text, start, start + n - 1)
}

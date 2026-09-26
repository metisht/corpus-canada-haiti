#!/usr/bin/env Rscript
# Focused connector regressions selected by HISTORICAL_REGRESSION_SECTION.
# Reads configuration and may perform narrowly targeted official retrievals; it
# never calls run_crawl() or writes candidates, review, diagnostics, or corpus.
options(warn = 1)
for (f in c("R/utils.R", "R/http.R", "R/discover.R", "R/extract.R", "R/crawl.R")) source(f, encoding="UTF-8")
cfg <- yaml::read_yaml("config/case.yml")
cfg$start_year <- 1991L; cfg$end_year <- 1998L
sources <- safe_read_csv("config/sources.csv")
live_force <- identical(tolower(Sys.getenv("HISTORICAL_REGRESSION_FORCE", "false")), "true")
section <- tolower(Sys.getenv("HISTORICAL_REGRESSION_SECTION", "all"))
run_section <- function(name) section %in% c("all", name)

result <- list()
check <- function(name, ok, detail="") {
  result[[length(result)+1L]] <<- data.frame(test=name, passed=isTRUE(ok), detail=detail, stringsAsFactors=FALSE)
  message(if (isTRUE(ok)) "PASS " else "FAIL ", name, if (nzchar(detail)) paste0(": ", detail) else "")
}
source_row <- function(id) sources[sources$source_id == id, , drop=FALSE][1,]

# House indexes must discover publications; localized pages and PDFs must come
# from links exposed by the viewer itself.
if (run_section("house")) {
sessions <- sessions_in_range(cfg)
check("five parliamentary sessions 1991-1998",
      setequal(paste(sessions$parliament, sessions$session, sep="-"), c("34-2","34-3","35-1","35-2","36-1")))
target_viewers <- c(
  sprintf("https://www.ourcommons.ca/DocumentViewer/en/35-1/house/sitting-%d/hansard", c(92,93,94,97,98,99)),
  sprintf("https://www.ourcommons.ca/DocumentViewer/en/35-2/house/sitting-%d/hansard", c(2,4,7,18,50))
)
viewer_dates <- vapply(target_viewers, function(page) {
  tryCatch({
    house_viewer_links(page, cfg, live_force)$publication_date
  }, error=function(e) NA_character_)
}, character(1))
expected_1996 <- c("1996-02-28","1996-03-01","1996-03-06","1996-03-21","1996-05-27")
check("1994 intervention debates", any(grepl("1994-09-(19|20|21|26|27|28)", viewer_dates)), paste(sum(substr(viewer_dates,1,4)=="1994",na.rm=TRUE), "1994 publications"))
check("five specified 1996 debate dates", all(expected_1996 %in% viewer_dates), paste(setdiff(expected_1996, viewer_dates), collapse=", "))
sample_viewer <- "https://www.ourcommons.ca/DocumentViewer/en/35-2/house/sitting-2/hansard"
sample_pair <- tryCatch(resolve_house_viewer_pair(sample_viewer, cfg, force=live_force), error=function(e) character())
house_viewers <- vapply(sample_pair, discovered_source_page, character(1), fallback="")
check("House viewer PDFs", length(sample_pair)==2L && all(grepl("\\.pdf($|[?#])",sample_pair,ignore.case=TRUE)) && all(nzchar(house_viewers)), paste(length(sample_pair),"PDF records"))
paired <- vapply(sample_pair, function(u) length(discovered_counterpart(u)) == 1L, logical(1))
check("House English/French pairing", all(paired), paste(sum(paired),"records with page-discovered counterparts"))
localized_fr <- house_viewers[grepl("-f\\.pdf$", sample_pair, ignore.case=TRUE)]
check("official French localized viewer routes",
      length(localized_fr)==1L && grepl("/(fr|fra)/.+/(chambre|seance|debats)", localized_fr, ignore.case=TRUE),
      paste(localized_fr, collapse=" | "))
sample_pdf <- sample_pair[grepl("/Debates/002/han002-e\\.pdf$", sample_pair, ignore.case=TRUE)][1]
sample_title <- tryCatch({
  fetched <- polite_get(sample_pdf, cfg, live_force)
  content <- extract_content(fetched$path, fetched$type, sample_pdf)
  parliament_metadata(content$title, content$text, sample_pdf, "en")$title
}, error=function(e) "")
check("authoritative Hansard sitting/date title",
      identical(sample_title, "House of Commons Debates (Hansard) No. 2 — 1996-02-28"), sample_title)

# Committee report regression fixture: page is a test input, never a crawl seed.
report_page <- "https://www.ourcommons.ca/DocumentViewer/en/39-1/FAAE/report-4"
report_pdfs <- resolve_house_viewer_pair(report_page, cfg, force=live_force)
check("committee report direct PDFs", length(report_pdfs)==2L && all(grepl("\\.pdf$",report_pdfs,ignore.case=TRUE)), paste(report_pdfs,collapse=" | "))
}

# Pre-1994 discovery must use the official Library of Parliament/LAC portal and
# emit stable download-resolver URLs rather than page-viewer URLs.
if (run_section("canadiana")) {
volumes_1992 <- c("https://parl.canadiana.ca/view/oop.com_HOC_3402_66_3",
                  "https://parl.canadiana.ca/view/oop.com_CDC_3402_66_3")
historical_urls <- unique(unlist(lapply(volumes_1992, discover_historical_volume, cfg=cfg), use.names=FALSE))
check("historical Parliament download URLs", length(historical_urls)>0 && all(grepl("/files/get/",historical_urls)), paste(length(historical_urls),"matching pages"))
record_1992 <- historical_urls[grepl("oop\\.com_(HOC|CDC)_3402_66_3", historical_urls, ignore.case=TRUE)]
record_languages <- vapply(record_1992, function(u) discovered_language(u) %||% "", character(1))
check("bilingual 1992 Canadiana record",
      any(grepl("_HOC_",record_1992)) && any(grepl("_CDC_",record_1992)) && all(c("en","fr") %in% record_languages),
      paste(record_languages, collapse=", "))
}

# Defence is traversed from the official archive only. Expected names are test
# assertions and are not injected into discovery output.
if (run_section("defence")) {
operation_urls <- discover_source(source_row("forces_archive"), cfg, force=live_force)
expected_operations <- c("ESCORT","FORWARD ACTION","CAULDRON","DIALOGUE","PIVOT","STANDARD (UNMIH)","STANDARD (UNSMIH)","STABLE","CONSTABLE","COMPLIMENT")
patterns <- c("/escort\\.html","forward-action","cauldron","dialogue","/pivot\\.html","standard-1996","standard-unsmih-1996","/stable\\.html","constable","compliment")
operation_subset <- operation_urls[Reduce(`|`,lapply(patterns,grepl,x=operation_urls,ignore.case=TRUE))]
operation_titles <- vapply(operation_subset, function(u) tryCatch({
  fetched<-polite_get(u,cfg,live_force); extract_content(fetched$path,fetched$type,u)$title
}, error=function(e) ""), character(1))
operation_text <- paste(c(operation_titles, operation_subset), collapse=" ")
for (i in seq_along(expected_operations)) check(paste("Defence",expected_operations[i]), grepl(patterns[i],operation_text,ignore.case=TRUE), paste(length(operation_urls),"archive links traversed"))
}

if (run_section("sanctions")) {
sanctions_source <- source_row("gac_sanctions")
sanctions_cfg <- cfg
sanctions_cfg$start_year <- 2022L
sanctions_cfg$end_year <- 2026L
sanctions_urls <- tryCatch(discover_source(sanctions_source, sanctions_cfg, force=live_force), error=function(e) character())
terms <- safe_read_csv("config/terms.csv"); families <- safe_read_csv("config/families.csv")
sanctions_records <- bind_rows_base(lapply(sanctions_urls, function(url) tryCatch(
  process_url(url, sanctions_source, sanctions_cfg, terms, families, force=live_force),
  error=function(e) NULL)))
policy <- sanctions_records[sanctions_records$document_type == "sanctions_policy", , drop=FALSE]
check("current GAC Haiti sanctions documents", nrow(sanctions_records) > 0L &&
      all(sanctions_records$families == "sanctions") &&
      all(sanctions_records$institution == "Global Affairs Canada"),
      paste(nrow(sanctions_records), "validated language records"))
policy_languages <- unique(policy$language[policy$language %in% c("en", "fr")])
check("current GAC Haiti sanctions bilingual policy", all(c("en", "fr") %in% policy_languages) &&
      any(vapply(split(policy, policy$document_id), function(x) all(c("en", "fr") %in% x$language), logical(1))),
      paste(policy_languages, collapse=", "))
check("GAC sanctions remain distinct from Gazette and Justice",
      !any(grepl("gazette\\.gc\\.ca|laws-lois\\.justice\\.gc\\.ca", sanctions_records$document_url, ignore.case=TRUE)))
reviews <- sanctions_records[sanctions_records$document_type == "regulatory_review", , drop=FALSE]
review_pair <- reviews[grepl("(?:sor|dors)-2022-226", reviews$document_url, ignore.case=TRUE), , drop=FALSE]
check("GAC Haiti regulatory review bilingual grouping", nrow(review_pair) == 2L &&
      length(unique(review_pair$document_id)) == 1L && all(c("en", "fr") %in% review_pair$language),
      paste(nrow(reviews), "language records"))
}

# Catalogue discovery is exercised without treating catalogue pages as records.
if (run_section("catalogue")) {
catalogue_source <- Sys.getenv("HISTORICAL_CATALOGUE_SOURCE", "")
catalogue_ids <- c("historical_diplomacy","historical_development","historical_sanctions","historical_police")
if (nzchar(catalogue_source)) catalogue_ids <- intersect(catalogue_ids, catalogue_source)
for (id in catalogue_ids) {
  source <- source_row(id)
  urls <- tryCatch(discover_publications_catalogue(source,cfg,force=live_force),error=function(e) character())
  domains <- source_domains(source)
  official <- vapply(urls, function(u) any(vapply(domains, grepl, logical(1), x=u, fixed=TRUE)), logical(1))
  route_ok <- length(urls)>0 && all(!grepl("/publication\\.html$",urls)) &&
    all(grepl("^https?://",urls)) && all(official) &&
    length(urls) <= as.integer(cfg$max_discovered_per_source)
  check(paste("official collection route",id), route_ok,
        paste(length(urls),"catalogue-discovered electronic routes"))
  terms <- safe_read_csv("config/terms.csv"); families <- safe_read_csv("config/families.csv")
  outcomes <- vapply(urls, function(url) tryCatch({
    process_url(url, source, cfg, terms, families, force=live_force)
    "candidate"
  }, crawler_condition=function(e) e$status, error=function(e) "failed"), character(1))
  check(paste("retrieved document validation",id),
        length(outcomes) == length(urls) && !any(outcomes == "failed"),
        paste(sum(outcomes == "candidate"),"candidates from",length(urls),"routes;",paste(names(table(outcomes)),table(outcomes),collapse=", ")))
}
}

summary <- bind_rows_base(result)
cat("\nFocused historical regression summary\n")
print(summary,row.names=FALSE)
if (any(!summary$passed)) quit(status=1L)

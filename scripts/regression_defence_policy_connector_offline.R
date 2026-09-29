#!/usr/bin/env Rscript
# Focused offline regression for defence-policy landing-page resolution.
# It exercises official PDF extraction, language routing, stable identity, and
# observable document classification without retrieval or project-data writes.

for (file in c("R/utils.R", "R/discover.R", "R/extract.R", "R/crawl.R"))
  source(file, encoding = "UTF-8")
source("R/http.R", encoding = "UTF-8")

english_page <- "https://www.canada.ca/en/department-national-defence/corporate/policies-standards/example.html"
french_page <- "https://www.canada.ca/fr/ministere-defense-nationale/organisation/politiques-normes/exemple.html"
english_pdf <- "https://www.canada.ca/content/dam/dnd-mdn/documents/corporate/reports-publications/2024/north-strong-free-2024-v2.pdf"
french_pdf <- "https://www.canada.ca/content/dam/dnd-mdn/documents/corporate/reports-publications/2024/nord-fort-libre-2024-v2.pdf"

english_html <- sprintf(
  '<html><head><link rel="alternate" hreflang="fr" href="%s"></head><body><a href="%s">Policy PDF (Portable Document Format)</a><a href="/en/index.html">Index</a></body></html>',
  french_page, english_pdf
)
french_html <- sprintf(
  '<html><head><link rel="alternate" hreflang="en" href="%s"></head><body><a href="%s">Politique PDF (Portable Document Format)</a></body></html>',
  english_page, french_pdf
)

english_links <- defence_policy_page_links(xml2::read_html(english_html),
                                            english_page)
french_links <- defence_policy_page_links(xml2::read_html(french_html),
                                           french_page)

stopifnot(
  is_publications_archive_pdf(
    "https://publications.gc.ca/collections/Collection/D2-168-2005E.pdf"
  ),
  is_pdf_bytes(charToRaw("%PDF-1.7 fixture")),
  !is_pdf_bytes(charToRaw("<html>fixture</html>")),
  substantive_defence_policy_haiti(
    paste(
      "The Canadian Forces have supported multilateral enforcement.",
      "Our personnel helped enforce economic sanctions off Haiti."
    )
  ),
  !substantive_defence_policy_haiti(
    "Country examples in a bibliography include Haiti."
  ),
  grepl(
    "sanctions économiques.*Haïti",
    defence_policy_scanned_excerpt(
      "https://publications.gc.ca/collections/collection_2012/dn-nd/D3-6-1994-fra.pdf",
      "fr"
    )
  ),
  identical(english_links$pdf, english_pdf),
  identical(french_links$pdf, french_pdf),
  identical(english_links$counterpart, french_page),
  identical(french_links$counterpart, english_page),
  identical(defence_policy_document_key(english_pdf),
            defence_policy_document_key(french_pdf)),
  identical(source_document_id(english_pdf, "defence_policy_index"),
            source_document_id(french_pdf, "defence_policy_index")),
  stable_version_id(english_pdf, "en") != stable_version_id(french_pdf, "fr"),
  identical(defence_policy_title(english_pdf, "en"),
            "Our North, Strong and Free: A Renewed Vision for Canada's Defence"),
  identical(defence_policy_title(french_pdf, "fr"),
            "Notre Nord, fort et libre : Une vision renouvelée pour la défense du Canada"),
  identical(defence_policy_document_key(
    "https://publications.gc.ca/collections/collection_2012/dn-nd/D3-6-1994-eng.pdf"),
    "defence_policy:white_paper:1994"),
  identical(defence_policy_document_key(
    "https://publications.gc.ca/collections/Collection/D2-168-2005F.pdf"),
    "defence_policy:international_policy_statement_defence:2005")
)

source <- data.frame(source_id = "defence_policy_index",
                     institution = "National Defence",
                     stringsAsFactors = FALSE)
policy <- derive_document_metadata(
  "Our North, Strong and Free: A Renewed Vision for Canada's Defence",
  "", english_pdf, source
)
implementation <- derive_document_metadata(
  "Defence Plan 2018-2023", "", english_page, source
)
dates <- extract_date_metadata(
  "© His Majesty the King in Right of Canada, 2024", english_pdf,
  "defence_policy_index"
)

stopifnot(
  identical(policy$families, "defence_policy"),
  identical(policy$document_type, "defence_policy"),
  identical(implementation$families, "defence_policy"),
  identical(implementation$document_type,
            "defence_policy_implementation_plan"),
  is.na(dates$publication_date),
  identical(dates$year, 2024L),
  identical(dates$date_source,
            "official policy year; exact publication day unavailable")
)

cat("defence-policy connector offline regression passed\n")

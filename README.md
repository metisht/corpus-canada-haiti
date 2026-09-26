# Canada–Haiti official documents corpus

A small reusable R crawler and Quarto website for official Canadian documents substantively concerning Haiti/Haïti, 1991–2026. Discovery uses free official Canadian sources; each candidate HTML page or PDF is opened and checked. Analytical concepts are editable coding fields, not inclusion criteria.

## Install

Requires R 4.2+ and Quarto. From this folder:

```powershell
& 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe' scripts/install_packages.R
```

If `Rscript` is already on `PATH`, the shorter `Rscript scripts/install_packages.R` works.

## Crawl, review, and render

The obvious crawler command is deliberately capped at five valid logical documents per source; the cap is not global:

```powershell
& 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe' run.R
```

Open `data/review.csv` in a spreadsheet or text editor. Leave new rows as `pending`, or set `decision` to `retain` or `exclude`; add optional notes, reviewer, and reviewed date. Never delete this file between runs. Decisions are joined by stable `document_id` and preserved.

Manual review changes each logical document from `pending` to `retain` or
`reject`. A logical document can have several English, French, or bilingual
version records. Rejected documents leave the active candidate and review files,
while their URLs, affected versions, reviewer, timestamp, and rationale remain
auditable in the diagnostics and validation history. Lists, indexes, procedural
references, comparisons, and isolated mentions may fall below the substantive
inclusion threshold without being historically uninteresting. Current counts
are read from the data files by the Quarto pages rather than maintained here.
Each decision is made once per logical `document_id` and propagated to every
active official-language version. `corpus.csv` contains retained versions only.
The public dashboard counts documents with distinct `document_id` values.

Parliamentary identity uses parliament, session, publication type, and sitting or report number. A House `DocumentViewer` page is provenance (`source_page`); its direct localized PDF is the document (`document_url`). Official language links group English and French versions. Defence records retain both the Canadian operation name and the associated international mission. Historical catalogue hits are discovery leads only: retrieved documents must independently pass date, substantive-Haiti, and family checks.

Haiti sanctions are collected in three linked but distinct documentary layers: Global Affairs Canada policy and announcement pages, original Canada Gazette instruments, and consolidated Justice Laws regulations. A GAC sanctions page does not need to be a regulation to qualify. The historical 1991–1998 sanctions connector currently validates no historical candidate documents; this finding does not apply to current GAC Haiti-sanctions material.

Use `--max-valid-per-source=N` to override the validation limit. The legacy `--max-valid=N` spelling retains the same per-source meaning.

Only after reviewing limited results, run the complete live crawl explicitly:

```powershell
& 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe' run.R --full
```

Render the website:

```powershell
quarto render
```

## Bilingual website maintenance

The English and French dashboards use the same implementation in `_includes/dashboard.qmd`. Reader-facing English and French labels, controlled-vocabulary display terms, and the render-time consistency check are maintained in `R/site_i18n.R`. The small files `index.qmd` and `fr/index.qmd` set the language and include the shared dashboard; `_includes/language-nav.html` provides equivalent-page navigation between the two sections. Shared visual styling remains in `styles.css`.

Run the one small workflow check with `Rscript scripts/smoke_check.R`.

## Development environment

The crawler, review workflow, and Quarto website are developed in R using the Positron IDE. OpenAI Codex is used as an assisted-development tool for programming, debugging, testing, and documentation. Corpus inclusion and exclusion decisions remain human review decisions. Codex does not autonomously admit documents to the reviewed corpus.

## Configuration and outputs

- `config/case.yml`: case labels, date range, request policy, and validation cap.
- `config/sources.csv`: institutions, official endpoints, discovery methods, and source defaults.
- `config/families.csv`: editable multi-label family rules.
- `config/terms.csv`: editable English/French analytical vocabulary.
- `data/candidates.csv`: content-validated candidates, initially `pending`.
- `data/review.csv`: durable human decisions and annotations.
- `data/corpus.csv`: records explicitly retained by review.
- `data/concepts.csv`: publishable vocabulary snapshot.
- `data/retrieval_diagnostics.csv`: failures and per-source run summaries.
- `data/validation_changes.csv`: auditable replacements and pre-review exclusions.

## Documentary unit and pre-review filtering

The unit of analysis is an official publication or sitting, not an individual
digitized page. Relevant Canadiana pages from one historical publication are
consolidated under one stable `document_id`; `component_urls` preserves every
page URL and page markers retain the provenance of combined extracted text. A
genuinely bilingual publication remains one bilingual version. Separate English
and French versions are created only when officially published separately.

Indexes, finding aids, navigation pages, tables of contents, purely procedural
references, and incidental comparisons are excluded before manual review but
remain traceable in diagnostics as `non_document` or `excluded`, never as
retrieval failures. A human `reject` means the publication did not meet this
corpus's inclusion threshold; it does not mean the mention has no historical
value. Human decisions are made at `document_id` level and apply only to true
official language versions.

Downloads in `data/cache/`, temporary files, `_site/`, credentials, and local IDE files are ignored by Git. The publishable CSVs, code, configuration, and documentation may be versioned.

## Adapt to another case

Copy the project, change the ID, bilingual case labels, and years in `config/case.yml`; replace or extend official endpoints in `sources.csv`; then edit `families.csv` and `terms.csv`. Empty the generated candidate/corpus/diagnostic rows while retaining their headers and start with a fresh review template. Run the capped command before any full crawl.

## GitHub Pages

Work directly on `main`. Create an empty GitHub repository, then:

```powershell
git remote add origin https://github.com/YOUR-ACCOUNT/YOUR-REPOSITORY.git
git push -u origin main
```

In repository settings, set Pages to **GitHub Actions**. The included workflow renders and deploys the site; it never launches the crawler.

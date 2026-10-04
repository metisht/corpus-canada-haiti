# Canada–Haiti official documents corpus

A longitudinal corpus examining how Canadian official institutions have represented Haiti and framed Canadian security and intervention in the country from 1991 to 2026. It brings together policy documents, parliamentary records, military and police materials, diplomatic statements, development publications, sanctions, and budget documents. Analytical concepts are coding fields, not inclusion criteria.

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

Defence-policy discovery starts from official National Defence presentation and Government of Canada Publications catalogue pages and follows their published PDF and language links. Retrieved policies remain subject to the same temporal and substantive-Haiti criteria as other documents.

Spatial coding begins with bilingual lexical proposals drawn from retained text. `data/spatial_review.csv` preserves occurrence-level evidence, while `data/spatial_review_queue.csv` consolidates review at `document_id + dimension + proposed_code`. A human reviewer confirms or dismisses each proposal; generic terms do not qualify without an observable spatial relation involving Haiti. Typical dismissals are incidental geography, an affiliation or job title, regulatory boilerplate, a term concerning another country or relationship, or insufficient context. The completed queue contains 160 `confirm`, 62 `dismiss`, and 0 `pending` decisions.

Haiti sanctions are collected in three linked but distinct documentary layers: Global Affairs Canada policy and announcement pages, original *Canada Gazette* instruments, and consolidated Justice Laws regulations. A Global Affairs Canada sanctions page does not need to be a regulation to qualify.

Use `--max-valid-per-source=N` to override the validation limit.

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

The crawler, review workflow, and Quarto website are developed in R using the Positron IDE. OpenAI Codex assists with programming, testing, and documentation. Human researchers remain responsible for corpus review, analytical coding, and methodological choices.

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

## Operational briefing board

The bilingual dashboard reads a curated engagement layer from `data/engagements.csv`, with many-to-many documentary evidence in `data/engagement_documents.csv` and instruments in `data/engagement_instruments.csv`. A `document_id` identifies an official source; an `engagement_id` identifies a documented Canadian action. Publications, debates, reports, and budgets do not automatically become timeline points. An engagement can combine military, police, institutional, diplomatic, regulatory, development, financial, and humanitarian instruments.

Engagement dates describe the Canadian action and never inherit a page-modification or publication date as a duration. Canadian operations remain distinct from international missions, including the UNMIH and UNSMIH phases of STANDARD. The 1993–1996 sequence separately represents CAULDRON, embargo enforcement, evacuation planning, the 1994 UNMIH advance team, the documented Canadian police-training proposal, PIVOT, and the two STANDARD phases. External reporting may guide archival searches but is not evidence until corroborated by a retained official Canadian source. The timeline measures engagements, not the number of documents, and does not infer causality from chronological succession.

The public board places each engagement in one or more horizontal instrument layers: military operations, police activity, development and institutional support, diplomatic and multilateral action, sanctions and regulation, and financial or humanitarian assistance. Repeated, vertically connected marks identify the same engagement across layers rather than additional engagements or documents. Filters can isolate or combine these repertoires of action. The layers show the sedimentation, coexistence, and modular combination of documented instruments. Their vertical order implies neither hierarchy nor causal dependence. Point density is not a measure of intervention intensity.

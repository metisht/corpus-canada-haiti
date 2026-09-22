# Canada–Haiti official documents corpus

A small reusable R crawler and Quarto website for official Canadian documents substantively concerning Haiti/Haïti, 1991–2026. Discovery uses free official Canadian sources; each candidate HTML page or PDF is opened and checked. Analytical concepts are editable coding fields, not inclusion criteria.

## Install

Requires R 4.2+ and Quarto. From this folder:

```powershell
& 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe' scripts/install_packages.R
```

If `Rscript` is already on `PATH`, the shorter `Rscript scripts/install_packages.R` works.

## Crawl, review, and render

The obvious crawler command is deliberately capped at five valid candidates per source:

```powershell
& 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe' run.R
```

Open `data/review.csv` in a spreadsheet or text editor. Leave new rows as `pending`, or set `decision` to `retain` or `exclude`; add optional notes, reviewer, and reviewed date. Never delete this file between runs. Decisions are joined by stable `document_id` and preserved.

Only after reviewing limited results, run the complete live crawl explicitly:

```powershell
& 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe' run.R --full
```

Render the website:

```powershell
quarto render
```

Run the one small workflow check with `Rscript scripts/smoke_check.R`.

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

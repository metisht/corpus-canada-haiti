# Proportional validation policy

Validation must be proportional to the risk and scope of the change. Do not run the full regression suite by default.

## Level 0 — Text and comments

Applies to:

- comments;
- README wording;
- methodology or limitations prose;
- labels with no code-path effect;
- spelling and punctuation.

Run only:

- inspect the diff;
- `git diff --check`;
- verify English/French equivalence when both language pages are affected.

Do not:

- run R regressions;
- run smoke checks;
- render the entire site.

## Level 1 — Presentation

Applies to:

- CSS;
- responsive layout;
- HTML structure without data logic changes;
- visual labels;
- spacing, typography and colors.

Run only:

- render the directly affected page or pages;
- inspect desktop and mobile rendering;
- check browser console if JavaScript or DOM structure is touched;
- `git diff --check`.

For shared bilingual dashboard components, render only:

- `index.qmd`;
- `fr/index.qmd`.

Do not run crawler, reconciliation or historical tests.

## Level 2 — Local component behavior

Applies to:

- dashboard JavaScript;
- `R/site_i18n.R`;
- filters;
- table grouping;
- localized formatting;
- interactive visualization logic;
- a contained R helper.

Run:

- parse only changed R files;
- the focused regression test for that component;
- render only affected pages;
- targeted accessibility or DOM assertions;
- `git diff --check`.

Do not run unrelated crawler or historical regressions.

## Level 3 — Data pipeline

Applies to:

- `R/crawl.R`;
- `R/discover.R`;
- `R/extract.R`;
- `R/reconcile.R`;
- review propagation;
- candidate identity;
- schema changes;
- active CSV changes;
- source configuration.

Run:

- parse changed R files;
- only regression suites relevant to the changed pipeline stage;
- smoke check when active data or corpus construction changes;
- data-integrity assertions;
- `git diff --check`.

Examples:

- discovery-only change: bounded-discovery tests, not dashboard rendering;
- reconciliation change: reconciliation and completed-review tests, not historical discovery unless identity logic is shared;
- historical parser change: historical regression plus smoke check;
- data-only reviewed decision update: reconciliation test plus smoke check.

## Level 4 — Release validation

Applies only when:

- preparing a commit for publication;
- changing shared architecture across several subsystems;
- modifying schemas used throughout the repository;
- explicitly requested by the researcher.

Run:

- all offline regression suites;
- smoke check;
- syntax checks;
- full Quarto render;
- English/French inspection;
- `git diff --check`;
- data hashes and Git audit.

Do not run live crawls unless explicitly authorized.

# General rules

1. Before testing, state the selected validation level and why.
2. Use the lowest level capable of detecting the likely regression.
3. Do not escalate merely because more tests exist.
4. Do not rerun an unchanged passing test during the same task.
5. Cache test results within the task when inputs have not changed.
6. If only comments changed after a successful test, use Level 0.
7. If a test exceeds five minutes, stop and report why unless the task explicitly authorizes a long run.
8. Never launch an unrestricted crawl as validation.
9. Live-network checks require explicit authorization.
10. A full Quarto render is not required for every site change.
11. Report tests not run as intentionally out of scope, not as omissions.
12. Do not add tests unrelated to the defect or feature being changed.
13. Do not turn a small task into a repository-wide audit.
14. Do not commit or push unless explicitly authorized.

# Task reporting

Every final report should include only:

- validation level selected;
- focused checks run;
- results;
- checks intentionally not run;
- any residual risk.

Avoid repeating the entire repository state for a localized task.

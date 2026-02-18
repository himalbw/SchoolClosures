AGENTS.md for SchoolClosures

Scope
- Economics project on causal effects of school closures on education outcomes.
- Raw data: `data/raw/`.
- Clean data: `data/clean/`.
- Data cleaning scripts: `source/`.
- Analysis scripts: `analysis/`.
- Figures: `figures/` (final PNGs).
- Tables: `tables/` (final LaTeX).


Build, Lint, Test
- No build, lint, or test tooling is configured in this repo.
- No single-test runner exists; add one only if a test framework is introduced.
- Run scripts directly:
  - Python cleaning scripts: `python source/<script>.py`.
  - R analysis scripts: `Rscript analysis/<script>.R`.


Repo Rules (must follow)
- Do not add inline comments or docstrings.
- Use ASCII unless the existing file already uses non-ASCII.
- Avoid defining helper functions inside other functions.
- Keep scripts ending with at least one newline.
- Sections in scripts are separated by two newlines; subsections by one newline.


Python Code Organization
- Section 1: Imports
  - Subsection A: language dependencies, alphabetized.
  - Subsection B: project dependencies, alphabetized.
- Section 2: Main function
  - A: declare global variables (constants, I/O paths) in ALL_CAPS.
  - B: read data.
  - C: call processing functions.
- Section 3: Processing functions, ordered by call sequence.
- Section 4: main method call only.


Python Style and Naming
- Variables, functions, files: `snake_case`.
- Classes: `CamelCase`.
- Globals declared in main: `ALL_CAPS`.
- Files that clean raw data: `build_<datasetname>.py`.
- Files that process clean data: `process_<datasetname>.py`.
- Each dataset source gets its own folder.
- Use `Path` objects for all paths; convert to string only when required.


Pandas Conventions
- Prefer method chaining for transforms.
- Use:
  - `.assign(...)` for mutations.
  - `.groupby(...).agg({...})` for aggregations.
  - `.groupby(...).transform(...)` for in-place group calculations.
  - `.query(...)` for simple filters; `.loc[...]` for complex filters.
  - `.merge(..., how=..., on=[...])` for joins.
  - `.drop(columns=[...])` to remove columns.
  - `.sort_values(by=[...])` to order rows.
- Avoid extra helper functions; each function should be cohesive.


R Conventions
- Use tidyverse syntax when possible.
- Prefer `%>%` pipelines.


LaTeX Conventions
- Tables must use `booktabs` and follow this order:
  - Caption
  - Label `tab:<table_name>`
  - `tabular` with `\toprule`/contents/`\bottomrule`
  - End `tabular`
  - `minipage` with width `\linewidth` for notes
- Figures must follow this order:
  - Caption
  - Label `fig:<figure_name>`
  - Subfigures (if any)
  - `minipage` with width `\linewidth` for notes


Error Handling
- Prefer explicit checks and fail fast for missing files or columns.
- Keep error handling minimal and local to the operation.


Files and Outputs
- Read raw inputs only from `data/raw/`.
- Write cleaned datasets to `data/clean/`.
- Write final figures to `figures/` as PNGs.
- Write final tables to `tables/` as LaTeX.



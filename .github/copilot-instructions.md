# Copilot Instructions for rubric-evaluation

Apply these rules by default in this repository unless the user explicitly overrides them.

## Rubric review scope
- Do not critique, revise, or assign findings to ER1 or ER2.
- Treat ER1 and ER2 as out of scope for rubric review unless the user explicitly requests their evaluation.

## CohortGenerator workflow (required)
- Use `CohortGenerator::getCohortTableNames(cohortTable = ...)` to resolve cohort table names.
- Create tables with `CohortGenerator::createCohortTables(...)` before generation when needed.
- Build `cohortDefinitionSet` as a data frame with columns:
  - `cohortId`
  - `cohortName`
  - `sql`
- Run cohorts with `CohortGenerator::generateCohortSet(...)`.
- Prefer `stopOnError = TRUE`.
- Prefer `incremental = TRUE` unless the existing file/function has a fixed, intentional alternative.

## Cohort definition sourcing (required)
- Prefer sourcing cohort definitions from `ROhdsiWebApi` when cohort IDs are provided and SQL is not manually supplied.
- Require a `webapiBaseUrl` parameter for functions that fetch cohort definitions from WebAPI.
- Use `ROhdsiWebApi::exportCohortDefinitionSet(...)` as the default retrieval approach.

## Naming conventions (required)
- Use camelCase for function names, argument names, object/list field names, and local variables in all active package code.
- Do not introduce new snake_case identifiers in `R/`, `README.md` examples, or `codeToRun.r`.
- Keep external contract names (for example SQL placeholders/column names or third-party API keys) unchanged when required for compatibility.

## Consistency requirements
- Keep parameter naming and control flow aligned with existing patterns in this repo.
- Reuse the same schema/table argument conventions used in current R scripts.
- Avoid introducing a different CohortGenerator style unless requested.

## CohortGenerator deprecation note
- Do NOT use `incrementalFolder` in new calls to `CohortGenerator::generateCohortSet(...)`.
- Keep `incremental = TRUE` as the default unless a function has a documented reason not to.

## Package compatibility and documentation standards (required)
- Follow roxygen2 documentation style used in `R/`:
  - Add `#'` docs for public/exported APIs.
  - Add `@param`, `@return`, and at least one minimal `@examples` block.
  - Add `@export` for functions/objects intended for package users.
- Keep `NAMESPACE` aligned with explicit roxygen-style exports/imports.
- Keep `DESCRIPTION` roxygen metadata intact (`Config/roxygen2/version`, `Roxygen: list(markdown = TRUE)`).
- Prefer adding new user-facing functions in `R/` (not `extras/original/R_orig/`).

## Change-management conventions (required)
- If user-facing behavior, API signatures, or workflow steps change, update:
  - `README.md` (usage and examples)
  - `REFACTORING.md` (architecture/flow notes)
- If exports/imports change, update roxygen comments and regenerate/check namespace consistency.
- Keep examples realistic for this repository, and wrap environment-dependent examples in `\dontrun{}`.

## Ambiguity handling
- If repository constraints conflict with this pattern, ask the user before changing approach.
- If there are multiple valid implementations, choose the simplest one that matches existing project style.

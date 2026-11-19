# Ithaca

Ithaca is an R package for creating rubric-based phenotype evaluation logic, generating OMOP-style SQL, managing concept sets, and scoring subjects across one or more databases.

## What the package does

1. Creates rubrics for target conditions (`getRubrics` / `runIthacaWorkflow`)
2. Creates SQL statements from rubric rules (OMOP-style prompt-driven SQL)
3. Builds and reuses concept sets (`createRubricConceptSets`)
4. Scores subjects across multiple conditions and databases (`scoreSubjects`)
5. Supports pre-run and post-run rubric edits with SQL regeneration (`adjustRubric` / workflow methods)
6. Uses CohortGenerator conventions for cohort generation (`generateCohorts`)
7. Enforces specialty-provider SQL constraints for specialty criteria (auto-correction requires provider join + `specialty_concept_id` filter)

Concept-set matching first performs exact, embedding, and fuzzy library matching without
LLM calls. Only unresolved names go through configured-LLM synonym expansion and filtered
Hecate searches when `useLlmAdjudication: true`. It searches the original term plus
clinical synonyms, applies a domain filter inferred from the concept-set suffix, merges
the candidate lists, and then performs the existing matching/adjudication steps. Setting
`useLlmAdjudication: false` disables both synonym expansion and LLM adjudication.

Generated concept-set names are canonicalized before use; repeated adjacent name tokens are
collapsed so malformed names such as `rescue_antiseizure_medication_medication_codes` become
`rescue_antiseizure_medication_codes`.

Names still unmatched are created, and then only those newly created names are matched again.
Names matched before creation keep that match, because a fresh Hecate/LLM pass can return
different results and silently drop them. `conceptSetMatchDiagnostics_V*.csv` has one row per
concept-set name. For a name that went through synonym expansion, that row is the expansion
pass's row. The post-create `none=` count means names that could not be created or matched;
these also trigger a warning, and scoring stops until they are resolved.

To inspect one rendered rubric rule without executing it:

```r
ruleSql <- getRubricRuleSqlFromConfig(
  conditionName = "Rheumatoid arthritis",
  rubricId = "R1",
  ruleId = "ER1"
)
cat(ruleSql$sql)
```

The function uses the first database in `config$cdm`, the latest rubric/concept-set artifacts,
and the current config settings. It requires the latest matched concept-set artifacts and
stops if a concept-set placeholder cannot be resolved.

To retrieve one concept set from the DuckDB library:

```r
result <- queryConceptSetLibrary(
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  conceptSetName = "heart_failure"
)

result$conceptSet
```

SQL-LIKE patterns are supported; `%` matches any sequence and `_` matches one character:

```r
matches <- queryConceptSetLibrary(
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  conceptSetName = "%depress%",
  smallList = TRUE
)
```

With `smallList = TRUE`, exact and wildcard queries return a compact data frame containing
only `conceptSetName` and `cleanName`. Otherwise, exact names return one entry and wildcard
patterns return a list of matching entries.

To export one concept set as ATLAS-style JSON text (exact `conceptSetName` or `cleanName`,
case-insensitive, no wildcards). It writes `<conceptSetName>.json` to `outputDirectory`
(default: the config's `outputDirectory`; use `writeFile = FALSE` to skip) and can also copy it
to the clipboard. If no matching concept set exists, it displays a message and returns `NULL`:

```r
exportConceptSetJson(
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  conceptSetName = "heart_failure",
  copyToClipboard = TRUE
)
```

To find concept sets that contain only the failed-resolution fallback concept ID:

```r
failedConceptSets <- listConceptSetsWithConceptId(
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb"
)
```

To remove a concept set by its clean/display name, preview first:

```r
deleteConceptSetFromLibraryByCleanName(
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  cleanName = "major depressive disorder",
  dryRun = TRUE
)
```

To insert or update a concept set from JSON, with `cleanName` derived from the
concept-set name:

```r
insertConceptSetFromJson(
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  conceptSetJsonPath = "P:/llm/rubrics/major_depressive_disorder_diagnosis_codes.json"
)
```

By default, the filename stem becomes the clean name with underscores converted to spaces,
and the concept-set name becomes the lowercase underscore-normalized clean name. Supply
`cleanName` and/or `conceptSetName` to override either derived value.

The lookup is case-insensitive and returns `NULL` when the name is not present.

## Repository structure

- `R/`: package source code (current implementation)
- `extras/original/R_orig/`: legacy scripts retained for reference
- `extras/original/docs/`: original prompt/source artifacts retained for reference
- `inst/prompts/`: LLM prompt templates used by workflow functions
- `inst/sql/sql_server/`: SQL templates used for data retrieval and helper queries
- `REFACTORING.md`: detailed refactor notes and architecture decisions
- `REVISION_SPEC_VNEXT.md`: target-state specification for planned full rewrite
- `config.yaml`: editable end-to-end run configuration (common defaults + contextual defaults + run overrides)

## Prerequisites

- R >= 4.2.0
- Access to required packages listed in `DESCRIPTION`
- Valid database connection specifications (for example `CCAE_OHDA_DBSpec`)
- API credentials configured through environment variables where required
- Access to concept set seed library if used:
  - `P:/llm/rubrics/rubricConceptSetLibrary.duckdb`

## Core workflow (recommended)

The recommended orchestration API is `runIthacaWorkflow()` or the `IthacaWorkflow` class.

For a ready-to-run starter script, see `codeToRun.r` at the repository root.
It contains a full parameterized run template (conditions, databases, optional edits, and scoring).

## YAML config-driven runs

You can run the full workflow from a YAML config:

```r
# one-time template creation
writeIthacaWorkflowConfigTemplate("config.yaml", overwrite = FALSE)

# edit config.yaml, then run
results <- runIthacaWorkflowFromConfig("config.yaml")
```

**Where `config.yaml` must live:** put `config.yaml` at the top level of the project that uses
Ithaca, and run R with that folder as the working directory. The easiest way is to open the
project's `.Rproj` file, or call `setwd("<project folder>")`. Config-driven functions default to
`configPath = "config.yaml"`, which R resolves from the current working directory (`getwd()`), not
from the project folder. When you are not at the project root (for example, when knitting a report in
a subfolder or running `Rscript` from another folder), the config is not found. In that case the
functions stop with an error that shows the path tried and the current working directory. Each run
prints `Using config: <full path>` so you can confirm which file was read. To run from a different
directory on purpose, pass an absolute path, for example
`reviewConciseRubricsFromConfig(configPath = "E:/Ithaca/config.yaml")`.

`config.yaml` is organized into sections:

- `general`: parameters shared by many functions (paths, LLM clients, databases via `cdm`, `conditionSpecs`, `dxCriteria`).
- `rubricDevelopment`: creating, reviewing, and editing rubrics.
- `conceptSetBuilding`: concept set creation and library matching (including LLM adjudication).
- `randomCohort`: random comparison cohort settings.
- `phenotypeReportScoring`: scoring of the phenotypes to evaluate.
- `dataDrivenScoring`: setting rule scores from the data (see "Data-driven rule scores").
- `other`: optional and legacy settings (`cohortDefinitionSet`, `databaseSpecNames`).

The older layout (`defaults.common`, `defaults.contextual`, `run`) is still accepted; when both are
present, the section keys override it. Keys in `defaults.*` and `run` map to the sections by name
(for example `run.cdm` is `general.cdm`, `run.databaseSpecNames` is `other.databaseSpecNames`), and
`writeIthacaWorkflowConfigTemplate()` writes the section layout from `inst/config_template.yaml`.
Documentation below uses the section names.

Old layout (still accepted):

- `defaults.common`: defaults typically used in most runs (for example similarity/adjudication thresholds).
- `defaults.contextual`: defaults usually environment-specific (for example paths and webapi URL).
- Anything under `run:` overrides defaults.

Database configuration options:

- Preferred: `general.cdm` as a named map (`databaseId: cdm_schema_name`) for Databricks-backed runs.
- Legacy fallback: `other.databaseSpecNames` as object names in the R environment.

For Databricks `general.cdm` usage, set environment variables:

- `DATABRICKS_TOKEN`
- `DATABRICKS_HOST`
- `DATABRICKS_HTTP_PATH`
- Optional `DATABRICKS_CATALOG`

For LLM and embedding clients, set:

- `GENAI_O3_ENDPOINT`
- `GENAI_GPT4O_ENDPOINT`
- `GENAI_API_GPT4_KEY`
- `GENAI_EMBED_ENDPOINT`
- `GENAI_API_EMBED_KEY`

All credentials are read from system environment variables only.

Config-driven runs now call package function `getDatabricksConnectionDetails()`
during config loading. By default it resolves:
- `password/token`: `DATABRICKS_TOKEN`
- `host`: `DATABRICKS_HOST`
- `httpPath`: `DATABRICKS_HTTP_PATH`

You can still override this by defining `getDatabricksConnectionDetails()` in
the global environment.
Host values may be provided with or without `https://`; `httpPath` may be
provided with or without a leading `/`.
`httpPath` must be only the warehouse endpoint path (for example
`/sql/1.0/warehouses/<warehouse-id>`); do not append extra JDBC settings.
JDBC uses `EnableArrow=0` by default for compatibility with the DatabaseConnector
query path. If additional JDBC settings are appended after `;` in `DATABRICKS_HTTP_PATH`,
they are preserved as extra JDBC connection-string settings.
JDBC user resolution order is: `getDatabricksConnectionDetails()$user` (if provided),
then `DATABRICKS_USER`,
then fallback `"token"`.

`cohortTablePrefix` is used to generate per-CDM cohort table names by auto-appending the CDM schema name.

To load condition specs from CSV instead of inline YAML list, set:

- `general.conditionSpecsCsvPath: extras/conditionSpecs_template.csv`

When `conditionSpecsCsvPath` is provided, it overrides `general.conditionSpecs`.
CSV template is provided at:

- `extras/conditionSpecs_template.csv`

`conditionSpecs` now supports condition-level cohort/report fields (inline YAML or CSV columns):

- `dxCriteria` (required choice key from `general.dxCriteria`; the choice's `criteria` becomes ER2's text and its `additionalPoints` are added to ER2's 2 points — see "Fixed rubric structure")
- `clinicalDefinitionFile` (optional config path to a CSV with `Phenotype` and `Definition` columns; fills null `additionalInformation` values by matching `testCondition` to `Phenotype`, ignoring case and all non-alphanumeric characters, so `crohns disease` matches `Crohn's disease`)
- `sourceCohortDefinitionId`
- `randomCohortDefinitionId` (required for scoring and scoring artifact lookup)
- `randomSampleFromCohortDefinitionId`
- `phenotypeCohortId` (list of cohort IDs for `testPhenotypeAlgorithmRubricReport(phenotypeCohortId = ...)`; CSV can use `;` or `,` delimiters)
- `ignore` (optional CSV column; `true`, `1`, or `yes` in any case skips that row in every workflow that reads the CSV; anything else, including blank, keeps it)
- `adjustableRuleId` (optional; one or more evidence-rule IDs such as `ER1` or `ER1;ER5` whose points are changed by `dxRuleScoreOverride`/`dxRuleScoreAdjustment` in `phenotypeReportScoring`. Blank means `ER1`, the index diagnosis rule in generated rubrics. Rubric edits and review applies stop if they would remove a rule listed here or change its type)
- `adjustableRuleScore` (optional; condition-level score applied to `adjustableRuleId` when rubric artifacts are generated. A blank `adjustableRuleId` uses `ER1`; an explicit `phenotypeReportScoring` override remains a separate runtime override.)

Rows marked `ignore` are dropped before validation, so an ignored row may have incomplete
values. The console lists the ignored conditions. If every row is ignored, the run stops with
an error. An ignored condition passed by name (for example to `reviewConciseRubricsFromConfig()`)
is reported as not configured.

Only `testCondition`, `dxCriteria`, and `rubricName` are required to
read a condition. `dxCriteria` must match a key in the configured `general.dxCriteria`
map; the resolved prose is passed to rubric generation. `additionalInformation`, `sourceCohortDefinitionId`,
`randomCohortDefinitionId`, `randomSampleFromCohortDefinitionId`, and `phenotypeCohortId` are optional; a blank CSV cell,
a missing CSV column, or an inline `null` all mean "not provided". Scripts that need a missing
value (for example `phenotypeCohortId` for the phenotype report, or `sourceCohortDefinitionId`
for building random cohorts) stop with their own clear message.

`webapiBaseUrl` is config-level (not condition-level) and should be set in `general.webapiBaseUrl`.

### Fixed rubric structure

Every newly generated rubric follows these rules. The prompt asks for them, and the code
overwrites the LLM's response so they always hold:

- YES threshold = 10 points, NO threshold = 2 points.
- `ER1` = `>=1 <condition> diagnosis within -5 to +5 days of index`, 3 points, concept set
  `<condition>_DIAGNOSIS_CODES` (for example `major_depressive_disorder_DIAGNOSIS_CODES`).
- `ER2` = the `criteria` of the condition's `general.dxCriteria` choice, worth 2 points plus that
  choice's `additionalPoints`, using the same diagnosis concept set plus visit sets for the
  care settings it names.
- All other evidence rules start at `ER3`. The prompt asks the LLM to scale their points
  to the 10-point YES threshold, but the code does not rescale them.
- `AE1` = the zero-occurrence penalty: `No <condition> diagnosis or related/differential diagnosis code
  within -30 to +30 days of index`, worth `-zeroOccurrencePenalty` points (default 3). Its SQL checks
  two concept sets: the condition's own `<condition>_DIAGNOSIS_CODES` (the set ER1 uses) and
  `<condition>_RELEVANT_DIAGNOSIS_CODES`, which holds 3-5 related or differential diagnoses and
  excludes the condition itself. The related concept IDs are found in Hecate, confirmed by the LLM,
  and stored with descendants during concept-set creation (no LLM-written SQL for this rule). If
  Hecate finds nothing or the LLM confirms nothing, a warning is printed (with the related terms,
  candidates, returned IDs and LLM rationale) and the relevant set holds only a no-match placeholder,
  so AE1 relies on the condition's own set. The LLM's
  alternative explanations start at `AE2`. Set the penalty in `general.zeroOccurrencePenalty`,
  or per condition with the `zeroOccurrencePenalty` conditionSpecs column (blank = config
  default). AE1 is protected from rubric review and concise-edit modify/remove, and it still
  applies when `excludeNegativeScores = TRUE`.

Rubrics created before AE1 existed can be migrated with
`addZeroOccurrencePenaltyRule(dryRun = TRUE)` (preview), then `addZeroOccurrencePenaltyRule()`.
It builds the concept set first, shifts existing AE IDs up by one, and saves new versioned
files (the concept-set library and rubric files can be reverted through their versions).
Rerun scoring afterwards.

Each `general.dxCriteria` choice is a block with two required entries:

```yaml
  dxCriteria:
    inpatientOutpatient:
      criteria: "One condition diagnosis code in an inpatient or emergency department setting within -5 to +5 of index, OR ..."
      additionalPoints: 0      # ER2 = 2 points
    inpatientOutpatientRare:
      criteria: "One condition diagnosis code in an inpatient or emergency department setting within -5 to +5 of index, OR ..."
      additionalPoints: 2      # ER2 = 4 points
```

`additionalPoints` must be a whole number from 0 to 4, so ER1 + ER2 stays below YES = 10.

For a symptom that cannot be tested (for example suicidal ideation), use a choice such as:

```yaml
    untestableSymptom:
      criteria: "One condition diagnosis code in any setting within -5 to +5 of index."
      additionalPoints: 4      # ER1 3 + ER2 6 = 9 points
```

One code earns 9 points, one short of YES (10), so one minor ER3+ rule confirms the case and any
negative evidence (an AE) keeps it below YES. ER1 and ER2 fire on the same code, so the 9 points
are one piece of evidence counted twice, not two independent signals. A subject with the code
never triggers AE1, and a subject at 9 sits between NO (2) and YES (10), so decide how your
report treats that middle zone.
A choice written as plain text, or missing either entry, stops config loading with an
example of the block format.

The console prints whatever was changed, for example
`Enforced fixed ER1/ER2/AE1 and YES/NO thresholds in template 1: YES threshold 8 -> 10; ...`.
Because `criteria` becomes ER2's criteria word for word, describe only the codes, care
settings, and date windows there, not point values. Existing rubrics are not
changed. Later edits (concise CSV edits or review apply) may change ER1, ER2, or the thresholds.

### YES rule combinations report

`createRubricYesCombinationReport(conditionName, outputDirectory)` lists the smallest sets of
positive evidence rules that reach the rubric's YES score. A set is listed only when removing
any one of its rules drops the total below YES, so totals land at or just above YES (for
example ER9 + ER10 + ER11 = 5 + 3 + 4 = 12 with YES = 10); adding another rule to a listed set
is never listed separately. Alternative explanations and hard exclusions are not used.

```r
createRubricYesCombinationReport(
  conditionName = "Multiple Myeloma",
  outputDirectory = "P:/llm/rubricsPears2"
)
```

It reads the latest concise rubric and saves `<condition>_yesRuleCombinations_V#.csv` in the
condition folder. Columns: `condition`, `rubric`, `ruleIds`, `criteria`, `ruleScores`,
`categories`, `distinctCategories` (known categories only), `includesDiagnosisRules` (ER1 or ER2
in the set), `rubricYesScore`, `ruleCount`, `totalScore`, and `pointsOverYes`. Rows with
`includesDiagnosisRules = TRUE` come first, then by `ruleCount` and `totalScore`.
Categories come from the latest `rubricList_V#.rds`; rules added by concise CSV edits or review
apply have no category there and show `unknown`. It runs automatically whenever a rubric is
created or changed (rubric creation, `applyConciseRubricEdits`, `applyConciseRubricReview`,
`adjustRubric`, and SQL fixes saved during scoring), so its version tracks the concise rubric.
A failure there is a warning, not an error. Turn it off with
`options(ithaca.yesCombinationReport = FALSE)`.

### Data-driven rule scores

The language model's rule points are not based on data. `applyDataDrivenRuleScoresFromConfig()`
(or `applyDataDrivenRuleScores("Seizure")` for named conditions; script:
`dataDrivenRuleScoresFromConfig.R`) replaces them with scores measured in the databases. Run it
after the concept sets are built and matched, and before scoring.

- Each rule's SQL runs on two populations in every database: people meeting ER2 around their first
  diagnosis (the cohort) and people with a differential diagnosis who never have the condition. The
  rule's score is `round(scaleMax * ln(LR) / ln(capLr))`, limited to plus or minus `scaleMax`, using the
  mean log likelihood ratio over the databases. The rule type follows the sign; a score of 0 keeps the
  type. A retyped rule gets a new ID and keeps its SQL.
- ER1, ER2 and AE1 are never changed. Hard exclusions, rules with 0 records in both populations,
  rules whose query failed and rules with an unresolved concept set keep their score; the report
  lists them under "RULES NOT SCORED FROM THE DATA".
- A gate decides whether the new scores are applied. With the proposed scores, cohort members (who
  get ER1 + ER2 points) must reach YES at least `positiveYesMin` and at most `positiveYesMax` of the
  time, differential negatives (no ER points) at most `negativeYesMax`, and the difference must be at
  least `separationMin`, in every database and rubric. If any limit fails, nothing is changed for that
  condition. The gate uses `yesScore` 10 and `noScore` 2 (not written to the rubric).
- Settings come from `dataDrivenScoring` in `config.yaml` (`capLr` 10 by default; per-condition values
  under `dataDrivenScoring.conditions.<rubricName>`); arguments of `applyDataDrivenRuleScores()`
  override them. The gate limits are provisional values to tune.
- Query results are saved per database under `<outputDirectory>/rubricEvidenceProbe/queryResults`;
  `reuseQueryResults = TRUE` reuses them (for example to try another `capLr`). The report, rules CSV
  and a flagged copy of the concise rubric are saved next to them. `applyChanges = FALSE` only reports.
- The latest concise rubric must have no unapplied edits (run `applyConciseRubricEditsFromConfig()` first).

```r
applyDataDrivenRuleScores("Seizure", applyChanges = FALSE)             # look first
applyDataDrivenRuleScores("Seizure", reuseQueryResults = TRUE)          # apply, reusing the queries
applyDataDrivenRuleScoresFromConfig("config.yaml")                      # every configured condition
```

The method is described in `docs/RubricImprovementProcess.md`. `extras/rubricImprovementProbe.R`
(`probeRubricImprovement()`) runs the same analysis without applying anything.

### Minimal full-run example

```r
conditions <- list(
  list(
    testCondition = "Gastroesophageal reflux disease",
    dxCriteria = "1 inpatient/ED diagnosis OR 2 outpatient diagnoses 1-60 days apart",
    additionalInformation = "",
    rubricName = "Gastroesophageal reflux disease",
    randomCohortDefinitionId = 14412
  )
)

databases <- list(CCAE_OHDA_DBSpec)

workflow <- runIthacaWorkflow(
  conditionSpecs = conditions,
  databases = databases,
  outputDirectory = "P:/llm/rubrics",
  seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  conceptLibraryVersion = 142,
  useExistingRubric = FALSE,
  generateCohorts = FALSE
)
```

### Full flow using class methods

```r
wf <- IthacaWorkflow$new(
  conditionSpecs = conditions,
  databases = databases,
  outputDirectory = "P:/llm/rubrics"
)

# 1) Create rubrics
wf$createRubrics(useExistingRubric = FALSE)

# 2) Optional pre-run edit
wf$editRubric(
  conditionName = "Gastroesophageal reflux disease",
  ruleId = "ER1",
  updates = list(criteria = "updated rule criteria"),
  regenerateSql = TRUE
)

# 3) Create concept sets
wf$createConceptSets(seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb")

# 4) Score subjects
scores <- wf$scoreSubjects(
  seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  conceptLibraryVersion = 142,
  generateCohorts = FALSE
)

# 5) Optional post-run edit
wf$postRunEdit(
  conditionName = "Gastroesophageal reflux disease",
  ruleId = "ER2",
  updates = list(criteria = "post-run updated criteria"),
  regenerateSql = TRUE
)
```

### Building concept sets in parallel

`createRubricConceptSetsFromConfig.R` reads two settings from `config.yaml`:

```yaml
general:
  conceptSetFolder: P:/llm/conceptSetBuild   # shared across runs and conditions; null = <outputDirectory>/conceptSetBuild
  parallelWorkers:
    conceptSetCreation: 4                    # 1 (default) = build one at a time
```

The older `conceptSetBuilding.conceptSetWorkers` key is still read when
`general.parallelWorkers.conceptSetCreation` is not set.

With `conceptSetCreation` above 1, every condition is matched against the library first, the
distinct missing concept sets across all conditions are built in parallel (one worker process each,
with its own LLM clients), and the main process writes them to the DuckDB library once all builds
finish. A finished set is saved as `<conceptSetFolder>/<conceptSetName>/result.rds`; after an
interruption a rerun reuses those files and only rebuilds the rest. A set that fails is not added to
the library (no placeholder) and gets an `error.txt` in its folder; rerun to retry it. Delete a set's
folder to force a rebuild. Each worker makes its own LLM calls, so more workers means more
concurrent requests against your rate limit. The clinical context of the requesting condition is no
longer sent to the concept set builder, so sets are not specialized to one condition.
With one worker, `conceptSetFolder` (when set) is used for the builder files instead of the condition folders.
`extras/parallelConceptSetProbe.R` is a small feasibility probe for timing a few builds.
Parallel concept-set builds use longest-predicted-first dispatch with dynamic worker replacement.
Known sets use the latest elapsed time in `conceptSetBuildTimes.csv` under `conceptSetFolder`;
new sets use deterministic complexity estimates that give extra weight to broad conditions,
multiple medications, routes, potency, and formulation terms. The first run uses heuristics and
records timings for later runs.

SQL rule scoring uses the same dynamic dispatch pattern. It uses the condition/database-specific
`ruleQueryTimes.csv` when available and estimates unknown query duration from SQL complexity
(joins, subqueries, unions, aggregation, temporal functions, and concept-set expansions).

#### Concept sets that come back empty

A build that raises an error is never stored (rerun to retry). A build that returns no concepts is
handled without failing the batch (`conceptSetBuilding.emptyConceptSet`, default `placeholder`):

1. An unreadable result is retried once.
2. With `emptyConceptSetFallback: true`, an existing (or newly built) set under a simplified name is
  used instead, e.g. `levothyroxine_brand_medication_codes` -> `levothyroxine_medication_codes`
  (only qualifier words such as brand/generic/specific/related are dropped). The alias is written to
  `matchedConceptSets` with match method `fallbackSimplified`; the rubric SQL is unchanged.
3. Otherwise the set is recorded as known empty: the placeholder concept 36433327 is stored in the
  library (`sourceFile` starts with `knownEmpty:`) and `<conceptSetFolder or condition folder>/<name>/knownEmpty.json`
  is written. Reruns skip it. Set `rebuildEmptyConceptSets: true` to rebuild known-empty sets.

Rules whose SQL uses a known-empty set are flagged `okToUse = FALSE` in the rubric (SQL untouched) and
listed in `<condition>_emptyConceptSetRules_V#.csv`; scoring prints a warning instead of stopping.
The flag is skipped with a warning when the concise rubric has unapplied edits. Each run writes
`conceptSetBuildSummary_V#.csv` to the output directory (status: built, reused, aliased, knownEmpty, error).
With `emptyConceptSet: stop`, the run stops after finishing when a set came back empty.

### Optional concept set library hints during rubric creation

You can ask rubric creation to suggest preferred concept set names from the existing
concept set library, while still allowing the LLM to choose or create new names when needed.

```r
getRubrics(
  conditionList = conditions,
  outputDirectory = "P:/llm/rubrics",
  useLibraryHints = TRUE,
  libraryHintSimilarityThreshold = 0.94,
  libraryHintTopCandidates = 5
)
```

When enabled, each condition folder saves:

- `conceptSetCandidateHints_V*.csv` (candidate retrieval diagnostics)
- `rubricConceptSetDecisions_V*.csv` (LLM decision audit: preferred/adapted/new)

## Resumable rubric creation

Rubric creation now writes intermediate checkpoint state per condition to support restart/resume.

- Checkpoint file: `<outputDirectory>/<Condition>/<Condition>_rubricCheckpoint.rds`
- Template-level responses and short-rubric SQL progress are saved incrementally.
- On rerun, incomplete checkpoints resume from the last completed step.
- Each LLM rubric response is checked before it is checkpointed: it must have a `rubricId`,
  yes/no decision thresholds, and at least one evidence rule. An incomplete response is retried
  (3 attempts); if all fail, the run stops and nothing is saved for that condition.
- An incomplete template found in an older checkpoint is regenerated instead of reused, and
  `useExistingRubric = TRUE` does not skip a condition whose latest concise rubric has no rules.

To skip rubric generation when final rubric artifacts already exist, set:

```r
getRubrics(
  conditionList = conditions,
  outputDirectory = "P:/llm/rubrics",
  useExistingRubric = TRUE
)
```

The same option is available in `wf$createRubrics(...)`, `runIthacaWorkflow(...)`, and `runIthacaExample(...)`.

If you need to clear resume state before rerunning:

```r
# Remove checkpoint only (keep generated rubric artifacts)
resetRubricCheckpoint(
  conditionName = "Gastroesophageal reflux disease",
  outputDirectory = "P:/llm/rubrics",
  deleteArtifacts = FALSE
)

# Remove checkpoint and rubric artifacts (force full rebuild)
resetRubricCheckpoint(
  conditionName = "Gastroesophageal reflux disease",
  outputDirectory = "P:/llm/rubrics",
  deleteArtifacts = TRUE
)
```

## Legacy-compatible callable entrypoints

These wrappers exist for compatibility with older script names:

- `getRubrics(...)`
- `createRubricConceptSets(...)`
- `adjustRubric(...)`
- `scoreSubjects(...)`
- `deleteConceptSetFromLibrary(...)`

Use these if you want to keep older calling patterns.

## Concept set library location behavior

`createRubricConceptSets(...)` treats `seedLibraryPath` as the canonical concept set
library location for both reads and writes:

- Matching reads from the canonical DuckDB library at `seedLibraryPath`.
- Each concept-set create/update writes immediately as a new `libraryVersion` row revision.
- `outputDirectory` still stores condition-specific artifacts (for example
  `matchedConceptSets.rds` and match diagnostics).

Pass `seedLibraryPath` as either:

- a direct DuckDB file path (for example `P:/llm/rubrics/rubricConceptSetLibrary.duckdb`), or
- a directory (for example `P:/llm/rubrics`), which resolves to `rubricConceptSetLibrary.duckdb`.

## Edit rubric by editing the concise CSV

You can now edit the concise rubric CSV directly, save it, and apply those edits back
into the workflow artifacts.

Supported edits:
- Update row values (except SQL text; SQL is managed by the app)
- Add rows (leave `ruleId` blank; the next ID for the row's type is assigned automatically)
- Delete rows

Rule IDs are owned by the app. A `ruleId` that is not in the current rubric (a renamed or
typed-in ID) stops the apply with an error, so renaming a rule is not possible. Changing a
row's `type` gives it a new ID (e.g. `AE4` becomes the next `HE#`). New IDs are always above
every ID used in any saved concise rubric version, so a removed rule's ID is never reused.
Rules listed in the condition's `adjustableRuleId` cannot be removed or retyped, and a criteria
change to one prints a warning.

When applying edits, the app validates required columns/completeness, regenerates SQL
for changed/new rows, updates `shortRubric` artifacts, updates `rubricList` needed concept
sets, and optionally regenerates summary text.

```r
applyConciseRubricEdits(
  conditionName = "Gastroesophageal reflux disease",
  outputDirectory = "P:/llm/rubrics",
  editedConciseRubricPath = "P:/llm/rubrics/Gastroesophageal reflux disease/Gastroesophageal reflux disease_conciseRubric_EDIT.csv",
  updateSummary = TRUE,
  verbose = TRUE
)
```

To get a critical LLM review of a concise rubric before editing, use `reviewConciseRubric(...)`.
It assesses clinical validity, weighting/thresholds, consistency with the `dxCriteria` and
`additionalInformation` used at creation (both resolved from `config.yaml` exactly as during
rubric creation: the condition CSV, falling back to `clinicalDefinitionFile`), and
alternative-explanation vs hard-exclusion classification. Each finding carries clinical
evidence recalled from the LLM's training data (verify citations before relying on them).
Findings are returned and saved as `<condition>_conciseRubricReview_V*.csv`.

The fixed diagnosis rules ER1 and ER2 are excluded from review and are never emitted as
review findings. Multiple Myeloma may intentionally have an empty `neededConceptSets` value
for a rule; concise-rubric validation permits that exception only for Multiple Myeloma.

```r
review <- reviewConciseRubric("Major Depressive Disorder")
```

To check that every concept set referenced (`@name`) in each rule's SQL is listed in that
rule's `neededConceptSets`, for all conditions in the config (or selected ones):

```r
mismatches <- checkSqlConceptSetsInNeededConceptsFromConfig()
mismatches <- checkSqlConceptSetsInNeededConceptsFromConfig("Confusion", addMissing = FALSE)
```

It returns one row per mismatched rule (`conditionName`, `rubric`, `ruleId`,
`missingConceptSets`, `neededConceptSets`); zero rows means all rules are consistent.
By default (`addMissing = TRUE`) the missing names are appended to the rule's
`neededConceptSets` in new short rubric, concise rubric CSV, and `rubricList.rds` versions
(SQL is unchanged). Rerun `createRubricConceptSetsFromConfig.R` afterwards so the added
concept sets are matched before scoring. Use `addMissing = FALSE` to only report.
Rubric creation (`getRubrics`/`getRubricsFromConfig.R`) applies the same correction
automatically after generating each rule's SQL.

To clean malformed concept-set names already saved in the rubrics (for example
`bile_duct_procedure_procedure_codes` to `bile_duct_procedure_codes`) for all configured
conditions:

```r
cleanRubricConceptSetNamesFromConfig(dryRun = TRUE) # preview
cleanRubricConceptSetNamesFromConfig()
```

Names are rewritten in `neededConceptSets`, SQL `@` references, and `rubricList.rds`, with new
short rubric, concise rubric CSV, and `rubricList.rds` versions saved. Conditions whose concise
rubric CSV has unapplied edits are skipped. Rerun `createRubricConceptSetsFromConfig.R` afterwards.

`outputDirectory` defaults to the config's `outputDirectory`; pass it explicitly to override.
The review prompt is `inst/prompts/reviewConciseRubric.txt` (override with `promptDir`, or the
config's `promptDir`).

LLM reviews vary from run to run, so each rubric is reviewed `reviewRuns` times (config
`reviewRuns`, default 3; override with `reviewConciseRubric(..., reviewRuns = 1)`). A further
LLM call (`inst/prompts/consolidateConciseRubricReview.txt`) merges the runs into one review:
duplicate findings are merged, invalid ones are dropped with a reason, and runs that disagree on
the fix become one finding with options, written as one row per option (shared `optionGroup`, labelled in `option`). `timesFlagged` (e.g. `3/3`)
is computed from each finding's `sourceFindingIds` and shows how many runs raised it. Rows are
sorted by rubric, then rule group (`GLOBAL`, `HE`, `AE`, `ER`), then rule number (`ER2` before
`ER10`), then `timesFlagged` (most first) and severity. The individual run findings are saved
as `<condition>_conciseRubricReviewRuns_V*.csv` with the same version number, with `droppedReason` filled
for findings the consolidation dropped or did not account for. Cost: `reviewRuns + 1` LLM calls
per rubric.

To review every condition in `config.yaml` (conditions without a concise rubric are skipped):

```r
source("reviewConciseRubricsFromConfig.R")
```

or, for selected conditions (names are case-insensitive; `config.yaml` is used unless you pass
`configPath`):

```r
reviewConciseRubricsFromConfig("Major Depressive Disorder")
reviewConciseRubricsFromConfig(c("Major Depressive Disorder", "Seizure"))
applyConciseRubricReviewsFromConfig("Major Depressive Disorder")
```

To apply the review, mark rows in the latest `<condition>_conciseRubricReview_V*.csv`:

- `Accept`: `Y`, `YES`, `TRUE`, `X`, or `1` to apply; blank, `N`, `NO`, or `FALSE` to skip.
- `newCriteria`: optional amendment; when blank, `suggestedCriteria` is used.

For reference, `originalCriteria` and `originalScore` show the cited rule's current criteria and
score (blank for `GLOBAL` findings).

Then run `applyConciseRubricReview("Major Depressive Disorder")` (or pass several names, e.g.
`applyConciseRubricReview(c("Major Depressive Disorder", "Seizure"))`, which returns a
per-condition status table), or for all conditions:

```r
source("applyConciseRubricReviewsFromConfig.R")
```

To apply both pending review findings and hand edits to the concise rubric CSV in one step:

```r
applyPendingRubricChangesFromConfig()                            # all conditions
applyPendingRubricChangesFromConfig("Major Depressive Disorder")
```

It first applies accepted, not-yet-applied review rows, then applies any remaining differences
between the latest concise rubric CSV and the short rubric. It returns `reviewStatus` and
`conciseStatus` per condition. After a review is applied, its accepted rows get an `applied`
value (the concise rubric file they produced) so later runs do not apply them again.

Rules: each accepted row must cite one rule (or `GLOBAL` to add a new rule with
`suggestedType` + criteria) and its `originalCriteria` must still match the latest concise
rubric. Type changes give the rule a new ID (e.g. `AE4` to the next `HE#`). Hard exclusions always
score -100; alternative explanations need a negative and evidence rules a positive
`suggestedScore`. Set `suggestedType` to `remove` (also accepted: `delete`), or write `remove`
in `newCriteria`, to delete the cited rule; removal cannot be combined with another change to the
same rule, and each rubric must keep at least one evidence rule. Removed rule IDs are never
reused, in this apply or later ones. Only one change per rule can be accepted in one apply: every
finding on the same rule (and every option of a finding) is written as one fully
structured row (same `optionGroup`, labelled in `option`, e.g. "Option 2 of 3"), each with
its own `recommendation`, `suggestedType`, `suggestedScore`, and `suggestedCriteria`; mark `Accept`
on exactly one row of a group (accepting two is an issue; rerun the review for further changes to the same rule). Ungrouped rows whose recommendation text
offers options (e.g. "move to alternativeExplanations or remove"), and options with no type, score,
or criteria, are an issue
until you write the chosen option in `newCriteria` (complete criteria text, or `remove`) and set
`suggestedType`/`suggestedScore` to match. Because the LLM does not always give structured options, the
recommendation text of ungrouped rows is checked: "or" followed by an action ("collapse ... or enforce ..."),
"either (a) ...", "alternatively", and "at minimum" count as options, while clinical "or"
("inpatient or emergency department visit") does not. A GLOBAL row that
discusses existing rules (e.g. "collapse ER7 and ER8") cannot be applied as-is; change its
`ruleId` to the one rule to change and write that rule's new criteria (or `remove`) in
`newCriteria`, using one row per rule (blank `originalCriteria` is allowed for such a re-targeted
row). Review files created before the `optionGroup`
column existed cannot be applied; re-run the review. Changes are all-or-nothing: on success new concise rubric, short rubric,
rubric list, and summary versions are written (via `applyConciseRubricEdits(...)`); otherwise
nothing changes, the console prints "no changes were made, there were issues with the
reviewed changes", and an `issues` column is written into the review file (ignored and
rewritten on the next pass).

To apply edits for every condition in `config.yaml`, edit the latest
`<condition>_conciseRubric_V*.csv` in place and run:

```r
source("applyConciseRubricEditsFromConfig.R")
```

Each condition's latest concise rubric is compared with its latest short rubric (SQL is
ignored), and `applyConciseRubricEdits(...)` runs only where they differ. The rule summary
(an LLM call) is regenerated only when rules, criteria, or needed concept sets changed.
When criteria changes, the default `refreshConceptSets = TRUE` asks the LLM to re-derive
the needed concept-set names before SQL regeneration (also for new rows and for rows whose
concept-set types conflict with the criteria); if inference fails, the apply stops with an error.

Every apply (from edits or from accepted review rows) also writes
`<condition>_rubricDelta_V#.csv` next to the new concise rubric (same version number). It lists
what changed from the previous version, one row per changed field: `condition`, `rubric`,
`ruleId`, `previousRuleId` (set when a type change renumbered the rule), `changeType`
(`modified`, `retyped`, `added`, `deleted`), `field` (`criteria`, `rationale`, `score`, `type`,
`neededConceptSets`, `sqlStatement`, `ruleId`, or `rubricYesScore`/`rubricNoScore` with ruleId
`(rubric)`), `oldValue`, and `newValue`. No file is written when nothing changed.

Concept-set names are checked wherever they enter a rubric: LLM rubric generation, edited
concise rubrics, LLM-inferred concept sets, and concept-set creation. A name must be one
token of letters, digits, and underscores, ending in a controlled suffix (`DIAGNOSIS_CODES`,
`PROCEDURE_CODES`, `MEDICATION_CODES`, `VISIT_CODES`, `MEASUREMENT_CODES`, `DEVICE_CODES`,
`PROVIDER_CODES`). A malformed name such as `p pulmonology_PROVIDER_CODES` is repaired
automatically when exactly one valid name from the rule's SQL or the rest of the rubric
appears inside it; each repair is printed. Otherwise:

- generation retries the LLM (up to `rubricResponseAttempts`);
- edits stop without saving and list each bad name by rule;
- concept-set creation stops.

Names already in the current rubric are exempt from the suffix rule, so older rubrics can
still be edited, and concept-set creation checks only the name format.
The returned table lists each condition as `unchanged`, `applied`, `skipped`, or `error`
(an error in one condition does not stop the others). Reading `config.yaml` requires
`DATABRICKS_TOKEN`, `DATABRICKS_HOST`, and `DATABRICKS_HTTP_PATH` to be set (they are
validated, not used to connect).

## Cohort generation conventions

Use the package helpers and keep this shape for cohort definitions:

- `cohortId`
- `cohortName`
- `sql`

Example:

```r
defs <- ithMakeCohortDefinitionSet(
  cohortIds = c(14412),
  cohortNames = c("gerdIndex"),
  cohortSql = c("SELECT ...")
)

generateCohorts(
  cohortDefinitionSet = defs,
  dbList = databases,
  stopOnError = TRUE,
  incremental = TRUE
)

# or fetch definitions directly from WebAPI
generateCohorts(
  cohortDefinitionSet = NULL,
  dbList = databases,
  cohortIds = c(14412),
  webapiBaseUrl = "https://your-ohdsi-server/WebAPI",
  stopOnError = TRUE,
  incremental = TRUE
)
```

Note: `incrementalFolder` is deprecated in CohortGenerator and is intentionally not used.

After every cohort generation (`generateCohorts(...)`, `buildRandomOmopCohort(...)` for the
source, sampling-pool and random cohorts, and the phenotype cohort generated by
`testPhenotypeAlgorithmRubric(...)`), the number of distinct subjects in each generated cohort is
queried and printed, for example
`Generated cohort [db1]: cohortId=10616001, subjects=1000 (cohort rows=1000)`. A cohort with zero
subjects is reported as `subjects=0`. If the count query fails, a warning is issued and the
workflow continues. `buildRandomOmopCohort(...)` also returns the counts as `cohortCounts`.

### Choosing the cohort table for scoring

Use `cohortTableName` when calling scoring to ensure reads and scoring SQL both target the intended cohort table:

```r
scoreSubjects(
  conditionList = conditions,
  database = databases,
  outputDirectory = "P:/llm/rubrics",
  conceptLibraryVersion = 142,
  cohortTableName = "pv_table_3",
  generateCohort = FALSE
)
```

The same argument is available in `wf$scoreSubjects(...)`, `runIthacaWorkflow(...)`, and `runIthacaExample(...)`.

When databases share a schema and require distinct cohort tables, pass a per-database mapping:

```r
scoreSubjects(
  conditionList = conditions,
  database = databases,
  outputDirectory = "P:/llm/rubrics",
  conceptLibraryVersion = 142,
  cohortTableName = c(
    CCAE = "pv_table_ccae",
    MDCR = "pv_table_mdcr",
    MDCD = "pv_table_mdcd"
  ),
  generateCohort = FALSE
)
```

Names must match each database's `databaseId`.

Before any rule is scored, scoring checks that each condition's `randomCohortDefinitionId` has rows in
the resolved cohort table on every database. If one is missing (for example, the random cohort was not
built yet), scoring stops and lists each missing condition and database instead of saving all-zero
scores. Build the cohort with `buildRandomOmopCohortFromConfig()` (or score with `generateCohort = TRUE`),
or use the conditionSpecs `ignore` column to score the other conditions. The check is skipped with
`useExisting = TRUE`.

### Running scoring queries in parallel

Set `general.parallelWorkers.scoring` in `config.yaml` (or `scoringWorkers` in `scoreSubjects()`)
above 1 to run the rubric rule queries on that many worker processes:

```yaml
general:
  parallelWorkers:
    scoring: 4    # 1 (default) = one query at a time in this R session
```

The workers start once per scoring run and are shared by every condition and database. Each opens
its own database connection, so the warehouse must allow that many concurrent queries. A query that
fails is corrected by the LLM and retried in the main session, as in serial runs, and the saved
scoring files are the same as a serial run's. Each run saves the query time for every rule in
`scoring/<condition>/<databaseId>/ruleQueryTimes.csv`; the next run starts the slowest rules first
so they do not finish last (with no saved times, AE rules go first). Workers write the query they are
running to `lastQuery/query_worker<pid>.sql`. In a probe of 21 rules on Ccae, 4 workers took 147 s
compared with 372 s serially. `extras/parallelScoringProbe.R` times a sample of rules without saving
anything.

### Generating cohorts in parallel

Set `general.parallelWorkers.cohortGeneration` in `config.yaml` above 1 to generate cohorts on that
many worker processes. It applies to every cohort generation job: the random cohorts
(`buildRandomOmopCohortFromConfig`, one job per condition and database), phenotype cohorts for the
phenotype report, cohorts generated before scoring (`generateCohorts(workers = )`,
`scoreSubjects(cohortWorkers = )`), and the pipeline's cohort step. For these, each cohort in each
database is its own job, so many cohorts in one database also run in parallel (a definition set with
subset cohorts is kept together).

```yaml
general:
  parallelWorkers:
    cohortGeneration: 5    # 1 = one job at a time in this R session
```

Each worker opens its own connection, so the warehouse must allow that many concurrent jobs. No
cohort ID may be shared by two jobs writing the same cohort table. Before any random cohort is built,
the run checks the condition specs and stops with a message naming each conflicting cohort ID,
database and conditions: a random cohort ID used by two conditions, a random ID that is also a source
or pool ID, or (with more than one worker) a source or pool cohort shared by two conditions. The cohort tables are created once
before the workers start. With more than one worker a failed job does not stop the others; the run
stops with an error naming each failed condition and database once all jobs end (the pipeline records
each unit as it finishes). Set the value back to 1 to run jobs one at a time. In a probe of 6 random
cohort builds (3 conditions on 2 databases), 5 workers took about 290 s compared with 790 s serially.
`extras/parallelCohortProbe.R` runs that comparison in separate scratch tables and drops them.
Phenotype cohorts generated through `testPhenotypeAlgorithmRubric(generateTestingPhenotypeFirst = TRUE)`
are still generated one at a time.

### Choosing the concept set library for scoring

Use `seedLibraryPath` when scoring if you want to load concept sets from a specific
DuckDB library path. By default scoring uses the newest library version; pass
`conceptLibraryVersion` only to pin an earlier snapshot:

```r
scoreSubjects(
  conditionList = conditions,
  database = databases,
  outputDirectory = "P:/llm/rubrics",
  seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  conceptLibraryVersion = 142,
  generateCohort = FALSE
)
```

`seedLibraryPath` can be either:

- a file path to one `.duckdb` concept set library, or
- a directory containing `rubricConceptSetLibrary.duckdb`.

`conceptLibraryVersion` is optional. When it is `NULL`, blank, or `"latest"`, scoring resolves the
newest library version once at the start of the run, so all conditions and databases share one
snapshot. It prints the version used and records it in each `pluginResolution_Cohort*_V#.csv`
(`conceptLibraryVersion` column). Concept sets created by `createRubricConceptSetsFromConfig.R` are
therefore picked up automatically. Set an integer to reproduce an earlier run; while pinned, newly
created concept sets are not visible until you change or blank it.

Before any cohort generation or database work, scoring checks every condition: it needs a
`matchedConceptSets_V*.rds` that covers every concept set in the latest short rubric's SQL.
If a condition fails this check, the run stops and lists the problems. Without it, a missing
concept set would quietly be replaced by a placeholder concept, and those rules would score
nobody. Run `createRubricConceptSetsFromConfig.R` for the listed conditions (also needed after a
rubric edit adds new concept sets), or mark them `ignore` in conditionSpecs to score the rest now.
It also reports, per rule, any concept set a rule's SQL uses that the rule's `neededConceptSets` does
not list; this happens when SQL generation invents a name. Rerunning concept-set creation cannot
fix that: correct the rule and run `applyConciseRubricEdits` to regenerate its SQL, or add the
names to `neededConceptSets`.

The same argument is available in `wf$scoreSubjects(...)`, `runIthacaWorkflow(...)`, and `runIthacaExample(...)`.

### Restricting random-cohort sampling pool

`buildRandomOmopCohort(...)` supports `randomSampleFromCohortDefinitionId`.

- When provided, that cohort is fetched from WebAPI and generated at runtime, and random subjects are sampled from its subject pool first.
- When `NULL`, random subjects are sampled from all eligible persons.
- `seedLibraryPath` can now be passed explicitly so the run records the concept-set seed library path used downstream.
- The function can be called with either explicit connection/schema/table args or a single `database` settings object.

```r
buildRandomOmopCohort(
  connectionDetails = CCAE_OHDA_DBSpec$connectionDetails,
  cdm_database_schema = CCAE_OHDA_DBSpec$cdmDatabaseSchema,
  cohort_database_schema = CCAE_OHDA_DBSpec$workDatabaseSchema,
  cohortTableName = "pv_table_3",
  cohort_definition_id = 26553001,
  sourceCohortDefinitionId = 26553,
  randomSampleFromCohortDefinitionId = 26553,
  webapiBaseUrl = "https://epi.jnj.com:8443/WebAPI",
  seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb"
)

# Unified database-object call
buildRandomOmopCohort(
  database = CCAE_OHDA_DBSpec,
  sourceCohortDefinitionId = 26553,
  randomSampleFromCohortDefinitionId = 26553,
  webapiBaseUrl = "https://epi.jnj.com:8443/WebAPI",
  seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb"
)
```

You can also run random cohort generation directly from YAML config/database mappings:

```r
out <- buildRandomOmopCohortFromConfig("config.yaml")
```

`buildRandomOmopCohortFromConfig(...)` now reads source cohort settings per condition from `general.conditionSpecs` (or `conditionSpecsCsvPath`) and returns results grouped by condition and then database.

To score directly against random cohort IDs from config, use:

```r
scoreOut <- scoreSubjectsAgainstRandomCohortFromConfig("config.yaml")
```

This helper uses each condition's explicit `randomCohortDefinitionId`.

To append each rule's concise-rubric `criteria` and `rationale` to the latest
full scoring artifact for every configured condition and database, use:

```r
combined <- combineFullScoringFilesFromConfig(
  configPath = "config.yaml",
  outputDirectory = "P:/llm/rubrics/combinedScoring"
)
```

This writes `<condition>_<database>_fullScoringFile_Cohort<id>_V#.csv` directly
under the selected output directory. With `outputDirectory = NULL`, the
configured output directory is used.

To count subjects by the combination of rules they met, pooled across databases, use:

```r
summarizeRuleCombinationsFromConfig("config.yaml", topN = 100)
```

This writes `<condition>_ruleCombinations_Cohort<id>_V#.csv` with `Rank`, `Combination`,
`Rule Count`, `Total Score` (sum of the combination's rule scores), pooled `Subject Count` and `Proportion Of Subjects`, a `<db> Subject Count` and
`<db> Proportion Of Subjects` pair per database (proportions rounded to 3 places), and the rules'
`Criteria`. Combinations are ranked on the pooled counts. The last row, `(none)`, counts subjects
with no rule hits and is not part of `topN`.

`conceptLibraryVersion` is optional; leave it `null` to score against the newest library version.

Call flow note: scoring runs through `ithScoreSubjects(...)` -> `ithScoreConditionDatabase(...)`; random cohort generation (`buildRandomOmopCohortFromConfig(...)`) is a separate workflow and does not directly invoke scoring.

This uses configured database mappings and automatically derives `cohortTableName` as:

- `cohortTablePrefix + "_" + cdm_schema_name` (normalized to safe characters).

### WebAPI-first cohort definition retrieval

When cohort SQL is not supplied manually, use WebAPI retrieval:

```r
defs <- ithGetWebapiCohortDefinitionSet(
  cohortIds = c(14412, 26187),
  webapiBaseUrl = "https://your-ohdsi-server/WebAPI"
)
```

You can pass this object to `generateCohorts(...)`, or skip it and provide
`cohortIds + webapiBaseUrl` directly.

## Concept set library behavior

- The pipeline maintains a canonical DuckDB concept set library (`rubricConceptSetLibrary.duckdb`).
- Concept set writes are immediate (per-set upserts), each creating a new numeric `libraryVersion`.
- During scoring, concept-set resolution now runs a Circe SQL expansion path first and only falls back to `RubricConcepts::resolveConceptSet(...)` if needed.
- If `RubricConcepts` is unavailable at scoring time, scoring can still resolve concept sets through the Circe path.

### Updating one concept set from JSON

Use `updateConceptSetLibraryFromJson(...)` to upsert a single concept set into an
existing DuckDB library.

```r
updateConceptSetLibraryFromJson(
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  csName = "immunomodulator_medication_codes",
  conceptSetJsonPath = "P:/llm/largeScalePhenotyping/Crohns disease/immunomodulator_medication_codes/immunomodulator medication codes.json"
)
```

Behavior:

- Resolves the DuckDB library file path.
- Loads the JSON concept set, upserts by `csName`, and refreshes embedding metadata.
- Persists provenance (`runId`, user, timestamp, condition, cohort, source file).
- Appends a new `libraryVersion` snapshot for reproducible scoring pins.

### Normalizing entire concept set library format

Use `normalizeConceptSetLibraryStorage(...)` to rewrite all concept-set payloads
to a canonical `list(items = ...)` shape.

```r
normalizeConceptSetLibraryStorage(
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  replaceUnreadableWithEmpty = TRUE
)
```

This appends new normalized concept-set revisions in DuckDB and returns counts of normalized and
replaced unreadable rows.
- Needed concept sets are matched by embedding similarity.
- Ambiguous matches can be adjudicated by LLM over top embedding candidates (default enabled in callable workflow helpers).
- Missing concept sets are generated and added to the library.
- Condition-specific matches are saved per condition directory.

### LLM adjudication for ambiguous concept-set matches

`createRubricConceptSets(...)` and workflow wrappers support a retrieval+adjudication pattern:

- Retrieve top candidates from embedding similarity.
- Trigger LLM adjudication only when ambiguous (gray-zone top score and/or small top-1 vs top-2 gap).
- Fall back to cosine/fuzzy-only matching when LLM is unavailable or low-confidence.

Common knobs:

- `useLlmAdjudication` (default `TRUE`)
- `llmTopCandidates` (default `25`)
- `llmTop2GapThreshold` (default `0.015`)
- `llmConfidenceThreshold` (default `0.60`)

Adjudication rationale/decision is logged in concept-set matching diagnostics.

Prompt source: `inst/prompts/conceptSetMatchAdjudication.txt`.

### One-time alias migration for concept set naming drift

If historical library names differ from newer rubric names, you can run a one-time alias migration.
This duplicates concept sets by alias name, copies JSON from the source concept set, recomputes embeddings,
and appends alias rows to the DuckDB library with provenance.

```r
# Preview changes only
migrateConceptSetLibraryAliases(
  outputDirectory = "P:/llm/rubrics",
  conceptLibraryFile = "rubricConceptSetLibrary.duckdb",
  dryRun = TRUE
)

# Apply migration
migrateConceptSetLibraryAliases(
  outputDirectory = "P:/llm/rubrics",
  conceptLibraryFile = "rubricConceptSetLibrary.duckdb",
  dryRun = FALSE
)
```

## SQL correction behavior

During scoring, generated SQL is retried with correction prompts when SQL errors are encountered.
This preserves existing behavior from the legacy scripts while making it callable and modular.

## Concept-set readability self-heal

During `createRubricConceptSets(...)`, when a concept set is matched from the
library but its JSON cannot be prepared for resolution, Ithaca now regenerates
that concept set and replaces the stored library JSON for the matched name.
This event is printed in console output.

If `RubricConcepts::createConceptSet(...)` errors at runtime (for example,
dependency DLL/load issues), Ithaca now falls back to a placeholder concept
set with conceptId `36433327` so concept-set creation can continue.

## Post-run scoring summary

After scoring completes, you can generate a compact summary from saved scoring artifacts:

```r
summaryResult <- summarizeScoringResults(
  conditionName = "Gastroesophageal reflux disease",
  databaseId = "CCAE",
  randomCohortDefinitionId = 14412,
  outputDirectory = "P:/llm/rubricsNew",
  topN = 10,
  saveSummary = TRUE
)
```

When `saveSummary = TRUE`, versioned summary files are written in the same scoring folder
as the cohort scoring outputs.

`scoreSubjects(...)` now runs this summary step automatically at the end of each
condition/database scoring job and writes these same summary files.

### Backfilling legacy scoring case labels

Use `backfillScoringCaseLabels(...)` when an older scoring run has correct
`total` values but incorrect case labels due to rubric threshold schema drift.
The function recomputes `case` labels in both `sumCohortScores` and
`finalScores`, then writes new versioned artifacts.

```r
backfillScoringCaseLabels(
  conditionName = "Confusion",
  databaseId = "Optum DOD",
  randomCohortDefinitionId = 26165001,
  outputDirectory = "P:/llm/largeScalePhenotyping",
  saveSummary = TRUE
)
```

When `saveSummary = TRUE`, the latest `scoringResultsSummary` and
`scoringResultsOverview` files are regenerated from the corrected artifacts.

After editing rule scores and/or yes/no thresholds with `applyConciseRubricEdits(...)`,
add `recomputeFromRuleScores = TRUE` to re-sum totals from the latest `fullScoringFile`
using the latest short rubric scores, without re-querying the databases. New
`fullScoringFile` and `subjectCount` versions carrying the updated scores are saved too.
This stops with an error if rules were added or removed or any rule's criteria changed;
rerun scoring in that case. For every condition and database in `config.yaml`:

```r
source("backfillScoringCaseLabelsFromConfig.R")
```

The script passes `skipIfUnchanged = TRUE`, so condition/database pairs whose rule scores,
thresholds, totals, and case labels already match the latest saved artifacts are skipped
and no new versions are written.

### Standalone scoring from fullScoringFile

Use `scoreFullScoringFileAdjusted(...)` to recompute case labels directly from a saved
`fullScoringFile_*.rds` artifact.

```r
scoreResult <- scoreFullScoringFileAdjusted(
  fullScoringFile = "P:/llm/rubrics/scoring/GERD/CCAE/fullScoringFile_Cohort14412_V1.rds",
  excludeNegativeScores = TRUE,
  yesScoreOverride = 1,
  noScoreOverride = 0,
  saveAdjustedOutput = TRUE
)
```

Notes:

- Set `excludeNegativeScores = TRUE` to ignore rows where `score < 0`, except the fixed `AE1`
  zero-occurrence penalty, which always applies.
- `yesScoreOverride` and `noScoreOverride` can be one value for all rubrics,
  or vectors keyed by `rubricId`.
- If overrides are omitted, the function looks for `yesScore`/`noScore` columns
  in the input file.
- Input resolution uses the latest-version lookup for the supplied
  `fullScoringFile` basename.
- Adjusted outputs are saved with names that include `Adjusted`
  (for example `sumCohortScoresAdjusted_*` and `finalScoresAdjusted_*`).

## Ported rubric-evaluation functionality

Ithaca now includes functionality ported from `P:/skills/rubric-evaluation`:

- `buildRandomOmopCohort(...)`
- `summarizeCohortOverlaps(...)`
- `summarizeCohortOverlapsFromTibble(...)`
- `testPhenotypeAlgorithmRubric(...)`
- `testPhenotypeAlgorithmRubricReport(...)`

These functions support random-cohort construction and rubric-vs-phenotype testing workflows,
including optional cohort generation from WebAPI.

When running `testPhenotypeAlgorithmRubric(...)` or
`testPhenotypeAlgorithmRubricReport(...)` with `webapiBaseUrl` provided,
the output `phenotype` label is automatically populated from the WebAPI cohort
definition name for each `phenotypeCohortId` when available.

Both testing functions now accept `caseZeroAsCase` (default `FALSE`).
`caseZeroAsNonCase` is always generated; set `caseZeroAsCase = TRUE` to also
produce `caseZeroAsCase` results.

Both testing functions also accept `fullScoringFile` and `excludeNegativeScores`
for single-rubric adjusted scoring. When `fullScoringFile` is supplied, rule scores
are re-summed from that file using `excludeNegativeScores` and the yes/no thresholds.
Each threshold can be set absolutely with `yesScoreOverride`/`noScoreOverride`
(e.g. `5` and `2`) or shifted from the current threshold with
`yesScoreAdjustment`/`noScoreAdjustment` (e.g. `-2` lowers it by 2); use one form
per threshold, not both. `evaluationCohortFile` must still be the
`sumCohortScores_*` artifact: it supplies the subject list and the current thresholds,
and subjects with no scoring rows are assigned a total of `0`.

```r
testPhenotypeAlgorithmRubricReport(
  ...,
  evaluationCohortFile = "<scoring dir>/sumCohortScores_Cohort<id>_V1.rds",
  fullScoringFile = "<scoring dir>/fullScoringFile_Cohort<id>_V1.rds",
  excludeNegativeScores = TRUE,
  yesScoreAdjustment = -2,
  noScoreAdjustment = -2
)
```

For config-driven runs, set the rules in `config.yaml` and source
`testPhenotypeAlgorithmRubricReportMultipleDatabases.R`, which calls
`testPhenotypeAlgorithmRubricReportFromConfig()` (or let `runIthacaPipeline()` run only the
reports that changed; see [Incremental pipeline](#incremental-pipeline)):

```yaml
run:
  phenotypeReportScoring:
    phenotypeReportScoringCsvPath: null # optional CSV; relative to general.outputDirectory
    default:
      analysisName: rubric_validation # report analysisName column and file-name part
      splayPrior: 7                 # required: days before the phenotype start date
      splayPost: 7                  # required: days after the phenotype start date
      sampleSubjectCount: 30        # subjects per TP/FP/TN/FN class
      frequencyPlotFpFnScores: false
      createSubjectSample: false
      classLabels: [FP, FN]
      excludeNegativeScores: false
      yesScoreOverride: null        # absolute, e.g. 5
      noScoreOverride: null         # absolute, e.g. 2
      yesScoreAdjustment: null      # added to current yes score, e.g. -2
      noScoreAdjustment: null       # added to current no score, e.g. -2
      dxRuleScoreOverride: null     # new points for the adjustableRuleId rule(s), e.g. 4
      dxRuleScoreAdjustment: null   # added to those rules' points, e.g. -1
    conditions:            # keyed by rubricName; null clears a default
      Rheumatoid arthritis:
        excludeNegativeScores: true
        yesScoreAdjustment: -2
        noScoreAdjustment: -2
```

For one or more scoring runs applied to every condition in `conditionSpecs`, set
`phenotypeReportScoringCsvPath` to a CSV such as
[`extras/phenotypeReportScoring_template.csv`](extras/phenotypeReportScoring_template.csv).
Each row must have a unique, non-empty `analysisName`; the other columns are optional and
blank cells use the YAML settings. `classLabels` uses semicolon-separated values such as
`FP;FN`. Extra columns are ignored.

When any rule is set for a condition, the script uses the latest `fullScoringFile`
for that condition/database; otherwise it uses `sumCohortScores` as before.
`createSubjectSample: true` on its own only reads the `fullScoringFile` for the
subject criteria report; it does not re-score subjects.
For each condition/database, the script reads `sumCohortScores` and (when needed)
`fullScoringFile` once, re-scores once when rules are set, and reuses the result for every
`phenotypeCohortId` on that condition's line in the CSV.
As soon as a condition finishes (all databases and phenotype cohorts), its sensitivity/PPV/
specificity/NPV report is saved to `<outputDirectory>/scoring/<condition>/` as
`<phenotype>_<analysisName>_phenotypeAlgorithmRubricReports_V#.csv` and `.rds` (for example
`Major_Depressive_Disorder_Default_scoring_phenotypeAlgorithmRubricReports_V1.csv`; spaces and
other non-filename characters become `_`), so it is available while later
conditions are still running.
`analysisName` (optional, default `rubric_validation`) fills the report's `analysisName`
column and is part of the supporting file names
(`<phenotype>_<analysisName>_cohort_<id>_subject_samples_V#.csv`); set it per condition to
keep different scoring variants apart.
`dxRuleScoreOverride` (absolute points, `>= 0`) or `dxRuleScoreAdjustment` (added to the
current points; the result must stay `>= 0`) re-scores the rule(s) listed in the condition's
`adjustableRuleId` column before the totals are summed; use one, not both. A blank
`adjustableRuleId` targets `ER1` (the fixed index diagnosis rule in generated rubrics) with a
console note; rule IDs never change once assigned (see the concise CSV edit section). Before scoring, the script checks each ID
against the latest concise rubric (it must exist and be an evidence rule) and prints the
rule's criteria and old -> new points. The report records `adjustableRuleId`,
`dxRuleScoreOverride`, and `dxRuleScoreAdjustment` columns. Each report row also has
`conditionName`, `testCondition`, and `reportStatus` (`ok`; `empty` when the phenotype cohort
has no analyzable results; or `error: <message>`). Rows that are not `ok` keep the identifying
columns and leave every metric `NA`, and the run continues with the next cohort.
Subject-sample and date-mismatch files are always written to the database scoring
directory. Frequency files are written only when `frequencyPlotFpFnScores: true`.
When `createSubjectSample: true`, the script also runs
`createSubjectSampleScoringReport()` using `classLabels`. Supporting files no longer
include timestamps; they use stable names with `_V1`, `_V2`, and later version suffixes.
The script also saves what was evaluated as new versions of
`sumCohortScoresAdjusted_Cohort<id>_V*.rds` and `finalScoresAdjusted_Cohort<id>_V*.rds`
in that database's scoring folder (nothing is written if the adjusted results are
unchanged), and records the path in the report's `adjustedSumCohortScoresFile` column.
The unadjusted `sumCohortScores` file is not modified; it stays the source of the subject
list and default thresholds. The report itself does not need these files, because it
re-sums from the `fullScoringFile` in memory. The re-sum stops with an error if no subjects
match between the two files (for example when `bit64` is not loaded for `integer64` IDs).

To expand FP/FN subject samples into subject-level rule/criteria scoring details,
use:

```r
createSubjectSampleScoringReport(
  subjectSampleFile = "P:/llm/rubrics/<run>_subject_samples.csv",
  fullScoringFile = "P:/llm/rubrics/scoring/<condition>/<database>/fullScoringFile_Cohort<id>_V*.rds",
  conciseRubricFile = "P:/llm/rubrics/<condition>/<condition>_conciseRubric_V*.csv",
  cohortId = 14412 # optional report override only; scoring artifacts are keyed by randomCohortDefinitionId
)
```

This writes a CSV with FP/FN subjects, matched score hits from `fullScoringFile`,
and translated rubric criteria text from the concise rubric. Subjects with no
score hits are retained with `NA` scoring fields.

## Incremental pipeline

`runIthacaPipeline()` runs the three config-driven jobs in order and, by default, only the
pieces whose inputs changed since they last ran successfully:

| Step | Unit | Same as | Reruns when |
|---|---|---|---|
| `cohort` | condition x database | `buildRandomOmopCohortMultipleDatabases.R` | database (CDM schema name, catalog, cohort table, `databaseVersion`), `randomCohort` settings or the condition's cohort IDs, the random-cohort SQL files, the source / sample-pool cohort definitions in WebAPI, or the Ithaca code this step uses changed |
| `score` | condition x database | `scoreSubjectsMultipleDatabases.R` | the evaluation cohort was rebuilt, or the short rubric, rubric list, concept-set matching, the library concept sets the rubric SQL uses, or the step's code changed |
| `report` | analysisName x condition x database x phenotype cohort | `testPhenotypeAlgorithmRubricReportMultipleDatabases.R` | the scores were recomputed, or the resolved `phenotypeReportScoring` settings (CSV row plus config), the phenotype cohort definition in WebAPI, the concise rubric (only with `createSubjectSample`), or the step's code changed |

A change only reruns what it affects: a new database or condition runs just its new units, an
edited phenotype cohort definition reruns just that cohort's reports, and a new analysis row
in the `phenotypeReportScoring` CSV runs just that analysis. Rubric and concept-set creation
are not part of the pipeline; run them first, and the pipeline picks up their new outputs.

```r
library(Ithaca)
runIthacaPipeline(dryRun = TRUE)                    # print the plan and the reason per unit
runIthacaPipeline()                                 # run what changed
runIthacaPipeline(fresh = TRUE)                     # rerun everything
runIthacaPipeline(fresh = "report", conditionNames = "suicidal ideation")
runIthacaPipeline(steps = "report", databaseIds = "Ccae")
```

- The ledger is `<outputDirectory>/pipeline/pipelineLedger.csv`, with one row per unit holding its
  fingerprint, the hash of each input, its status, and when it finished. Deleting a row forces that
  unit to run.
- A failed unit is retried on the next run, and the units downstream of it are reported as
  `blocked` until it succeeds.
- Code changes are tracked by hashing the Ithaca functions each step uses. Comment-only and
  formatting-only edits do not trigger reruns.
- The database version comes from the CDM schema name, for example `cdm_optum_extended_dod_v4155`.
  You can also set a version per database:

  ```yaml
  general:
    databaseVersion:
      Ccae: "2025q2"
  ```
- WebAPI cohort definitions are fetched once per run. If WebAPI cannot be reached, the last
  fetched hashes are reused, so an outage is not treated as a change.
- Reports are saved per unit under `pipeline/reportRows/`. After any report runs, the
  per-condition and combined report files are rebuilt from every unit's latest rows.
- To start using the pipeline on an output folder that already has results, run
  `runIthacaPipeline(adoptExisting = TRUE)` once. Units with existing outputs (scoring files,
  or report rows for that analysis, database and cohort) are recorded as done instead of
  rerun. A report is only adopted if it is newer than the scores it reads, so reports
  made from earlier scores still run. Wait until any script run that is still in progress
  finishes before adopting. Running `adoptExisting = TRUE` again is safe, because it only
  affects units that have no ledger row yet.

## Output artifacts

Typical artifacts include:

- `rubricList_*.rds`
- `<Condition>_shortRubric_*.rds`
- `<Condition>_conciseRubric_*.csv`
- `matchedConceptSets_*.rds`
- `subjectCount_Cohort*.csv`
- `fullScoringFile_Cohort*.rds`
- `sumCohortScores_Cohort*.rds`
- `finalScores_Cohort*.rds`
- `scoringResultsSummary_Cohort*.rds`
- `scoringResultsOverview_Cohort*.csv`

## Documentation and maintenance conventions

When changing package behavior:

1. Update roxygen docs for affected exported APIs (`@param`, `@return`, `@examples`, `@export`)
2. Keep `NAMESPACE` export/import declarations aligned
3. Update `README.md` usage examples when behavior/signatures change
4. Update `REFACTORING.md` for architecture/workflow changes

These conventions are also enforced in `.github/copilot-instructions.md` for future coding sessions.

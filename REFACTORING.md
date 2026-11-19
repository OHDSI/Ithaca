# Ithaca Refactor Summary (October 4, 2026)

## What was changed

This refactor introduces a modular, function-first architecture under `R/` and keeps the original scripts in `extras/original/R_orig/` as historical reference.

The package documentation model now follows the same compatibility pattern used in `Phenelope`:

- roxygen-style documentation blocks on public APIs,
- package-level import declarations in `R/Ithaca-package.R`,
- roxygen-style explicit `NAMESPACE` exports/imports,
- `DESCRIPTION` roxygen metadata (`Config/roxygen2/version`, `Roxygen: list(markdown = TRUE)`).
- comprehensive `@export` roxygen blocks for exported compatibility wrappers and workflow APIs.

Additionally, repository maintenance conventions are now formalized in
`.github/copilot-instructions.md`, including mandatory updates to `README.md`
and `REFACTORING.md` when user-facing behavior or workflow steps change.

### New code layout

- `R/ithacaUtils.R`
  - Prompt loading (`inst/prompts` first, then fallback)
  - Versioned save/load utilities
  - Concept-set string parsing helpers

- `R/ithacaLlmSql.R`
  - LLM client builders (`o3`, `gpt-4o`)
  - Structured query helper with retries
  - SQL creation and SQL correction prompt functions
  - Specialty-criteria SQL safeguard: post-generation validation enforces provider join + `specialty_concept_id` filter, with corrective retry when missing
  - Compatibility wrappers for legacy names (`queryLLM`, `createSqlStatement`, etc.)

- `R/ithacaCohorts.R`
  - Cohort generation utilities using CohortGenerator conventions:
    - `CohortGenerator::getCohortTableNames(cohortTable = ...)`
    - `CohortGenerator::createCohortTables(...)`
    - `CohortGenerator::generateCohortSet(...)`
  - `ithReportCohortCounts(...)` runs after every `generateCohortSet(...)` call (here, in
    `buildRandomOmopCohort(...)` and in `generateTestingPhenotypeCohort(...)`) and messages the
    distinct-subject count (plus row count) for each generated cohort id; failures only warn
  - Cohort definition retrieval from OHDSI WebAPI via `ROhdsiWebApi::exportCohortDefinitionSet(...)`
    when `cohortDefinitionSet` is not supplied.
  - Requires cohort definition data frame with columns:
    - `cohortId`
    - `cohortName`
    - `sql`
  - Defaults to `stopOnError = TRUE` and `incremental = TRUE`
  - Does not use `incrementalFolder` (deprecated in current CohortGenerator docs)

- `R/ithacaRubricCombinations.R`
  - `createRubricYesCombinationReport(conditionName, outputDirectory, maxCombinations, verbose)` (exported) reads the latest concise rubric, keeps positive-score evidence rules per rubric, and lists minimal sets reaching `rubricYesScore` via `ithMinimalYesCombinations(...)`: depth-first include/exclude over scores sorted largest first, pruned when the remaining points cannot reach YES; a set is recorded when the total reaches YES and the last (smallest) rule is needed, and the search does not extend past it, so no superset is listed. Stops above `maxCombinations` (default 100000). Categories come from `ithRubricRuleCategories(rubricList)` (`"<rubricId> <ruleId>"` -> category); rules missing there are `unknown`. Saved as `<condition>_yesRuleCombinations_V#.csv` with `ithSaveVersioned`. Called automatically through `ithRunYesCombinationReport(...)` (errors become warnings; off with `options(ithaca.yesCombinationReport = FALSE)`) from `ithCreateRubrics` after the concise rubric is saved, from `ithSaveShortRubric(..., yesCombinationReport = TRUE)` (adjustRubric, workflow edits, SQL fixes during scoring), and from `ithApplyConciseRubricEdits` after the rubricList is saved (it passes `yesCombinationReport = FALSE` to `ithSaveShortRubric` so categories come from the updated rubricList); review apply goes through `ithApplyConciseRubricEdits`

- `R/ithacaRubrics.R`
  - Rubric generation from condition specs + templates
  - Concept-set names collapse repeated adjacent tokens during rubric-response cleanup, with the scoring prompt carrying the same constraint.
  - Concise-rubric validation allows an empty `neededConceptSets` only for Multiple Myeloma; fixed diagnosis rules ER1 and ER2 are excluded from concise-rubric review.
  - Fixed rubric structure: `ithEnforceFixedRubricRules(rubricItem, conditionName, dxCriteria, dxAdditionalPoints)` runs on every LLM rubric response in `ithCreateRubrics` (before concept-set cleaning) and forces YES = 10 / NO = 2, ER1 = `>=1 <condition> diagnosis within -5 to +5 days of index` (3 points, `<condition>_DIAGNOSIS_CODES`), ER2 = the resolved `dxCriteria` text (`ithEr2Points(dxAdditionalPoints)` = 2 + additionalPoints, the diagnosis set plus the LLM's ER2 visit/provider sets, or visit sets inferred from the care settings named in dxCriteria). The prompt receives the same values through `<ER2_POINTS>` and `<DX_RULE_POINTS>` (ER1 + ER2). An LLM ER1/ER2 is replaced only when it is a diagnosis rule; other rules, including exact copies of the ER1/ER2 criteria, are dropped (copies) or kept and renumbered from ER3. Changes are printed. Other point values are prompt guidance only (`scoringRubric.txt` section 16). Edits after generation (CSV, review apply) are not checked against these rules
  - `general.dxCriteria` choices are blocks `{criteria, additionalPoints}` validated by `ithValidateDxCriteriaChoices(...)` (both required, no other entries, `additionalPoints` a whole number 0-`ithMaxDxAdditionalPoints` (4)); `ithResolveDxCriteria(...)` returns `list(choice, criteria, additionalPoints)` and `ithNormalizeConditionSpec(...)` stores `dxCriteriaChoice`, `dxCriteria` (criteria text), and `dxAdditionalPoints` on the spec. Specs passed directly in R (not via config) have no `dxAdditionalPoints`, which means 0. Criteria text carries no point guidance because it becomes ER2's criteria verbatim
  - `ithResolvePhenotypeReportScoring(...)` defaults a blank `adjustableRuleId` to `ER1` when a dx rule score setting is present (the edit/review protected-rule guard still uses only the explicit column)
  - Collection of needed concept sets
  - `cleanRubricConceptSetNamesFromConfig(...)` (`R/ithacaEntrypoints.R`) rewrites saved concept-set names to `ithNormalizeConceptSetName` form via `ithCleanRubricConceptSetNames(shortRubric, rubricList)` (`neededConceptSets`, SQL `@` references, `conceptSets_needed`; `neededConcepts` rebuilt), skipping conditions whose concise CSV differs from the short rubric (`ithDiffConciseRubric`); `dryRun = TRUE` only reports
  - `checkSqlConceptSetsInNeededConceptsFromConfig(...)` (`R/ithacaEntrypoints.R`) loops over the config conditions, loads each latest short rubric, and per rule reports SQL `@` concept sets (`ithScoringSqlConceptSets`) missing from `neededConceptSets` (compared via `ithNormalizeConceptSetName`); with `addMissing = TRUE` (default) it appends them to the short rubric `neededConceptSets` (saved via `ithSaveShortRubric`, which also writes the concise CSV) and to the matching rule's `conceptSets_needed` plus `neededConcepts` in a new `rubricList.rds` version; SQL and the rubric checkpoint are not changed. The comparison/append logic lives in `ithSyncSqlConceptSetsToNeeded(shortRubric, rubricList)` (`R/ithacaRubrics.R`), which appends names in `ithNormalizeConceptSetName` form (repeated tokens collapsed) and which `ithCreateRubrics` also calls right after the short rubric is built (before the `shortRubricBuilt` checkpoint and artifact saves), recollecting `neededConcepts` from the updated rubricList
  - Optional rubric-time concept set candidate hint pass (`useLibraryHints = TRUE`) that prefers existing library names but allows create-new choices
  - Saves candidate/decision audit artifacts per condition (`conceptSetCandidateHints_V*.csv`, `rubricConceptSetDecisions_V*.csv`)
  - Short-rubric generation with SQL for each rule
  - Rule-level editing and SQL regeneration support
  - `applyConciseRubricEdits(...)` writes edited `rubricYesScore`/`rubricNoScore` into the new `rubricList_V*.rds` `decisionThresholds` (scoring reads thresholds from `rubricList`, not the short rubric); conflicting yes/no scores within one rubric are an error
  - Rule IDs are system-owned: `ithAssignConciseRuleIds(...)` (called by `ithApplyConciseRubricEdits` before validation) normalizes `type`, rejects ruleIds not in the current short rubric and duplicate rules, and assigns IDs to blank-ruleId rows and retyped rows from a per-rubric, per-prefix high-water mark over the current short rubric, the edited CSV, and every saved `<condition>_conciseRubric_V*.csv`, so removed IDs are never reused. `ithBuildReviewedConciseRubric(...)` leaves new rows' ruleId blank and keeps the old ID on retyped rows so the same allocator numbers them (`ithNextReviewRuleId` was removed).
  - `reviewConciseRubric(...)` (`R/ithacaRubricReview.R`) reads the latest concise rubric CSV, runs the structured LLM review `reviewRuns` times per rubric (config `reviewRuns`, default 3) and, when more than one run, consolidates them with one more LLM call (`consolidateConciseRubricReview.txt`, which embeds the full review prompt so all output rules still apply); `timesFlagged` is computed in R from the consolidated `sourceFindingIds` rather than trusted from the LLM, and raw runs are saved as `<condition>_conciseRubricReviewRuns_V*.csv` (same version as the review). It returns/saves (`<condition>_conciseRubricReview_V*.csv`, first column `condition`) findings (prompt: `inst/prompts/reviewConciseRubric.txt`) in four categories (clinicalValidity, weighting, additionalInformation, ruleTypeClassification), each with LLM-recalled clinical evidence; `dxCriteria`/`additionalInformation` are resolved from `config.yaml` via `ithReadIthacaConditionConfig(...)` (the condition-spec half of `ithReadIthacaWorkflowConfig(...)`, which skips database validation) and `ithFindConfigConditionSpec(...)` (case-insensitive match on `rubricName` or `testCondition`); `reviewConciseRubricsFromConfig(...)` loops over all config conditions (script: `reviewConciseRubricsFromConfig.R`), skipping conditions without a concise rubric and returning a per-condition status table with combined findings in the `findings` attribute
  - `applyConciseRubricReview(...)` (`R/ithacaRubricReviewApply.R`) validates every accepted review row first (`ithPlanConciseRubricReview(...)`, no LLM calls), then builds the edited concise rubric in memory (`ithBuildReviewedConciseRubric(...)`; new GLOBAL rules get LLM-inferred concept sets; rules with `suggestedType = "remove"` are dropped last so their IDs are not reused in the same apply) and hands it to `ithApplyConciseRubricEdits(...)` via a temp CSV, so versioning, SQL regeneration, concept-set refresh, and rubricList updates follow the same path as CSV edits; non-type actions such as `remove` are registered in `ithReviewRuleActions` so future actions can be added in one place; the LLM returns extra options as `option2.../option3...` fields that `ithExpandReviewOptions(...)` splits into one structured row per option (shared `optionGroup`; the planner rejects accepting more than one row of a group), and `ithDetectReviewAlternatives(...)` (action verb after "or", "either (a)", "alternatively", "at minimum") backs it up for ungrouped rows in the apply planner; flagged rows and options with no suggested fields are blocked until the user writes the chosen action in `newCriteria`; GLOBAL rows without a new rule that mention existing rule IDs get a specific issue telling the user to re-target the row, and blank `originalCriteria` skips the staleness check for such re-targeted rows; review files without the `optionGroup` column are rejected (re-run the review); any issue (including an apply failure before files are saved) leaves artifacts untouched and writes an `issues` column into the review CSV. `applyConciseRubricReviewsFromConfig(...)` loops over config conditions (script: `applyConciseRubricReviewsFromConfig.R`); on success the accepted rows get an `applied` column value (the new concise rubric file name) and `ithPlanConciseRubricReview(...)` skips rows with `applied` set. `applyPendingRubricChangesFromConfig(...)` runs the review apply per condition and then, unless the review had issues or errors, `applyConciseRubricEdits(...)` when `ithDiffConciseRubric(...)` still finds differences
  - `applyConciseRubricEditsFromConfig(...)` (`R/ithacaRunConfig.R`) loops over the config conditions, uses `ithDiffConciseRubric(...)` to compare each latest concise rubric CSV with the latest short rubric (SQL ignored, whitespace/line-ending noise ignored), and calls `applyConciseRubricEdits(...)` only for conditions that differ; per-condition errors are returned in the status table
  - All config-driven functions read the config through `ithReadIthacaConditionConfig(...)`, which resolves `configPath` (default `"config.yaml"`) against the working directory, prints `Using config: <full path>`, and stops with a clear message (path tried, current working directory, and how to fix it) when the file is missing or is not valid YAML; `config.yaml` is expected at the project root
  - Intermediate checkpoint saves during rubric generation for restart/resume
  - `ithRubricResponseProblems(...)` validates each structured rubric response (rubricId, yes/no thresholds, >=1 evidence rule); `ithCreateRubrics(...)` retries incomplete responses (`rubricResponseAttempts`, default 3), regenerates invalid checkpointed templates (discarding seeded short-rubric SQL), refuses to save an empty short rubric, and `ithLatestConciseRubricHasRules(...)` stops `useExistingRubric` from skipping empty artifacts. `ithQueryLlm(...)` only retries on errors, so a parsed-but-empty structured response must be caught by the caller
  - Optional skip behavior when rubric artifacts already exist (`useExistingRubric = TRUE`)

- `R/ithacaConceptsScoring.R`
  - Concept-set library bootstrap from seed library
  - Matching needed concept sets to existing library via embeddings
  - Creation of missing concept sets and storage in a versioned library
  - `ithSaveVersioned(...)` writes `_V<highest existing + 1>` (case-insensitive match on `_V#`), so a deleted version is never reused (V1, V2, V4, V5 -> V6)
  - If a matched concept set exists in the library but cannot be prepared for
    resolution, it is regenerated and the library JSON is replaced (with console logging)
  - If `RubricConcepts::createConceptSet(...)` throws a runtime error, concept
    set creation falls back to a placeholder concept set (`36433327`) so the
    workflow can continue.
  - Subject scoring across multiple conditions and multiple databases
  - SQL correction loop during scoring (same behavior class as original code)
  - Scoring accepts explicit `cohortTableName` to control cohort table used for generation/reads and SQL rendering
  - `cohortTableName` now supports per-database mapping (named by `databaseId`) so each database can use a distinct cohort table in the same schema
  - Post-run scoring artifact summary via `summarizeScoringResults(...)`
  - `scoreSubjects(...)` automatically runs summary generation after each condition/database job
  - Adds `backfillScoringCaseLabels(...)` internal workflow to recompute case labels and regenerate versioned scoring artifacts for legacy runs
  - `backfillScoringCaseLabels(..., recomputeFromRuleScores = TRUE)` (`ithRescoreFromRuleScores(...)`) also re-sums rubric totals from the latest `fullScoringFile` using the latest short rubric rule scores and saves new `fullScoringFile`/`subjectCount` versions; it errors when the rule set (from `subjectCount_*`) or any criteria text differs from the scoring run; `skipIfUnchanged = TRUE` returns `skipped = TRUE` without saving when rule scores, thresholds, totals, and cases are unchanged
  - Concept-set plugin resolution during scoring now uses a Circe-first expansion path and falls back to `RubricConcepts::resolveConceptSet(...)` only when needed
  - Resolver no longer hard-fails if `RubricConcepts` is absent during scoring, as long as Circe-based resolution succeeds
  - Concept-set creation now resolves and uses a canonical DuckDB concept set library location from `seedLibraryPath` for both matching reads and immediate per-concept-set writes (instead of output-directory-local library state)
  - Concept-set matching now supports ambiguity-gated LLM adjudication over top embedding candidates (`llmTopCandidates`), with confidence thresholding and automatic fallback to cosine/fuzzy-only matching when LLM is unavailable or low-confidence.

- `R/ithacaWorkflow.R`
  - `R6` orchestration class: `IthacaWorkflow`
  - Pipeline methods:
    1. `createRubrics()`
    2. `editRubric()`
    3. `createConceptSets()`
    4. `scoreSubjects()`
    5. `postRunEdit()`
    6. `runFull()`
  - Convenience runner: `runIthacaWorkflow(...)`

- `R/ithacaDataAccess.R`
  - Migrated subject/cohort data retrieval helpers
  - Sampling utilities and ingredient query helper
  - Legacy-compatible wrapper names retained

- `R/ithacaLegacyHelpers.R`
  - Migrated rubric flattening and 4-level classification helpers
  - Proportion/stat helper utilities
  - Vector search standard helper and exclusion-check helper

- `R/ithacaConceptHelpers.R`
  - Migrated advanced concept search helpers (`vectorSearch`, `vectorSearchAtc`)
  - Concept JSON descendants builder
  - Concept set creation wrapper (`getConceptSet`)

- `R/ithacaIoHelpers.R`
  - Migrated list/bullet document helpers
  - Migrated bulk cohort load helpers for new and existing tables

- `R/ithacaEntrypoints.R`
  - Explicit top-level callable entry points aligned to prior script naming:
    - `getRubrics(...)`
    - `createRubricConceptSets(...)`
    - `adjustRubric(...)`
    - `scoreSubjects(...)`
    - `backfillScoringCaseLabels(...)`
    - `updateConceptSetLibraryFromJson(...)`
    - `normalizeConceptSetLibraryStorage(...)`
    - `deleteConceptSetFromLibrary(...)`
    - `exportConceptSetJson(...)` displays a message and returns `NULL` when the requested
      concept set is not found, rather than throwing an error.
  - `scoreSubjects(...)` now accepts `seedLibraryPath` (DuckDB file or containing directory). `conceptLibraryVersion` is optional: `ithResolveScoringConceptLibraryVersion(...)` turns `NULL`, blank, or `"latest"` into the newest library version once per `ithScoreSubjects(...)` run, and accepts a positive integer as a pin. The version used is written to the `conceptLibraryVersion` column of `pluginResolution_Cohort*.csv`. `ithLoadConceptSetLibraryForScoring(...)` uses the same resolver.
  - `ithScoreSubjects(...)` calls `ithCheckConceptSetsReadyForScoring(...)` before cohort generation (skipped when `useExisting = TRUE`). It stops when any condition lacks `matchedConceptSets_V*.rds`, or when the latest short rubric's SQL `@` concept sets (`ithScoringSqlConceptSets`) are not all in it (compared with `ithNormalizeConceptSetName`). Without this, `ithResolveConceptSetPluginDetails` would return `missingMatch` and the placeholder concept 36433327, giving silent zero-hit rules. SQL concept sets that no rule lists in `neededConceptSets` (invented during SQL generation) are reported per rule, separately from stale match files.
  - After cohort generation (when requested) and before the first rule query, `ithScoreSubjects(...)` calls `ithCheckRandomCohortsReadyForScoring(...)` (also skipped when `useExisting = TRUE`). It runs one grouped `COUNT(*)` per database on the resolved cohort table (`ithCountCohortRows`) and stops, listing every condition × database whose `randomCohortDefinitionId` has no rows and every cohort table that cannot be read. Without it, an unbuilt random cohort gives every rule zero subjects and the run saves all-zero scores.
  - Concept-set name check: `ithCheckRuleConceptSets(...)` (in `R/ithacaRubrics.R`, with `ithConceptSetNameProblem` and `ithControlledConceptSetSuffixes`) validates the name format and the controlled suffix. It repairs a malformed name when exactly one known valid name (the rule's SQL `@` names, or other rubric names) is a token inside it, and rewrites the list text only when it repairs something. Where it is used:
    - `ithCreateRubrics` runs it through `ithCleanRubricResponseConceptSets(...)`; unrepaired names count as response problems and trigger a retry.
    - `ithApplyConciseRubricEdits` checks every edited row before any LLM or SQL work (names in the existing rubric are exempt from the suffix rule). It also checks inferred concept sets through `checkInferred`.
    - `ithCreateRubricConceptSets` checks only the format.
  - `ithCreateRubricConceptSets(...)` re-matches only newly created names after creation and keeps the pre-create matches for everything else. Re-running the full LLM/Hecate match could drop names that had already matched, and those names were never created. A warning lists names still unmatched. `ithMatchConceptSets(...)` keeps one diagnostics row per name (the expanded-pass row replaces the first-pass row), so the summary counts are per name.
  - `scoreFullScoringFileAdjusted(...)` adds standalone case-label scoring from a `fullScoringFile_*.rds` artifact with latest-version input resolution, optional negative-score exclusion, yes/no threshold overrides, and adjusted output files (`sumCohortScoresAdjusted_*`, `finalScoresAdjusted_*`).
  - `updateConceptSetLibraryFromJson(...)` adds standalone upsert of a single concept set JSON into the resolved DuckDB concept set library, including embedding refresh and provenance capture (`runId`, `userName`, `createdAt`, `conditionName`, `cohortId`, `sourceFile`).
  - `normalizeConceptSetLibraryStorage(...)` adds standalone normalization of all concept-set JSON payloads to canonical `list(items = ...)` storage, with optional unreadable-row replacement and versioned DuckDB upserts.
  - `deleteConceptSetFromLibrary(...)` adds standalone selective deletion by concept set name (case-insensitive against `conceptSetName`/`cleanName`), with dry-run preview and `conceptSetCurrent` rebuild from remaining revisions.

- `R/runIthacaExample.R`
  - Adds `runIthacaExample(...)` as a single callable demonstration of the full lifecycle:
    1) create rubrics,
    2) optional pre-run edit,
    3) create concept sets,
    4) score subjects (multi-condition/multi-database),
    5) optional post-run edit.

- `R/ithacaRunConfig.R`
  - Adds YAML-driven execution wrappers (`runIthacaWorkflowFromConfig(...)`, `buildRandomOmopCohortFromConfig(...)`).
  - Preferred database configuration now supports Databricks CDM map input (`general.cdm`), with legacy fallback to `other.databaseSpecNames`.
  - Databricks connection details resolve via package function `getDatabricksConnectionDetails()` during config load (environment variables only), and can be overridden by defining a global `getDatabricksConnectionDetails()`.
  - LLM and embedding credentials resolve from `GENAI_O3_ENDPOINT`, `GENAI_GPT4O_ENDPOINT`, `GENAI_API_GPT4_KEY`, `GENAI_EMBED_ENDPOINT`, and `GENAI_API_EMBED_KEY` environment variables only.
  - `cohortTablePrefix` now supports auto-derivation of per-CDM cohort table names by appending normalized CDM schema identifiers.
  - Condition specs now support condition-level random cohort source fields (`sourceCohortDefinitionId`, `randomCohortDefinitionId`, `randomSampleFromCohortDefinitionId`, `webapiBaseUrl`) for both inline YAML and CSV-driven runs.
  - Condition specs now support `phenotypeCohortId` as a list of cohort IDs (CSV delimiter list supported) for downstream phenotype-report workflows.
  - `ithReadConditionSpecsCsv(...)` honors an optional `ignore` column (case-insensitive header; `ithIsIgnoreFlag(...)` treats `true`/`1`/`yes` as ignore). Ignored rows are dropped before `ithNormalizeConditionSpec(...)` validation and never reach any workflow, the column is not copied into the spec, and an all-ignored CSV is an error. Error row labels keep the original CSV row numbers.
  - Optional condition fields (`additionalInformation`, `sourceCohortDefinitionId`, `randomCohortDefinitionId`, `randomSampleFromCohortDefinitionId`, `phenotypeCohortId`) treat a blank/NA CSV cell, a missing CSV column, and an inline `null` identically as "not provided"; blank or NA required text values are rejected with a clear error. The generated config template no longer repeats `randomCohortDefinitionId` in the inline condition. Random cohort/scoring workflows still validate that they receive a random cohort ID.
  - `buildRandomOmopCohortFromConfig(...)` now resolves random cohort source settings per condition and returns results grouped by condition and database.
  - Adds `scoreSubjectsAgainstRandomCohortFromConfig(...)` to score using resolved per-condition random cohort target IDs (`randomCohortDefinitionId`).
  - Adds `combineFullScoringFilesFromConfig(...)`, which joins each latest `fullScoringFile_Cohort*_V*.rds` to the matching concise-rubric `criteria` and `rationale` and writes `<condition>_<database>_fullScoringFile_Cohort*_V*.csv` under the selected output directory.
  - Call graph clarification: scoring runs via `ithScoreSubjects(...)` -> `ithScoreConditionDatabase(...)`; random cohort generation flow does not call scoring directly.

### 2026-10 Phase 2 cohort-key migration (breaking)

- Scoring contract now requires `randomCohortDefinitionId` on condition specs; generic scoring use of `cohortId` has been removed.
- Scoring internals (`ithScoreSubjects`, `ithScoreConditionDatabase`, summary/backfill helpers) now key artifact resolution and denominator cohort reads from `randomCohortDefinitionId`.
- Config-driven scoring and CSV templates now require `randomCohortDefinitionId` as the condition-level scoring cohort key.

- `R/buildRandomOmopCohort.R`
  - Ported from `P:/skills/rubric-evaluation`.
  - Adds random OMOP cohort construction and overlap summarization utilities.
  - Optional `randomSampleFromCohortDefinitionId` is fetched from WebAPI and generated via CohortGenerator at runtime, then used to restrict random sampling before writing the random cohort.
  - `buildRandomOmopCohort(...)` now accepts explicit `seedLibraryPath` and records it in run output metadata for downstream concept-set library seeding workflows.

- `R/testRubricPhenotypeAlgorithm.R`
  - Ported from `P:/skills/rubric-evaluation`.
  - Adds rubric-vs-phenotype algorithm validation/reporting helpers.
  - Adds `createSubjectSampleScoringReport(...)` to translate FP/FN subject sample rows
    into subject-level scoring outputs with concise-rubric criteria text.
  - `createSubjectSampleScoringReport(...)` supports optional `cohortId` override;
    otherwise it reads cohort ID from `*_subject_samples.csv` and emits a fixed
    output column schema for downstream consumption.
  - When `webapiBaseUrl` is provided, phenotype labels in report outputs are sourced from WebAPI cohort definition names when available.
  - `testPhenotypeAlgorithmRubric(...)` / `testPhenotypeAlgorithmRubricReport(...)` accept optional `fullScoringFile` and `excludeNegativeScores` (single rubric only). When `fullScoringFile` is supplied, rule scores are re-summed through `ithScoreFullScoringFile(...)` with `excludeNegativeScores` and yes/no thresholds, each set absolutely (`yesScoreOverride`/`noScoreOverride`) or shifted from the evaluation file's current threshold (`yesScoreAdjustment`/`noScoreAdjustment`; not both for one threshold); `evaluationCohortFile` (`sumCohortScores_*`) still supplies the subject list (zero-hit subjects get total 0) and the current thresholds. Output adds `excludeNegativeScores`, `yesScoreAdjustment`, and `noScoreAdjustment` columns.
  - Per-rule re-scoring: `adjustableRuleId` (conditionSpecs column, parsed by `ithParseRuleIdList(...)` into upper-case IDs) plus `dxRuleScoreOverride`/`dxRuleScoreAdjustment` (`phenotypeReportScoring`, mutually exclusive, included in `specified`). `ithResolvePhenotypeReportScoring(config, conditionName, adjustableRuleId)` uses `ER1` with a message when the IDs are blank. `ithCheckAdjustableRules(...)` validates the IDs against the latest concise rubric (exist, evidence rule, non-negative result). `ithScoreFullScoringFile(...)` changes matching `ruleId` rows before the `excludeNegativeScores` filter; the arguments flow through `applyAdjustedScoringToEvaluation`, `saveAdjustedScoringFiles`, `testPhenotypeAlgorithmRubric`, and `testPhenotypeAlgorithmRubricReport`, and the report adds `adjustableRuleId`, `dxRuleScoreOverride`, and `dxRuleScoreAdjustment` columns.
  - Protected rules: `ithPlanConciseRubricReview(..., protectedRuleIds)` flags accepted review rows that remove or retype an `adjustableRuleId` rule (`applyConciseRubricReview` reads the IDs through `ithConfiguredAdjustableRuleIds(configPath, conditionName)`); `ithApplyConciseRubricEdits(..., adjustableRuleId)` stops when the edited CSV drops such a rule or makes it non-evidence. `applyConciseRubricEditsFromConfig()` passes the spec's IDs.
  - `config.yaml` section `phenotypeReportScoring` (`default` plus per-condition `conditions` keyed by `rubricName`) is resolved by `ithResolvePhenotypeReportScoring(...)` in `R/ithacaRunConfig.R` and consumed by `testPhenotypeAlgorithmRubricReportMultipleDatabases.R`. `splayPrior`/`splayPost` (whole days >= 0, default 7/7 in `config.yaml`) are required for every condition and are passed to the report's date-match window; they do not count as scoring rules, so they never trigger the `fullScoringFile` path.
  - `phenotypeReportScoringCsvPath` optionally points under `outputDirectory` to a CSV of
    scoring runs applied to every condition. `ithReadPhenotypeReportScoringCsv(...)` reads
    it once, requires unique non-empty `analysisName` values, parses typed values and
    semicolon-separated `classLabels`, ignores extra columns, and leaves blank cells to
    fall back to YAML configuration. The template is
    `extras/phenotypeReportScoring_template.csv`.
  - `analysisName` (default `rubric_validation`; feeds the report `analysisName` column and the supporting-file stem, which the multi-database script also uses to locate the subject-sample file), `sampleSubjectCount` (default 30), `frequencyPlotFpFnScores` (default `FALSE`), `createSubjectSample` (default `FALSE`), and `classLabels` (default `c("FP", "FN")`) are resolved per condition. Subject-sample/date-mismatch outputs are always written under the database scoring directory; the criteria report is generated only when `createSubjectSample` is `TRUE`.
  - `saveAdjustedScoringFiles(...)` (`R/testRubricPhenotypeAlgorithm.R`) writes the adjusted per-subject totals/cases as versioned `sumCohortScoresAdjusted_*`/`finalScoresAdjusted_*` (same naming as `scoreFullScoringFileAdjusted`, so the baseline `sumCohortScores_*` is never replaced) and skips saving when identical to the latest adjusted file. `applyAdjustedScoringToEvaluation(...)` errors when no evaluation subject matches the full scoring file, instead of silently assigning every subject a total of 0.
  - Empty or failing phenotype cohorts: the per-cohort block in `testPhenotypeAlgorithmRubricReportFromConfig(...)` (`R/ithacaPhenotypeReportRun.R`; `testPhenotypeAlgorithmRubricReportMultipleDatabases.R` is now a thin wrapper around it) traps errors from `testPhenotypeAlgorithmRubricReport(...)` and from the scoring criteria report. A cohort with no analyzable results (for example "Phenotype cohort is empty") gets a one-row report with the identifying columns filled and every metric `NA`; `reportStatus` is `ok`, `empty`, or `error: <message>`; a missing subject sample file only warns, and the run continues.
  - Load/re-score once per condition/database: `ithRunPhenotypeReportConditionDatabase(...)` reads `sumCohortScores` and `fullScoringFile` once before looping over `phenotypeCohortId`s. `saveAdjustedScoringFiles(...)` takes the loaded evaluation plus `fullScoringData` (passed through `applyAdjustedScoringToEvaluation` to `ithScoreFullScoringFile`, which copies it instead of reading the file) and returns the re-scored `evaluation`. Each cohort's report receives that data frame as `evaluationCohortFile` with `evaluationAdjusted = TRUE`, so `testPhenotypeAlgorithmRubric(...)` skips re-scoring and only records the settings; `createSubjectSampleScoringReport(...)` receives the loaded `fullScoringFile` data frame plus `adjustableRuleId`, `dxRuleScoreOverride`, and `dxRuleScoreAdjustment`, so its per-rule `score` column matches the adjusted `evaluationTotalScore`. Re-scoring runs only when `specified` is TRUE; `createSubjectSample` alone only loads the file.
  - The per-condition `<phenotype>_<analysisName>_phenotypeAlgorithmRubricReports_V#.csv/.rds` (in `scoring/<condition>/`; same sanitized stem as the support files) is saved at the end of each condition's iteration rather than after all conditions; rows are combined with `ithBindRows(...)` so conditions with different report columns (e.g. `subjectScoringCriteriaReportRows`) do not break the bind.

## Prompt relocation

Prompt files were copied from `extras/original/docs/` to `inst/prompts/` and are now loaded from package paths by default.

## Naming conventions

Active package code uses camelCase for function names, arguments, object/list fields, and local variables.

External contract names remain unchanged when required for compatibility, including SQL placeholders,
database column names, and third-party API keys.

## SQL assets added

The following SQL files were ported from `P:/skills/rubric-evaluation/inst/sql/sql_server/`:

- `inst/sql/sql_server/buildRandomOmopCohort.sql`
- `inst/sql/sql_server/summarizeCohortOverlaps.sql`
- `inst/sql/sql_server/validate_overwrite_dates.sql`

## Flow implemented

The new workflow supports the requested lifecycle:

1. Create rubrics
2. Edit rubrics
3. Create concept sets
4. Score subjects for multiple conditions across multiple databases
5. Post-run rubric edits and SQL regeneration
6. SQL correction in scoring retries for malformed statements

## Concept-set library strategy

The new approach uses a canonical DuckDB central concept-set library:

- Canonical path: `P:/llm/rubrics/rubricConceptSetLibrary.duckdb`
- Embedding-based matching for reuse (`similarityThreshold` configurable)
- Creation of only missing concept sets
- Immediate write after each concept set create/update (single-user write assumption)
- Numeric `libraryVersion` snapshots for reproducible scoring pins
- Condition-specific match snapshots (`matchedConceptSets.rds`)

If unavailable, the code creates an empty DuckDB scaffold.

## Backward compatibility

Selected legacy function names are preserved as wrappers to ease migration.

## Defects fixed during refactor

1. Fixed score assignment bug in short-rubric creation for hard exclusions.
2. Extended latest-version loader to correctly read `.rds`, `.csv`, and `.txt` outputs.

## CohortGenerator API update

Based on current CohortGenerator reference docs, `incrementalFolder` is deprecated.
The refactored pipeline no longer passes `incrementalFolder` into
`generateCohortSet(...)`, and wrapper function signatures were updated to remove
`incrementalFolder` entirely.

## Usage skeleton

```r
# 1) Define condition specs
conditions <- list(
  list(
    testCondition = "Gastroesophageal reflux disease",
    dxCriteria = "...",
    additionalInformation = "",
    rubricName = "Gastroesophageal reflux disease",
    cohortId = 14412
  )
)

# 2) Define database specs (existing pattern in your environment)
databases <- list(CCAE_OHDA_DBSpec, MDCR_OHDA_DBSpec)

# 3) Optional cohort definition set (required when generateCohorts = TRUE)
cohort_definitions <- ithMakeCohortDefinitionSet(
  cohortIds = c(14412),
  cohortNames = c("gerd_index"),
  cohortSql = c("SELECT ...")
)

# 4) Run pipeline
workflow <- runIthacaWorkflow(
  conditionSpecs = conditions,
  databases = databases,
  outputDirectory = "P:/llm/rubrics",
  generateCohorts = FALSE,
  cohortDefinitionSet = cohort_definitions
)

# 5) Post-run edit + SQL regeneration for one rule
workflow$postRunEdit(
  conditionName = "Gastroesophageal reflux disease",
  ruleId = "ER1",
  updates = list(criteria = "updated criteria text"),
  regenerateSql = TRUE
)
```

## Fixed AE1 zero-occurrence penalty

- New file `R/ithacaAe1.R`: rule row/SQL builders, Hecate + LLM concept-set builder (`ithBuildAe1ConceptSet`), library insertion (`ithEnsureAe1ConceptSetInLibrary`, one new library version), and the exported migration `addZeroOccurrencePenaltyRule()`.
- `ithEnforceFixedRubricRules` always sets AE1 first and renumbers the LLM's AEs from `AE2`; `ithCreateShortRubric` uses the SQL template for AE1 (no LLM SQL call).
- `ithCreateRubricConceptSets` builds the AE1 concept set before matching (falls back to standard creation with a warning on failure).
- `ithScoreFullScoringFile`/`excludeNegativeScores` keep AE1 rows; rubric review and apply protect the fixed AE1.
- Config: `general.zeroOccurrencePenalty` (default 3) and the `zeroOccurrencePenalty` conditionSpecs CSV column.
- Existing scoring results need a rerun after migration because AE IDs shift.

## Parallel concept-set creation (`R/ithacaConceptSetParallel.R`)

- `ithCreateRubricConceptSets` is split into `startCondition` (rubric names, AE1, first matching, regeneration of unreadable matches) and `finishCondition` (create/match missing names, save match files). With one worker they run per condition in order, as before.
- With more than one worker (`conceptSetWorkers` argument), all conditions run `startCondition`, the union of missing names is built by `ithBuildConceptSetsParallel` (PSOCK cluster, load-balanced dispatch with progress messages), the main process writes the results to DuckDB (one library version per set), then each condition runs `finishCondition(createMissing = FALSE)`.
- Workers run `ithBuildConceptSetOnWorker`: own LLM clients (from `llmConfig`), own working directory (RubricConcepts writes `./lastPrompt`), result saved atomically to `<conceptSetFolder>/<name>/result.rds`, failures to `error.txt` (never a placeholder). Reruns reuse `result.rds`.
- `ithCreateConceptSetSafe` is the shared builder wrapper (placeholder fallback). It no longer passes `clinicalContext` to `RubricConcepts::createConceptSet`.
- Empty builds: `ithCreateConceptSetSafe` returns `outcome` (ok/error/empty/unreadable), carried through workers (`outcome` column, `error.txt` prefix). `resolveFailedBuild` in `ithCreateRubricConceptSets` handles non-ok results: error -> left unbuilt (never stored); unreadable -> one retry; empty -> simplified-name alias (`ithSimplifiedConceptSetNames`, qualifier words only) -> known-empty record (placeholder 36433327, `knownEmpty.json`; helpers in `R/ithacaConceptSetEmpty.R`). Aliases are applied in `finishCondition` as extra `matches` rows (diagnostics method `fallbackSimplified`). `ithFlagRulesForEmptyConceptSets` sets `okToUse = FALSE` via `ithApplyConciseRubricEdits(repairConceptSetMismatches = FALSE)` and writes `<condition>_emptyConceptSetRules_V#.csv`, which the readiness check reads to warn. New args/config: `emptyConceptSet`, `emptyConceptSetFallback`, `rebuildEmptyConceptSets`; each run writes `conceptSetBuildSummary_V#.csv`.
- New config keys: `general.conceptSetFolder`, `general.parallelWorkers.conceptSetCreation` (the older `conceptSetBuilding.conceptSetWorkers` is still read as a fallback by `ithConfigParallelWorkers`). The test-only `conceptSetBuilder` argument replaces the RubricConcepts call on workers.
- Parallel concept-set dispatch uses `ithConceptSetDispatchPlan`: historical successful build times are persisted in `conceptSetBuildTimes.csv`, while new sets receive deterministic complexity estimates based on category, breadth, medication route/potency/formulation terms, and broad-condition signals. `ithBuildConceptSetsParallel` sends the longest-predicted tasks first and dynamically fills workers as they finish.
- Live LLM/Databricks parallel runs are untested beyond `extras/parallelConceptSetProbe.R` (10 sets, 4 workers, no failures); tests use a stub builder.

## Parallel scoring queries (`R/ithacaParallel.R`)

- Shared helpers: `ithStartWorkerCluster` (PSOCK cluster, Ithaca loaded via `ithLoadIthacaOnWorker`, session `sqlRender*`/`DatabaseConnector*` options copied), `ithStopWorkerCluster` (disconnects worker connections first), `ithRunOnCluster` (load-balanced dispatch in a given order with a per-result callback) and `ithConfigParallelWorkers` (`general.parallelWorkers.<step>`, optional legacy key, default 1). `ithBuildConceptSetsParallel` uses `ithStartWorkerCluster`.
- `ithScoreSubjects(scoringWorkers = 1L)` starts one cluster per call when `scoringWorkers > 1` (not with `useExisting = TRUE`) and passes it to each `ithScoreConditionDatabase(cluster = ...)`. `scoreSubjectsAgainstRandomCohortFromConfig` reads `general.parallelWorkers.scoring`; exported `scoreSubjects` has `scoringWorkers`.
- `ithScoreConditionDatabase` now runs in phases: plugin resolution for all rules (main process), first query attempts (main connection when serial; workers via `ithRunScoringQueryOnWorker` when a cluster is given, ordered by `ithScoringDispatchOrder`), then per rule in rubric order the LLM correction/retry loop (`runRuleAttempts`, main process and connection) and result assembly. Rule results are collected in lists and bound once, replacing the in-loop `rbind`. Saved outputs, messages and their order per rule are unchanged; workers add one progress line per finished query.
- `ithSaveScoringRuleTimes` writes `ruleQueryTimes.csv` (unversioned, overwritten) in the scoring folder with each rule's first-attempt query time and row count. `ithScoringDispatchOrder` sorts slowest first from that file; rules without a time go first, AE rules ahead of others.
- Workers keep one connection per `databaseId` in `ithScoringWorkerState` (worker global env) and write `lastQuery/query_worker<pid>.sql`.
- For rules without timing history, `ithScoringDispatchOrder` estimates query complexity from joins, subqueries, unions, aggregation, temporal functions, and concept-set expansions; historical `ruleQueryTimes.csv` values remain authoritative.
- Worker counts are not part of pipeline fingerprints, so changing them does not mark units stale.

## Parallel cohort generation (`R/ithacaParallel.R`, `R/ithacaCohorts.R`, `R/ithacaRunConfig.R`)

- `general.parallelWorkers.cohortGeneration` (default 1) applies to every cohort generation job: `buildRandomOmopCohortFromConfig`, `ithGenerateCohorts`/`generateCohorts(workers = )`, the phenotype cohorts generated by `testPhenotypeAlgorithmRubricReportFromConfig`, `scoreSubjects(generateCohort = TRUE, cohortWorkers = )`, and the pipeline's cohort step and phenotype cohort generation. `generateTestingPhenotypeCohort` (the single-database `generateTestingPhenotypeFirst` option) stays serial.
- `ithRunJobs` runs `taskFunction(task)` serially or on a cluster and returns `list(ok, value, error, seconds)` outcomes. Serial runs stop at the first failure (as before); worker runs let every job finish. `ithRunOnCluster` gained an `onDispatch` hook.
- Jobs: random cohorts are one task per condition x database (`ithRandomCohortTasks`, run by `ithRunRandomCohortTasks`, which prints the start and finish lines); other cohorts go through `ithGenerateCohortJobs`, which with more than one worker splits each database's definition set into one task per cohort (`ithRunCohortGenerationJob`; sets with subset/dependency columns stay together). Cohort IDs must not be shared by two tasks writing the same table. Outcomes carry `databaseId` and `cohortIds`.
- `ithCheckRandomCohortTasks` runs at the start of `ithRunRandomCohortTasks` (before tables are created or anything is built) and stops with a message listing each conflicting cohort ID per database and table: a random ID used by two conditions or also used as a source/pool ID, and with more than one worker a source/pool cohort shared by two conditions.
- Cohort tables are created once in the main session before dispatch (`ithCreateCohortTables`); workers creating the same table at once fail with SQL state 42P07. Serial runs create tables inside the job as before.
- Failures are raised after all jobs finish (`Random cohort build failed for condition ... on database ...`, `Cohort generation failed for database ...`). The pipeline passes `stopOnFailure = FALSE` and records each unit through `onUnitDone` as it finishes; `ithPipelineRunCohortUnits` and `ithPipelineGeneratePhenotypeCohorts(configPath, cohortIdsByDatabase, envir)` replace the one-unit-at-a-time runners, and phenotype definitions are fetched from WebAPI once per call instead of once per database.
- Pipeline code hashes cover the changed functions, so the next pipeline run treats cohort, score and report units as code-changed.
- Probe: `extras/parallelCohortProbe.R` (6 jobs, 5 workers: about 2.6 to 2.8 times faster than serial; each job ran 30 to 45 percent slower while sharing the warehouse).

## Incremental pipeline (`R/ithacaPipeline.R`)

- `runIthacaPipeline(...)` (exported) plans and runs three steps in order: cohort (`buildRandomOmopCohortFromConfig`), score (`scoreSubjectsAgainstRandomCohortFromConfig`), and report (`testPhenotypeAlgorithmRubricReportFromConfig`). The units are condition x database for cohort and score, and analysisName x condition x database x phenotypeCohortId for report. The condition key is `coalesce(rubricName, testCondition)`.
- Ledger: `<outputDirectory>/pipeline/pipelineLedger.csv`, written through a temporary file after every unit. Columns: `step`, `unitKey`, the unit fields, `fingerprint`, `components` (a JSON map of component to hash, so the plan can name what changed), `status`, `runId`, `finishedAt`, and `message`. CSV was chosen over DuckDB so the ledger can be read and edited by hand.
- Fingerprint components:
  - cohort: `database`, `settings`, `sqlFiles`, `definitions`, and `code`;
  - score: `upstream`, `database`, `rubric` (latest short rubric, rubric list, and matched concept-set files), `conceptSets`, `settings`, and `code`;
  - report: `upstream`, `database`, `settings` (resolved phenotypeReportScoring rules), `definition`, `conciseRubric`, and `code`.
- How some components are computed:
  - `upstream` is the upstream unit's `runId`. The `runId` is unique per execution, so any rerun upstream invalidates the units downstream.
  - `conceptSets` loads the scoring library version once. For every `@plugin` in the short rubric SQL, it hashes the status and JSON returned by `ithSelectLibraryConceptSet(...)`. That helper was extracted from `ithResolveConceptSetPluginDetails(...)`, and scoring uses the same selection logic.
  - `code` (`ithPipelineCodeHash`) is the transitive closure of the Ithaca namespace objects that the step's entry function reaches. Calls are found with `codetools::findGlobals` and explicit `Ithaca:::` references. The deparsed text is hashed after `utils::removeSource`.
- The score fingerprint is recomputed after the step runs, because SQL correction can save a new short rubric.
- WebAPI definition hashes are fetched once per run and cached in `pipeline/definitionHashes.csv`. When the fetch fails, the cached hashes are used.
- Decisions:
  - `blocked`: the upstream unit's last status is not `done`.
  - `run`: `fresh`, no ledger row, the last run failed, or the fingerprint changed (the reason names the changed components).
  - `skip`: none of the above.
  - Upstream steps that were not selected and were never tracked count as `untracked` and do not block.
  - `dryRun` treats a unit whose upstream unit is planned to run as needing to run.
- Reports:
  - The phenotype cohorts a run needs are generated once per database.
  - Each analysis x condition x database group runs in a `tryCatch`. A unit fails when its rows have an `error:` status or no rows are returned.
  - Unit rows are stored as `pipeline/reportRows/<hash(unitKey)>.rds`. `ithPipelineSaveReportAggregates(...)` rebuilds the per-condition files for the groups that changed, and the combined file from all units.
- `adoptExisting = TRUE` records units as done without running them when their outputs already exist:
  - cohort and score: `sumCohortScores_Cohort<id>` and `fullScoringFile_Cohort<id>` exist;
  - report: the latest per-condition report has a row for the analysis, database, and cohort, and the report file is not older than the latest `sumCohortScores`/`fullScoringFile` for that condition and database. Those rows are copied into the unit store.
- The step runners (`ithPipelineRunCohortUnit`, `ithPipelineRunScoreUnit`, `ithPipelineGeneratePhenotypeCohorts`, and `ithPipelineRunReportGroup`), `ithPipelineFetchDefinitionHashes`, and `ithPipelineStepCodeHash` are separate functions so `tests/testthat/test-pipeline.R` can mock them.
- `testPhenotypeAlgorithmRubricReportFromConfig(...)` (`R/ithacaPhenotypeReportRun.R`, exported) replaces the body of `testPhenotypeAlgorithmRubricReportMultipleDatabases.R`. It accepts a `units` table, so it can run only selected cohorts, and `generatePhenotypeCohorts`, `saveReports`, and `phenotypePopulationCache`, so the pipeline can share the work.

## Data-driven rule scores (`R/ithacaDataDrivenEvidence.R`, `R/ithacaDataDrivenScores.R`)

- `applyDataDrivenRuleScores(conditionName, ...)` and `applyDataDrivenRuleScoresFromConfig(configPath, conditionNames, applyChanges)` (exported) set each rule's score from its likelihood ratio and apply the scores to the concise rubric through `ithApplyConciseRubricEdits`. Run after concept-set creation and before scoring; it is not a pipeline step yet.
- Settings: `dataDrivenScoring` config section (nested like `phenotypeReportScoring`, kept by `ithApplyConfigSections`). Precedence: built-in defaults (`ithDataDrivenDefaults`), then the section, then `dataDrivenScoring.conditions.<name>`, then function arguments (`ithDataDrivenSettings`); `gate` and `cdm` merge by name, a null gate limit removes that check, and `ithDataDrivenCheckSettings` validates. `capLr` defaults to 10.
- Evidence (`ithDataDrivenEvidence`): per database, populations are built in a scratch Delta table (candidate people, differential negatives, ER2 positives and negatives, people who fail ER2) and each rule runs once on their union, with hits joined back to the population; counts and a deterministic person sample are saved as `rubricEvidenceProbe/queryResults/<condition>_queryResults_<database>_<time>.rds` with a fingerprint (rules, rule SQL, ER1/ER2 SQL, CDM schema, `personModulus`, `matrixPersons`); `reuseQueryResults` loads the latest matching file. SQL tasks run on the PSOCK cluster with `ithDataDrivenRunSqlTask`.
- `ithDataDrivenRecommend`: score = `ithDataDrivenScaleScore(mean ln LR)`; type follows the sign (`CHANGE_TYPE`), `RESCALE` when only the score changes. Hard exclusions, rules without hits, failed queries and unresolved concept sets keep their score and are marked `skipped`.
- Gate (`ithDataDrivenGate`): computed from the person sample with the proposed scores, cohort members getting ER1 + ER2 points and differential negatives none (AE1 = 0), at `yesScore`; limits `positiveYesMax`, `positiveYesMin`, `negativeYesMax`, `separationMin`. Every database and rubric must pass; otherwise the status is `gateFailed` and nothing is applied.
- `ithDataDrivenApplyScores` writes the new scores and types into a temporary copy of the latest concise rubric and applies it as an edit; the latest concise rubric must match the short rubric (`ithDiffConciseRubric`) or the condition stops before any query. The summary is refreshed only when a rule was retyped (`updateSummary = NULL`).
- `ithApplyConciseRubricEdits` now keeps the SQL of a retyped rule (new ID) when its criteria and concept sets are unchanged, instead of regenerating it with the LLM; this also applies to review applies and hand edits.
- `extras/rubricImprovementProbe.R` is a thin wrapper (`probeRubricImprovement()`) over `ithDataDrivenEvidence`; the previous standalone version is in `archive/rubricImprovementProbe_standalone.R`. `extras/rubricPopulationProbe.R` is unchanged. Tests: `tests/testthat/test-dataDrivenScores.R`.

## Notes and next migration steps

1. `extras/original/R_orig/` scripts remain as reference and can be retired after validation.
2. If needed, additional legacy helper functions from `functionsPhenotype.R` can be migrated into smaller modules using this same pattern.
3. End-to-end validation should be run in the target environment with actual credentials, database specs, and API access.

## Documentation status

- Top-level `README.md` now provides explicit package usage instructions,
  end-to-end flow examples, and maintenance conventions.

# Ithaca: LLM-Assisted Rubric Phenotyping in OMOP

## 1) Basic Idea of the Package
**Ithaca** is an R package for building and testing rubric-based phenotype logic in OMOP data.

It helps teams:
- Generate structured clinical rubrics from condition definitions.
- Convert rubric rules into SQL that can run on OMOP databases.
- Reuse and manage concept sets efficiently.
- Score subjects across one or more databases.
- Compare rubric-based case classification against reference phenotype cohorts.

In short: **Ithaca turns clinical reasoning into reproducible, testable database logic**.

---

## 2) General Scheme: How Ithaca Works
### Step A — Rubric Creation
- Create condition-specific rubrics (`getRubrics` / workflow methods).
- Output includes detailed rules, concise rubric tables, and generated SQL fragments.

### Step B — Concept Set Resolution
- Needed concept sets are extracted from rubric rules.
- Ithaca matches against a versioned concept set library.
- Missing concept sets are generated and added back to the library.

### Step C — Cohort + Scoring Execution
- Optional cohort generation via CohortGenerator/WebAPI.
- Rule SQL is run to compute evidence/exclusion points per subject.
- Rubric totals and final case decisions are saved as versioned artifacts.

### Step D — Evaluation & Reporting
- Compare rubric output to a target phenotype cohort.
- Produce sensitivity/PPV/specificity/NPV and supporting subject-level reports.

### Core Design Principles
- **Versioned artifacts** for reproducibility.
- **Checkpoint/resume behavior** for long LLM workflows.
- **Modular entry points** (run one stage or full pipeline).
- **WebAPI-first compatibility** for cohort definition sourcing.

---

## 3) Important Options (Most Used in Practice)
### Rubric Generation Options
- `useExistingRubric`: skip regeneration when artifacts already exist.
- `includeLabs`: include/exclude lab-value logic in rubric prompting.
- `useLibraryHints`: suggest preferred concept-set names from library while still allowing new names.
- `libraryHintSimilarityThreshold`, `libraryHintTopCandidates`: tune how strong/wide concept-set suggestions are.

### Concept Set + Matching Options
- `similarityThreshold`: controls concept-set reuse strictness.
- Diagnostic artifacts (`conceptSetMatchDiagnostics`, decisions files) help audit matching behavior.

### Scoring / Cohort Options
- `generateCohorts`: optionally run cohort generation before scoring.
- `cohortDefinitionSet` or `webapiBaseUrl`: choose manual SQL definitions vs WebAPI retrieval.
- `cohortTableName`: explicitly control which cohort table is used in generation/reads/scoring.
- `useExistingScores`: reuse prior scoring artifacts.

### Validation / Testing Options
- `testPhenotypeAlgorithmRubricReport(...)` for rubric-vs-phenotype performance reporting.
- `yesScoreOverride`, `noScoreOverride`: recalculate case logic from raw totals for scenario testing.
- `generateTestingPhenotypeFirst`: optionally generate comparison phenotype cohort at runtime.
- `splayPrior`, `splayPost`: date-window matching tolerance between rubric and phenotype cohorts.

---

## Typical Outputs
- Rubric artifacts (`rubricList`, short/concise rubric files)
- Matching diagnostics and concept-set decisions
- Subject-level scoring files (`fullScoringFile`, `sumCohortScores`, `finalScores`)
- Summary reports and phenotype validation metrics

---

## Conference Takeaway
Ithaca provides a practical bridge between **clinical phenotype reasoning** and **production-ready, auditable OMOP execution**—with enough flexibility for rapid iteration and enough structure for reproducible analytics.
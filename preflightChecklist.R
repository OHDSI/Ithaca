# Ithaca preflight checklist: populate everything below before running a script.
# Scripts are run from E:/Ithaca after: devtools::load_all("E:/Ithaca")
#
# ---------------------------------------------------------------------------
# 1. R environment (all scripts)
# ---------------------------------------------------------------------------
# [ ] R >= 4.2 with the packages in DESCRIPTION installed (including Helios).
# [ ] RubricConcepts installed (needed for concept set creation only).
# [ ] Network/VPN access to the company AI endpoints, Databricks and the OHDSI WebAPI.
# [ ] Read/write access to the P: drive (outputDirectory and the concept set library).
#
# ---------------------------------------------------------------------------
# 2. Environment variables (set with Sys.setenv() or in your .Renviron)
# ---------------------------------------------------------------------------
# LLM client (rubrics, concept sets, scoring):
# [ ] GENAI_O3_ENDPOINT
# [ ] GENAI_GPT4O_ENDPOINT         (concept sets, which use the gpt-4o client)
# [ ] GENAI_API_GPT4_KEY
# Embeddings (concept sets):
# [ ] GENAI_EMBED_ENDPOINT
# [ ] GENAI_API_EMBED_KEY
# Databricks (concept sets, cohorts, scoring, phenotype report):
# [ ] DATABRICKS_TOKEN
# [ ] DATABRICKS_HOST
# [ ] DATABRICKS_HTTP_PATH         (e.g. /sql/1.0/warehouses/<id>, no extra JDBC settings)
# Optional:
# [ ] DATABRICKS_USER              (defaults to "token")
# [ ] DATABRICKS_CATALOG           (only used when config catalog is not set)
# [ ] DATABRICKS_SCRATCH_SCHEMA    (only used when config cohortDatabaseSchema is not set)
#
# ---------------------------------------------------------------------------
# 3. config.yaml
# ---------------------------------------------------------------------------
# [ ] general.conditionSpecsCsvPath    path to the condition CSV (section 4)
# [ ] general.dxCriteria               named choice map; CSV dxCriteria cells must use a key
# [ ] clinicalDefinitionFile       optional CSV with Phenotype, Definition; fills null additionalInformation
# [ ] outputDirectory              root folder for rubric and scoring artifacts
# [ ] seedLibraryPath              concept set library (.duckdb file or its folder)
# [ ] general.cdm                      one "databaseId: cdm_schema_name" entry per database
# [ ] catalog                      a single catalog, or a per-database map when the
#                                  databases are in different catalogs, e.g.
#                                  OptumDod: optum_extended_dod / OptumEhr: optum_ehr
# [ ] cohortDatabaseSchema         writable scratch schema for cohort tables
# [ ] cohortTablePrefix            cohort tables are named <prefix>_<cdm schema>
# [ ] webapiBaseUrl                needed to build or generate cohorts from WebAPI
# [ ] conceptLibraryVersion        integer that exists in the library's libraryVersionHistory
#                                  (needed for scoring only; null stops the run)
# [ ] llmReasoning / llmNonReasoning
# [ ] conceptSetDatabaseId         optional; database used for concept sets (default: first cdm entry)
# [ ] phenotypeReportScoring      splayPrior/splayPost required (default 7/7);
#                                  sampleSubjectCount defaults to 30;
#                                  frequencyPlotFpFnScores defaults to false;
#                                  createSubjectSample defaults to false;
#                                  classLabels defaults to [FP, FN];
#                                  excludeNegativeScores and yes/no score overrides (absolute)
#                                  or adjustments (added to current) are optional
#
# ---------------------------------------------------------------------------
# 4. Condition CSV columns (one row per condition)
# ---------------------------------------------------------------------------
# [ ] testCondition                condition name
# [ ] dxCriteria                   diagnosis criteria text
# [ ] rubricName                   also the folder name for the condition's artifacts
# [ ] randomCohortDefinitionId     optional for rubric creation; required for random cohort/scoring artifacts
# [ ] sourceCohortDefinitionId     integer WebAPI cohort ID; needed to build random cohorts (or set
#                                  randomCohort.sourceCohortDefinitionId); blank is allowed otherwise
# [ ] phenotypeCohortId            phenotype report only; one or more IDs separated by ; or ,
# [ ] additionalInformation        optional text (may be blank)
# [ ] randomSampleFromCohortDefinitionId  optional; limits the random sampling pool
#
# ---------------------------------------------------------------------------
# 5. Run order and what each script needs from the earlier steps
# ---------------------------------------------------------------------------
# 1) source("getRubricsFromConfig.R")
#      Needs: sections 1-4 (o3 LLM variables, CSV, outputDirectory).
# 2) source("createRubricConceptSetsFromConfig.R")
#      Needs: rubrics from step 1, both LLM endpoints, embedding variables,
#             Databricks variables, writable seedLibraryPath.
# 3) source("buildRandomOmopCohortMultipleDatabases.R")
#      Needs: Databricks variables, webapiBaseUrl, cohortDatabaseSchema,
#             sourceCohortDefinitionId and randomCohortDefinitionId in the CSV.
# 4) source("scoreSubjectsMultipleDatabases.R")
#      Needs: rubrics (1), concept sets (2), random cohorts (3), conceptLibraryVersion,
#             o3 LLM variables, Databricks variables.
# 5) source("testPhenotypeAlgorithmRubricReportMultipleDatabases.R")
#      Needs: scoring output (4), phenotypeCohortId in the CSV, webapiBaseUrl,
#             Databricks variables. With phenotypeReportScoring rules set, the latest
#             fullScoringFile is re-scored as well as the latest sumCohortScores.
#
# ---------------------------------------------------------------------------
# 6. Quick check before you start
# ---------------------------------------------------------------------------
# required <- c("GENAI_O3_ENDPOINT", "GENAI_GPT4O_ENDPOINT", "GENAI_API_GPT4_KEY",
#               "GENAI_EMBED_ENDPOINT", "GENAI_API_EMBED_KEY",
#               "DATABRICKS_TOKEN", "DATABRICKS_HOST", "DATABRICKS_HTTP_PATH")
# setdiff(required, names(Sys.getenv()[nzchar(Sys.getenv())]))   # character(0) when all set
#
# config <- Ithaca:::ithReadIthacaWorkflowConfig("config.yaml")
# length(config$conditionSpecs)   # number of CSV conditions loaded
# names(config$cdm)               # databases that will be used

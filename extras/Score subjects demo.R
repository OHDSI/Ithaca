library(Ithaca)

conditionList <- list(
  list(
    testCondition = "Gastroesophageal reflux disease",
    dxCriteria = "1 inpatient/ED diagnosis OR 2 outpatient diagnoses 1-60 days apart",
    additionalInformation = "",
    rubricName = "Gastroesophageal reflux disease",
    cohortId = 14412
  )
)

conditionList <- list(
  list(
    testCondition = "ST-elevation myocardial infarction",
    dxCriteria = "1 inpatient/ED dx",
    additionalInformation = "",
    rubricName = "ST-elevation myocardial infarction",
    cohortId = 26553001
  )
)

conditions$randCohortId <- as.integer(paste0(conditions$prevCohort_id, "001"))
conditions$cohortId <- as.integer(paste0(conditions$prevCohort_id, "001"))

conditionList <- list()
for(conditionUp in seq_len(nrow(conditions))) {
  cond <- list(testCondition = conditions$condition[[conditionUp]],
               dxCriteria = dxCriteria,
               additionalInformation = "",
               rubricName = conditions$condition[[conditionUp]],
               cohortId = conditions$cohortId[[conditionUp]])
  conditionList[[conditionUp]] <- cond
}

conditionList <- conditionList[14:15]

databases <- list(DOD_OHDA_DBSpec, Panther_OHDA_DBSpec)

#use this to evaluate the 2M reference cohort
scoreResults <- scoreSubjects(
  conditionList = conditionList,
  database = databases,
  cohortTableName = c(gsub(" ", "", paste0("largeScale", databases[[1]]$databaseId)),
                      gsub(" ", "", paste0("largeScale", databases[[2]]$databaseId))),
  outputDirectory = "P:/llm/largeScalePhenotyping",
  useExisting = FALSE,          # TRUE to reuse prior scoring files
  generateCohort = FALSE,       # TRUE only if you want cohorts generated now
  cohortDefinitionSet = NULL,   # provide if generateCohort = TRUE and not using WebAPI
  webapiBaseUrl = "https://epi.jnj.com:8443/WebAPI",         # set if generateCohort = TRUE and pulling from WebAPI
  llmClient = getLlmo3Client(), # used for SQL correction retries
  seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.rds",
  promptDir = NULL,
  verbose = TRUE
)

#use this to evaluate the 2M reference cohort with special adjustments (e.g., no negative values)
result <- scoreFullScoringFile(
  fullScoringFile = "P:/llm/rubrics/scoring/Abdominal Pain/CCAE/fullScoringFile_Cohort26154001.rds",
  excludeNegativeScores = FALSE,
  yesScoreOverride = c(R1 = 1),
  noScoreOverride = c(R1 = 0),
  saveAdjustedOutput = TRUE,
  outputDirectory = "P:/llm/rubrics/scoring/Abdominal Pain/CCAE"
)

#use this to test a phenotype against the evaluation (random) cohort
reportResults <- testPhenotypeAlgorithmRubricReport(
  phenotype = "ST-elevation myocardial infarction",
  analysisName = "rubric_validation",
  runDateTime = as.character(Sys.time()),
  connectionDetails = CCAE_OHDA_DBSpec$connectionDetails,
  evaluationCohortFile = "P:/llm/rubricsNew/scoring/ST-elevation myocardial infarction/CCAE/sumCohortScores_Cohort26553001_V1.rds", # use your actual file
  phenotypeCohortId = c(26405,
                        26401,
                        26397,
                        26407,
                        26404,
                        26403,
                        26400,
                        26399,
                        26406,
                        26396,
                        26402,
                        26398,
                        26104,
                        26079,
                        26077,
                        26075,
                        26074,
                        26073,
                        26072,
                        26071,
                        26068,
                        26067,
                        26066,
                        26065,
                        26064,
                        25868,
                        26126,
                        25912,
                        25913,
                        25914),                    # can be vector too: c(26553, 12345)
  cdmDatabaseSchema = CCAE_OHDA_DBSpec$cdmDatabaseSchema,
  databaseId = CCAE_OHDA_DBSpec$databaseId %||% "CCAE",
  cohortDatabaseSchema = CCAE_OHDA_DBSpec$workDatabaseSchema,
  cohortTable = "pv_table_3",
  yesScoreOverride = 8,
  noScoreOverride = 2,
  outputFolder = "P:/llm/rubricsNew/scoring",
  sampleSubjectCount = 30,
  frequencyPlotFpFnScores = TRUE,
  generateTestingPhenotypeFirst = TRUE,        # set TRUE if you want this function to generate phenotype cohort(s)
  webapiBaseUrl = "https://epi.jnj.com:8443/WebAPI",                         # required if generateTestingPhenotypeFirst=TRUE
  phenotypeCohortName = "ST-elevation myocardial infarction",                   # optional label when generating
  generationIncremental = TRUE,
  splayPrior = 0,
  splayPost = 0,
  requireObservationPeriod = TRUE,
  cohort_start_date = "01/01/2000",                     # "MM/DD/YYYY" or NULL
  cohort_end_date = "12/31/2026"                        # "MM/DD/YYYY" or NULL
)

write.csv(reportResults, "p:/pvtest/stemi.csv", row.names = F)

conditionToUse <- "ST-elevation myocardial infarction"
files <- list.files(
  path = "path/to/dir",
  pattern = "_cohort_1400_subject_samples",
  full.names = TRUE
)
databseToUse <- DOD_OHDA_DBSpec
report <- createSubjectSampleScoringReport(
  subjectSampleFile = "P:/llm/largeScalePhenotyping/scoring/ST-elevation_myocardial_infarction_rubric_validation_2026-10-12_01_10_13_918546_cohort_26104_subject_samples.csv",
  fullScoringFile = "P:/llm/largeScalePhenotyping/scoring/ST-elevation myocardial infarction/Optum DOD/fullScoringFile_Cohort26148001_V2.rds",
  conciseRubricFile = "P:/llm/largeScalePhenotyping/ST-elevation myocardial infarction/ST-elevation myocardial infarction_conciseRubric_V2.csv",
  outputFile = "p:/pvtest/stemiFpFnDOD.csv",
  classLabels = c("FP", "FN"),
  cohortId = 26104)


#########
#new to score random cohort
library(DBI)
library(duckdb)

connection <- dbConnect(
  duckdb::duckdb(),
  dbdir = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  read_only = TRUE
)

dbGetQuery(
  connection,
  "SELECT MAX(libraryVersion) AS latestVersion
   FROM libraryVersionHistory"
)

dbDisconnect(connection, shutdown = TRUE)
#then put the library version in config.yaml

devtools::load_all("E:/Ithaca")

source("scoreSubjectsMultipleDatabases.R")


#########
devtools::load_all("E:/Ithaca")
source("testPhenotypeAlgorithmRubricReportMultipleDatabases.R")

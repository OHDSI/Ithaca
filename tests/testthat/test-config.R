writeConfigFile <- function(text) {
  path <- tempfile(fileext = ".yaml")
  writeLines(text, path)
  path
}

oldLayoutConfig <- '
configVersion: 1
defaults:
  common:
    verbose: true
    reviewRuns: 3
    llmConfidenceThreshold: 0.8
  contextual:
    outputDirectory: P:/out
    webapiBaseUrl: "https://example.org/WebAPI"
run:
  dxCriteria:
    outpatientOnly:
      criteria: "Two outpatient codes."
      additionalPoints: 0
  conditionSpecs:
    - testCondition: GERD
      dxCriteria: outpatientOnly
      additionalInformation: ""
      rubricName: GERD
      randomCohortDefinitionId: 14412
  cdm:
    Ccae: cdm_ccae
  randomCohort:
    sampleSize: 1000
'

newLayoutConfig <- '
configVersion: 1
general:
  outputDirectory: P:/out
  webapiBaseUrl: "https://example.org/WebAPI"
  verbose: true
  cdm:
    Ccae: cdm_ccae
  dxCriteria:
    outpatientOnly:
      criteria: "Two outpatient codes."
      additionalPoints: 0
  conditionSpecs:
    - testCondition: GERD
      dxCriteria: outpatientOnly
      additionalInformation: ""
      rubricName: GERD
      randomCohortDefinitionId: 14412
rubricDevelopment:
  reviewRuns: 3
conceptSetBuilding:
  llmConfidenceThreshold: 0.8
randomCohort:
  sampleSize: 1000
'

readConfig <- function(text) {
  suppressMessages(Ithaca:::ithReadIthacaConditionConfig(writeConfigFile(text)))
}

test_that("the section layout loads the same values as the old layout", {
  old <- readConfig(oldLayoutConfig)
  new <- readConfig(newLayoutConfig)

  for (key in c("outputDirectory", "webapiBaseUrl", "verbose", "reviewRuns", "llmConfidenceThreshold",
                "cdm", "dxCriteria", "randomCohort")) {
    expect_equal(new[[key]], old[[key]], info = key)
  }
  expect_equal(new$conditionSpecs[[1]]$dxCriteria, "Two outpatient codes.")
  expect_equal(new$conditionSpecs[[1]]$randomCohortDefinitionId, 14412L)
})

test_that("condition and rubric names are trimmed before artifact paths are built", {
  configText <- sub(
    "testCondition: GERD",
    "testCondition: '  GERD  '",
    sub("rubricName: GERD", "rubricName: '  GERD  '", newLayoutConfig)
  )
  config <- readConfig(configText)

  expect_equal(config$conditionSpecs[[1]]$testCondition, "GERD")
  expect_equal(config$conditionSpecs[[1]]$rubricName, "GERD")
})

test_that("randomCohort and phenotypeReportScoring stay nested", {
  config <- readConfig(paste0(newLayoutConfig, "
phenotypeReportScoring:
  default:
    splayPrior: 7
  conditions: {}
"))

  expect_equal(config$randomCohort$sampleSize, 1000)
  expect_equal(config$phenotypeReportScoring$default$splayPrior, 7)
})

test_that("phenotype scoring CSV runs are read once with typed values", {
  outputDirectory <- withr::local_tempdir()
  csvPath <- file.path(outputDirectory, "scoring-runs.csv")
  writeLines(
    paste(
      "analysisName,excludeNegativeScores,splayPrior,classLabels,extraColumn",
      "lower_threshold,true,5,FP;FN,ignored",
      sep = "\n"
    ),
    csvPath
  )

  expect_message(
    runs <- Ithaca:::ithReadPhenotypeReportScoringCsv(list(
      outputDirectory = outputDirectory,
      phenotypeReportScoring = list(phenotypeReportScoringCsvPath = basename(csvPath))
    )),
    "Ignoring unknown"
  )
  expect_length(runs, 1)
  expect_equal(runs[[1]]$analysisName, "lower_threshold")
  expect_true(runs[[1]]$excludeNegativeScores)
  expect_equal(runs[[1]]$splayPrior, 5)
  expect_equal(runs[[1]]$classLabels, c("FP", "FN"))
  expect_false("extraColumn" %in% names(runs[[1]]))
})

test_that("phenotype scoring CSV requires unique non-empty analysis names", {
  outputDirectory <- withr::local_tempdir()
  csvPath <- file.path(outputDirectory, "scoring-runs.csv")
  writeLines("analysisName\nrun_a\nrun_a", csvPath)

  expect_error(
    Ithaca:::ithReadPhenotypeReportScoringCsv(list(
      outputDirectory = outputDirectory,
      phenotypeReportScoring = list(phenotypeReportScoringCsvPath = basename(csvPath))
    )),
    "must be unique"
  )

  writeLines("analysisName,excludeNegativeScores\n,", csvPath)
  expect_error(
    Ithaca:::ithReadPhenotypeReportScoringCsv(list(
      outputDirectory = outputDirectory,
      phenotypeReportScoring = list(phenotypeReportScoringCsvPath = basename(csvPath))
    )),
    "analysisName is required"
  )
})

test_that("section keys override the old layout when both are present", {
  config <- readConfig(paste0(oldLayoutConfig, "
general:
  verbose: false
"))

  expect_false(config$verbose)
})

test_that("a config must define dxCriteria choices and conditions", {
  expect_error(readConfig("configVersion: 1\ngeneral:\n  outputDirectory: x\n"), "dxCriteria")
})

test_that("an unknown dxCriteria key is rejected", {
  text <- sub("dxCriteria: outpatientOnly", "dxCriteria: nope", newLayoutConfig, fixed = TRUE)

  expect_error(readConfig(text), "Unknown dxCriteria choice")
})

test_that("dxCriteria additionalPoints must stay within 0 to 4", {
  text <- sub("additionalPoints: 0", "additionalPoints: 9", newLayoutConfig, fixed = TRUE)

  expect_error(readConfig(text), "additionalPoints must be a whole number")
})

test_that("the shipped template is valid and loads", {
  template <- tempfile(fileext = ".yaml")
  Ithaca::writeIthacaWorkflowConfigTemplate(template)

  config <- suppressMessages(Ithaca:::ithReadIthacaConditionConfig(template))

  expect_true(length(config$conditionSpecs) >= 1L)
  expect_true(length(config$dxCriteria) >= 1L)
  expect_equal(config$llmReasoning$model, "o3")
  expect_equal(config$llmNonReasoning$apiArgs$seed, 1234)
})

test_that("a missing config file explains where R looked", {
  expect_error(
    suppressMessages(Ithaca:::ithReadIthacaConditionConfig(file.path(tempdir(), "missing.yaml"))),
    "Config file not found"
  )
  expect_error(Ithaca:::ithReadIthacaConditionConfig("GERD"), "not a .yaml/.yml file")
})

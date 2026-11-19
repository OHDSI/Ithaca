endToEndRules <- list(
  # In the order the short rubric is built: hard exclusion, alternative explanation, then evidence rules.
  list(set = "cystic_fibrosis_DIAGNOSIS_CODES", days = 30L),
  list(set = "asthma_DIAGNOSIS_CODES", days = 5L),
  list(set = "gi_bleed_DIAGNOSIS_CODES", days = 30L),
  list(set = "gi_bleed_DIAGNOSIS_CODES", days = 365L),
  list(set = "tiotropium_MEDICATION_CODES", days = 365L),
  list(set = "spirometry_MEASUREMENT_CODES", days = 30L)
)

# Concept the fake builder gives each concept set; the rest get a concept that is not in the CDM.
endToEndConcepts <- c(gi_bleed = 192671L, asthma = 80180L, tiotropium = 4112343L)

endToEndBuilder <- function(args) {
  key <- sub(" .*$", "", args$conceptSetTarget)
  key <- if (grepl("^gi bleed", args$conceptSetTarget)) "gi_bleed" else key
  conceptId <- endToEndConcepts[key]
  if (is.na(conceptId)) {
    conceptId <- 999999999L
  }
  list(finalConceptSet = conceptSetWithConcept(conceptId, paste("concept for", args$conceptSetTarget)))
}

test_that("a rubric, its concept sets, and scoring work together on a real CDM", {
  database <- makeEunomiaDatabase()
  generateEunomiaCohorts(database)
  outputDirectory <- withr::local_tempdir()
  withr::local_dir(withr::local_tempdir())
  conditionName <- "GI bleed"
  sqlCall <- 0L
  llm <- rubricLlm(
    responder = function(n) makeFakeRubricResponse("R1"),
    text = function(prompt) {
      sqlCall <<- sqlCall + 1L
      rule <- endToEndRules[[sqlCall]]
      eunomiaRuleSql(paste0("@", rule$set), rule$days)
    },
    other = function(kind, prompt, call) {
      if (kind == "ae1Terms") {
        return(list(relatedTerms = c("Peptic ulcer", "Gastritis"), rationale = "differentials"))
      }
      if (kind == "ae1Selection") {
        return(list(relatedConceptIds = 192671L, rationale = "related only"))
      }
      stop("unexpected structured call: ", kind)
    }
  )
  testthat::local_mocked_bindings(
    ithAe1DefaultSearch = function(term) {
      data.frame(
        conceptId = 192671L, conceptName = "Gastrointestinal hemorrhage", domainId = "Condition",
        vocabularyId = "SNOMED", conceptClassId = "Clinical Finding", standardConcept = "S",
        stringsAsFactors = FALSE
      )
    },
    .package = "Ithaca"
  )
  spec <- c(rubricSpec(conditionName), list(randomCohortDefinitionId = 2L))

  # 1. Rubric creation.
  createRubrics(outputDirectory, llm, specs = list(spec), saveSummary = FALSE)
  concise <- readConciseRubric(outputDirectory, conditionName = conditionName)
  ruleIds <- c("HE1", "AE1", "AE2", "ER1", "ER2", "ER3", "ER4")
  expect_equal(concise$ruleId, ruleIds)

  # 2. Concept set creation, with the builder, embeddings, and Hecate replaced.
  builderState <- mockConceptServices(
    builder = endToEndBuilder,
    embeddings = local({
      base <- fakeEmbeddingFunction(fallback = c(1, 0, 0))
      function(text, ...) if (any(grepl("relevant", text, ignore.case = TRUE))) list(c(0, 0, 1)) else base(text, ...)
    })
  )
  createConceptSets(outputDirectory, conditions = conditionName, database = database, llm = llm)
  expect_gte(length(builderState$builderCalls), 5L)
  matched <- readRDS(file.path(outputDirectory, conditionName, "matchedConceptSets_V1.rds"))
  expect_true(all(c("gi_bleed_diagnosis_codes", "tiotropium_medication_codes", "gi_bleed_relevant_diagnosis_codes") %in% tolower(matched$csName)))
  expect_true("gi_bleed_relevant_diagnosis_codes" %in% tolower(libraryConceptSets(outputDirectory)[[1]]$conceptSetName))

  # 3. Cohorts and scoring, from the saved rubric and library.
  output <- suppressMessages(utils::capture.output(
    scored <- Ithaca:::ithScoreSubjects(
      conditionSpecs = list(spec), databases = list(Eunomia = database), outputDirectory = outputDirectory,
      seedLibraryPath = libraryPathFor(outputDirectory), generateCohorts = TRUE,
      cohortDefinitionSet = eunomiaCohortDefinitions(), llmClient = llm, verbose = FALSE
    )
  ))

  directory <- scoringDirectory(outputDirectory, conditionName)
  counts <- utils::read.csv(file.path(directory, "subjectCount_Cohort2_V1.csv"), stringsAsFactors = FALSE)
  llmRuleCounts <- vapply(endToEndRules, function(rule) {
    key <- if (grepl("^gi_bleed", rule$set)) "gi_bleed" else sub("_.*$", "", rule$set)
    if (!key %in% names(endToEndConcepts)) 0L else length(expectedRuleSubjects(
      database, eunomiaDescendants(database, endToEndConcepts[[key]]), rule$days
    ))
  }, integer(1))
  # AE1 fires for subjects with no relevant diagnosis within 30 days of index.
  withDiagnosis <- length(expectedRuleSubjects(database, eunomiaDescendants(database, 192671L), 30L))
  ae1Count <- nrow(eunomiaCohortRows(database, 2)) - withDiagnosis
  expectedCounts <- c(llmRuleCounts[1], ae1Count, llmRuleCounts[-1])
  expect_equal(counts$subjectCount[match(ruleIds, counts$ruleId)], expectedCounts)
  expect_gt(expectedCounts[[4]], 0L)

  # 4. Totals follow from the rule scores, and the case labels from the 10/2 thresholds.
  sumScores <- readRDS(file.path(directory, "sumCohortScores_Cohort2_V1.rds"))
  expect_equal(nrow(sumScores), nrow(eunomiaCohortRows(database, 2)))
  full <- readRDS(file.path(directory, "fullScoringFile_Cohort2_V1.rds"))
  perSubject <- tapply(full$score, full$subjectId, sum)
  expectedTotal <- as.numeric(perSubject[as.character(sumScores$subjectId)])
  expectedTotal[is.na(expectedTotal)] <- 0
  expect_equal(as.numeric(sumScores$total), expectedTotal)
  expect_equal(unique(sumScores$yesScore), 10)
  expect_equal(unique(sumScores$noScore), 2)
  expect_equal(
    as.numeric(sumScores$case),
    ifelse(sumScores$total >= 10, 1, ifelse(sumScores$total > 2, 0, -1))
  )

  # 5. The phenotype report accepts the scoring output.
  report <- suppressMessages(utils::capture.output(
    result <- Ithaca::testPhenotypeAlgorithmRubric(
      phenotype = conditionName, analysisName = "endToEnd", runDateTime = "20260101000000",
      connectionDetails = database$connectionDetails,
      evaluationCohortFile = file.path(directory, "sumCohortScores_Cohort2_V1.rds"),
      phenotypeCohortId = 1, cdmDatabaseSchema = "main", databaseId = "Eunomia",
      cohortDatabaseSchema = "main", cohortTable = database$cohortTable,
      splayPrior = 30, splayPost = 30, randomCohortDefinitionId = 2
    )
  ))
  expect_equal(
    result$truePositives + result$falsePositives + result$trueNegatives + result$falseNegatives,
    result$totalSubjects
  )
  expect_gt(result$totalSubjects, 0)
})

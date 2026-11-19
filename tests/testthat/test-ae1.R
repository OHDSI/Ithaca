hecateStub <- function(term) {
  data.frame(
    conceptId = 192671L, conceptName = "Gastrointestinal hemorrhage", domainId = "Condition",
    vocabularyId = "SNOMED", conceptClassId = "Clinical Finding", standardConcept = "S",
    stringsAsFactors = FALSE
  )
}

ae1Llm <- function(conditionIds = 192671L) {
  rubricLlm(
    responder = function(n) stop("unexpected rubric call"),
    other = function(kind, prompt, call) {
      if (kind == "ae1Terms") {
        return(list(relatedTerms = c("Gastritis", "gi BLEED"), rationale = "x"))
      }
      list(relatedConceptIds = conditionIds, rationale = "x")
    }
  )
}

test_that("ithAe1ParsePenalty defaults blanks and rejects bad values", {
  expect_equal(Ithaca:::ithAe1ParsePenalty(NULL), 3)
  expect_equal(Ithaca:::ithAe1ParsePenalty(""), 3)
  expect_equal(Ithaca:::ithAe1ParsePenalty(NA), 3)
  expect_equal(Ithaca:::ithAe1ParsePenalty("5"), 5)
  expect_equal(Ithaca:::ithAe1ParsePenalty(2.5), 2.5)
  expect_error(Ithaca:::ithAe1ParsePenalty("-1"))
  expect_error(Ithaca:::ithAe1ParsePenalty("abc"))
  expect_error(Ithaca:::ithAe1ParsePenalty(0))
})

test_that("AE1 names and SQL follow the condition", {
  expect_equal(Ithaca:::ithAe1ConceptSetName("GI bleed"), "gi_bleed_RELEVANT_DIAGNOSIS_CODES")
  sql <- Ithaca:::ithAe1Sql(Ithaca:::ithAe1NeededConceptSets("GI bleed"))
  expect_match(sql, "NOT EXISTS", fixed = TRUE)
  expect_match(sql, "IN (@gi_bleed_RELEVANT_DIAGNOSIS_CODES, @gi_bleed_DIAGNOSIS_CODES)", fixed = TRUE)
  row <- Ithaca:::ithAe1RuleRow("GI bleed", 4)
  expect_equal(row$points, -4)
  expect_equal(row$conceptSets_needed, "gi_bleed_RELEVANT_DIAGNOSIS_CODES,gi_bleed_DIAGNOSIS_CODES")
})

test_that("ithBuildAe1ConceptSet keeps confirmed concepts with descendants", {
  built <- Ithaca:::ithBuildAe1ConceptSet("GI bleed", ae1Llm(), searchFunction = hecateStub, verbose = FALSE)
  expect_equal(built$conceptIds, 192671L)
  expect_true(built$conceptSet$items[[1]]$includeDescendants)
  expect_equal(built$relatedTerms, "Gastritis")
})

test_that("ithBuildAe1ConceptSet falls back to a placeholder set when nothing is confirmed", {
  expect_warning(
    built <- Ithaca:::ithBuildAe1ConceptSet(
      "GI bleed", ae1Llm(conditionIds = 1L), searchFunction = hecateStub, verbose = FALSE
    ),
    "placeholder"
  )
  expect_true(built$placeholder)
  expect_length(built$conceptIds, 0L)
  expect_equal(built$conceptSet$items[[1]]$concept$CONCEPT_ID, 36433327L)
  expect_warning(
    built <- Ithaca:::ithBuildAe1ConceptSet(
      "GI bleed", ae1Llm(), searchFunction = function(term) hecateStub(term)[0, ], verbose = FALSE
    ),
    "no concepts"
  )
  expect_true(built$placeholder)
})

test_that("ithEnsureAe1ConceptSetInLibrary creates a version once", {
  testthat::local_mocked_bindings(
    ithGetEmbeddingVectorsInternal = function(text, ...) list(c(1, 0, 0)), .package = "Ithaca"
  )
  directory <- withr::local_tempdir()
  libraryPath <- file.path(directory, "lib.duckdb")
  first <- Ithaca:::ithEnsureAe1ConceptSetInLibrary(
    "GI bleed", libraryPath, ae1Llm(), searchFunction = hecateStub, verbose = FALSE
  )
  expect_equal(first$status, "created")
  versionAfterFirst <- Ithaca:::ithDuckdbGetLatestLibraryVersion(libraryPath)
  second <- Ithaca:::ithEnsureAe1ConceptSetInLibrary(
    "GI bleed", libraryPath, ae1Llm(), searchFunction = hecateStub, verbose = FALSE
  )
  expect_equal(second$status, "existing")
  expect_equal(Ithaca:::ithDuckdbGetLatestLibraryVersion(libraryPath), versionAfterFirst)
})

test_that("ithEnsureAe1ConceptSetInLibrary writes Hecate terms, candidates and the expression", {
  testthat::local_mocked_bindings(
    ithGetEmbeddingVectorsInternal = function(text, ...) list(c(1, 0, 0)), .package = "Ithaca"
  )
  directory <- withr::local_tempdir()
  artifactDirectory <- file.path(directory, "sets", "gi_bleed_relevant_diagnosis_codes")
  Ithaca:::ithEnsureAe1ConceptSetInLibrary(
    "GI bleed", file.path(directory, "lib.duckdb"), ae1Llm(), searchFunction = hecateStub,
    artifactDirectory = artifactDirectory, verbose = FALSE
  )
  expect_equal(utils::read.csv(file.path(artifactDirectory, "hecateSearchTerms.csv"))$searchTerm, "Gastritis")
  candidates <- utils::read.csv(file.path(artifactDirectory, "hecateCandidates.csv"))
  expect_equal(candidates$conceptId, 192671L)
  expect_true(candidates$selected)
  expression <- jsonlite::fromJSON(file.path(artifactDirectory, "conceptSetExpression.json"), simplifyVector = FALSE)
  expect_equal(expression$items[[1]]$concept$CONCEPT_ID, 192671L)
})

test_that("exportRelevantDiagnosisConceptSets writes the json and candidate csv for each relevant set", {
  directory <- withr::local_tempdir()
  Ithaca:::ithDuckdbInitializeLibrary(libraryPathFor(directory), verbose = FALSE)
  addLibraryConceptSet(directory, "gi_bleed_RELEVANT_DIAGNOSIS_CODES")
  addLibraryConceptSet(directory, "gi_bleed_DIAGNOSIS_CODES")
  configPath <- withr::local_tempfile(fileext = ".yaml")
  setsFolder <- file.path(directory, "sets")
  writeLines(paste0(
    "configVersion: 1\ngeneral:\n  outputDirectory: ", directory, "\n  conceptSetFolder: ", setsFolder, "\n  verbose: false\n",
    "  dxCriteria:\n    outpatientOnly:\n      criteria: \"Two outpatient codes.\"\n      additionalPoints: 0\n",
    "  conditionSpecs:\n    - testCondition: GI bleed\n      dxCriteria: outpatientOnly\n      rubricName: GI bleed\n"
  ), configPath)

  exported <- suppressMessages(exportRelevantDiagnosisConceptSets(configPath))

  expect_equal(exported$conceptSetName, "gi_bleed_relevant_diagnosis_codes")
  folder <- file.path(setsFolder, "gi_bleed_relevant_diagnosis_codes")
  expect_equal(utils::read.csv(file.path(folder, "hecateCandidates.csv"))$conceptId, 192671L)
  expect_equal(
    jsonlite::fromJSON(file.path(folder, "conceptSetExpression.json"), simplifyVector = FALSE)$items[[1]]$concept$CONCEPT_ID,
    192671L
  )
  expect_false(dir.exists(file.path(setsFolder, "gi_bleed_diagnosis_codes")))
})

test_that("ithAe1ShiftIds moves AE IDs up by one", {
  expect_equal(Ithaca:::ithAe1ShiftIds(c("AE1", "AE2", "ER1")), c("AE2", "AE3", "ER1"))
})

migrationSetup <- function(env = parent.frame()) {
  outputDirectory <- withr::local_tempdir(.local_envir = env)
  libraryPath <- writeEunomiaRubric(outputDirectory)
  configPath <- withr::local_tempfile(fileext = ".yaml", .local_envir = env)
  writeLines(paste0(
    "configVersion: 1\ngeneral:\n  outputDirectory: ", outputDirectory, "\n  verbose: false\n",
    "  dxCriteria:\n    outpatientOnly:\n      criteria: \"Two outpatient codes.\"\n      additionalPoints: 0\n",
    "  conditionSpecs:\n    - testCondition: GI bleed\n      dxCriteria: outpatientOnly\n      rubricName: GI bleed\n"
  ), configPath)
  list(outputDirectory = outputDirectory, libraryPath = libraryPath, configPath = configPath)
}

test_that("addZeroOccurrencePenaltyRule shifts AE IDs, adds the concept set, and is idempotent", {
  testthat::local_mocked_bindings(
    ithAe1DefaultSearch = hecateStub,
    ithGetEmbeddingVectorsInternal = function(text, ...) list(c(1, 0, 0)),
    .package = "Ithaca"
  )
  setup <- migrationSetup()

  dry <- suppressMessages(addZeroOccurrencePenaltyRule(
    configPath = setup$configPath, llmClient = ae1Llm(), dryRun = TRUE, verbose = FALSE
  ))
  expect_equal(dry$status, "wouldAdd")
  expect_false("AE2" %in% Ithaca:::ithLoadLatestShortRubric("GI bleed", setup$outputDirectory)$ruleId)

  result <- suppressMessages(addZeroOccurrencePenaltyRule(
    configPath = setup$configPath, llmClient = ae1Llm(), verbose = FALSE
  ))
  expect_equal(result$status, "added")
  expect_equal(result$conceptSetStatus, "created")
  shortRubric <- Ithaca:::ithLoadLatestShortRubric("GI bleed", setup$outputDirectory)
  expect_setequal(shortRubric$ruleId, c("ER1", "ER2", "AE1", "AE2"))
  ae1 <- shortRubric[shortRubric$ruleId == "AE1", ]
  expect_equal(ae1$score, -3)
  expect_match(ae1$sqlStatement, "NOT EXISTS", fixed = TRUE)
  expect_equal(shortRubric$criteria[shortRubric$ruleId == "AE2"], "arthritis")

  matched <- Ithaca:::ithLoadLatest(file.path(setup$outputDirectory, "GI bleed"), "matchedConceptSets.rds")
  expect_true("gi_bleed_relevant_diagnosis_codes" %in% tolower(matched$csName))

  again <- suppressMessages(addZeroOccurrencePenaltyRule(
    configPath = setup$configPath, llmClient = ae1Llm(), verbose = FALSE
  ))
  expect_equal(again$status, "alreadyPresent")
})

test_that("addZeroOccurrencePenaltyRule leaves files untouched when the concept set build errors", {
  testthat::local_mocked_bindings(
    ithAe1DefaultSearch = hecateStub,
    ithBuildAe1ConceptSet = function(...) stop("llm down"),
    .package = "Ithaca"
  )
  setup <- migrationSetup()
  before <- Ithaca:::ithLoadLatestShortRubric("GI bleed", setup$outputDirectory)

  result <- suppressMessages(addZeroOccurrencePenaltyRule(
    configPath = setup$configPath, llmClient = ae1Llm(), verbose = FALSE
  ))
  expect_equal(result$status, "failed")
  expect_equal(Ithaca:::ithLoadLatestShortRubric("GI bleed", setup$outputDirectory), before)
})

test_that("zeroOccurrencePenalty comes from the config default or the condition", {
  setup <- migrationSetup()
  config <- suppressMessages(Ithaca:::ithReadIthacaConditionConfig(setup$configPath))
  expect_equal(config$conditionSpecs[[1]]$zeroOccurrencePenalty, 3)
})

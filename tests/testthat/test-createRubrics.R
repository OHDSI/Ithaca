test_that("a rubric run saves the rubric list, short and concise rubrics, summary, and checkpoint", {
  outputDirectory <- withr::local_tempdir()
  llm <- rubricLlm(text = function(prompt) "select 1 as fake_sql")

  createRubrics(outputDirectory, llm)

  for (file in c("rubricList_V1.rds", "COPD_shortRubric_V1.rds", "COPD_conciseRubric_V1.csv",
                 "COPD_RuleSummary_V1.txt", "COPD_rubricCheckpoint.rds")) {
    expect_true(file.exists(conditionPath(outputDirectory, file)), info = file)
  }
  expect_equal(readRDS(conditionPath(outputDirectory, "COPD_rubricCheckpoint.rds"))$status, "completed")
  expect_equal(llm$state$kinds, "rubric")

  concise <- readConciseRubric(outputDirectory)
  expect_equal(concise$ruleId, c("HE1", "AE1", "AE2", "ER1", "ER2", "ER3", "ER4"))
  expect_equal(concise$type, c("hard_exclusion", rep("alternativeExplanations", 2), rep("evidenceRules", 4)))
  expect_equal(concise$score, c(-100, -3, -4, 3, 2, 4, 3))
  expect_true(all(concise$sqlStatement[concise$ruleId != "AE1"] == "select 1 as fake_sql"))
  expect_match(concise$sqlStatement[concise$ruleId == "AE1"], "NOT EXISTS", fixed = TRUE)
  expect_match(concise$sqlStatement[concise$ruleId == "AE1"], "@copd_RELEVANT_DIAGNOSIS_CODES", fixed = TRUE)
  expect_equal(concise$neededConceptSets[concise$ruleId == "AE1"], "copd_RELEVANT_DIAGNOSIS_CODES,copd_DIAGNOSIS_CODES")
  expect_match(concise$sqlStatement[concise$ruleId == "AE1"], "@copd_DIAGNOSIS_CODES", fixed = TRUE)
  expect_true(all(nzchar(concise$neededConceptSets)))
  expect_equal(unique(concise$rubricYesScore), 10)
  expect_equal(unique(concise$rubricNoScore), 2)

  rubricList <- readRDS(conditionPath(outputDirectory, "rubricList_V1.rds"))
  expect_equal(nrow(rubricList$neededConcepts), 7L)
  expect_equal(readLines(conditionPath(outputDirectory, "COPD_RuleSummary_V1.txt")), "select 1 as fake_sql")
})

test_that("the prompt is fully filled in with the condition, diagnosis criteria, and template", {
  outputDirectory <- withr::local_tempdir()
  llm <- rubricLlm()

  createRubrics(outputDirectory, llm, specs = list(rubricSpec(information = "Exclude pregnancy-related cases.")))

  prompt <- llm$state$prompts[[1]]
  expect_match(prompt, "COPD", fixed = TRUE)
  expect_match(prompt, "Two outpatient codes within -60 to +60 of index.", fixed = TRUE)
  expect_match(prompt, "Exclude pregnancy-related cases.", fixed = TRUE)
  expect_match(prompt, "Template A", fixed = TRUE)
  for (placeholder in c("<PHENOTYPE_NAME>", "<RUBRIC_SPECIFICS>", "<DX_CRITERA>", "<ER2_POINTS>",
                        "<DX_RULE_POINTS>", "<ADDITIONAL_INFORMATION>", "<EXCLUDE_LABS>", "<INCLUDE_LABS>")) {
    expect_false(grepl(placeholder, prompt, fixed = TRUE), info = placeholder)
  }
})

test_that("labs are excluded from the prompt unless includeLabs is set", {
  withLabs <- rubricLlm()
  withoutLabs <- rubricLlm()

  createRubrics(withr::local_tempdir(), withLabs, includeLabs = TRUE)
  createRubrics(withr::local_tempdir(), withoutLabs, includeLabs = FALSE)

  expect_match(withLabs$state$prompts[[1]], "- Lab values and results", fixed = TRUE)
  expect_match(withoutLabs$state$prompts[[1]], "- Lab values or results", fixed = TRUE)
})

test_that("the fixed diagnosis rules and thresholds replace whatever the LLM returned", {
  outputDirectory <- withr::local_tempdir()
  wrong <- function(n) {
    response <- makeFakeRubricResponse(paste0("R", n), thresholds = list(yes = 8L, no = 3L, insufficient = "x"))
    response$evidenceRules$points[response$evidenceRules$ruleId == "ER1"] <- 5L
    response
  }

  expect_message(
    createRubrics(outputDirectory, rubricLlm(wrong), quiet = FALSE),
    "Enforced fixed ER1/ER2/AE1 and YES/NO thresholds"
  )

  concise <- readConciseRubric(outputDirectory)
  expect_equal(concise$score[concise$ruleId == "ER1"], 3)
  expect_equal(unique(concise$rubricYesScore), 10)
  expect_equal(unique(concise$rubricNoScore), 2)
  expect_equal(concise$criteria[concise$ruleId == "ER2"], "Two outpatient codes within -60 to +60 of index.")
  expect_match(concise$neededConceptSets[concise$ruleId == "ER2"], "copd_DIAGNOSIS_CODES", fixed = TRUE)
  expect_match(concise$neededConceptSets[concise$ruleId == "ER2"], "outpatient_VISIT_CODES", fixed = TRUE)
})

test_that("the diagnosis-criteria choice sets the points of ER2", {
  outputDirectory <- withr::local_tempdir()

  createRubrics(outputDirectory, rubricLlm(), specs = list(rubricSpec(extraPoints = 2L)))

  concise <- readConciseRubric(outputDirectory)
  expect_equal(concise$score[concise$ruleId == "ER2"], 4)
  expect_equal(sum(concise$score[concise$ruleId %in% c("ER1", "ER2")]), 7)
})

test_that("an incomplete response is retried and a persistent one stops the run without saving", {
  incomplete <- function(n) makeFakeRubricResponse(paste0("R", n), evidence = data.frame(ruleId = character()))
  recovering <- rubricLlm(function(n) if (n == 1L) incomplete(n) else makeFakeRubricResponse("R1"))

  outputDirectory <- withr::local_tempdir()
  createRubrics(outputDirectory, recovering)
  expect_equal(recovering$state$kinds, c("rubric", "rubric"))
  expect_true(file.exists(conditionPath(outputDirectory, "COPD_conciseRubric_V1.csv")))

  failing <- rubricLlm(incomplete)
  failedDirectory <- withr::local_tempdir()
  expect_error(
    createRubrics(failedDirectory, failing, rubricResponseAttempts = 2L),
    "incomplete rubric for 'COPD'.*on all 2 attempt"
  )
  expect_equal(failing$state$kinds, c("rubric", "rubric"))
  expect_false(file.exists(conditionPath(failedDirectory, "COPD_conciseRubric_V1.csv")))
})

test_that("existing rubric artifacts are kept when useExistingRubric is set", {
  outputDirectory <- withr::local_tempdir()
  createRubrics(outputDirectory, rubricLlm())
  llm <- rubricLlm()

  createRubrics(outputDirectory, llm, useExistingRubric = TRUE)

  expect_equal(llm$state$structuredCalls, 0L)
  expect_length(llm$state$prompts, 0L)
  expect_false(file.exists(conditionPath(outputDirectory, "COPD_conciseRubric_V2.csv")))
})

test_that("without useExistingRubric a new rubric version is generated", {
  outputDirectory <- withr::local_tempdir()
  createRubrics(outputDirectory, rubricLlm())
  llm <- rubricLlm()

  createRubrics(outputDirectory, llm, useExistingRubric = FALSE)

  expect_equal(llm$state$kinds, "rubric")
  expect_true(file.exists(conditionPath(outputDirectory, "COPD_conciseRubric_V2.csv")))
  expect_true(file.exists(conditionPath(outputDirectory, "rubricList_V2.rds")))
})

test_that("a run that fails part way resumes from its checkpoint without repeating work", {
  outputDirectory <- withr::local_tempdir()
  successes <- 0L
  firstRun <- rubricLlm(text = function(prompt) {
    successes <<- successes + 1L
    if (successes > 2L) stop("llm went away")
    "select 1 -- run1"
  })

  expect_error(createRubrics(outputDirectory, firstRun), "LLM query failed")
  checkpoint <- readRDS(conditionPath(outputDirectory, "COPD_rubricCheckpoint.rds"))
  expect_equal(checkpoint$status, "buildingShortRubric")
  expect_equal(nrow(checkpoint$shortRubric), 3L)
  expect_false(file.exists(conditionPath(outputDirectory, "COPD_conciseRubric_V1.csv")))

  secondRun <- rubricLlm(text = function(prompt) "select 1 -- run2")
  createRubrics(outputDirectory, secondRun)

  expect_equal(secondRun$state$structuredCalls, 0L)
  expect_length(secondRun$state$prompts, 5L)
  concise <- readConciseRubric(outputDirectory)
  expect_match(concise$sqlStatement[concise$ruleId %in% c("HE1", "AE2")], "run1")
  expect_match(concise$sqlStatement[concise$type == "evidenceRules"], "run2")
  expect_equal(readRDS(conditionPath(outputDirectory, "COPD_rubricCheckpoint.rds"))$status, "completed")
})

test_that("several templates produce several rubrics for one condition", {
  outputDirectory <- withr::local_tempdir()
  llm <- rubricLlm()

  createRubrics(outputDirectory, llm, rubricTemplates = c("Template A", "Template B"))

  expect_equal(llm$state$kinds, c("rubric", "rubric"))
  expect_match(llm$state$prompts[[1]], "Template A", fixed = TRUE)
  expect_match(llm$state$prompts[[2]], "Template B", fixed = TRUE)
  concise <- readConciseRubric(outputDirectory)
  expect_setequal(unique(concise$rubric), c("R1", "R2"))
  expect_length(readRDS(conditionPath(outputDirectory, "rubricList_V1.rds"))$rubricList, 2L)
})

test_that("the summary can be turned off", {
  outputDirectory <- withr::local_tempdir()
  llm <- rubricLlm()

  createRubrics(outputDirectory, llm, saveSummary = FALSE)

  expect_false(file.exists(conditionPath(outputDirectory, "COPD_RuleSummary_V1.txt")))
  expect_length(llm$state$prompts, 1L + 6L)
})

test_that("each condition gets its own rubric folder", {
  outputDirectory <- withr::local_tempdir()
  llm <- rubricLlm()

  createRubrics(outputDirectory, llm, specs = list(rubricSpec("COPD"), rubricSpec("Asthma")))

  expect_true(file.exists(conditionPath(outputDirectory, "COPD_conciseRubric_V1.csv", "COPD")))
  expect_true(file.exists(conditionPath(outputDirectory, "Asthma_conciseRubric_V1.csv", "Asthma")))
  expect_equal(llm$state$kinds, c("rubric", "rubric"))
})

test_that("library hints rename a requested concept set to a matching library set", {
  outputDirectory <- withr::local_tempdir()
  Ithaca:::ithDuckdbInitializeLibrary(libraryPathFor(outputDirectory), verbose = FALSE)
  addLibraryConceptSet(
    outputDirectory, "long_acting_muscarinic_antagonist_MEDICATION_CODES", embedding = vecWithCosine(0.95)
  )
  testthat::local_mocked_bindings(
    ithGetEmbeddingVectorsInternal = fakeEmbeddingFunction(list(tiotropium = c(1, 0, 0)), fallback = c(0, 0, 1)),
    ithGetHecateCandidateDetails = function(targetName, limit = 5, llmClient = NULL) {
      list(searchTerm = "", candidates = character(), rawCandidates = character(),
           rejectedCandidates = character(), usedFallback = FALSE)
    },
    .package = "Ithaca"
  )
  decisions <- data.frame(
    requestedConceptSetName = "tiotropium_MEDICATION_CODES",
    chosenConceptSetName = "long_acting_muscarinic_antagonist_MEDICATION_CODES",
    decisionSource = "library_preferred", rationale = "same drug class", stringsAsFactors = FALSE
  )
  llm <- rubricLlm(other = function(kind, prompt, call) decisions)

  createRubrics(outputDirectory, llm, useLibraryHints = TRUE)

  expect_equal(llm$state$kinds, c("rubric", "hintDecisions"))
  expect_match(llm$state$prompts[[2]], "tiotropium_medication_codes", fixed = TRUE)
  concise <- readConciseRubric(outputDirectory)
  expect_equal(concise$neededConceptSets[concise$ruleId == "ER3"], "long_acting_muscarinic_antagonist_medication_codes")
  audit <- utils::read.csv(conditionPath(outputDirectory, "rubricConceptSetDecisions_V1.csv"), stringsAsFactors = FALSE)
  expect_true(audit$applied)
  expect_true(audit$changed)
  expect_true(file.exists(conditionPath(outputDirectory, "conceptSetCandidateHints_V1.csv")))
})

test_that("hints are skipped quietly when there is no concept set library yet", {
  outputDirectory <- withr::local_tempdir()
  llm <- rubricLlm()
  testthat::local_mocked_bindings(ithGetLlmClientFromConfig = function(...) llm, .package = "Ithaca")
  withr::local_options(ithaca.yesCombinationReport = FALSE)

  result <- captureMessages(Ithaca:::ithCreateRubrics(
    list(rubricSpec()), outputDirectory, rubricTemplates = "Template A", llmClient = llm,
    useLibraryHints = TRUE, verbose = TRUE
  ))

  expect_true(any(grepl("Concept set hint pass skipped: no concept set name library available.", result$messages, fixed = TRUE)))
  expect_equal(llm$state$kinds, "rubric")
  expect_true(file.exists(conditionPath(outputDirectory, "COPD_conciseRubric_V1.csv")))
})

test_that("rubrics created here can be reviewed and edited by the later steps", {
  outputDirectory <- withr::local_tempdir()
  createRubrics(outputDirectory, rubricLlm())

  concise <- readConciseRubric(outputDirectory)
  expect_silent(Ithaca:::ithValidateEditedConciseRubric(concise, conditionName = "COPD"))
  shortRubric <- Ithaca:::ithLoadLatestShortRubric("COPD", outputDirectory)
  expect_false(Ithaca:::ithDiffConciseRubric(concise, shortRubric)$differs)
})

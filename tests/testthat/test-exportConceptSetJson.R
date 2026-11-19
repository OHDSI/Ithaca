test_that("exportConceptSetJson returns the library concept set as ATLAS-style JSON text", {
  outputDirectory <- withr::local_tempdir()
  Ithaca:::ithDuckdbInitializeLibrary(libraryPathFor(outputDirectory), verbose = FALSE)
  addLibraryConceptSet(outputDirectory, "Asthma_DIAGNOSIS_CODES", conceptSet = conceptSetWithConcept(317009L, "Asthma"))

  json <- exportConceptSetJson(outputDirectory, "asthma diagnosis codes", writeFile = FALSE)

  expect_s3_class(json, "json")
  expect_true(jsonlite::validate(json))
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(parsed$items[[1]]$concept$CONCEPT_ID, 317009)
  expect_false(parsed$items[[1]]$isExcluded)
  expect_true(parsed$items[[1]]$includeDescendants)
})

test_that("exportConceptSetJson matches names exactly and writes to the output directory", {
  outputDirectory <- withr::local_tempdir()
  Ithaca:::ithDuckdbInitializeLibrary(libraryPathFor(outputDirectory), verbose = FALSE)
  addLibraryConceptSet(outputDirectory, "Asthma_DIAGNOSIS_CODES")
  exportDirectory <- file.path(outputDirectory, "exports")

  expect_message(
    json <- exportConceptSetJson(outputDirectory, "ASTHMA_DIAGNOSIS_CODES", pretty = FALSE, outputDirectory = exportDirectory),
    "Asthma_DIAGNOSIS_CODES.json"
  )

  outputFile <- file.path(exportDirectory, "Asthma_DIAGNOSIS_CODES.json")
  expect_equal(paste(readLines(outputFile), collapse = "\n"), as.character(json))
  expect_message(
    missingConceptSet <- exportConceptSetJson(
      outputDirectory,
      "Asthma_DIAGNOSIS_CODE_",
      writeFile = FALSE
    ),
    "Concept set not found in library"
  )
  expect_null(missingConceptSet)
})

test_that("exportConceptSetJson writes to the config outputDirectory by default", {
  outputDirectory <- withr::local_tempdir()
  Ithaca:::ithDuckdbInitializeLibrary(libraryPathFor(outputDirectory), verbose = FALSE)
  addLibraryConceptSet(outputDirectory, "Asthma_DIAGNOSIS_CODES")
  configPath <- withr::local_tempfile(fileext = ".yaml")
  writeLines(paste0(
    "configVersion: 1\ngeneral:\n  outputDirectory: ", outputDirectory, "\n",
    "  dxCriteria:\n    outpatientOnly:\n      criteria: \"Two outpatient codes.\"\n      additionalPoints: 0\n",
    "  conditionSpecs:\n    - testCondition: COPD\n      dxCriteria: outpatientOnly\n      rubricName: COPD\n"
  ), configPath)

  suppressMessages(exportConceptSetJson(outputDirectory, "Asthma_DIAGNOSIS_CODES", configPath = configPath))

  expect_true(file.exists(file.path(outputDirectory, "Asthma_DIAGNOSIS_CODES.json")))
})

test_that("exportConceptSetJson can copy the JSON to the clipboard", {
  outputDirectory <- withr::local_tempdir()
  Ithaca:::ithDuckdbInitializeLibrary(libraryPathFor(outputDirectory), verbose = FALSE)
  addLibraryConceptSet(outputDirectory, "Asthma_DIAGNOSIS_CODES")
  copied <- NULL
  local_mocked_bindings(ithCopyToClipboard = function(text) copied <<- text)

  expect_message(json <- exportConceptSetJson(outputDirectory, "Asthma_DIAGNOSIS_CODES", writeFile = FALSE, copyToClipboard = TRUE), "clipboard")

  expect_equal(copied, as.character(json))
})

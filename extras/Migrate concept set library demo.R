# Preview only
migrateConceptSetLibraryAliases(
  outputDirectory = "P:/llm/rubricsNew",
  conceptLibraryFile = "rubricConceptSetLibrary.rds",
  dryRun = TRUE,
  verbose = TRUE
)

# Apply
migrateConceptSetLibraryAliases(
  outputDirectory = "P:/llm/rubricsNew",
  conceptLibraryFile = "rubricConceptSetLibrary.rds",
  dryRun = FALSE,
  verbose = TRUE
)

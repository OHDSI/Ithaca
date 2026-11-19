ithDefaultConceptLibraryFileName <- function() {
  "rubricConceptSetLibrary.duckdb"
}

ithResolveDuckdbLibraryPath <- function(libraryPath,
                                        defaultFileName = ithDefaultConceptLibraryFileName(),
                                        mustExist = FALSE) {
  if (is.null(libraryPath) || !nzchar(as.character(libraryPath[[1]]))) {
    stop("libraryPath must be a non-empty path to a DuckDB file or directory")
  }

  inputPath <- as.character(libraryPath[[1]])
  resolvedPath <- NULL

  if (dir.exists(inputPath)) {
    resolvedPath <- file.path(inputPath, defaultFileName)
  } else {
    ext <- tolower(tools::file_ext(inputPath))
    if (ext == "duckdb") {
      parentDirectory <- dirname(inputPath)
      if (!dir.exists(parentDirectory)) {
        stop("Concept set library directory does not exist: ", parentDirectory)
      }
      resolvedPath <- inputPath
    } else {
      stop("Concept set library must be a .duckdb file or a directory: ", inputPath)
    }
  }

  if (mustExist && !file.exists(resolvedPath)) {
    stop("Concept set DuckDB library does not exist: ", resolvedPath)
  }

  resolvedPath
}

ithConnectDuckdbConceptLibrary <- function(libraryPath, readOnly = FALSE) {
  DBI::dbConnect(duckdb::duckdb(), dbdir = libraryPath, read_only = isTRUE(readOnly))
}

ithDisconnectDuckdbConceptLibrary <- function(connection) {
  if (!is.null(connection)) {
    tryCatch(DBI::dbDisconnect(connection, shutdown = TRUE), error = function(e) NULL)
  }
}

ithInitializeDuckdbConceptLibrarySchema <- function(connection) {
  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE IF NOT EXISTS conceptSetRevision (",
      "revisionId BIGINT PRIMARY KEY,",
      "libraryVersion BIGINT NOT NULL,",
      "conceptSetName VARCHAR NOT NULL,",
      "cleanName VARCHAR NOT NULL,",
      "conceptSetJson VARCHAR NOT NULL,",
      "embedJson VARCHAR NOT NULL,",
      "createdAt TIMESTAMP NOT NULL,",
      "runId VARCHAR,",
      "userName VARCHAR,",
      "conditionName VARCHAR,",
      "cohortId BIGINT,",
      "sourceFile VARCHAR",
      ")"
    )
  )

  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE IF NOT EXISTS conceptSetCurrent (",
      "conceptSetName VARCHAR PRIMARY KEY,",
      "cleanName VARCHAR NOT NULL,",
      "conceptSetJson VARCHAR NOT NULL,",
      "embedJson VARCHAR NOT NULL,",
      "libraryVersion BIGINT NOT NULL,",
      "updatedAt TIMESTAMP NOT NULL,",
      "runId VARCHAR,",
      "userName VARCHAR,",
      "conditionName VARCHAR,",
      "cohortId BIGINT,",
      "sourceFile VARCHAR",
      ")"
    )
  )

  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE IF NOT EXISTS libraryVersionHistory (",
      "libraryVersion BIGINT PRIMARY KEY,",
      "createdAt TIMESTAMP NOT NULL,",
      "changeType VARCHAR NOT NULL,",
      "conceptSetName VARCHAR,",
      "cleanName VARCHAR,",
      "runId VARCHAR,",
      "userName VARCHAR,",
      "conditionName VARCHAR,",
      "cohortId BIGINT,",
      "sourceFile VARCHAR,",
      "note VARCHAR",
      ")"
    )
  )

  DBI::dbExecute(
    connection,
    "CREATE INDEX IF NOT EXISTS idxConceptSetRevisionNameVersion ON conceptSetRevision(conceptSetName, libraryVersion)"
  )

  DBI::dbExecute(
    connection,
    "CREATE INDEX IF NOT EXISTS idxConceptSetCurrentVersion ON conceptSetCurrent(libraryVersion)"
  )
}

ithValidateDuckdbConceptLibrarySchema <- function(connection,
                                                  libraryPath = NULL) {
  requiredTables <- c("conceptSetRevision", "conceptSetCurrent", "libraryVersionHistory")
  tableInfo <- DBI::dbGetQuery(connection, "SHOW TABLES")

  tableColumn <- if ("name" %in% names(tableInfo)) {
    "name"
  } else {
    names(tableInfo)[[1]]
  }

  existingTables <- tolower(as.character(tableInfo[[tableColumn]]))
  missingTables <- requiredTables[!(tolower(requiredTables) %in% existingTables)]
  if (length(missingTables) > 0) {
    locationText <- if (is.null(libraryPath)) "" else paste0(" at ", libraryPath)
    stop(
      "DuckDB concept set library schema is missing required table(s)",
      locationText,
      ": ",
      paste(missingTables, collapse = ", "),
      ". Initialize schema with ithDuckdbInitializeLibrary(...) using write access."
    )
  }

  invisible(TRUE)
}

ithDuckdbLibraryExists <- function(libraryPath) {
  file.exists(libraryPath)
}

ithDuckdbInitializeLibrary <- function(libraryPath,
                                       seedLibraryPath = NULL,
                                       verbose = TRUE) {
  libraryDirectory <- dirname(libraryPath)
  if (!dir.exists(libraryDirectory)) {
    dir.create(libraryDirectory, recursive = TRUE, showWarnings = FALSE)
  }

  connection <- ithConnectDuckdbConceptLibrary(libraryPath, readOnly = FALSE)
  on.exit(ithDisconnectDuckdbConceptLibrary(connection), add = TRUE)
  ithInitializeDuckdbConceptLibrarySchema(connection)

  if (!is.null(seedLibraryPath) && nzchar(as.character(seedLibraryPath[[1]]))) {
    seedPath <- as.character(seedLibraryPath[[1]])
    if (file.exists(seedPath) && tolower(tools::file_ext(seedPath)) == "rds") {
      migrated <- ithMigrateConceptLibraryRdsToDuckdb(
        rdsLibraryPath = seedPath,
        duckdbLibraryPath = libraryPath,
        overwriteExisting = FALSE,
        verbose = verbose
      )
      return(invisible(migrated))
    }
  }

  if (nrow(DBI::dbGetQuery(connection, "SELECT 1 FROM libraryVersionHistory LIMIT 1")) == 0) {
    DBI::dbExecute(
      connection,
      "INSERT INTO libraryVersionHistory(libraryVersion, createdAt, changeType, note) VALUES (1, CURRENT_TIMESTAMP, 'initialize', 'empty library scaffold')"
    )
  }

  invisible(list(libraryPath = libraryPath, initialized = TRUE))
}

ithDuckdbGetLatestLibraryVersion <- function(libraryPath) {
  if (!file.exists(libraryPath)) {
    return(NULL)
  }

  connection <- ithConnectDuckdbConceptLibrary(libraryPath, readOnly = TRUE)
  on.exit(ithDisconnectDuckdbConceptLibrary(connection), add = TRUE)
  ithValidateDuckdbConceptLibrarySchema(connection, libraryPath = libraryPath)

  versionRow <- DBI::dbGetQuery(connection, "SELECT MAX(libraryVersion) AS latestVersion FROM libraryVersionHistory")
  latestVersion <- suppressWarnings(as.integer(versionRow$latestVersion[[1]]))

  if (is.na(latestVersion)) {
    return(NULL)
  }

  latestVersion
}

ithDuckdbGetConceptSetLibrary <- function(libraryPath,
                                          libraryVersion = NULL) {
  if (!file.exists(libraryPath)) {
    stop("Concept set DuckDB library does not exist: ", libraryPath)
  }

  connection <- ithConnectDuckdbConceptLibrary(libraryPath, readOnly = TRUE)
  on.exit(ithDisconnectDuckdbConceptLibrary(connection), add = TRUE)
  ithValidateDuckdbConceptLibrarySchema(connection, libraryPath = libraryPath)

  latestVersion <- ithDuckdbGetLatestLibraryVersion(libraryPath)
  if (is.null(latestVersion)) {
    latestVersion <- 1L
  }

  resolvedVersion <- if (is.null(libraryVersion)) latestVersion else as.integer(libraryVersion[[1]])
  if (is.na(resolvedVersion) || resolvedVersion < 1) {
    stop("libraryVersion must be a positive integer")
  }

  versionExists <- DBI::dbGetQuery(
    connection,
    paste0("SELECT 1 AS ok FROM libraryVersionHistory WHERE libraryVersion = ", resolvedVersion, " LIMIT 1")
  )
  if (nrow(versionExists) == 0) {
    stop("Requested libraryVersion does not exist in concept set library: ", resolvedVersion)
  }

  revisionTable <- DBI::dbGetQuery(
    connection,
    paste0(
      "SELECT conceptSetName, cleanName, conceptSetJson, embedJson ",
      "FROM conceptSetRevision ",
      "WHERE libraryVersion <= ", resolvedVersion, " ",
      "QUALIFY row_number() OVER (PARTITION BY lower(conceptSetName) ORDER BY libraryVersion DESC, revisionId DESC) = 1"
    )
  )

  if (nrow(revisionTable) == 0) {
    return(list(
      data.frame(conceptSetName = character(), cleanName = character(), conceptSet = I(list())),
      data.frame(conceptSetName = character(), cleanName = character(), embed = I(list()))
    ))
  }

  conceptSetList <- lapply(
    revisionTable$conceptSetJson,
    function(value) {
      tryCatch(jsonlite::fromJSON(value, simplifyVector = FALSE), error = function(e) list(items = list()))
    }
  )

  embedList <- lapply(
    revisionTable$embedJson,
    function(value) {
      parsed <- tryCatch(jsonlite::fromJSON(value, simplifyVector = TRUE), error = function(e) NA_real_)
      as.numeric(parsed)
    }
  )

  conceptTable <- data.frame(
    conceptSetName = as.character(revisionTable$conceptSetName),
    cleanName = as.character(revisionTable$cleanName),
    conceptSet = I(conceptSetList),
    stringsAsFactors = FALSE
  )

  embedTable <- data.frame(
    conceptSetName = as.character(revisionTable$conceptSetName),
    cleanName = as.character(revisionTable$cleanName),
    embed = I(embedList),
    stringsAsFactors = FALSE
  )

  list(conceptTable, embedTable)
}

ithDuckdbUpsertConceptSet <- function(libraryPath,
                                      conceptSetName,
                                      cleanName,
                                      conceptSet,
                                      embedding,
                                      runId = NA_character_,
                                      userName = NA_character_,
                                      conditionName = NA_character_,
                                      cohortId = NA_integer_,
                                      sourceFile = NA_character_) {
  if (is.null(conceptSetName) || !nzchar(as.character(conceptSetName[[1]]))) {
    stop("conceptSetName must be a non-empty string")
  }

  if (is.null(cleanName) || !nzchar(as.character(cleanName[[1]]))) {
    stop("cleanName must be a non-empty string")
  }

  if (!file.exists(libraryPath)) {
    ithDuckdbInitializeLibrary(libraryPath = libraryPath, verbose = FALSE)
  }

  connection <- ithConnectDuckdbConceptLibrary(libraryPath, readOnly = FALSE)
  on.exit(ithDisconnectDuckdbConceptLibrary(connection), add = TRUE)
  ithInitializeDuckdbConceptLibrarySchema(connection)

  DBI::dbExecute(connection, "BEGIN TRANSACTION")
  on.exit(tryCatch(DBI::dbExecute(connection, "ROLLBACK"), error = function(e) NULL), add = TRUE)

  latestVersionRow <- DBI::dbGetQuery(connection, "SELECT COALESCE(MAX(libraryVersion), 0) AS maxVersion FROM libraryVersionHistory")
  latestVersion <- as.integer(latestVersionRow$maxVersion[[1]])
  nextVersion <- latestVersion + 1L

  revisionIdRow <- DBI::dbGetQuery(connection, "SELECT COALESCE(MAX(revisionId), 0) AS maxRevisionId FROM conceptSetRevision")
  nextRevisionId <- as.integer(revisionIdRow$maxRevisionId[[1]]) + 1L

  conceptJson <- jsonlite::toJSON(conceptSet, auto_unbox = TRUE, null = "null")
  embedJson <- jsonlite::toJSON(as.numeric(embedding), auto_unbox = FALSE, null = "null")

  DBI::dbExecute(
    connection,
    paste0(
      "INSERT INTO conceptSetRevision(revisionId, libraryVersion, conceptSetName, cleanName, conceptSetJson, embedJson, createdAt, runId, userName, conditionName, cohortId, sourceFile) VALUES (",
      nextRevisionId, ", ",
      nextVersion, ", ",
      DBI::dbQuoteString(connection, as.character(conceptSetName[[1]])), ", ",
      DBI::dbQuoteString(connection, as.character(cleanName[[1]])), ", ",
      DBI::dbQuoteString(connection, conceptJson), ", ",
      DBI::dbQuoteString(connection, embedJson), ", CURRENT_TIMESTAMP, ",
      DBI::dbQuoteString(connection, as.character(runId[[1]])), ", ",
      DBI::dbQuoteString(connection, as.character(userName[[1]])), ", ",
      DBI::dbQuoteString(connection, as.character(conditionName[[1]])), ", ",
      if (is.na(cohortId[[1]])) "NULL" else as.integer(cohortId[[1]]), ", ",
      DBI::dbQuoteString(connection, as.character(sourceFile[[1]])),
      ")"
    )
  )

  DBI::dbExecute(
    connection,
    paste0(
      "DELETE FROM conceptSetCurrent WHERE lower(conceptSetName) = lower(",
      DBI::dbQuoteString(connection, as.character(conceptSetName[[1]])),
      ")"
    )
  )

  DBI::dbExecute(
    connection,
    paste0(
      "INSERT INTO conceptSetCurrent(conceptSetName, cleanName, conceptSetJson, embedJson, libraryVersion, updatedAt, runId, userName, conditionName, cohortId, sourceFile) VALUES (",
      DBI::dbQuoteString(connection, as.character(conceptSetName[[1]])), ", ",
      DBI::dbQuoteString(connection, as.character(cleanName[[1]])), ", ",
      DBI::dbQuoteString(connection, conceptJson), ", ",
      DBI::dbQuoteString(connection, embedJson), ", ",
      nextVersion, ", CURRENT_TIMESTAMP, ",
      DBI::dbQuoteString(connection, as.character(runId[[1]])), ", ",
      DBI::dbQuoteString(connection, as.character(userName[[1]])), ", ",
      DBI::dbQuoteString(connection, as.character(conditionName[[1]])), ", ",
      if (is.na(cohortId[[1]])) "NULL" else as.integer(cohortId[[1]]), ", ",
      DBI::dbQuoteString(connection, as.character(sourceFile[[1]])),
      ")"
    )
  )

  DBI::dbExecute(
    connection,
    paste0(
      "INSERT INTO libraryVersionHistory(libraryVersion, createdAt, changeType, conceptSetName, cleanName, runId, userName, conditionName, cohortId, sourceFile, note) VALUES (",
      nextVersion, ", CURRENT_TIMESTAMP, 'upsertConceptSet', ",
      DBI::dbQuoteString(connection, as.character(conceptSetName[[1]])), ", ",
      DBI::dbQuoteString(connection, as.character(cleanName[[1]])), ", ",
      DBI::dbQuoteString(connection, as.character(runId[[1]])), ", ",
      DBI::dbQuoteString(connection, as.character(userName[[1]])), ", ",
      DBI::dbQuoteString(connection, as.character(conditionName[[1]])), ", ",
      if (is.na(cohortId[[1]])) "NULL" else as.integer(cohortId[[1]]), ", ",
      DBI::dbQuoteString(connection, as.character(sourceFile[[1]])), ", 'single concept set upsert')"
    )
  )

  DBI::dbExecute(connection, "COMMIT")

  list(
    libraryPath = libraryPath,
    conceptSetName = as.character(conceptSetName[[1]]),
    libraryVersion = nextVersion,
    revisionId = nextRevisionId
  )
}

ithMigrateConceptLibraryRdsToDuckdb <- function(rdsLibraryPath,
                                                duckdbLibraryPath,
                                                overwriteExisting = FALSE,
                                                verbose = TRUE) {
  if (!file.exists(rdsLibraryPath)) {
    stop("RDS concept set library not found: ", rdsLibraryPath)
  }

  libraryObject <- readRDS(rdsLibraryPath)
  if (!is.list(libraryObject) || length(libraryObject) < 2) {
    stop("RDS concept set library must be a list with concept and embedding tables")
  }

  conceptTable <- libraryObject[[1]]
  embedTable <- libraryObject[[2]]
  if (!is.data.frame(conceptTable) || !all(c("conceptSetName", "cleanName", "conceptSet") %in% names(conceptTable))) {
    stop("RDS concept table is missing required columns")
  }
  if (!is.data.frame(embedTable) || !all(c("conceptSetName", "cleanName", "embed") %in% names(embedTable))) {
    stop("RDS embedding table is missing required columns")
  }

  if (file.exists(duckdbLibraryPath) && !isTRUE(overwriteExisting)) {
    stop("DuckDB concept set library already exists: ", duckdbLibraryPath)
  }

  if (file.exists(duckdbLibraryPath) && isTRUE(overwriteExisting)) {
    file.remove(duckdbLibraryPath)
  }

  ithDuckdbInitializeLibrary(duckdbLibraryPath, verbose = verbose)

  migratedCount <- 0L
  for (rowUp in seq_len(nrow(conceptTable))) {
    conceptSetName <- as.character(conceptTable$conceptSetName[[rowUp]])
    cleanName <- as.character(conceptTable$cleanName[[rowUp]])
    conceptSet <- conceptTable$conceptSet[[rowUp]]

    embedIdx <- which(tolower(as.character(embedTable$conceptSetName)) == tolower(conceptSetName))
    embedValue <- if (length(embedIdx) > 0) embedTable$embed[[embedIdx[[1]]]] else NA_real_

    ithDuckdbUpsertConceptSet(
      libraryPath = duckdbLibraryPath,
      conceptSetName = conceptSetName,
      cleanName = cleanName,
      conceptSet = conceptSet,
      embedding = embedValue,
      runId = "migration",
      userName = Sys.info()[["user"]],
      conditionName = NA_character_,
      cohortId = NA_integer_,
      sourceFile = rdsLibraryPath
    )

    migratedCount <- migratedCount + 1L
  }

  latestVersion <- ithDuckdbGetLatestLibraryVersion(duckdbLibraryPath)
  result <- list(
    rdsLibraryPath = rdsLibraryPath,
    duckdbLibraryPath = duckdbLibraryPath,
    migratedConceptSetCount = migratedCount,
    latestLibraryVersion = latestVersion
  )

  if (isTRUE(verbose)) {
    message(
      "Migrated concept set library from RDS to DuckDB: ",
      migratedCount,
      " concept set(s), latest libraryVersion=",
      latestVersion
    )
  }

  invisible(result)
}

ithCoalesce <- function(value, fallback) {
  if (is.null(value) || length(value) == 0) {
    return(fallback)
  }
  value
}

ithNullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0 || (is.character(x) && length(x) == 1 && !nzchar(x))
}

ithBuildSafeTableSuffix <- function(value) {
  normalized <- tolower(as.character(value))
  normalized <- gsub("[^a-z0-9]+", "_", normalized)
  normalized <- gsub("_+", "_", normalized)
  normalized <- gsub("^_+|_+$", "", normalized)
  normalized
}

ithBuildCohortTableNameFromPrefix <- function(cohortTablePrefix, cdmSchemaName) {
  if (ithNullOrEmpty(cohortTablePrefix)) {
    stop("cohortTablePrefix must be provided")
  }

  if (ithNullOrEmpty(cdmSchemaName)) {
    stop("cdm schema name must be provided to build cohort table name suffix")
  }

  paste0(as.character(cohortTablePrefix[[1]]), "_", ithBuildSafeTableSuffix(cdmSchemaName[[1]]))
}

ithGetCatalog <- function(databaseId,
                          config) {
  if (!is.null(config$catalog)) {
    if (is.character(config$catalog) && length(config$catalog) == 1 && nzchar(config$catalog[[1]])) {
      return(as.character(config$catalog[[1]]))
    }

    if (is.list(config$catalog) && databaseId %in% names(config$catalog)) {
      resolved <- config$catalog[[databaseId]]
      if (!is.null(resolved) && nzchar(as.character(resolved[[1]]))) {
        return(as.character(resolved[[1]]))
      }
    }
  }

  envCatalog <- Sys.getenv("DATABRICKS_CATALOG")
  if (nzchar(envCatalog)) {
    return(envCatalog)
  }

  ""
}

getCatalog <- function(databaseId,
                       config) {
  ithGetCatalog(databaseId = databaseId, config = config)
}

ithNormalizeDatabricksHost <- function(host) {
  resolved <- trimws(as.character(host[[1]]))
  resolved <- sub("^https?://", "", resolved, ignore.case = TRUE)
  resolved <- sub("/+$", "", resolved)
  resolved
}

ithNormalizeDatabricksHttpPath <- function(httpPath) {
  resolved <- trimws(as.character(httpPath[[1]]))
  if (nzchar(resolved) && !startsWith(resolved, "/")) {
    resolved <- paste0("/", resolved)
  }
  resolved
}

ithSplitDatabricksHttpPathAndExtraSettings <- function(httpPath) {
  resolved <- trimws(as.character(httpPath[[1]]))
  pieces <- strsplit(resolved, ";", fixed = TRUE)[[1]]
  basePath <- if (length(pieces) > 0) trimws(pieces[[1]]) else ""
  extras <- character(0)
  if (length(pieces) > 1) {
    extras <- trimws(pieces[2:length(pieces)])
    extras <- extras[nzchar(extras)]
  }

  list(
    httpPath = ithNormalizeDatabricksHttpPath(basePath),
    extraSettings = extras
  )
}

ithResolveDatabricksJdbcUser <- function(explicitUser = NULL) {
  if (!is.null(explicitUser) && nzchar(trimws(as.character(explicitUser[[1]])))) {
    return(trimws(as.character(explicitUser[[1]])))
  }

  envUser <- trimws(Sys.getenv("DATABRICKS_USER"))
  if (nzchar(envUser)) {
    return(envUser)
  }

  "token"
}

getDatabricksConnectionDetails <- function() {
  token <- trimws(Sys.getenv("DATABRICKS_TOKEN"))

  list(
    user = ithResolveDatabricksJdbcUser(),
    password = token,
    host = trimws(Sys.getenv("DATABRICKS_HOST")),
    httpPath = trimws(Sys.getenv("DATABRICKS_HTTP_PATH"))
  )
}

ithResolveDatabricksConnectionDetails <- function() {
  provider <- getDatabricksConnectionDetails
  if (exists("getDatabricksConnectionDetails", envir = .GlobalEnv, mode = "function", inherits = FALSE)) {
    provider <- get("getDatabricksConnectionDetails", envir = .GlobalEnv, mode = "function", inherits = FALSE)
  }

  resolved <- provider()
  if (!is.list(resolved)) {
    stop("getDatabricksConnectionDetails() must return a named list")
  }

  namesLower <- tolower(names(resolved))

  tokenIdx <- match(TRUE, namesLower %in% c("password", "token", "databricks_token"))
  hostIdx <- match(TRUE, namesLower %in% c("host", "databricks_host"))
  httpPathIdx <- match(TRUE, namesLower %in% c("httppath", "http_path", "databricks_http_path"))
  userIdx <- match(TRUE, namesLower %in% c("user", "username", "uid", "databricks_user"))

  token <- if (is.na(tokenIdx)) "" else trimws(as.character(resolved[[tokenIdx]][[1]]))
  host <- if (is.na(hostIdx)) "" else ithNormalizeDatabricksHost(resolved[[hostIdx]])
  httpPathParts <- if (is.na(httpPathIdx)) {
    list(httpPath = "", extraSettings = character(0))
  } else {
    ithSplitDatabricksHttpPathAndExtraSettings(resolved[[httpPathIdx]])
  }
  httpPath <- httpPathParts$httpPath
  extraSettings <- httpPathParts$extraSettings
  jdbcUser <- if (is.na(userIdx)) NULL else resolved[[userIdx]]

  if (!nzchar(token) || !nzchar(host) || !nzchar(httpPath)) {
    stop(
      "getDatabricksConnectionDetails() must return non-empty token/password, host, and httpPath values"
    )
  }

  list(
    token = token,
    host = host,
    httpPath = httpPath,
    extraSettings = extraSettings,
    user = ithResolveDatabricksJdbcUser()
  )
}

.getDatabricksConnectionDetails <- function(asList = FALSE) {
  resolvedConnection <- ithResolveDatabricksConnectionDetails()

  databricksConnectionString <- paste0(
    "jdbc:databricks://",
    resolvedConnection$host,
    ":443/default;transportMode=http;ssl=1;AuthMech=3;httpPath=",
    resolvedConnection$httpPath
  )
  connectionExtras <- unique(c("EnableArrow=0", resolvedConnection$extraSettings))
  if (length(connectionExtras) > 0) {
    databricksConnectionString <- paste0(
      databricksConnectionString,
      ";",
      paste(connectionExtras, collapse = ";")
    )
  }

  cdList <- list(
    dbms = "spark",
    connectionString = databricksConnectionString,
    user = resolvedConnection$user,
    password = resolvedConnection$token
  )

  if (asList) {
    return(cdList)
  }

  do.call(DatabaseConnector::createConnectionDetails, cdList)
}

createDatabaseSettings <- function(databaseId,
                                   config) {
  if (is.null(config$cdm) || !(databaseId %in% names(config$cdm))) {
    stop("databaseId not found in config$cdm: ", databaseId)
  }

  cdmSchemaName <- as.character(config$cdm[[databaseId]])
  catalog <- ithGetCatalog(databaseId = databaseId, config = config)
  cdmSchema <- if (nzchar(catalog)) {
    paste0(catalog, ".", cdmSchemaName)
  } else {
    cdmSchemaName
  }

  scratchSchema <- as.character(ithCoalesce(config$cohortDatabaseSchema, Sys.getenv("DATABRICKS_SCRATCH_SCHEMA"))[[1]])
  if (!nzchar(scratchSchema)) {
    stop("cohortDatabaseSchema is not configured and DATABRICKS_SCRATCH_SCHEMA is empty")
  }

  cohortTablePrefix <- as.character(ithCoalesce(config$cohortTablePrefix, "cohort")[[1]])
  cohortTable <- ithBuildCohortTableNameFromPrefix(
    cohortTablePrefix = cohortTablePrefix,
    cdmSchemaName = cdmSchemaName
  )

  options(sqlRenderTempEmulationSchema = scratchSchema)

  database <- list(
    platform = "databricks",
    cdmDatabaseSchema = cdmSchema,
    scratchSchema = scratchSchema,
    workDatabaseSchema = scratchSchema,
    cohortDatabaseSchema = scratchSchema,
    databaseId = databaseId,
    cdm = cdmSchemaName,
    connectionDetailsList = .getDatabricksConnectionDetails(asList = TRUE),
    connectionDetails = .getDatabricksConnectionDetails(),
    cohortTable = cohortTable
  )

  class(database) <- "cdmSettings"
  database
}

buildDatabaseList <- function(config) {
  if (is.null(config$cdm) || length(config$cdm) == 0) {
    stop("config$cdm must define at least one CDM mapping")
  }

  cdmSources <- names(config$cdm)
  databases <- vector("list", length(cdmSources))

  for (sourceUp in seq_along(cdmSources)) {
    databaseId <- cdmSources[[sourceUp]]
    databases[[sourceUp]] <- createDatabaseSettings(databaseId = databaseId, config = config)
  }

  names(databases) <- cdmSources
  databases
}

ithNormalizeCohortDefinitionSet <- function(cohortDefinitionSet) {
  if (is.null(cohortDefinitionSet)) {
    return(NULL)
  }

  if (is.data.frame(cohortDefinitionSet)) {
    requiredColumns <- c("cohortId", "cohortName", "sql")
    missingColumns <- setdiff(requiredColumns, names(cohortDefinitionSet))
    if (length(missingColumns) > 0) {
      stop("cohortDefinitionSet is missing required columns: ", paste(missingColumns, collapse = ", "))
    }
    return(cohortDefinitionSet)
  }

  cohortDf <- tryCatch(as.data.frame(cohortDefinitionSet, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(cohortDf)) {
    stop("cohortDefinitionSet must be NULL, a data.frame, or a list coercible to a data.frame")
  }

  requiredColumns <- c("cohortId", "cohortName", "sql")
  missingColumns <- setdiff(requiredColumns, names(cohortDf))
  if (length(missingColumns) > 0) {
    stop("cohortDefinitionSet is missing required columns: ", paste(missingColumns, collapse = ", "))
  }

  cohortDf
}

ithResolveDatabaseSpecsFromConfig <- function(databaseSpecNames,
                                              envir = .GlobalEnv) {
  if (is.null(databaseSpecNames) || length(databaseSpecNames) == 0) {
    stop("databaseSpecNames must contain at least one database spec object name")
  }

  if (!is.character(databaseSpecNames)) {
    stop("databaseSpecNames must be a character vector of object names")
  }

  resolved <- lapply(databaseSpecNames, function(specName) {
    if (!exists(specName, envir = envir, inherits = TRUE)) {
      stop("Database spec object not found in environment: ", specName)
    }

    get(specName, envir = envir, inherits = TRUE)
  })

  names(resolved) <- databaseSpecNames
  resolved
}

ithResolveDatabasesFromConfig <- function(config,
                                          envir = .GlobalEnv) {
  if (!is.null(config$cdm) && length(config$cdm) > 0) {
    return(buildDatabaseList(config))
  }

  if (!is.null(config$databaseSpecNames) && length(config$databaseSpecNames) > 0) {
    return(ithResolveDatabaseSpecsFromConfig(
      databaseSpecNames = as.character(config$databaseSpecNames),
      envir = envir
    ))
  }

  stop("Config must define either general.cdm mappings or other.databaseSpecNames")
}

ithParseIntegerVectorCsvCell <- function(cellValue) {
  if (is.null(cellValue) || length(cellValue) == 0 || is.na(cellValue[[1]])) {
    return(integer())
  }

  value <- as.character(cellValue[[1]])
  if (!nzchar(trimws(value)) || identical(tolower(trimws(value)), "na") || identical(trimws(value), "[]")) {
    return(integer())
  }

  value <- gsub("^[[:space:]]*\\[|\\][[:space:]]*$", "", value)
  pieces <- unlist(strsplit(value, "[,;|]", fixed = FALSE), use.names = FALSE)
  pieces <- trimws(pieces)
  pieces <- pieces[nzchar(pieces)]

  if (length(pieces) == 0) {
    return(integer())
  }

  parsed <- suppressWarnings(as.integer(pieces))
  if (any(is.na(parsed))) {
    stop("Invalid phenotypeCohortId list value: ", value, ". Use comma/semicolon-separated integers.")
  }

  unique(parsed)
}

ithMaxDxAdditionalPoints <- 4L

# Each general.dxCriteria choice must be a block with `criteria` (text copied into ER2) and
# `additionalPoints` (0-4, added to ER2's base 2 points so ER1 + ER2 stays below YES = 10).
ithValidateDxCriteriaChoices <- function(dxCriteriaChoices) {
  if (is.null(dxCriteriaChoices) || length(dxCriteriaChoices) == 0L ||
      is.null(names(dxCriteriaChoices))) {
    stop("Config must define a named dxCriteria choice map", call. = FALSE)
  }
  for (choiceName in names(dxCriteriaChoices)) {
    choice <- dxCriteriaChoices[[choiceName]]
    if (!is.list(choice) || !all(c("criteria", "additionalPoints") %in% names(choice))) {
      stop(
        "dxCriteria choice '", choiceName, "' must have 'criteria' and 'additionalPoints' entries, e.g.\n",
        "  ", choiceName, ":\n",
        "    criteria: \"One condition diagnosis code ... within -5 to +5 of index.\"\n",
        "    additionalPoints: 0",
        call. = FALSE
      )
    }
    unknown <- setdiff(names(choice), c("criteria", "additionalPoints"))
    if (length(unknown) > 0L) {
      stop("dxCriteria choice '", choiceName, "' has unknown entr(ies): ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    criteria <- choice$criteria
    if (is.null(criteria) || length(criteria) != 1L || is.na(criteria) || !nzchar(trimws(as.character(criteria)))) {
      stop("dxCriteria choice '", choiceName, "' has an empty criteria definition", call. = FALSE)
    }
    points <- choice$additionalPoints
    if (is.null(points) || length(points) != 1L || !is.numeric(points) || is.na(points) ||
        points != round(points) || points < 0 || points > ithMaxDxAdditionalPoints) {
      stop(
        "dxCriteria choice '", choiceName, "' additionalPoints must be a whole number from 0 to ",
        ithMaxDxAdditionalPoints, " (ER1 3 + ER2 2 + additionalPoints must stay below YES = 10)",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

# Returns list(choice, criteria, additionalPoints) for the condition's dxCriteria key.
ithResolveDxCriteria <- function(dxCriteria,
                                 dxCriteriaChoices,
                                 rowLabel = "conditionSpec") {
  if (is.null(dxCriteria) || length(dxCriteria) == 0L || is.na(dxCriteria[[1]]) ||
      !nzchar(trimws(as.character(dxCriteria[[1]])))) {
    stop("Condition spec is missing dxCriteria choice: ", rowLabel)
  }
  ithValidateDxCriteriaChoices(dxCriteriaChoices)

  choiceName <- trimws(as.character(dxCriteria[[1]]))
  if (!(choiceName %in% names(dxCriteriaChoices))) {
    stop(
      "Unknown dxCriteria choice '", choiceName, "' in ", rowLabel,
      ". Valid choices: ", paste(names(dxCriteriaChoices), collapse = ", ")
    )
  }

  choice <- dxCriteriaChoices[[choiceName]]
  list(
    choice = choiceName,
    criteria = trimws(as.character(choice$criteria)),
    additionalPoints = as.integer(choice$additionalPoints)
  )
}

ithReadClinicalDefinitionFile <- function(clinicalDefinitionFile) {
  if (is.null(clinicalDefinitionFile) || length(clinicalDefinitionFile) == 0L ||
      is.na(clinicalDefinitionFile[[1]]) || !nzchar(trimws(as.character(clinicalDefinitionFile[[1]])))) {
    return(NULL)
  }

  path <- as.character(clinicalDefinitionFile[[1]])
  if (!file.exists(path)) {
    stop("clinicalDefinitionFile not found: ", path)
  }

  definitions <- utils::read.csv(path, stringsAsFactors = FALSE)
  requiredColumns <- c("Phenotype", "Definition")
  missingColumns <- setdiff(requiredColumns, names(definitions))
  if (length(missingColumns) > 0L) {
    stop("clinicalDefinitionFile is missing required column(s): ", paste(missingColumns, collapse = ", "))
  }

  definitions$Phenotype <- ithNormalizeUtf8Text(definitions$Phenotype)
  definitions$Definition <- ithNormalizeUtf8Text(definitions$Definition)
  definitions$phenotypeKey <- ithPhenotypeMatchKey(definitions$Phenotype)
  definitions$definitionValue <- definitions$Definition
  definitions
}

# Matching key for condition names: lower case with every non-alphanumeric character removed,
# so "Crohn's disease", "crohns disease" and "Crohns-Disease" all match.
ithPhenotypeMatchKey <- function(values) {
  tolower(gsub("[^[:alnum:]]", "", as.character(values)))
}

ithNormalizeUtf8Text <- function(values) {
  values <- as.character(values)
  normalized <- iconv(values, from = "UTF-8", to = "UTF-8", sub = "")
  fallback <- is.na(normalized) & !is.na(values)
  if (any(fallback)) {
    normalized[fallback] <- iconv(values[fallback], from = "", to = "UTF-8", sub = "")
  }
  normalized
}

ithResolveClinicalDefinition <- function(testCondition, clinicalDefinitions, rowLabel) {
  if (is.null(clinicalDefinitions)) {
    return(NULL)
  }

  key <- ithPhenotypeMatchKey(testCondition[[1]])
  matches <- which(clinicalDefinitions$phenotypeKey == key)
  if (length(matches) > 1L) {
    stop(
      "clinicalDefinitionFile has multiple matches for condition ", rowLabel,
      " (case and non-alphanumeric characters ignored): ",
      paste(clinicalDefinitions$Phenotype[matches], collapse = " | ")
    )
  }
  if (length(matches) == 0L) {
    return(NULL)
  }

  clinicalDefinitions$definitionValue[[matches[[1]]]]
}

ithNormalizeConditionSpec <- function(conditionSpec,
                                      rowLabel = "conditionSpec",
                                      dxCriteriaChoices = NULL,
                                      clinicalDefinitions = NULL) {
  spec <- conditionSpec

  spec$testCondition <- trimws(ithNormalizeUtf8Text(spec$testCondition))
  resolvedDx <- ithResolveDxCriteria(
    dxCriteria = spec$dxCriteria,
    dxCriteriaChoices = dxCriteriaChoices,
    rowLabel = rowLabel
  )
  spec$dxCriteriaChoice <- resolvedDx$choice
  spec$dxCriteria <- resolvedDx$criteria
  spec$dxAdditionalPoints <- resolvedDx$additionalPoints
  if (is.null(spec$additionalInformation) || is.na(spec$additionalInformation)) {
    clinicalDefinition <- ithResolveClinicalDefinition(
      testCondition = spec$testCondition,
      clinicalDefinitions = clinicalDefinitions,
      rowLabel = rowLabel
    )
    spec$additionalInformation <- if (is.null(clinicalDefinition)) "" else clinicalDefinition
  } else {
    spec$additionalInformation <- ithNormalizeUtf8Text(spec$additionalInformation)
  }
  spec$rubricName <- trimws(ithNormalizeUtf8Text(spec$rubricName))
  randomCohortValue <- spec$randomCohortDefinitionId
  if (is.null(randomCohortValue) || length(randomCohortValue) == 0L ||
      is.na(randomCohortValue[[1]]) || !nzchar(trimws(as.character(randomCohortValue[[1]])))) {
    spec$randomCohortDefinitionId <- NULL
  } else {
    spec$randomCohortDefinitionId <- suppressWarnings(as.integer(randomCohortValue[[1]]))
  }

  isBlank <- function(x) length(x) == 0 || is.na(x[[1]]) || !nzchar(trimws(x[[1]]))
  if (isBlank(spec$testCondition) || isBlank(spec$dxCriteria) || isBlank(spec$rubricName)) {
    stop("Condition spec has blank required values: ", rowLabel)
  }
  if (!is.null(spec$randomCohortDefinitionId) && is.na(spec$randomCohortDefinitionId)) {
    stop("Condition spec has invalid randomCohortDefinitionId: ", rowLabel)
  }

  # Blank/NA means not provided, the same for CSV cells and inline YAML.
  if (!is.null(spec$sourceCohortDefinitionId)) {
    sourceValue <- spec$sourceCohortDefinitionId[[1]]
    if (is.na(sourceValue) || !nzchar(trimws(as.character(sourceValue)))) {
      spec$sourceCohortDefinitionId <- NULL
    } else {
      spec$sourceCohortDefinitionId <- suppressWarnings(as.integer(sourceValue))
      if (is.na(spec$sourceCohortDefinitionId)) {
        stop("Condition spec has invalid sourceCohortDefinitionId: ", rowLabel)
      }
    }
  }

  if (is.null(spec$randomCohortDefinitionId)) {
    spec$cohortId <- NULL
  } else {
    spec$cohortId <- spec$randomCohortDefinitionId
  }

  if (!is.null(spec$randomSampleFromCohortDefinitionId)) {
    if (is.na(spec$randomSampleFromCohortDefinitionId) || !nzchar(as.character(spec$randomSampleFromCohortDefinitionId))) {
      spec$randomSampleFromCohortDefinitionId <- NULL
    } else {
      spec$randomSampleFromCohortDefinitionId <- as.integer(spec$randomSampleFromCohortDefinitionId)
      if (is.na(spec$randomSampleFromCohortDefinitionId)) {
        stop("Condition spec has invalid randomSampleFromCohortDefinitionId: ", rowLabel)
      }
    }
  }

  if (!is.null(spec$phenotypeCohortId)) {
    if (is.character(spec$phenotypeCohortId) && length(spec$phenotypeCohortId) == 1) {
      spec$phenotypeCohortId <- ithParseIntegerVectorCsvCell(spec$phenotypeCohortId)
    } else {
      parsed <- suppressWarnings(as.integer(unlist(spec$phenotypeCohortId, use.names = FALSE)))
      parsed <- parsed[!is.na(parsed)]
      spec$phenotypeCohortId <- unique(parsed)
    }
  } else {
    spec$phenotypeCohortId <- integer()
  }

  spec$adjustableRuleId <- ithParseRuleIdList(spec$adjustableRuleId, rowLabel = rowLabel)

  adjustableRuleScore <- spec$adjustableRuleScore
  if (is.null(adjustableRuleScore) || length(adjustableRuleScore) == 0L ||
      is.na(adjustableRuleScore[[1]]) || !nzchar(trimws(as.character(adjustableRuleScore[[1]])))) {
    spec$adjustableRuleScore <- NULL
  } else {
    spec$adjustableRuleScore <- suppressWarnings(as.numeric(adjustableRuleScore[[1]]))
    if (is.na(spec$adjustableRuleScore) || spec$adjustableRuleScore < 0) {
      stop("Condition spec adjustableRuleScore must be a non-negative number: ", rowLabel)
    }
  }

  # NULL means "use the config default"; ithReadIthacaConditionConfig fills it in.
  spec$zeroOccurrencePenalty <- ithAe1ParsePenalty(spec$zeroOccurrencePenalty, default = NULL, rowLabel = rowLabel)

  spec
}

# Parses rule IDs such as "ER1;ER5" or "ER1, ER5" (or an inline YAML list) into c("ER1", "ER5").
# Blank/NA gives character(0).
ithParseRuleIdList <- function(value, rowLabel = "conditionSpec") {
  values <- as.character(unlist(value, use.names = FALSE))
  values <- values[!is.na(values)]
  ids <- toupper(trimws(unlist(strsplit(values, "[;,]"))))
  ids <- unique(ids[nzchar(ids)])
  invalid <- ids[!grepl("^[A-Z]+[0-9]+$", ids)]
  if (length(invalid) > 0L) {
    stop(
      "Condition spec has invalid adjustableRuleId value(s) ", paste0("'", invalid, "'", collapse = ", "),
      " (use rule IDs such as ER1, separated by ';'): ", rowLabel,
      call. = FALSE
    )
  }
  ids
}

# Sort key for rule IDs: GLOBAL first, then ER, HE, AE, then any other prefix, each by number
# (ER1, ER2, ER10). Equal IDs share a key, so it can be combined with other order() keys.
ithRuleIdOrder <- function(ruleId) {
  id <- toupper(trimws(as.character(ruleId)))
  prefix <- sub("^([A-Z]*).*$", "\\1", id)
  number <- suppressWarnings(as.integer(sub("^[A-Z]*([0-9]+).*$", "\\1", id)))
  group <- match(prefix, c("ER", "HE", "AE"))
  group[is.na(group)] <- 4L
  group[prefix == "GLOBAL"] <- 0L
  sortedIds <- id[order(group, prefix, number, id)]
  match(id, unique(sortedIds))
}

ithNormalizeConditionSpecs <- function(conditionSpecs,
                                       dxCriteriaChoices = NULL,
                                       clinicalDefinitions = NULL) {
  if (is.null(conditionSpecs) || length(conditionSpecs) == 0) {
    return(conditionSpecs)
  }

  lapply(seq_along(conditionSpecs), function(index) {
    ithNormalizeConditionSpec(
      conditionSpecs[[index]],
      rowLabel = paste0("conditionSpecs[[", index, "]]"),
      dxCriteriaChoices = dxCriteriaChoices,
      clinicalDefinitions = clinicalDefinitions
    )
  })
}

# TRUE/1/YES (any case, surrounding spaces allowed) means ignore; anything else, including blank, means include.
ithIsIgnoreFlag <- function(values) {
  normalized <- toupper(trimws(as.character(values)))
  !is.na(normalized) & normalized %in% c("TRUE", "1", "YES")
}

ithCoalesceText <- function(primary, fallback) {
  primary <- as.character(primary)
  useFallback <- is.na(primary) | !nzchar(trimws(primary))
  primary[useFallback] <- as.character(fallback)[useFallback]
  primary
}

ithReadConditionSpecsCsv <- function(conditionSpecsCsvPath,
                                     dxCriteriaChoices = NULL,
                                     clinicalDefinitions = NULL) {
  if (is.null(conditionSpecsCsvPath) || !is.character(conditionSpecsCsvPath) || length(conditionSpecsCsvPath) != 1 || !nzchar(conditionSpecsCsvPath)) {
    stop("conditionSpecsCsvPath must be a non-empty CSV file path")
  }

  if (!file.exists(conditionSpecsCsvPath)) {
    stop("Condition specs CSV file not found: ", conditionSpecsCsvPath)
  }

  conditionTable <- utils::read.csv(conditionSpecsCsvPath, stringsAsFactors = FALSE)
  if (nrow(conditionTable) == 0) {
    stop("Condition specs CSV has no rows: ", conditionSpecsCsvPath)
  }

  requiredColumns <- c("testCondition", "dxCriteria", "rubricName")
  missingColumns <- setdiff(requiredColumns, names(conditionTable))
  if (length(missingColumns) > 0) {
    stop("Condition specs CSV is missing required columns: ", paste(missingColumns, collapse = ", "))
  }

  if (!("additionalInformation" %in% names(conditionTable))) {
    conditionTable$additionalInformation <- ""
  }

  optionalColumns <- c(
    "randomCohortDefinitionId",
    "sourceCohortDefinitionId",
    "randomSampleFromCohortDefinitionId",
    "phenotypeCohortId",
    "adjustableRuleId",
    "adjustableRuleScore",
    "zeroOccurrencePenalty"
  )
  for (columnName in optionalColumns) {
    if (!(columnName %in% names(conditionTable))) {
      conditionTable[[columnName]] <- NA
    }
  }

  keepRows <- seq_len(nrow(conditionTable))
  ignoreColumn <- names(conditionTable)[tolower(names(conditionTable)) == "ignore"]
  if (length(ignoreColumn) > 0) {
    ignoreFlags <- ithIsIgnoreFlag(conditionTable[[ignoreColumn[[1]]]])
    if (any(ignoreFlags)) {
      ignoredNames <- ithCoalesceText(conditionTable$rubricName[ignoreFlags], conditionTable$testCondition[ignoreFlags])
      message(
        "Ignoring ", sum(ignoreFlags), " condition(s) marked ignore in ", basename(conditionSpecsCsvPath), ": ",
        paste(ignoredNames, collapse = ", ")
      )
    }
    keepRows <- which(!ignoreFlags)
    conditionTable[ignoreColumn] <- NULL
    if (length(keepRows) == 0) {
      stop("Every row in the condition specs CSV is marked ignore: ", conditionSpecsCsvPath, call. = FALSE)
    }
  }

  conditionSpecs <- lapply(keepRows, function(rowUp) {
    row <- conditionTable[rowUp, , drop = FALSE]
    rowList <- lapply(row, function(value) value[[1]])
    rowList$phenotypeCohortId <- ithParseIntegerVectorCsvCell(rowList$phenotypeCohortId)

    ithNormalizeConditionSpec(
      rowList,
      rowLabel = paste0("CSV row ", rowUp),
      dxCriteriaChoices = dxCriteriaChoices,
      clinicalDefinitions = clinicalDefinitions
    )
  })

  conditionSpecs
}

ithReadPhenotypeReportScoringCsv <- function(config) {
  section <- ithCoalesce(config$phenotypeReportScoring, list())
  csvPath <- section$phenotypeReportScoringCsvPath
  if (is.null(csvPath) || !nzchar(trimws(as.character(csvPath[[1]])))) {
    return(NULL)
  }

  csvPath <- trimws(as.character(csvPath[[1]]))
  if (!grepl("^([A-Za-z]:[\\\\/]|[\\\\/]{2})", csvPath)) {
    outputDirectory <- config$outputDirectory
    if (is.null(outputDirectory) || !nzchar(as.character(outputDirectory[[1]]))) {
      stop("phenotypeReportScoringCsvPath is relative but outputDirectory is not defined")
    }
    csvPath <- file.path(as.character(outputDirectory[[1]]), csvPath)
  }
  if (!file.exists(csvPath)) {
    stop("phenotypeReportScoring CSV file not found: ", csvPath)
  }

  scoringTable <- utils::read.csv(
    csvPath,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
  if (nrow(scoringTable) == 0L) {
    stop("phenotypeReportScoring CSV has no rows: ", csvPath)
  }

  knownRules <- c(
    "analysisName",
    "excludeNegativeScores", "yesScoreOverride", "noScoreOverride",
    "yesScoreAdjustment", "noScoreAdjustment", "splayPrior", "splayPost",
    "sampleSubjectCount", "frequencyPlotFpFnScores", "createSubjectSample", "classLabels",
    "dxRuleScoreOverride", "dxRuleScoreAdjustment"
  )
  if (!("analysisName" %in% names(scoringTable))) {
    stop("phenotypeReportScoring CSV must contain an analysisName column: ", csvPath)
  }
  unknownColumns <- setdiff(names(scoringTable), knownRules)
  if (length(unknownColumns) > 0L) {
    message(
      "Ignoring unknown phenotypeReportScoring CSV column(s): ",
      paste(unknownColumns, collapse = ", ")
    )
  }

  parseCell <- function(value, columnName, rowIndex) {
    if (length(value) == 0L || is.na(value) || !nzchar(trimws(as.character(value)))) {
      return(NULL)
    }
    value <- trimws(as.character(value))
    if (columnName %in% c(
      "yesScoreOverride", "noScoreOverride", "yesScoreAdjustment", "noScoreAdjustment",
      "splayPrior", "splayPost", "sampleSubjectCount",
      "dxRuleScoreOverride", "dxRuleScoreAdjustment"
    )) {
      parsed <- suppressWarnings(as.numeric(value))
      if (is.na(parsed)) {
        stop("Invalid numeric value in phenotypeReportScoring CSV column '", columnName,
             "', row ", rowIndex)
      }
      return(parsed)
    }
    if (columnName %in% c("excludeNegativeScores", "frequencyPlotFpFnScores", "createSubjectSample")) {
      parsed <- tolower(value)
      if (!(parsed %in% c("true", "false"))) {
        stop("Invalid logical value in phenotypeReportScoring CSV column '", columnName,
             "', row ", rowIndex, "; use true or false")
      }
      return(parsed == "true")
    }
    if (columnName == "classLabels") {
      labels <- trimws(unlist(strsplit(value, ";", fixed = TRUE)))
      labels <- labels[nzchar(labels)]
      if (length(labels) == 0L) {
        stop("classLabels must contain at least one label in phenotypeReportScoring CSV row ", rowIndex)
      }
      return(labels)
    }
    value
  }

  runs <- lapply(seq_len(nrow(scoringTable)), function(rowIndex) {
    row <- list()
    for (columnName in intersect(names(scoringTable), knownRules)) {
      parsed <- parseCell(scoringTable[[columnName]][[rowIndex]], columnName, rowIndex)
      if (!is.null(parsed)) {
        row[[columnName]] <- parsed
      }
    }
    analysisName <- row$analysisName
    if (is.null(analysisName) || !nzchar(trimws(as.character(analysisName[[1]])))) {
      stop("analysisName is required in phenotypeReportScoring CSV row ", rowIndex)
    }
    row$analysisName <- trimws(as.character(analysisName[[1]]))
    row
  })

  analysisNames <- vapply(runs, function(run) run$analysisName, character(1))
  if (anyDuplicated(analysisNames)) {
    duplicates <- unique(analysisNames[duplicated(analysisNames)])
    stop(
      "analysisName values in phenotypeReportScoring CSV must be unique; duplicated value(s): ",
      paste(duplicates, collapse = ", ")
    )
  }
  runs
}

ithResolvePhenotypeReportScoring <- function(config,
                                             conditionName,
                                             adjustableRuleId = NULL,
                                             adjustableRuleScore = NULL,
                                             overrides = NULL) {
  section <- ithCoalesce(config$phenotypeReportScoring, list())
  rules <- ithCoalesce(section$default, list())

  # A condition-level null removes the default for that setting.
  conditionRules <- section$conditions[[conditionName]]
  for (ruleName in names(conditionRules)) {
    rules[[ruleName]] <- conditionRules[[ruleName]]
  }
  if (!is.null(adjustableRuleScore)) {
    rules$dxRuleScoreOverride <- adjustableRuleScore
    rules$dxRuleScoreAdjustment <- NULL
  }
  if (!is.null(overrides)) {
    for (ruleName in names(overrides)) {
      rules[[ruleName]] <- overrides[[ruleName]]
    }
  }

  knownRules <- c(
    "analysisName",
    "excludeNegativeScores", "yesScoreOverride", "noScoreOverride",
    "yesScoreAdjustment", "noScoreAdjustment", "splayPrior", "splayPost",
    "sampleSubjectCount", "frequencyPlotFpFnScores", "createSubjectSample", "classLabels",
    "dxRuleScoreOverride", "dxRuleScoreAdjustment"
  )
  unknownRules <- setdiff(names(rules), knownRules)
  if (length(unknownRules) > 0) {
    stop(
      "Unknown phenotypeReportScoring setting(s) for condition '", conditionName, "': ",
      paste(unknownRules, collapse = ", ")
    )
  }

  excludeNegativeScores <- ithCoalesce(rules[["excludeNegativeScores"]], FALSE)
  if (!is.logical(excludeNegativeScores) || length(excludeNegativeScores) != 1 || is.na(excludeNegativeScores)) {
    stop("phenotypeReportScoring excludeNegativeScores must be true or false for condition: ", conditionName)
  }

  for (ruleName in c(
    "yesScoreOverride", "noScoreOverride", "yesScoreAdjustment", "noScoreAdjustment",
    "dxRuleScoreOverride", "dxRuleScoreAdjustment"
  )) {
    value <- rules[[ruleName]]
    if (!is.null(value) && (!is.numeric(value) || length(value) != 1 || is.na(value))) {
      stop("phenotypeReportScoring ", ruleName, " must be a single number for condition: ", conditionName)
    }
  }

  for (ruleName in c("splayPrior", "splayPost")) {
    value <- rules[[ruleName]]
    if (is.null(value) || value < 0 || value != round(value)) {
      stop("phenotypeReportScoring ", ruleName, " is required and must be a whole number of days >= 0 for condition: ", conditionName)
    }
  }

  sampleSubjectCount <- rules[["sampleSubjectCount"]]
  if (is.null(sampleSubjectCount) || !is.numeric(sampleSubjectCount) || length(sampleSubjectCount) != 1 ||
      is.na(sampleSubjectCount) || sampleSubjectCount < 1 || sampleSubjectCount != round(sampleSubjectCount)) {
    stop("phenotypeReportScoring sampleSubjectCount is required and must be a positive whole number for condition: ", conditionName)
  }

  for (ruleName in c("frequencyPlotFpFnScores", "createSubjectSample")) {
    value <- rules[[ruleName]]
    if (is.null(value) || !is.logical(value) || length(value) != 1 || is.na(value)) {
      stop("phenotypeReportScoring ", ruleName, " is required and must be true or false for condition: ", conditionName)
    }
  }

  classLabels <- rules[["classLabels"]]
  if (is.null(classLabels) || !is.character(classLabels) || length(classLabels) == 0 ||
      any(!nzchar(trimws(classLabels)))) {
    stop("phenotypeReportScoring classLabels is required and must be a non-empty character list for condition: ", conditionName)
  }
  classLabels <- unique(trimws(classLabels))

  analysisName <- ithCoalesce(rules[["analysisName"]], "rubric_validation")
  if (!is.character(analysisName) || length(analysisName) != 1 || is.na(analysisName) || !nzchar(trimws(analysisName))) {
    stop("phenotypeReportScoring analysisName must be a non-empty text value for condition: ", conditionName)
  }
  analysisName <- trimws(analysisName)

  yesScoreOverride <- rules[["yesScoreOverride"]]
  noScoreOverride <- rules[["noScoreOverride"]]
  yesScoreAdjustment <- rules[["yesScoreAdjustment"]]
  noScoreAdjustment <- rules[["noScoreAdjustment"]]

  if (!is.null(yesScoreOverride) && !is.null(yesScoreAdjustment)) {
    stop("phenotypeReportScoring: set either yesScoreOverride or yesScoreAdjustment, not both, for condition: ", conditionName)
  }
  if (!is.null(noScoreOverride) && !is.null(noScoreAdjustment)) {
    stop("phenotypeReportScoring: set either noScoreOverride or noScoreAdjustment, not both, for condition: ", conditionName)
  }

  if (!is.null(yesScoreOverride) && !is.null(noScoreOverride) && yesScoreOverride <= noScoreOverride) {
    stop("phenotypeReportScoring yesScoreOverride must be greater than noScoreOverride for condition: ", conditionName)
  }

  dxRuleScoreOverride <- rules[["dxRuleScoreOverride"]]
  dxRuleScoreAdjustment <- rules[["dxRuleScoreAdjustment"]]
  if (!is.null(dxRuleScoreOverride) && !is.null(dxRuleScoreAdjustment)) {
    stop("phenotypeReportScoring: set either dxRuleScoreOverride or dxRuleScoreAdjustment, not both, for condition: ", conditionName)
  }
  if (!is.null(dxRuleScoreOverride) && dxRuleScoreOverride < 0) {
    stop("phenotypeReportScoring dxRuleScoreOverride must be >= 0 (it sets an evidence rule's points) for condition: ", conditionName)
  }
  adjustableRuleId <- ithParseRuleIdList(adjustableRuleId, rowLabel = conditionName)
  if ((!is.null(dxRuleScoreOverride) || !is.null(dxRuleScoreAdjustment)) && length(adjustableRuleId) == 0L) {
    # Generated rubrics always put the index diagnosis rule in ER1.
    message(
      "phenotypeReportScoring dx rule setting for '", conditionName,
      "' uses ER1 because its adjustableRuleId in conditionSpecs is blank."
    )
    adjustableRuleId <- "ER1"
  }
  if (is.null(dxRuleScoreOverride) && is.null(dxRuleScoreAdjustment)) {
    adjustableRuleId <- character(0)
  }

  list(
    analysisName = analysisName,
    excludeNegativeScores = excludeNegativeScores,
    yesScoreOverride = yesScoreOverride,
    noScoreOverride = noScoreOverride,
    yesScoreAdjustment = yesScoreAdjustment,
    noScoreAdjustment = noScoreAdjustment,
    splayPrior = as.integer(rules[["splayPrior"]]),
    splayPost = as.integer(rules[["splayPost"]]),
    sampleSubjectCount = as.integer(sampleSubjectCount),
    frequencyPlotFpFnScores = rules[["frequencyPlotFpFnScores"]],
    createSubjectSample = rules[["createSubjectSample"]],
    classLabels = classLabels,
    adjustableRuleId = adjustableRuleId,
    dxRuleScoreOverride = dxRuleScoreOverride,
    dxRuleScoreAdjustment = dxRuleScoreAdjustment,
    specified = isTRUE(excludeNegativeScores) || !is.null(yesScoreOverride) || !is.null(noScoreOverride) ||
      !is.null(yesScoreAdjustment) || !is.null(noScoreAdjustment) ||
      !is.null(dxRuleScoreOverride) || !is.null(dxRuleScoreAdjustment)
  )
}

# Checks that each adjustable rule ID is an evidence rule in the latest concise rubric and that
# the adjusted score is not negative; prints each rule's criteria and old -> new points.
ithCheckAdjustableRules <- function(conditionDir,
                                    conditionName,
                                    adjustableRuleId,
                                    dxRuleScoreOverride = NULL,
                                    dxRuleScoreAdjustment = NULL) {
  if (length(adjustableRuleId) == 0L || (is.null(dxRuleScoreOverride) && is.null(dxRuleScoreAdjustment))) {
    return(invisible(NULL))
  }
  conciseRubric <- ithLoadLatest(conditionDir, paste0(conditionName, "_conciseRubric.csv"))
  if (is.null(conciseRubric)) {
    stop("No concise rubric found to check adjustableRuleId for condition: ", conditionName, call. = FALSE)
  }
  ruleIds <- toupper(trimws(as.character(conciseRubric$ruleId)))
  for (ruleId in adjustableRuleId) {
    rows <- which(ruleIds == ruleId)
    if (length(rows) == 0L) {
      stop(
        "adjustableRuleId '", ruleId, "' for '", conditionName, "' is not in the latest concise rubric",
        " (it may have been renumbered or removed); update adjustableRuleId in conditionSpecs.",
        call. = FALSE
      )
    }
    for (row in rows) {
      if (!identical(ithNormalizeReviewRuleType(conciseRubric$type[[row]]), "evidenceRules")) {
        stop(
          "adjustableRuleId '", ruleId, "' for '", conditionName, "' is a ", conciseRubric$type[[row]],
          " rule, not an evidence rule; update adjustableRuleId in conditionSpecs.",
          call. = FALSE
        )
      }
      oldScore <- as.numeric(conciseRubric$score[[row]])
      newScore <- if (!is.null(dxRuleScoreOverride)) dxRuleScoreOverride else oldScore + dxRuleScoreAdjustment
      if (newScore < 0) {
        stop(
          "dxRuleScoreAdjustment would make ", ruleId, " (", oldScore, " points) negative for '", conditionName, "'.",
          call. = FALSE
        )
      }
      message(
        "Adjusting ", conditionName, " ", conciseRubric$rubric[[row]], " ", ruleId, ": ", oldScore, " -> ", newScore,
        " points (", conciseRubric$criteria[[row]], ")"
      )
    }
  }
  invisible(NULL)
}

ithReadIthacaWorkflowConfig <- function(configPath) {
  merged <- ithReadIthacaConditionConfig(configPath)

  hasCdm <- !is.null(merged$cdm) && length(merged$cdm) > 0
  hasDatabaseSpecNames <- !is.null(merged$databaseSpecNames) && length(merged$databaseSpecNames) > 0
  if (!hasCdm && !hasDatabaseSpecNames) {
    stop("Config must define at least one database under general.cdm or other.databaseSpecNames")
  }

  if (hasCdm) {
    ithResolveDatabricksConnectionDetails()
  }

  merged
}

# Section layout: general/rubricDevelopment/conceptSetBuilding/other keys are flattened to the top
# level; randomCohort, phenotypeReportScoring and dataDrivenScoring stay nested because code reads them
# as blocks.
ithApplyConfigSections <- function(merged, config) {
  for (name in c("general", "rubricDevelopment", "conceptSetBuilding", "other")) {
    if (is.list(config[[name]])) {
      merged <- modifyList(merged, config[[name]])
    }
  }
  for (name in c("randomCohort", "phenotypeReportScoring", "dataDrivenScoring")) {
    if (is.list(config[[name]])) {
      merged[[name]] <- modifyList(ithCoalesce(merged[[name]], list()), config[[name]])
    }
  }
  merged
}

# Reads the config and resolves condition specs (dxCriteria choices, additionalInformation
# from the condition CSV or clinicalDefinitionFile) without validating database settings.
ithReadIthacaConditionConfig <- function(configPath) {
  if (is.null(configPath) || !is.character(configPath) || length(configPath) != 1 || !nzchar(configPath)) {
    stop("configPath must be a non-empty path to a YAML file")
  }

  if (!grepl("\\.ya?ml$", configPath, ignore.case = TRUE)) {
    stop(
      "configPath '", configPath, "' is not a .yaml/.yml file.\n",
      "  If this is a condition name, pass it as the first argument of the ...FromConfig function, e.g.\n",
      "  reviewConciseRubricsFromConfig(\"", configPath, "\")\n",
      "  applyConciseRubricReviewsFromConfig(\"", configPath, "\")\n",
      "  or use the single-condition functions reviewConciseRubric(\"", configPath, "\") / ",
      "applyConciseRubricReview(\"", configPath, "\").",
      call. = FALSE
    )
  }
  resolvedPath <- file.path(
    normalizePath(dirname(configPath), winslash = "/", mustWork = FALSE),
    basename(configPath)
  )
  if (!file.exists(configPath)) {
    stop(
      "Config file not found: ", resolvedPath, "\n",
      "  Current working directory: ", normalizePath(getwd(), winslash = "/"), "\n",
      "  config.yaml must be at the top level of your project, and R's working directory must be ",
      "that folder.\n",
      "  Open the project's .Rproj file, or run setwd(\"<project folder>\"), then try again.",
      call. = FALSE
    )
  }
  message("Using config: ", resolvedPath)

  config <- tryCatch(
    yaml::read_yaml(configPath),
    error = function(e) {
      stop("Config file is not valid YAML: ", resolvedPath, "\n  ", conditionMessage(e), call. = FALSE)
    }
  )
  if (is.null(config) || !is.list(config)) {
    stop("Config file is empty or invalid YAML: ", resolvedPath, call. = FALSE)
  }

  defaults <- ithCoalesce(config$defaults, list())
  defaultsCommon <- ithCoalesce(defaults$common, list())
  defaultsContextual <- ithCoalesce(defaults$contextual, list())
  run <- ithCoalesce(config$run, list())

  merged <- modifyList(defaultsCommon, defaultsContextual)
  merged <- modifyList(merged, run)
  merged <- ithApplyConfigSections(merged, config)

  ithValidateDxCriteriaChoices(merged$dxCriteria)

  clinicalDefinitions <- ithReadClinicalDefinitionFile(merged$clinicalDefinitionFile)

  hasConditionSpecsCsv <- !is.null(merged$conditionSpecsCsvPath) &&
    nzchar(as.character(merged$conditionSpecsCsvPath[[1]]))
  if (hasConditionSpecsCsv) {
    merged$conditionSpecs <- ithReadConditionSpecsCsv(
      as.character(merged$conditionSpecsCsvPath[[1]]),
      dxCriteriaChoices = merged$dxCriteria,
      clinicalDefinitions = clinicalDefinitions
    )
  } else {
    merged$conditionSpecs <- ithNormalizeConditionSpecs(
      merged$conditionSpecs,
      dxCriteriaChoices = merged$dxCriteria,
      clinicalDefinitions = clinicalDefinitions
    )
  }

  if (is.null(merged$conditionSpecs) || length(merged$conditionSpecs) == 0) {
    stop("Config must define at least one condition under general.conditionSpecs")
  }

  defaultPenalty <- ithAe1ParsePenalty(merged$zeroOccurrencePenalty, default = ithAe1DefaultPenalty, rowLabel = "general.zeroOccurrencePenalty")
  merged$zeroOccurrencePenalty <- defaultPenalty
  merged$conditionSpecs <- lapply(merged$conditionSpecs, function(spec) {
    spec$zeroOccurrencePenalty <- ithCoalesce(spec$zeroOccurrencePenalty, defaultPenalty)
    spec
  })

  merged
}

# The ...FromConfig review wrappers take conditionNames first; catch a config path passed there.
ithCheckConditionNamesNotConfig <- function(conditionNames) {
  if (is.character(conditionNames) && any(grepl("\\.ya?ml$", conditionNames, ignore.case = TRUE))) {
    path <- conditionNames[grepl("\\.ya?ml$", conditionNames, ignore.case = TRUE)][[1]]
    stop(
      "'", path, "' looks like a config file, but the first argument is conditionNames.\n",
      "  Pass the config by name, e.g. configPath = \"", path, "\"",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

ithFindConfigConditionSpec <- function(config, conditionName) {
  key <- tolower(trimws(conditionName))
  matches <- Filter(function(spec) {
    names <- c(spec$rubricName, spec$testCondition)
    any(tolower(trimws(as.character(names))) == key)
  }, config$conditionSpecs)
  if (length(matches) == 0L) {
    stop("Condition not found in config conditionSpecs (rubricName or testCondition): ", conditionName)
  }
  if (length(matches) > 1L) {
    stop("Multiple config conditionSpecs match condition: ", conditionName)
  }
  matches[[1]]
}

# adjustableRuleId for a condition from the config's conditionSpecs; character(0) when the config
# or condition is not available (the protected-rule guard is then skipped).
ithConfiguredAdjustableRuleIds <- function(configPath, conditionName) {
  if (is.null(configPath) || !file.exists(configPath)) {
    return(character(0))
  }
  spec <- tryCatch(
    ithFindConfigConditionSpec(ithReadIthacaConditionConfig(configPath), conditionName),
    error = function(e) NULL
  )
  if (is.null(spec)) {
    return(character(0))
  }
  ithParseRuleIdList(spec$adjustableRuleId, rowLabel = conditionName)
}

ithDefaultIthacaWorkflowConfigText <- function() {
  templatePath <- system.file("config_template.yaml", package = "Ithaca")
  if (!nzchar(templatePath)) {
    stop("config_template.yaml not found in the Ithaca package; reinstall the package")
  }
  paste(readLines(templatePath, warn = FALSE), collapse = "\n")
}

#' Write a template YAML config for end-to-end workflow runs
#'
#' @param configPath Path to write the YAML template. Defaults to `config.yaml`.
#' @param overwrite Logical; when `TRUE`, overwrite an existing file.
#'
#' @return Invisible list with `configPath`.
#' @examples
#' \dontrun{
#' writeIthacaWorkflowConfigTemplate("config.yaml", overwrite = FALSE)
#' }
#' @export
writeIthacaWorkflowConfigTemplate <- function(configPath = "config.yaml",
                                              overwrite = FALSE) {
  if (!is.character(configPath) || length(configPath) != 1 || !nzchar(configPath)) {
    stop("configPath must be a non-empty file path")
  }

  if (file.exists(configPath) && !isTRUE(overwrite)) {
    stop("Config file already exists: ", configPath, " (set overwrite=TRUE to replace)")
  }

  writeLines(ithDefaultIthacaWorkflowConfigText(), con = configPath)

  invisible(list(configPath = configPath))
}

#' Run Ithaca end-to-end workflow from a YAML config file
#'
#' @param configPath Path to YAML configuration file.
#' @param envir Environment where legacy `databaseSpecNames` objects are looked up by name.
#'
#' @return List containing workflow object and score results.
#' @examples
#' \dontrun{
#' writeIthacaWorkflowConfigTemplate("config.yaml", overwrite = FALSE)
#' results <- runIthacaWorkflowFromConfig("config.yaml")
#' }
#' @export
runIthacaWorkflowFromConfig <- function(configPath = "config.yaml",
                                        envir = .GlobalEnv) {
  config <- ithReadIthacaWorkflowConfig(configPath)

  databases <- ithResolveDatabasesFromConfig(config = config, envir = envir)

  cohortDefinitionSet <- ithNormalizeCohortDefinitionSet(config$cohortDefinitionSet)


  runIthacaExample(
    conditionSpecs = config$conditionSpecs,
    databases = databases,
    outputDirectory = as.character(config$outputDirectory[[1]]),
    promptDir = config$promptDir,
    rubricTemplates = config$rubricTemplates,
    includeLabs = isTRUE(config$includeLabs),
    useLibraryHints = isTRUE(config$useLibraryHints),
    libraryHintSimilarityThreshold = as.numeric(ithCoalesce(config$libraryHintSimilarityThreshold, 0.94)[[1]]),
    libraryHintTopCandidates = as.integer(ithCoalesce(config$libraryHintTopCandidates, 5)[[1]]),
    useExistingRubric = isTRUE(config$useExistingRubric),
    seedLibraryPath = as.character(config$seedLibraryPath[[1]]),
    useLlmAdjudication = isTRUE(config$useLlmAdjudication),
    llmTopCandidates = as.integer(ithCoalesce(config$llmTopCandidates, 25)[[1]]),
    llmAmbiguityMinScore = config$llmAmbiguityMinScore,
    llmAmbiguityMaxScore = config$llmAmbiguityMaxScore,
    llmTop2GapThreshold = as.numeric(ithCoalesce(config$llmTop2GapThreshold, 0.015)[[1]]),
    llmConfidenceThreshold = as.numeric(ithCoalesce(config$llmConfidenceThreshold, 0.60)[[1]]),
    conceptLibraryVersion = config$conceptLibraryVersion,
    generateCohorts = isTRUE(config$generateCohorts),
    cohortDefinitionSet = cohortDefinitionSet,
    cohortTableName = config$cohortTableName,
    webapiBaseUrl = config$webapiBaseUrl,
    preRunEdit = config$preRunEdit,
    postRunEdit = config$postRunEdit,
    useExistingScores = isTRUE(config$useExistingScores),
    llmClientReasoning = ithGetLlmClientFromConfig(config, "reasoning"),
    llmClientNonReasoning = ithGetLlmClientFromConfig(config, "nonReasoning"),
    verbose = isTRUE(ithCoalesce(config$verbose, TRUE))
  )
}

#' Build random OMOP cohorts from YAML config database mappings
#'
#' Uses `general.cdm` (preferred) or `other.databaseSpecNames` (legacy) to resolve
#' database settings, then calls [buildRandomOmopCohort()] for one or more
#' databases. Cohort table names are derived from `cohortTablePrefix` and have
#' the CDM schema identifier appended automatically.
#'
#' @param configPath Path to YAML configuration file.
#' @param databaseIds Optional character vector of database IDs to run. When
#' `NULL`, all configured databases are used.
#' @param conditionNames Optional character vector of condition names
#' (`rubricName` or `testCondition`) to run. When `NULL`, all configured
#' conditions are used.
#' @param envir Environment where legacy `databaseSpecNames` objects are looked up by name.
#'
#' @return Named list by condition, then by database ID, of results from [buildRandomOmopCohort()].
#' @examples
#' \dontrun{
#' out <- buildRandomOmopCohortFromConfig("config.yaml")
#' }
#' @export
buildRandomOmopCohortFromConfig <- function(configPath = "config.yaml",
                                            databaseIds = NULL,
                                            conditionNames = NULL,
                                            envir = .GlobalEnv) {
  config <- ithReadIthacaWorkflowConfig(configPath)
  databases <- ithResolveDatabasesFromConfig(config = config, envir = envir)

  if (is.null(databaseIds)) {
    selectedIds <- names(databases)
  } else {
    if (!is.character(databaseIds)) {
      stop("databaseIds must be NULL or a character vector")
    }
    missingIds <- setdiff(databaseIds, names(databases))
    if (length(missingIds) > 0) {
      stop("Requested databaseIds are not configured: ", paste(missingIds, collapse = ", "))
    }
    selectedIds <- databaseIds
  }

  conditionSpecs <- config$conditionSpecs
  if (is.null(conditionSpecs) || length(conditionSpecs) == 0) {
    stop("No conditionSpecs available in config")
  }

  if (!is.null(conditionNames)) {
    if (!is.character(conditionNames)) {
      stop("conditionNames must be NULL or a character vector")
    }
    conditionLookup <- vapply(conditionSpecs, function(spec) as.character(ithCoalesce(spec$rubricName, spec$testCondition)[[1]]), character(1))
    selectedIdx <- which(conditionLookup %in% conditionNames)
    if (length(selectedIdx) == 0) {
      stop("No conditionSpecs match conditionNames")
    }
    conditionSpecs <- conditionSpecs[selectedIdx]
  }

  conditionNamesSelected <- vapply(conditionSpecs, function(spec) as.character(ithCoalesce(spec$rubricName, spec$testCondition)[[1]]), character(1))
  pairs <- data.frame(
    conditionIndex = rep(seq_along(conditionSpecs), each = length(selectedIds)),
    databaseId = rep(selectedIds, times = length(conditionSpecs)),
    stringsAsFactors = FALSE
  )
  tasks <- ithRandomCohortTasks(config, databases, conditionSpecs, pairs)
  outcomes <- ithRunRandomCohortTasks(tasks, workers = ithConfigParallelWorkers(config, "cohortGeneration"))

  results <- vector("list", length(conditionSpecs))
  names(results) <- conditionNamesSelected
  for (conditionUp in seq_along(conditionSpecs)) {
    conditionResults <- vector("list", length(selectedIds))
    names(conditionResults) <- selectedIds
    for (id in selectedIds) {
      taskIndex <- which(pairs$conditionIndex == conditionUp & pairs$databaseId == id)
      conditionResults[id] <- list(outcomes[[taskIndex]]$value)
    }
    results[[conditionUp]] <- conditionResults
  }

  results
}

# One task per (conditionIndex, databaseId) row of `pairs`, with everything buildRandomOmopCohort
# needs; missing source cohort or WebAPI settings are reported before anything is built.
ithRandomCohortTasks <- function(config, databases, conditionSpecs, pairs) {
  randomDefaults <- if (!is.null(config$randomCohortDefaults)) {
    ithCoalesce(config$randomCohortDefaults, list())
  } else {
    ithCoalesce(config$randomCohort, list())
  }

  lapply(seq_len(nrow(pairs)), function(pairUp) {
    conditionSpec <- conditionSpecs[[pairs$conditionIndex[[pairUp]]]]
    conditionName <- as.character(ithCoalesce(conditionSpec$rubricName, conditionSpec$testCondition)[[1]])
    id <- pairs$databaseId[[pairUp]]

    sourceCohortDefinitionId <- conditionSpec$sourceCohortDefinitionId
    if (is.null(sourceCohortDefinitionId)) {
      sourceCohortDefinitionId <- randomDefaults$sourceCohortDefinitionId
    }
    if (is.null(sourceCohortDefinitionId) || is.na(as.integer(sourceCohortDefinitionId[[1]]))) {
      stop("sourceCohortDefinitionId is required in conditionSpecs (or randomCohortDefaults) for condition: ", conditionName)
    }

    webapiBaseUrl <- ithCoalesce(config$webapiBaseUrl, randomDefaults$webapiBaseUrl)
    if (is.null(webapiBaseUrl) || !nzchar(as.character(webapiBaseUrl[[1]]))) {
      stop("webapiBaseUrl is required in config (general.webapiBaseUrl) for condition: ", conditionName)
    }

    database <- databases[[id]]
    cohortTableName <- if (!is.null(database$cohortTable) && nzchar(as.character(database$cohortTable[[1]]))) {
      as.character(database$cohortTable[[1]])
    } else {
      ithBuildCohortTableNameFromPrefix(
        cohortTablePrefix = as.character(ithCoalesce(config$cohortTablePrefix, "cohort")[[1]]),
        cdmSchemaName = as.character(ithCoalesce(database$cdm, database$cdmDatabaseSchema)[[1]])
      )
    }
    database$cohortTable <- cohortTableName

    randomSampleFromCohortDefinitionId <- conditionSpec$randomSampleFromCohortDefinitionId
    if (is.null(randomSampleFromCohortDefinitionId)) {
      randomSampleFromCohortDefinitionId <- randomDefaults$randomSampleFromCohortDefinitionId
    }
    if (!is.null(randomSampleFromCohortDefinitionId)) {
      parsedRandomSampleFrom <- suppressWarnings(as.integer(randomSampleFromCohortDefinitionId[[1]]))
      randomSampleFromCohortDefinitionId <- if (is.na(parsedRandomSampleFrom)) NULL else parsedRandomSampleFrom
    }

    conditionRandomCohortDefinitionId <- ithCoalesce(
      conditionSpec$randomCohortDefinitionId,
      ithCoalesce(randomDefaults$randomCohortDefinitionId, 1L)
    )

    list(
      conditionName = conditionName,
      databaseId = id,
      args = list(
        database = database,
        cohort_definition_id = as.integer(conditionRandomCohortDefinitionId[[1]]),
        sourceCohortDefinitionId = as.integer(sourceCohortDefinitionId[[1]]),
        webapiBaseUrl = as.character(webapiBaseUrl[[1]]),
        randomSampleFromCohortDefinitionId = randomSampleFromCohortDefinitionId,
        sample_size = as.integer(ithCoalesce(randomDefaults$sampleSize, 2000000L)[[1]]),
        min_visit_start_date = as.character(ithCoalesce(randomDefaults$minVisitStartDate, "01/01/2000")[[1]]),
        max_visit_end_date = as.character(ithCoalesce(randomDefaults$maxVisitEndDate, "12/31/2026")[[1]]),
        min_age = as.numeric(ithCoalesce(randomDefaults$minAge, 0)[[1]]),
        max_age = as.numeric(ithCoalesce(randomDefaults$maxAge, 120)[[1]]),
        cohortName = as.character(ithCoalesce(randomDefaults$cohortName, "random_visit_sample")[[1]]),
        sourceCohortName = as.character(ithCoalesce(randomDefaults$sourceCohortName, "source_webapi_cohort")[[1]]),
        seedLibraryPath = as.character(ithCoalesce(randomDefaults$seedLibraryPath, config$seedLibraryPath)[[1]]),
        sqlFilePath = as.character(ithCoalesce(randomDefaults$sqlFilePath, file.path("inst", "sql", "sql_server", "build_random_omop_cohort.sql"))[[1]]),
        validationSqlFilePath = as.character(ithCoalesce(randomDefaults$validationSqlFilePath, file.path("inst", "sql", "sql_server", "validate_overwrite_dates.sql"))[[1]])
      )
    )
  })
}

# Builds one task's random cohort. Used in-process and on workers; returns
# list(ok, value, error, seconds) instead of raising.
ithRunRandomCohortJob <- function(task) {
  startedAt <- Sys.time()
  outcome <- tryCatch(
    list(ok = TRUE, value = do.call(buildRandomOmopCohort, task$args), error = NULL),
    error = function(e) list(ok = FALSE, value = NULL, error = conditionMessage(e))
  )
  outcome$seconds <- as.numeric(difftime(Sys.time(), startedAt, units = "secs"))
  outcome
}

# Stops before anything is built when two builds would write the same cohort ID to the same table:
# a shared random cohort ID, a random ID that is also a source or sample pool ID, or (with workers)
# a source or pool cohort that two builds would generate at the same time.
ithCheckRandomCohortTasks <- function(tasks, workers = 1L) {
  rows <- do.call(rbind, lapply(seq_along(tasks), function(index) {
    args <- tasks[[index]]$args
    database <- args$database
    schema <- if (!is.null(database$cohortDatabaseSchema)) database$cohortDatabaseSchema else database$workDatabaseSchema
    pool <- args$randomSampleFromCohortDefinitionId
    roles <- c("random", "source", if (!is.null(pool)) "sample pool")
    data.frame(
      task = index,
      condition = tasks[[index]]$conditionName,
      databaseId = tasks[[index]]$databaseId,
      table = paste0(schema, ".", database$cohortTable),
      cohortId = c(args$cohort_definition_id, args$sourceCohortDefinitionId, if (!is.null(pool)) pool),
      role = roles,
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(rows)) {
    return(invisible(TRUE))
  }

  quoted <- function(values) paste0("'", unique(values), "'", collapse = ", ")
  problems <- character(0)
  for (key in unique(paste(rows$databaseId, rows$table, sep = "\r"))) {
    inTable <- rows[paste(rows$databaseId, rows$table, sep = "\r") == key, , drop = FALSE]
    for (cohortId in unique(inTable$cohortId)) {
      uses <- inTable[inTable$cohortId == cohortId, , drop = FALSE]
      random <- uses[uses$role == "random", , drop = FALSE]
      other <- uses[uses$role != "random", , drop = FALSE]
      where <- paste0("Cohort ID ", cohortId, " on database '", uses$databaseId[[1]], "' (table ", uses$table[[1]], ")")
      if (length(unique(random$task)) > 1L) {
        problems <- c(problems, paste0(
          where, " is the random cohort for ", quoted(random$condition),
          ". Each build replaces the cohort with that ID, so only the last would remain."
        ))
      } else if (nrow(random) > 0L && nrow(other) > 0L) {
        problems <- c(problems, paste0(
          where, " is the random cohort for ", quoted(random$condition), " but is also the ",
          paste(unique(other$role), collapse = " / "), " cohort for ", quoted(other$condition),
          ". Building one would overwrite the other."
        ))
      } else if (workers > 1L && length(unique(other$task)) > 1L) {
        problems <- c(problems, paste0(
          where, " is the ", paste(unique(other$role), collapse = " / "), " cohort for ", quoted(other$condition),
          ". With ", workers, " workers they would generate it at the same time and corrupt it. ",
          "This is only safe with general.parallelWorkers.cohortGeneration set to 1."
        ))
      }
    }
  }

  if (length(problems) > 0L) {
    stop(
      "Random cohort build stopped before anything was built because the cohort IDs in the condition specs conflict:\n  ",
      paste(problems, collapse = "\n  "),
      "\nAmend sourceCohortDefinitionId, randomSampleFromCohortDefinitionId and randomCohortDefinitionId in the ",
      "condition specs so each cohort ID is used by one condition per database, then run again.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Runs the tasks from ithRandomCohortTasks on `workers` processes, printing a start and a finish line
# per task. `onUnitDone(index, error)` runs in this session as each task ends (error is NULL on
# success). A failed task raises an error after the others finish unless stopOnFailure = FALSE.
ithRunRandomCohortTasks <- function(tasks, workers = 1L, stopOnFailure = TRUE, onUnitDone = NULL) {
  workers <- min(max(1L, as.integer(workers)), max(1L, length(tasks)))
  ithCheckRandomCohortTasks(tasks, workers)
  if (workers > 1L) {
    created <- character(0)
    for (task in tasks) {
      database <- task$args$database
      schema <- if (!is.null(database$cohortDatabaseSchema)) database$cohortDatabaseSchema else database$workDatabaseSchema
      key <- paste(database$databaseId, schema, database$cohortTable)
      if (key %in% created) next
      ithCreateCohortTables(database, database$cohortTable, cohortDatabaseSchema = schema)
      created <- c(created, key)
    }
  }

  outcomes <- ithRunJobs(
    tasks, ithRunRandomCohortJob, workers = workers,
    onStart = function(index) {
      task <- tasks[[index]]
      message(sprintf(
        "=== [%s] Random cohort %d/%d: condition '%s' on database '%s' (source cohort %s -> random cohort %s, table %s) ===",
        format(Sys.time(), "%H:%M:%S"), index, length(tasks), task$conditionName, task$databaseId,
        task$args$sourceCohortDefinitionId, task$args$cohort_definition_id, task$args$database$cohortTable
      ))
    },
    onFinish = function(index, outcome) {
      task <- tasks[[index]]
      if (isTRUE(outcome$ok)) {
        message(sprintf(
          "=== [%s] Finished random cohort for condition '%s' on database '%s' in %.1f min ===",
          format(Sys.time(), "%H:%M:%S"), task$conditionName, task$databaseId, outcome$seconds / 60
        ))
      } else {
        message(sprintf(
          "=== [%s] Failed random cohort for condition '%s' on database '%s': %s ===",
          format(Sys.time(), "%H:%M:%S"), task$conditionName, task$databaseId, outcome$error
        ))
      }
      if (!is.null(onUnitDone)) {
        onUnitDone(index, if (isTRUE(outcome$ok)) NULL else outcome$error)
      }
    },
    stopOnFirstError = stopOnFailure
  )

  failed <- which(vapply(outcomes, function(outcome) !is.null(outcome) && !isTRUE(outcome$ok), logical(1)))
  if (isTRUE(stopOnFailure) && length(failed) > 0L) {
    stop(
      paste(vapply(failed, function(index) {
        paste0(
          "Random cohort build failed for condition '", tasks[[index]]$conditionName, "' on database '",
          tasks[[index]]$databaseId, "': ", outcomes[[index]]$error
        )
      }, character(1)), collapse = "\n"),
      call. = FALSE
    )
  }
  outcomes
}

#' Score subjects against random cohort IDs from YAML config
#'
#' Uses `general.cdm` (preferred) or `other.databaseSpecNames` (legacy) to resolve
#' databases, then calls [scoreSubjects()] using each condition's explicit
#' `randomCohortDefinitionId`.
#'
#' @param configPath Path to YAML configuration file.
#' @param databaseIds Optional character vector of database IDs to run. When
#' `NULL`, all configured databases are used.
#' @param conditionNames Optional character vector of condition names
#' (`rubricName` or `testCondition`) to run. When `NULL`, all configured
#' conditions are used.
#' @param envir Environment where legacy `databaseSpecNames` objects are looked up by name.
#'
#' @return Named list of scoring outputs by condition and database, including
#' `scoringSummary` generated from latest saved scoring artifacts.
#' @examples
#' \dontrun{
#' out <- scoreSubjectsAgainstRandomCohortFromConfig("config.yaml")
#' }
#' @export
scoreSubjectsAgainstRandomCohortFromConfig <- function(configPath = "config.yaml",
                                                       databaseIds = NULL,
                                                       conditionNames = NULL,
                                                       envir = .GlobalEnv) {
  config <- ithReadIthacaWorkflowConfig(configPath)
  databases <- ithResolveDatabasesFromConfig(config = config, envir = envir)

  if (is.null(databaseIds)) {

    selectedIds <- names(databases)
  } else {
    if (!is.character(databaseIds)) {
      stop("databaseIds must be NULL or a character vector")
    }
    missingIds <- setdiff(databaseIds, names(databases))
    if (length(missingIds) > 0) {
      stop("Requested databaseIds are not configured: ", paste(missingIds, collapse = ", "))
    }
    selectedIds <- databaseIds
  }

  conditionSpecs <- config$conditionSpecs
  if (is.null(conditionSpecs) || length(conditionSpecs) == 0) {
    stop("No conditionSpecs available in config")
  }

  if (!is.null(conditionNames)) {
    if (!is.character(conditionNames)) {
      stop("conditionNames must be NULL or a character vector")
    }
    conditionLookup <- vapply(conditionSpecs, function(spec) as.character(ithCoalesce(spec$rubricName, spec$testCondition)[[1]]), character(1))
    selectedIdx <- which(conditionLookup %in% conditionNames)
    if (length(selectedIdx) == 0) {
      stop("No conditionSpecs match conditionNames")
    }
    conditionSpecs <- conditionSpecs[selectedIdx]
  }

  scoringConditionSpecs <- lapply(seq_along(conditionSpecs), function(conditionUp) {
    conditionSpec <- conditionSpecs[[conditionUp]]

    resolvedRandomCohortDefinitionId <- suppressWarnings(as.integer(conditionSpec$randomCohortDefinitionId[[1]]))

    conditionLabel <- as.character(ithCoalesce(conditionSpec$rubricName, conditionSpec$testCondition)[[1]])
    if (length(resolvedRandomCohortDefinitionId) == 0 || is.na(resolvedRandomCohortDefinitionId)) {
      stop(
        "randomCohortDefinitionId is required in conditionSpecs for condition: ",
        conditionLabel
      )
    }

    conditionSpec$randomCohortDefinitionId <- resolvedRandomCohortDefinitionId
    conditionSpec
  })

  selectedDatabases <- databases[selectedIds]
  cohortDefinitionSet <- ithNormalizeCohortDefinitionSet(config$cohortDefinitionSet)

  ithScoreSubjects(
    conditionSpecs = scoringConditionSpecs,
    databases = selectedDatabases,
    outputDirectory = as.character(config$outputDirectory[[1]]),
    seedLibraryPath = config$seedLibraryPath,
    conceptLibraryVersion = config$conceptLibraryVersion,
    cohortTableName = config$cohortTableName,
    useExisting = isTRUE(config$useExistingScores),
    generateCohorts = FALSE,
    cohortDefinitionSet = cohortDefinitionSet,
    webapiBaseUrl = config$webapiBaseUrl,
    llmClient = ithGetLlmClientFromConfig(config, "reasoning"),
    promptDir = config$promptDir,
    scoringWorkers = ithConfigParallelWorkers(config, "scoring"),
    cohortWorkers = ithConfigParallelWorkers(config, "cohortGeneration"),
    verbose = isTRUE(ithCoalesce(config$verbose, TRUE))
  )
}

#' Combine full scoring files with concise rubric text
#'
#' Reads the latest `fullScoringFile_Cohort*.rds` for every configured condition
#' and database, appends the matching concise-rubric `criteria` and `rationale`
#' columns, and writes a CSV beside each source RDS.
#'
#' @param configPath Path to YAML configuration file.
#' @param outputDirectory Optional output root for generated CSV files. When
#' `NULL`, files are written under the configured output directory.
#' @param databaseIds Optional character vector of database IDs to process. When
#' `NULL`, all configured databases are used.
#' @param conditionNames Optional character vector of condition names to process.
#' When `NULL`, all configured conditions are used.
#' @param verbose Logical; when `TRUE`, print progress messages.
#' @param envir Environment where legacy database specification objects are looked up.
#' @return Named list with one entry per condition/database. Each entry contains
#' `status`, `inputPath`, and `outputPath`; missing artifacts are reported as
#' `status = "missing"`.
#' @examples
#' \dontrun{
#' outputs <- combineFullScoringFilesFromConfig("config.yaml")
#' }
#' @export
combineFullScoringFilesFromConfig <- function(configPath = "config.yaml",
                                              outputDirectory = NULL,
                                              databaseIds = NULL,
                                              conditionNames = NULL,
                                              verbose = TRUE,
                                              envir = .GlobalEnv) {
  config <- ithReadIthacaWorkflowConfig(configPath)
  databases <- ithResolveDatabasesFromConfig(config = config, envir = envir)
  configuredOutputDirectory <- as.character(config$outputDirectory[[1]])
  if (is.null(outputDirectory)) {
    resolvedOutputDirectory <- configuredOutputDirectory
  } else {
    if (!is.character(outputDirectory) || length(outputDirectory) != 1L ||
        !nzchar(trimws(outputDirectory))) {
      stop("outputDirectory must be NULL or one non-empty path")
    }
    resolvedOutputDirectory <- outputDirectory[[1]]
  }

  if (is.null(databaseIds)) {
    selectedDatabaseIds <- names(databases)
  } else {
    if (!is.character(databaseIds)) {
      stop("databaseIds must be NULL or a character vector")
    }
    missingDatabaseIds <- setdiff(databaseIds, names(databases))
    if (length(missingDatabaseIds) > 0L) {
      stop("Requested databaseIds are not configured: ", paste(missingDatabaseIds, collapse = ", "))
    }
    selectedDatabaseIds <- databaseIds
  }

  conditionSpecs <- config$conditionSpecs
  if (is.null(conditionSpecs) || length(conditionSpecs) == 0L) {
    stop("No conditionSpecs available in config")
  }
  conditionLookup <- vapply(
    conditionSpecs,
    function(spec) as.character(ithCoalesce(spec$rubricName, spec$testCondition)[[1]]),
    character(1)
  )
  if (!is.null(conditionNames)) {
    if (!is.character(conditionNames)) {
      stop("conditionNames must be NULL or a character vector")
    }
    selectedConditionIdx <- which(conditionLookup %in% conditionNames)
    if (length(selectedConditionIdx) == 0L) {
      stop("No conditionSpecs match conditionNames")
    }
    conditionSpecs <- conditionSpecs[selectedConditionIdx]
    conditionLookup <- conditionLookup[selectedConditionIdx]
  }

  results <- list()
  for (conditionUp in seq_along(conditionSpecs)) {
    conditionName <- conditionLookup[[conditionUp]]
    conditionDirectory <- file.path(configuredOutputDirectory, conditionName)
    concise <- ithLoadLatest(conditionDirectory, paste0(conditionName, "_conciseRubric.csv"))
    if (is.null(concise)) {
      stop("No concise rubric found for condition: ", conditionName)
    }
    requiredRubricColumns <- c("rubric", "ruleId", "criteria", "rationale")
    missingRubricColumns <- setdiff(requiredRubricColumns, names(concise))
    if (length(missingRubricColumns) > 0L) {
      stop(
        "Concise rubric for condition '", conditionName,
        "' is missing required columns: ", paste(missingRubricColumns, collapse = ", ")
      )
    }

    for (databaseId in selectedDatabaseIds) {
      key <- paste(conditionName, databaseId, sep = "::")
      scoringDirectory <- file.path(configuredOutputDirectory, "scoring", conditionName, databaseId)
      randomCohortDefinitionId <- conditionSpecs[[conditionUp]]$randomCohortDefinitionId
      if (is.null(randomCohortDefinitionId) || is.na(randomCohortDefinitionId[[1]])) {
        results[[key]] <- list(status = "missing", inputPath = NULL, outputPath = NULL)
        warning("Skipping ", key, ": randomCohortDefinitionId is missing", call. = FALSE)
        next
      }
      baseName <- paste0("fullScoringFile_Cohort", as.integer(randomCohortDefinitionId[[1]]), ".rds")
      latestName <- ithFindLatestVersion(scoringDirectory, baseName)
      if (is.null(latestName)) {
        results[[key]] <- list(status = "missing", inputPath = NULL, outputPath = NULL)
        ithMaybeMessage(verbose, "Skipping ", key, ": no full scoring file found.")
        next
      }

      inputPath <- file.path(scoringDirectory, latestName)
      fullScoring <- readRDS(inputPath)
      if (is.null(fullScoring)) {
        fullScoring <- data.frame(stringsAsFactors = FALSE)
      }
      if (!is.data.frame(fullScoring)) {
        fullScoring <- as.data.frame(fullScoring, stringsAsFactors = FALSE)
      }
      missingScoringColumns <- setdiff(c("rubricId", "ruleId"), names(fullScoring))
      if (length(missingScoringColumns) > 0L) {
        stop(
          "Full scoring file '", inputPath,
          "' is missing required columns: ", paste(missingScoringColumns, collapse = ", ")
        )
      }

      rubricText <- concise[, c("rubric", "ruleId", "criteria", "rationale"), drop = FALSE]
      names(rubricText)[names(rubricText) == "rubric"] <- "rubricId"
      rubricText <- rubricText[!duplicated(rubricText[c("rubricId", "ruleId")]), , drop = FALSE]
      combined <- merge(fullScoring, rubricText, by = c("rubricId", "ruleId"), all.x = TRUE, sort = FALSE)
      outputPath <- file.path(
        resolvedOutputDirectory,
        paste0(
          conditionName, "_", databaseId, "_",
          sub("\\.rds$", ".csv", latestName, ignore.case = TRUE)
        )
      )
      dir.create(dirname(outputPath), recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(combined, outputPath, row.names = FALSE, na = "")
      results[[key]] <- list(status = "written", inputPath = inputPath, outputPath = outputPath)
      ithMaybeMessage(verbose, "Wrote combined scoring file: ", outputPath)
    }
  }

  results
}

#' Summarize the most common rule combinations per condition
#'
#' For each configured condition, finds the rules each random-cohort subject met in the latest
#' `fullScoringFile_Cohort*.rds` of every database and counts subjects per combination of rules.
#' Combinations are ranked by their counts pooled across databases, and the CSV
#' `<condition>_ruleCombinations_Cohort<id>_V#.csv` has pooled and per-database
#' `<db>_subjectCount` / `<db>_proportionOfSubjects` columns (proportions rounded to 3 places,
#' relative to all subjects in that database's random cohort). The final row, `(none)`, counts
#' subjects who met no rules, using the subjects in the latest `finalScores_Cohort*.rds`; it is
#' not part of `topN`.
#'
#' @param configPath Path to YAML configuration file.
#' @param outputDirectory Optional folder for the CSV files. `NULL` uses the configured output directory.
#' @param databaseIds Optional character vector of database IDs to include. `NULL` uses all configured databases.
#' @param conditionNames Optional character vector of condition names. `NULL` uses all configured conditions.
#' @param topN Number of combinations to keep per condition.
#' @param verbose Logical; when `TRUE`, print progress messages.
#' @param envir Environment where legacy database specification objects are looked up.
#' @return Named list by condition with `status`, `outputPath`, `databases`, and `totalSubjects`.
#' @examples
#' \dontrun{
#' summarizeRuleCombinationsFromConfig("config.yaml", topN = 100)
#' }
#' @export
summarizeRuleCombinationsFromConfig <- function(configPath = "config.yaml",
                                                outputDirectory = NULL,
                                                databaseIds = NULL,
                                                conditionNames = NULL,
                                                topN = 100L,
                                                verbose = TRUE,
                                                envir = .GlobalEnv) {
  topN <- as.integer(topN)
  if (length(topN) != 1L || is.na(topN) || topN < 1L) {
    stop("topN must be a positive whole number")
  }
  config <- ithReadIthacaWorkflowConfig(configPath)
  databases <- ithResolveDatabasesFromConfig(config = config, envir = envir)
  configuredOutputDirectory <- as.character(config$outputDirectory[[1]])
  resolvedOutputDirectory <- if (is.null(outputDirectory)) configuredOutputDirectory else as.character(outputDirectory[[1]])

  selectedDatabaseIds <- if (is.null(databaseIds)) names(databases) else databaseIds
  missingDatabaseIds <- setdiff(selectedDatabaseIds, names(databases))
  if (length(missingDatabaseIds) > 0L) {
    stop("Requested databaseIds are not configured: ", paste(missingDatabaseIds, collapse = ", "))
  }

  conditionSpecs <- config$conditionSpecs
  conditionLookup <- vapply(
    conditionSpecs,
    function(spec) as.character(ithCoalesce(spec$rubricName, spec$testCondition)[[1]]),
    character(1)
  )
  if (!is.null(conditionNames)) {
    keep <- which(conditionLookup %in% conditionNames)
    if (length(keep) == 0L) {
      stop("No conditionSpecs match conditionNames")
    }
    conditionSpecs <- conditionSpecs[keep]
    conditionLookup <- conditionLookup[keep]
  }

  results <- list()
  for (conditionUp in seq_along(conditionSpecs)) {
    conditionName <- conditionLookup[[conditionUp]]
    cohortId <- suppressWarnings(as.integer(conditionSpecs[[conditionUp]]$randomCohortDefinitionId[[1]]))
    concise <- ithLoadLatest(file.path(configuredOutputDirectory, conditionName), paste0(conditionName, "_conciseRubric.csv"))
    if (is.null(concise) || is.na(cohortId)) {
      results[[conditionName]] <- list(status = "missing", outputPath = NULL, databases = character(), totalSubjects = 0L)
      ithMaybeMessage(verbose, "Skipping ", conditionName, ": no concise rubric or randomCohortDefinitionId.")
      next
    }

    ruleKeys <- paste(concise$rubric, concise$ruleId, sep = "/")
    criteriaByRule <- stats::setNames(as.character(concise$criteria), ruleKeys)
    counts <- list()
    databaseTotals <- numeric()
    scoreByRule <- numeric()
    usedDatabases <- character()
    totalSubjects <- 0L

    for (databaseId in selectedDatabaseIds) {
      scoringDirectory <- file.path(configuredOutputDirectory, "scoring", conditionName, databaseId)
      fullName <- ithFindLatestVersion(scoringDirectory, paste0("fullScoringFile_Cohort", cohortId, ".rds"))
      finalName <- ithFindLatestVersion(scoringDirectory, paste0("finalScores_Cohort", cohortId, ".rds"))
      if (is.null(fullName) || is.null(finalName)) {
        ithMaybeMessage(verbose, "Skipping ", conditionName, "::", databaseId, ": scoring files not found.")
        next
      }
      scored <- readRDS(file.path(scoringDirectory, fullName))
      finalScores <- readRDS(file.path(scoringDirectory, finalName))
      scoredRules <- unique(data.frame(
        ruleKey = paste(scored$rubricId, scored$ruleId, sep = "/"), score = as.numeric(scored$score), stringsAsFactors = FALSE
      ))
      newRules <- scoredRules[!duplicated(scoredRules$ruleKey) & !(scoredRules$ruleKey %in% names(scoreByRule)), , drop = FALSE]
      scoreByRule[newRules$ruleKey] <- newRules$score
      subjects <- data.table::as.data.table(scored)[, c("subjectId", "cohortStartDate", "rubricId", "ruleId")]
      subjects[, ruleKey := paste(rubricId, ruleId, sep = "/")]
      subjects <- unique(subjects[, c("subjectId", "cohortStartDate", "ruleKey")])
      levelsInOrder <- unique(c(ruleKeys, subjects$ruleKey))
      subjects[, ruleIndex := match(ruleKey, levelsInOrder)]
      data.table::setorderv(subjects, c("subjectId", "cohortStartDate", "ruleIndex"))
      combinations <- subjects[
        , .(combination = paste(ruleKey, collapse = "; "), ruleCount = .N),
        by = .(subjectId, cohortStartDate)
      ]
      databaseCounts <- combinations[, .(subjectCount = .N), by = .(combination, ruleCount)]

      databaseSubjects <- nrow(unique(data.table::as.data.table(finalScores)[, c("subjectId", "cohortStartDate")]))
      noRuleSubjects <- databaseSubjects - nrow(combinations)
      if (noRuleSubjects > 0L) {
        databaseCounts <- rbind(
          databaseCounts,
          data.table::data.table(combination = "(none)", ruleCount = 0L, subjectCount = noRuleSubjects)
        )
      }
      counts[[databaseId]] <- as.data.frame(databaseCounts)
      databaseTotals[[databaseId]] <- databaseSubjects
      usedDatabases <- c(usedDatabases, databaseId)
      totalSubjects <- totalSubjects + databaseSubjects
    }

    if (length(counts) == 0L) {
      results[[conditionName]] <- list(status = "missing", outputPath = NULL, databases = character(), totalSubjects = 0L)
      next
    }

    allCounts <- do.call(rbind, counts)
    pooled <- stats::aggregate(subjectCount ~ combination + ruleCount, data = allCounts, FUN = sum)
    ranked <- pooled[pooled$combination != "(none)", , drop = FALSE]
    ranked <- ranked[order(-ranked$subjectCount, ranked$ruleCount, ranked$combination), , drop = FALSE]
    ranked <- utils::head(ranked, topN)
    ranked$rank <- seq_len(nrow(ranked))
    noRuleRow <- data.frame(
      combination = "(none)", ruleCount = 0L,
      subjectCount = sum(allCounts$subjectCount[allCounts$combination == "(none)"]),
      rank = NA_integer_, stringsAsFactors = FALSE
    )
    pooled <- rbind(ranked, noRuleRow)
    pooled$proportionOfSubjects <- round(pooled$subjectCount / totalSubjects, 3)
    pooled$totalScore <- vapply(pooled$combination, function(combination) {
      rules <- strsplit(combination, "; ", fixed = TRUE)[[1]]
      sum(scoreByRule[rules[rules != "(none)"]], na.rm = TRUE)
    }, numeric(1), USE.NAMES = FALSE)
    for (databaseId in usedDatabases) {
      databaseCounts <- counts[[databaseId]]
      found <- match(pooled$combination, databaseCounts$combination)
      databaseSubjectCounts <- ifelse(is.na(found), 0L, databaseCounts$subjectCount[found])
      pooled[[paste0(databaseId, "_subjectCount")]] <- as.integer(databaseSubjectCounts)
      pooled[[paste0(databaseId, "_proportionOfSubjects")]] <- round(databaseSubjectCounts / databaseTotals[[databaseId]], 3)
    }
    pooled$criteria <- vapply(pooled$combination, function(combination) {
      rules <- strsplit(combination, "; ", fixed = TRUE)[[1]]
      rules <- rules[rules != "(none)"]
      paste0(rules, ": ", ifelse(is.na(criteriaByRule[rules]), "", criteriaByRule[rules]), collapse = " | ")
    }, character(1), USE.NAMES = FALSE)
    databaseColumns <- as.vector(rbind(paste0(usedDatabases, "_subjectCount"), paste0(usedDatabases, "_proportionOfSubjects")))
    pooled <- pooled[, c("rank", "combination", "ruleCount", "totalScore", "subjectCount", "proportionOfSubjects", databaseColumns, "criteria")]
    names(pooled) <- c(
      "Rank", "Combination", "Rule Count", "Total Score", "Subject Count", "Proportion Of Subjects",
      as.vector(rbind(paste(usedDatabases, "Subject Count"), paste(usedDatabases, "Proportion Of Subjects"))),
      "Criteria"
    )

    outputPath <- ithSaveVersioned(
      saveDirectory = resolvedOutputDirectory,
      objectToSave = pooled,
      fileName = paste0(conditionName, "_ruleCombinations_Cohort", cohortId, ".csv")
    )
    results[[conditionName]] <- list(
      status = "written", outputPath = outputPath, databases = usedDatabases, totalSubjects = totalSubjects
    )
    ithMaybeMessage(verbose, "Wrote rule combination summary: ", outputPath)
  }

  results
}

#' Apply concise rubric edits for every condition in a config
#'
#' For each condition in the config (including `conditionSpecsCsvPath`), compares the
#' latest `<condition>_conciseRubric_V*.csv` with the latest short rubric and runs
#' [applyConciseRubricEdits()] only when they differ. SQL columns are not compared.
#' The rule summary (an LLM call) is refreshed only when rules, criteria, or needed
#' concept sets changed; score and threshold edits make no LLM calls.
#'
#' @param configPath Path to YAML configuration file.
#' @param conditionNames Optional character vector of condition names
#' (`rubricName` or `testCondition`) to check. When `NULL`, all configured
#' conditions are checked.
#'
#' @return Data frame with one row per condition: `conditionName`, `status`
#' (`unchanged`, `applied`, `skipped: ...`, or `error: ...`), and the kinds of change found.
#' @examples
#' \dontrun{
#' results <- applyConciseRubricEditsFromConfig("config.yaml")
#' }
#' @export
applyConciseRubricEditsFromConfig <- function(configPath = "config.yaml",
                                              conditionNames = NULL) {
  config <- ithReadIthacaWorkflowConfig(configPath)
  outputDirectory <- as.character(config$outputDirectory[[1]])
  verbose <- isTRUE(ithCoalesce(config$verbose, TRUE))

  selectedNames <- vapply(
    config$conditionSpecs,
    function(spec) as.character(ithCoalesce(spec$rubricName, spec$testCondition)[[1]]),
    character(1)
  )
  if (!is.null(conditionNames)) {
    if (!is.character(conditionNames)) {
      stop("conditionNames must be NULL or a character vector")
    }
    missingNames <- setdiff(conditionNames, selectedNames)
    if (length(missingNames) > 0) {
      stop("Requested conditionNames are not configured: ", paste(missingNames, collapse = ", "))
    }
    selectedNames <- selectedNames[selectedNames %in% conditionNames]
  }

  statusRow <- function(conditionName, status, changes = "") {
    data.frame(conditionName = conditionName, status = status, changes = changes, stringsAsFactors = FALSE)
  }

  results <- lapply(selectedNames, function(conditionName) {
    conditionDir <- file.path(outputDirectory, conditionName)
    latestFile <- ithFindLatestVersion(conditionDir, paste0(conditionName, "_conciseRubric.csv"))
    if (is.null(latestFile)) {
      return(statusRow(conditionName, "skipped: no concise rubric found"))
    }

    tryCatch({
      concisePath <- file.path(conditionDir, latestFile)
      changes <- ithDiffConciseRubric(
        utils::read.csv(concisePath, stringsAsFactors = FALSE),
        ithLoadLatestShortRubric(conditionName, outputDirectory)
      )
      changed <- setdiff(names(changes)[vapply(changes, isTRUE, logical(1))], "differs")

      if (!isTRUE(changes$differs)) {
        ithMaybeMessage(verbose, "No concise rubric changes for condition: ", conditionName)
        return(statusRow(conditionName, "unchanged"))
      }

      applyConciseRubricEdits(
        conditionName = conditionName,
        outputDirectory = outputDirectory,
        editedConciseRubricPath = concisePath,
        llmClient = ithGetLlmClientFromConfig(config, "reasoning"),
        promptDir = config$promptDir,
        updateSummary = changes$rulesChanged || changes$criteriaChanged || changes$conceptSetsChanged,
        verbose = verbose,
        adjustableRuleId = ithFindConfigConditionSpec(config, conditionName)$adjustableRuleId
      )
      statusRow(conditionName, "applied", paste(changed, collapse = ", "))
    }, error = function(e) {
      statusRow(conditionName, paste("error:", conditionMessage(e)))
    })
  })

  results <- do.call(rbind, results)
  failed <- startsWith(results$status, "error")
  if (any(failed)) {
    warning(sum(failed), " condition(s) failed; see the status column.")
  }

  results
}

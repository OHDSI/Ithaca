library(httr)
library(jsonlite)
library(SqlRender)
library(DatabaseConnector)
library(Helios)
library(dplyr)
library(data.table)
library(stringr)
library(RubricConcepts)


options(databaseConnectorInteger64AsNumeric = FALSE)

getSubjectData <- function(subjectsToTest, database, endDays, exclude = FALSE, excludedConditionCode = NULL) {
  connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = database$connectionDetails))
  sqlQuery <- readSql("./inst/sql/sql_server/patientProfilesGroup.sql")
  dfAll <- NULL
  for(test in 1:nrow(subjectsToTest)) {
    subjectUp <- test
    ParallelLogger::logInfo("", subjectUp, ") Getting data for Subject Id: ", subjectsToTest$subjectId[subjectUp])
    sql <- render(sqlQuery,
                  cdm_schema = database$cdmDatabaseSchema,
                  subjectId = subjectsToTest$subjectId[subjectUp],
                  endDays = endDays,
                  startDate = paste0('"',subjectsToTest$cohortStartDate[subjectUp],'"'))
    df <- querySql(connection, sql, snakeCaseToCamelCase = TRUE)
    df <- df[,c(1:3)]
    dfAll <- rbind(dfAll, df)
  }
  dfAll$dayInStudy <- as.integer(dfAll$dayInStudy)
  colnames(dfAll)[colnames(dfAll) %in% c("conceptName","dayInStudy")] <- c("Health Record Item","Day in Study")


  disconnect(connection)
  return(dfAll)
}

getCohortSubjectData <- function(cohortsToTest, database, endDays) {
  connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = database$connectionDetails))
  sqlQuery <- readSql("./inst/sql/sql_server/subjectDataCohorts.sql")
  subjectUp <- test
  ParallelLogger::logInfo("--Getting subject data for cohorts: ", cohortsToTest)
  tempTable <- gsub("[.]", "_", paste0("pv_table_", database$cdmDatabaseSchema)) #this will hold all the cohorts for all the runs

  sql <- render(sqlQuery,
                cdm_schema = database$cdmDatabaseSchema,
                cohortSchema = database$workDatabaseSchema,
                cohortTable = tempTable,
                cohorts = cohortsToTest,
                endDays = endDays)
  df <- querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  disconnect(connection)
  return(df)
}

getSubjectDataSingleOld <- function(connection, subjectsToTest, database, endDays, excludedConditionCode = NULL) {
  # connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = database$connectionDetails))
  sqlQuery <- readSql("./inst/sql/sql_server/patientProfilesGroup.sql")
  sql <- render(sqlQuery,
                cdm_schema = database$cdmDatabaseSchema,
                subjectId = subjectsToTest$subjectId,
                endDays = endDays,
                excludedConditionCode = excludedConditionCode,
                startDate = paste0('"',subjectsToTest$cohortStartDate,'"'))
  df <- querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  df$dayInStudy <- as.integer(df$dayInStudy)
  colnames(df)[colnames(df) %in% c("conceptName","dayInStudy")] <- c("Health Record Item","Day in Study")

  # disconnect(connection)
  return(df)
}

getSubjectDataSingle <- function(connection, subjectsToTest, database, endDays, excludedConditionCode = "") {
  if(!is.null(excludedConditionCode)) {
    ParallelLogger::logInfo("--Excluding the descendants of Concept ID: ",
                            paste(unlist(excludedConditionCode, use.names = FALSE), collapse = ", "))
  }

  connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = database$connectionDetails))
  sqlQuery <- readSql("./inst/sql/sql_server/patientProfilesGroup.sql")

  finalList <- ""
  for(subUp in 1:(nrow(subjectsToTest)-1)) {
    finalList <- paste0(finalList, "(", subjectsToTest$subjectId[subUp], ', "', subjectsToTest$cohortStartDate[subUp],'"),')
  }
  finalList <- paste0(finalList, "(", subjectsToTest$subjectId[subUp+1], ', "', subjectsToTest$cohortStartDate[subUp+1],'")')

  sql <- render(sqlQuery,
                cdm_schema = database$cdmDatabaseSchema,
                allValues = finalList,
                endDays = endDays,
                excludedConditionCode = paste(unlist(excludedConditionCode, use.names = FALSE), collapse = ", "))
  df <- querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  df$dayInStudy <- as.integer(df$dayInStudy)
  colnames(df)[colnames(df) %in% c("conceptName","dayInStudy")] <- c("Health Record Item","Day in Study")

  disconnect(connection)
  return(df)
}

queryLLM <- function(llmClient, prompt, systemPrompt = NULL, silent = TRUE, output = "data frame", array = F, ellmerTypeObject = NULL) {
  if(!silent) {ParallelLogger::logInfo("\n--Querying LLM...")}

  retry_limit <- 10  # Maximum number of retries
  attempt <- 0      # Initial attempt counter
  success <- FALSE  # Flag to indicate success

  while (attempt <= retry_limit && !success) { #llm will mislabel column headers occasionally - usually fixed with a re-try
    tryCatch({
      attempt <- attempt + 1  # Increment the attempt count

      # text <- getLlmResponse(prompt = prompt, systemPrompt = systemPrompt, reportTime = F, model = model)

      if(output == "text") { #simple text return
        text <- llmClient$chat(prompt,
                               echo = "none")
        results <- text
      } else { #json return
        text <- llmClient$chat_structured(prompt,
                                          echo = "none",
                                          type = ellmerTypeObject
        )

        if(!array) {fromLLM <- fromJSON(toJSON(text)) #fromJSON(toJSON(text), simplifyVector = FALSE)
        } else {fromLLM <- fromJSON(text)
        }

        fromLLM$cost <- sprintf("%.4f", llmClient$get_cost(include = "last"))

        if(output == "data frame") {
          results <- data.frame(fromLLM)
        } else {
          results <- fromLLM
        }
      }
      success <- TRUE
    },
    error = function(e) {
      llmClient$set_turns(list()) # Reset the chat
      # Handle the error: print a message and increment the attempt counter
      message(paste("Attempt", attempt, "failed:", e$message))
      if(grepl("abort", e$message, ignore.case=TRUE)) {
        cat("Stopping the run as requested.\n")
        stop("Execution stopped by user.")
      }
      if(attempt >= retry_limit) {
        message(paste("Reached attempt limit."))
        if(e$message != "Content filter triggered") { #only abort without asking for Content filter triggered
          user_input <- readline(prompt = "Do you want to continue with the next concept? y/n ")
          if(user_input == tolower('n')) {
            cat("Stopping the run as requested.\n")
            stop("Execution stopped by user.")
          }
        }
      }
      return(NULL)  # Return NULL in case of error
    })
  }

  return(results)
}

generateCohorts <- function(cohortList, dbList, drive) {
  library(DatabaseConnector)
  library(CohortGenerator)

  incrementalFolder = paste0(drive,":/ProjectData/cohortGenerationIncrementalFolder")

  for (dbUp in 1:length(dbList)) {
    connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = dbList[[dbUp]]$connectionDetails))

    tempTable <- gsub("[.]", "_", paste0("pv_table_", dbList[[dbUp]]$cdmDatabaseSchema)) #this will hold all the cohorts for all the runs

    if(grepl(substr(dbList[[dbUp]]$cdmDatabaseSchema, nchar(dbList[[dbUp]]$cdmDatabaseSchema),
                    nchar(dbList[[dbUp]]$cdmDatabaseSchema)), "0123456789")) {
      incTable <- dbList[[dbUp]]$cdmDatabaseSchema #used for ohda cdms
    } else {
      incTable <- paste0(dbList[[dbUp]]$databaseId, "_v", dbList[[dbUp]]$cdmVersion, "_rhealth") #for rhealth cdms
    }

    #check if database file is currently locked and wait until it is unlocked
    timeToGiveUp <- 0
    while (file.exists(file.path(incrementalFolder, incTable, "fileLocked.csv")) & timeToGiveUp <= 120) {
      #if database is in use
      timeToGiveUp <- timeToGiveUp + 1
      timeToGiveUpS <- sprintf('%3d', timeToGiveUp)
      if(timeToGiveUp == 1) {cat(paste0("Waiting for database to be unlocked - Try: ", timeToGiveUpS))}
      cat(paste0("\b\b\b", timeToGiveUpS))
      Sys.sleep(60) #wait 60 seconds
    }

    #remove lock file after 1 hour
    if(timeToGiveUp >= 60) {
      writeLines(paste0("\n Database file locked for over 2 hours...removing lock file"))
      file.remove(file.path(incrementalFolder, incTable, "fileLocked.csv"))
    }

    #before writing to database, lock the database file
    dir.create(file.path(file.path(incrementalFolder, incTable)), showWarnings = FALSE)
    write.csv("Locked", file.path(incrementalFolder, incTable, "fileLocked.csv"))

    httr::set_config(httr::config(ssl_verifypeer = FALSE))
    baseUrl <- "https://epi.jnj.com:8443/WebAPI"

    ROhdsiWebApi::authorizeWebApi(baseUrl, "windows") # Windows
    cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(baseUrl = baseUrl,
                                                                   cohortIds = unique(cohortList),
                                                                   generateStats = TRUE)

    cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = tempTable)

    CohortGenerator::createCohortTables(connectionDetails = dbList[[dbUp]]$connectionDetails,
                                        cohortTableNames = cohortTableNames,
                                        cohortDatabaseSchema = dbList[[dbUp]]$workDatabaseSchema,
                                        incremental = T)

    cohortsGenerated <- CohortGenerator::generateCohortSet(connectionDetails = dbList[[dbUp]]$connectionDetails,
                                                           cdmDatabaseSchema = dbList[[dbUp]]$cdmDatabaseSchema,
                                                           cohortDatabaseSchema = dbList[[dbUp]]$workDatabaseSchema,
                                                           cohortTableNames = cohortTableNames,
                                                           cohortDefinitionSet = cohortDefinitionSet,
                                                           incrementalFolder = file.path(incrementalFolder,
                                                                                         incTable),
                                                           incremental = T)

    #unlock the database file after completing writing
    file.remove(file.path(incrementalFolder, incTable, "fileLocked.csv"))

    #get the cohort counts
    sql <- paste0("select cohort_definition_id, count(*) as SubjectCount from ",dbList[[dbUp]]$workDatabaseSchema, ".", cohortTableNames$cohortTable,
                  " where cohort_definition_id in (", paste(cohortList, collapse = ", "), ") group by cohort_definition_id")

    connect <- connect(dbList[[dbUp]]$connectionDetails)
    ParallelLogger::logInfo("Database: ", dbList[[dbUp]]$databaseId )
    print(renderTranslateQuerySql(connect, sql))

    disconnect(connect)
  }
}

getCohortData <- function(cohortList, dbList, drive) {
  library(DatabaseConnector)
  library(CohortGenerator)

  incrementalFolder = paste0(drive,":/ProjectData/cohortGenerationIncrementalFolder")

  for (dbUp in 1:length(dbList)) {
    ParallelLogger::logInfo("Database: ", dbList[[dbUp]]$databaseId )
    connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = dbList[[dbUp]]$connectionDetails))

    tempTable <- gsub("[.]", "_", paste0("pv_table_", dbList[[dbUp]]$cdmDatabaseSchema)) #this will hold all the cohorts for all the runs

    if(grepl(substr(dbList[[dbUp]]$cdmDatabaseSchema, nchar(dbList[[dbUp]]$cdmDatabaseSchema),
                    nchar(dbList[[dbUp]]$cdmDatabaseSchema)), "0123456789")) {
      incTable <- dbList[[dbUp]]$cdmDatabaseSchema #used for ohda cdms
    } else {
      incTable <- paste0(dbList[[dbUp]]$databaseId, "_v", dbList[[dbUp]]$cdmVersion, "_rhealth") #for rhealth cdms
    }

    httr::set_config(httr::config(ssl_verifypeer = FALSE))
    baseUrl <- "https://epi.jnj.com:8443/WebAPI"

    ROhdsiWebApi::authorizeWebApi(baseUrl, "windows") # Windows
    cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(baseUrl = baseUrl,
                                                                   cohortIds = unique(cohortList),
                                                                   generateStats = TRUE)

    cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = tempTable)

    #get the cohort counts
    sql <- paste0("select cohort_definition_id, count(*) as SubjectCount from ",dbList[[dbUp]]$workDatabaseSchema, ".", cohortTableNames$cohortTable,
                  " where cohort_definition_id in (", paste(cohortList, collapse = ", "), ") group by cohort_definition_id")

    connect <- suppressMessages(connect(dbList[[dbUp]]$connectionDetails))
    cohortCount <- renderTranslateQuerySql(connect, sql, snakeCaseToCamelCase = T)
    disconnect(connect)
    return(merge(cohortDefinitionSet, cohortCount, by.x = "cohortId", by.y = "cohortDefinitionId"))
  }
}

getRandomSample <- function(cohort, database, count) {
  library(SqlRender)
  library(DatabaseConnector)

  tempTable <- gsub("[.]", "_", paste0("pv_table_", database$cdmDatabaseSchema)) #this will hold all the cohorts for all the runs

  sql <- paste0("SELECT TOP ", count," subject_id, cohort_start_date ",
                "FROM ", database$workDatabaseSchema, ".", tempTable,
                " where cohort_definition_id in (", cohort, ")",
                " ORDER BY NEWID();")
  sql <- translate(sql, "spark")

  connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = database$connectionDetails))
  results <- querySql(sql, connection = connection, snakeCaseToCamelCase = TRUE)
  disconnect(connection)

  return(results)
}

getSpecifiedSample <- function(cohort, database, subjectList, count = 10000000) {
  library(SqlRender)
  library(DatabaseConnector)

  tempTable <- gsub("[.]", "_", paste0("pv_table_", database$cdmDatabaseSchema)) #this will hold all the cohorts for all the runs

  sql <- paste0("SELECT subject_id, cohort_start_date ",
                "FROM ", database$workDatabaseSchema, ".", tempTable,
                " where cohort_definition_id in (", cohort, ")",
                " and subject_id in (", subjectList, ")")
  sql <- translate(sql, "spark")

  connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = database$connectionDetails))
  results <- querySql(sql, connection = connection, snakeCaseToCamelCase = TRUE)
  disconnect(connection)

  set.seed(123)
  idx <- sample.int(nrow(results), size = count, replace = FALSE)
  results <- results[idx, ]

  return(results)
}

getSubjects <- function(subjects, cohorts, database) {
  library(SqlRender)
  library(DatabaseConnector)

  tempTable <- gsub("[.]", "_", paste0("pv_table_", database$cdmDatabaseSchema)) #this will hold all the cohorts for all the runs

  sql <- paste0("SELECT distinct subject_id, max(cohort_start_date) as cohort_start_date ",
                "FROM ", database$workDatabaseSchema, ".", tempTable,
                " where cohort_definition_id in (", cohorts, ") ",
                " and subject_id in (", subjects, ") ",
                " group by subject_id;")
  sql <- translate(sql, "spark")

  connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = database$connectionDetails))

  results <- querySql(sql, connection = connection, snakeCaseToCamelCase = TRUE)
  disconnect(connection)

  return(results)
}

getCohortSubjects <- function(cohorts, database, cohortTable) {
  library(SqlRender)
  library(DatabaseConnector)

  # tempTable <- gsub("[.]", "_", paste0("pv_table_", database$cdmDatabaseSchema)) #this will hold all the cohorts for all the runs

  sql <- paste0("SELECT * ",
                "FROM ", database$workDatabaseSchema, ".", cohortTable,
                " where cohort_definition_id in (", cohorts, "); ")
  sql <- translate(sql, "spark")

  connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = database$connectionDetails))

  results <- querySql(sql, connection = connection, snakeCaseToCamelCase = TRUE)
  disconnect(connection)

  return(results)
}

getIngredients <- function(database, conceptList) {
  sqlFile <- readSql("./inst/sql/sql_server/getIngredients.sql")
  sql <- render(sqlFile,
                cdm_schema = database$cdmDatabaseSchema,
                conceptList = conceptList)
  sql <- translate(sql, targetDialect = "spark")
  connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = database$connectionDetails))
  ingredients <- querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  disconnect(connection)
  return(unlist(unname(ingredients)))
}

is_comma_separated_numbers_regex <- function(s) {
  if (!nzchar(s)) return(FALSE) # empty string -> FALSE
  pattern <- "^\\s*[+-]?(?:\\d+\\.?\\d*|\\.\\d+)(?:[eE][+-]?\\d+)?(?:\\s*,\\s*[+-]?(?:\\d+\\.?\\d*|\\.\\d+)(?:[eE][+-]?\\d+)?)*\\s*$"
  grepl(pattern, s, perl = TRUE)
}

computeSingleProportion <- function(i, x, n) {
  if(as.integer(n[i]) < as.integer(x[i]) | as.integer(n[i]) == 0) {n[i] <- 1; x[i] <- 0} #set to 0 if n < x or n = 0 to avoid error
  exact <- binom.test(as.integer(x[i]), as.integer(n[i]), conf.level = 0.95)
  return(tibble::tibble(
    estimate = exact$estimate,
    ci95Lb = exact$conf.int[1],
    ci95Ub = exact$conf.int[2]
  ))
}

computeProportions <- function(x, n, name) {
  proportions <- lapply(1:length(x), computeSingleProportion, x = abs(x), n = n)
  proportions <- dplyr::bind_rows(proportions)
  names(proportions) <- paste0(name, c("", "Ci95Lb", "Ci95Ub"))
  proportions[x < 0, ] <- -proportions[x < 0, ]
  return(proportions)
}

saveVersionedFile <- function(saveDirectory, objectToSave, fileName, rowNumbers = F, replace = F) {
  ext <- tolower(tools::file_ext(fileName))
  name <- basename(tools::file_path_sans_ext(fileName))

  if (!dir.exists(saveDirectory)) {
    dir.create(saveDirectory, recursive = TRUE)
  }

  if(replace) {
    fileName <- findLatestFileVersion(saveDirectory, fileName)
    if(is.null(fileName)) {fileName <- file.path(saveDirectory, paste0(name, "_V1",".",ext))}

    save_by_ext(objectToSave, file.path(saveDirectory, fileName), overwrite = replace)
    # write.csv(dataFrame, file.path(saveDirectory, fileName), row.names = rowNumbers)
    ParallelLogger::logInfo("File replaced: ", file.path(saveDirectory, fileName))
  } else {
    pass <- 1
    while(file.exists(file.path(saveDirectory, paste0(name, "_V", pass,".",ext)))) {
      pass <- pass + 1
    }

    save_by_ext(objectToSave, file.path(saveDirectory, paste0(name, "_V", pass,".",ext)), overwrite = replace)
    # write.csv(dataFrame, file.path(saveDirectory, paste0(fileName, "_V", pass,".csv")), row.names = rowNumbers)
    ParallelLogger::logInfo("File saved: ", file.path(saveDirectory, paste0(name, "_V", pass,".",ext)))
  }

  return(file.path(file.path(saveDirectory, paste0(name, "_V", pass,".", ext))))
}

# findLatestFileVersion <- function(saveDirectory, fileName) {
#   ext <- tolower(tools::file_ext(fileName))
#   name <- basename(tools::file_path_sans_ext(fileName))
#
#   files <- list.files(path = saveDirectory, pattern = paste0("^", name, "_V[0-9]+\\.", ext, "$"))
#   if (length(files) == 0) return(NULL)
#
#   nums <- str_extract(files, paste0("(?<=", name, "_V)\\d+"))
#   latest_file <- files[which.max(as.integer(nums))]
#
#   return(latest_file)
# }

findLatestFileVersion <- function(saveDirectory, fileName) {
  # helper to escape regex metacharacters
  escape_regex <- function(x) {
    gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", x)
  }

  ext <- tolower(tools::file_ext(fileName))
  name <- basename(tools::file_path_sans_ext(fileName))

  name_esc <- escape_regex(name)
  ext_esc  <- escape_regex(ext)

  # pattern for list.files (case-insensitive via ignore.case)
  pattern <- paste0("^", name_esc, "_V[0-9]+\\.", ext_esc, "$")
  files <- list.files(path = saveDirectory, pattern = pattern, ignore.case = TRUE)
  if (length(files) == 0L) return(NULL)

  # extract the numeric version using a case-insensitive regex (PCRE)
  pattern_num <- paste0("(?i)^", name_esc, "_V([0-9]+)\\.", ext_esc, "$")
  matches <- regexec(pattern_num, files, perl = TRUE)
  groups <- regmatches(files, matches)
  nums <- vapply(groups, function(g) if (length(g) >= 2) g[2] else NA_character_, character(1))

  latest_file <- files[which.max(as.integer(nums))]
  return(latest_file)
}

findPreviousFileVersion <- function(fileName) {
  earlier_file <- decrement_before_underscore(fileName)
  return(earlier_file)
}

decrement_before_underscore <- function(x, clamp_to_zero = FALSE) {
  # x: character vector
  stopifnot(is.character(x))
  m <- regexec("^(.*?)(\\d+)(_.*)$", x, perl = TRUE)
  parts <- regmatches(x, m)
  vapply(seq_along(parts), function(i) {
    p <- parts[[i]]
    if (length(p) == 0) return(x[i])           # no match -> return original
    prefix <- p[2]; num <- as.integer(p[3]); suffix <- p[4]
    if (is.na(num)) return(x[i])
    newnum <- num - 1
    if (clamp_to_zero && newnum < 0) newnum <- 0
    paste0(prefix, newnum, suffix)
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

# save_by_ext: save an R object to disk using an appropriate function based on file extension
# - x: object to save
# - file: target filename (used to pick format)
# - overwrite: logical; whether to overwrite existing file (default FALSE)
# - use_readr: logical; if TRUE and readr available, use readr::write_* for csv/tsv (default TRUE)
# - ...: passed to the underlying write function when possible (careful: different functions accept different args)
save_by_ext <- function(x, file,
                        overwrite = FALSE,
                        use_readr = TRUE,
                        ...) {
  if (!nzchar(file)) stop("Please provide a non-empty 'file' path.")
  if (file.exists(file) && !isTRUE(overwrite)) {
    stop("File already exists. Set overwrite = TRUE to replace it.")
  }
  ext <- tolower(tools::file_ext(file))
  if (ext == "") stop("Cannot detect file extension. Please supply a filename with an extension (e.g. .csv, .rds, .txt).")

  switch(ext,
         # CSV: prefer readr if available, otherwise base write.csv
         "csv" = {
           if (is.data.frame(x) || is.matrix(x)) {
             if (use_readr && requireNamespace("readr", quietly = TRUE)) {
               readr::write_csv(as.data.frame(x), file, ...)
             } else {
               # default: write.csv with row.names = FALSE unless user overrides via ...
               args <- list(x = as.data.frame(x), file = file)
               if (!"...row.names" %in% names(match.call(expand.dots = FALSE)$...)) {
                 args$row.names <- FALSE
               }
               do.call(utils::write.csv, c(args, list(...)))
             }
           } else {
             # for non-data objects, coerce to character and write lines
             writeLines(as.character(x), con = file)
           }
         },
         # TSV / tab-separated
         "tsv" = {
           if (is.data.frame(x) || is.matrix(x)) {
             if (use_readr && requireNamespace("readr", quietly = TRUE)) {
               readr::write_tsv(as.data.frame(x), file, ...)
             } else {
               args <- list(x = as.data.frame(x), file = file, sep = "\t")
               if (!"...row.names" %in% names(match.call(expand.dots = FALSE)$...)) {
                 args$row.names <- FALSE
               }
               do.call(utils::write.table, c(args, list(...)))
             }
           } else {
             writeLines(as.character(x), con = file)
           }
         },
         # RDS: single R object
         "rds" = {
           # allow passing 'compress' in ...
           do.call(saveRDS, c(list(object = x, file = file), list(...)))
         },
         # RData / RDA: save object with original name into an .RData file
         "rda" = ,
         "rdata" = {
           # Capture the name of the object as provided by the caller so save() stores that name
           obj_name <- deparse(substitute(x))
           save(list = obj_name, file = file, envir = parent.frame(), ...)
         },
         # Plain text: character vectors -> writeLines; others -> write.table
         "txt" = {
           if (is.character(x)) {
             writeLines(x, con = file, ...)
           } else if (is.data.frame(x) || is.matrix(x)) {
             args <- list(x = as.data.frame(x), file = file)
             if (!"...row.names" %in% names(match.call(expand.dots = FALSE)$...)) {
               args$row.names <- FALSE
             }
             do.call(utils::write.table, c(args, list(...)))
           } else {
             writeLines(capture.output(print(x)), con = file)
           }
         },
         # JSON using jsonlite if available
         "json" = {
           if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required to write JSON. Install it or choose another format.")
           jsonlite::write_json(x, path = file, auto_unbox = TRUE, pretty = TRUE, ...)
         },
         # fallback: try write.table for tabular objects, otherwise saveRDS
         {
           if (is.data.frame(x) || is.matrix(x)) {
             args <- list(x = as.data.frame(x), file = file)
             if (!"...row.names" %in% names(match.call(expand.dots = FALSE)$...)) {
               args$row.names <- FALSE
             }
             do.call(utils::write.table, c(args, list(...)))
           } else {
             # use RDS as a safe default for arbitrary R objects
             warning(sprintf("Unknown extension '.%s'. Falling back to saveRDS (binary R object).", ext))
             saveRDS(x, file = file, ...)
           }
         }
  )

  invisible(normalizePath(file, mustWork = FALSE))
}

# Convert a nested list to lines with nested bullets and optionally write to file
listToBullets <- function(x, bullets = c("-", "*", "+", "•"), depth = 1) {
  lines <- character()
  bullet <- bullets[((depth - 1) %% length(bullets)) + 1]

  if (is.list(x)) {
    nms <- names(x)
    for (i in seq_along(x)) {
      name <- if (!is.null(nms) && nms[i] != "") nms[i] else NULL
      item <- x[[i]]
      prefix <- paste0(strrep("  ", depth - 1), bullet, " ")

      if (is.list(item)) {
        # If it's a sub-list, print the name as a bullet (if available) and recurse
        if (!is.null(name)) {
          lines <- c(lines, paste0(prefix, name))
        } else {
          lines <- c(lines, paste0(prefix, "(list)"))
        }
        lines <- c(lines, listToBullets(item, bullets = bullets, depth = depth + 1))
      } else {
        # Atomic or other object: show name (if present) and value (collapsed if vector)
        val <- if (is.atomic(item)) paste(as.character(item), collapse = ", ") else "<non-atomic>"
        if (!is.null(name)) {
          lines <- c(lines, paste0(prefix, name, ": ", val))
        } else {
          lines <- c(lines, paste0(prefix, val))
        }
      }
    }
  } else {
    # Top-level is not a list
    lines <- c(paste0(strrep("  ", depth - 1), bullet, " ", paste(as.character(x), collapse = ", ")))
  }

  lines
}

# Wrapper to write to file (or return lines)
writeListDoc <- function(x, file = NULL, bullets = c("-", "*", "+", "•"), header = NULL) {
  lines <- listToBullets(x, bullets = bullets, depth = 1)
  if (!is.null(header)) {
    lines <- c(header, "", lines)
  }
  if (!is.null(file)) {
    writeLines(lines, con = file)
    invisible(lines)
  } else {
    lines
  }
}

is_numeric_str <- function(x) {
  x_chr <- trimws(as.character(x))
  ok <- !is.na(x_chr) & x_chr != ""
  res <- rep(FALSE, length(x_chr))
  res[ok] <- !is.na(suppressWarnings(as.numeric(x_chr[ok])))
  res
}

getRubricDataSummary <- function(dataframe) {
  df_summary <- dataframe %>%
    mutate(
      finalAnswer = tolower(trimws(as.character(finalAnswer))),
      score = case_when(
        finalAnswer == "yes"                    ~ 1,
        finalAnswer == "insufficient evidence" ~ 0.5,
        finalAnswer == "no"                     ~ 0,
        finalAnswer == "no - hard exclusion"    ~ 0,
        TRUE                                     ~ NA_real_
      )
    ) %>%
    group_by(subjectId) %>%
    summarise(
      meanScore = if (all(is.na(score))) NA_real_ else mean(score, na.rm = TRUE),
      n = n(),
      n_missing = sum(is.na(score)),
      .groups = "drop"
    )

  return(df_summary)
}

# Returns a list with:
# - counts: one row per subject with counts for each answer category
# - summary: one row per subject with meanScore, n, n_missing
getRubricDataSummaryBoth <- function(dataframe) {
  df_prepared <- dataframe %>%
    mutate(
      finalAnswer = tolower(trimws(as.character(finalAnswer))),
      score = case_when(
        finalAnswer == "yes"                    ~ 1,
        finalAnswer == "insufficient evidence" ~ 0.5,
        finalAnswer == "no"                    ~ 0,
        finalAnswer == "no - hard exclusion"   ~ 0,
        TRUE                                    ~ NA_real_
      )
    )

  counts_per_subject <- df_prepared %>%
    group_by(subjectId) %>%
    summarise(
      count_yes = sum(finalAnswer == "yes", na.rm = TRUE),
      count_insufficient_evidence = sum(finalAnswer == "insufficient evidence", na.rm = TRUE),
      count_no = sum(finalAnswer == "no", na.rm = TRUE),
      count_no_hard_exclusion = sum(finalAnswer == "no - hard exclusion", na.rm = TRUE),
      result = if_else(count_yes > count_no, "YES",
                       if_else(count_yes < count_no, "NO", "TIE")),
      questionable = (count_insufficient_evidence >= 5),
      .groups = "drop"
    )

  summary_per_subject <- df_prepared %>%
    group_by(subjectId) %>%
    summarise(
      meanScore = if (sum(!is.na(score)) == 0) NA_real_ else mean(score, na.rm = TRUE),
      n = n(),
      n_missing = sum(is.na(score)),
      medianScore = if (sum(!is.na(score)) == 0) NA_real_ else median(as.numeric(patientScore), na.rm = TRUE),
      iQR = if (sum(!is.na(score)) == 0) NA_real_ else IQR(as.numeric(patientScore), na.rm = TRUE),
      .groups = "drop"
    )

  # Return both tables in a list
  list(
    counts = counts_per_subject,
    summary = summary_per_subject
  )
}

createSqlStatement <- function(criteria, conceptSets, llmClient) {
  syntaxRules <- paste(readLines("./docs/sqlSyntax.txt"), collapse = "\n")
  originalPrompt <- paste(readLines("./docs/createSqlFromRule.txt"), collapse = "\n")
  updated_lines <- gsub("<CRITERION>", criteria, originalPrompt)
  updated_lines <- gsub("<CONCEPT_SET_NAMES>", conceptSets, updated_lines)
  updated_lines <- gsub("<OHDSI_SYNTAX>", syntaxRules, updated_lines)

  prompt <<- paste(updated_lines, collapse = "\n")

  results <- queryLLM(llmClient = getLlmo3Client(), prompt, output = "text")

  return(results)
}

correctSqlStatement <- function(sql, error = NULL) {
  syntaxRules <- paste(readLines("./docs/sqlSyntax.txt"), collapse = "\n")
  if(is.null(error)) {
    originalPrompt <- paste(readLines("./docs/correctSqlSyntax.txt"), collapse = "\n")
    updated_lines <- gsub("<SQL_QUERY>", sql, originalPrompt)
  } else {
    originalPrompt <- paste(readLines("./docs/correctSqlError.txt"), collapse = "\n")
    updated_lines <- gsub("<SQL_QUERY>", sql, originalPrompt)
    updated_lines <- gsub("<QUERY_ERROR>", error, updated_lines)
  }

  ellmerTypeObject <- ellmer::type_object(
    originalQuery = ellmer::type_string(),
    changesMade = ellmer::type_string(),
    correctedQuery = ellmer::type_string(),
    unableToCorrect = ellmer::type_enum(values = c('YES', 'NO'))
  )

  updated_lines <- gsub("<OHDSI_SYNTAX>", syntaxRules, updated_lines)
  prompt <- paste(updated_lines, collapse = "\n")
  results <- queryLLM(llmClient = getLlmo3Client(), prompt, ellmerTypeObject = ellmerTypeObject)

  return(results)
}

getLlmo3Client <- function() {
  ellmerClient <- ellmer::chat_azure_openai(
    endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
    api_version = "2024-12-01-preview",
    model = "o3",
    credentials = function() keyring::key_get("genai_api_gpt4_key")
  )

  return(ellmerClient)
}

getLlm4oClient <- function() {
  ellmerClient <- ellmer::chat_azure_openai(
    endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_gpt4o_endpoint")),
    api_version = "2023-03-15-preview",
    model = "gpt-4o",
    credentials = function() keyring::key_get("genai_api_gpt4_key")
  )

  return(ellmerClient)
}

getEmbeddingVectors <- function(text, reportTime = TRUE, silent = FALSE) {
  # text can be a single string, or a vector of strings. The output is always
  # a list of vectors
  startTime <- Sys.time()

  batchSize <- 1000
  start <- 1
  vectors <- list()
  tokens <- 0
  while (start <= length(text)) {
    end <- min(start + batchSize - 1, length(text))
    if(!silent) {
      cat(paste0("\rCurrent starting point: ", start, " to ", end, " out of ", length(text)))
    }
    json <- jsonlite::toJSON(list(input = text[start:end]), auto_unbox = TRUE)

    # response <- POST(
    #   url = keyring::key_get("genai_embed_endpoint"),
    #   body = json,
    #   add_headers("Content-Type" = "application/json",
    #               "api-key" = keyring::key_get("genai_api_embed_key"))
    # )

    response <- tryCatch(
      RETRY("POST", url = keyring::key_get("genai_embed_endpoint"),
            body = json,
            add_headers("Content-Type" = "application/json",
                        "api-key" = keyring::key_get("genai_api_embed_key")),
            timeout(600), times = 3, pause_base = 1),
      error = function(e) {
        message("All attempts failed: ", e$message)
        NULL
      }
    )

    result <- httr::content(response, "text", encoding = "UTF-8")
    result <- jsonlite::fromJSON(result)
    vector <- result$data$embedding
    vectors <- append(vectors, vector)
    tokens <- tokens + result$usage$prompt_tokens
    start <- end + 1
  }

  if (reportTime) {
    delta <- Sys.time() - startTime
    message(sprintf("\n- Generating response took %0.1f %s", delta, attr(delta, "units")))
  }
  attr(vectors, "input tokens") <- tokens
  # Compute cost in US$ based on Jan 2025 prices:
  attr(vectors, "cost") <- tokens * 0.02 / 1e6

  if(!silent) {
    cat(paste0("\n...done"))
  }

  return(vectors)
}

flattenRubric <- function(rubric) {
  flatRubric <- NULL
  if(is.data.frame(rubric[[1]]$hard_exclusions)) { #new format
    for(rubricUp in 1:length(rubric)) {
      hardExclusions <- rubric[[rubricUp]]$hard_exclusions[,c("rule_id", "rule_type","criteria")]
      hardExclusions$points <- -100

      alternateExplanations <- rubric[[rubricUp]]$alternative_explanations[,c("rule_id", "rule_type","criteria", "points")]
      evidenceRules <- rubric[[rubricUp]]$evidence_rules[,c("rule_id", "rule_type","criteria", "points")]

      ruleList <- rbind(hardExclusions, alternateExplanations, evidenceRules)
      ruleList$phenotype <- rubric[[rubricUp]]$phenotype
      ruleList$rubric_id <- rubric[[rubricUp]]$rubric_id
      ruleList$yes_score <- rubric[[rubricUp]]$decision_thresholds$yes
      ruleList$no_score <- rubric[[rubricUp]]$decision_thresholds$no
      flatRubric <- rbind(flatRubric, ruleList)
    }
  } else { #old format
    for(rubricUp in 1:length(rubric)) {
      hardExclusions <- rubric[[rubricUp]]$hard_exclusions
      tempDf <- NULL
      for(tempUp in 1:length(hardExclusions)) {
        tempDf <- rbind(tempDf, data.frame(hardExclusions[[tempUp]]))
      }
      hardExclusions <- tempDf[,c("rule_id", "rule_type","criteria")]
      hardExclusions$points <- -100

      alternateExplanations <- rubric[[rubricUp]]$alternative_explanations
      tempDf <- NULL
      for(tempUp in 1:length(alternateExplanations)) {
        tempDf <- rbind(tempDf, data.frame(alternateExplanations[[tempUp]]))
      }
      alternateExplanations <- tempDf[,c("rule_id", "rule_type","criteria", "points")]

      evidenceRules <- rubric[[rubricUp]]$evidence_rules
      tempDf <- NULL
      for(tempUp in 1:length(evidenceRules)) {
        tempDf <- rbind(tempDf, data.frame(evidenceRules[[tempUp]]))
      }
      evidenceRules <- tempDf[,c("rule_id", "rule_type","criteria", "points")]

      ruleList <- rbind(hardExclusions, alternateExplanations, evidenceRules)
      ruleList$phenotype <- rubric[[rubricUp]]$phenotype[[1]]
      ruleList$rubric_id <- rubric[[rubricUp]]$rubric_id[[1]]
      ruleList$yes_score <- rubric[[rubricUp]]$decision_thresholds$yes[[1]]
      ruleList$no_score <- rubric[[rubricUp]]$decision_thresholds$no[[1]]
      flatRubric <- rbind(flatRubric, ruleList)
    }
  }
  return(flatRubric)
}

library(dplyr)
library(tidyr)

library(data.table)

#--------------------------------------------------
# 1) Derive rubric-level assessment from score columns
#--------------------------------------------------
derive_rubric_assessment <- function(total, yesScore, noScore) {
  data.table::fcase(
    is.na(total) | is.na(yesScore) | is.na(noScore), "insufficient",
    total >= yesScore, "case",
    total <= noScore, "non_case",
    default = "insufficient"
  )
}

#--------------------------------------------------
# 2) Subject-level 4-level classifier
#    Returns a named list so it fits naturally in data.table
#--------------------------------------------------
classify_subject_4level <- function(
    n_case,
    n_non_case,
    n_insufficient,
    n_total,
    density_case_threshold = 0.50,
    density_non_case_threshold = 0.50,
    positive_rate_case_threshold = 0.75,
    positive_rate_possible_threshold = 0.75,
    positive_rate_non_case_threshold = 0.25,
    sparse_density_threshold = 0.30,
    min_case_n = 2,
    min_non_case_n = 2,
    min_net_case = 2,
    max_net_non_case = -2
) {
  n_observed <- n_case + n_non_case
  evidence_density <- ifelse(n_total > 0, n_observed / n_total, NA_real_)
  positive_rate <- ifelse(n_observed > 0, n_case / n_observed, NA_real_)
  net_score <- n_case - n_non_case

  subject_assessment <- data.table::fcase(
    n_case >= min_case_n & #has to be a certain number of cases total
      evidence_density >= density_case_threshold & #for 7 rubrics, must have scores for at least 4
      !is.na(positive_rate) &
      positive_rate >= positive_rate_case_threshold & #min 1 yes/1 yes, 2/2, 3/3, 3 yes/4 (3 yes, 1 no), 4/4, 4/5, 5/5, 5/6, 6/6, 6/7, 7/7
      net_score >= min_net_case, #number of yeses - number of no's have to be >= 2
    "high_confidence_case",

    n_case >= 1 & #CHANGED to 2 for MM (from 1)
      evidence_density < density_case_threshold & #no threshold for number of scored rubrics
      !is.na(positive_rate) &
      positive_rate >= positive_rate_possible_threshold,  #min 1/1, 2/2, 3/3, 3/4, 4/4, 4/5, 5/5, 5/6, 6/6, 6/7, 7/7
    "possible_case",

    n_non_case >= min_non_case_n &
      evidence_density >= density_non_case_threshold & #for 7 rubrics, must have scores for at least 4
      !is.na(positive_rate) &
      positive_rate <= positive_rate_non_case_threshold & #1/4, 1/5, 1/6, 1/7, any 0 yeses
      net_score <= max_net_non_case, "likely_non_case",

    default = "indeterminate"
  )

  list(
    n_observed = n_observed,
    evidence_density = evidence_density,
    positive_rate = positive_rate,
    net_score = net_score,
    subject_assessment = subject_assessment
  )
}

#--------------------------------------------------
# 3) Main scoring function for a data.table
#--------------------------------------------------
score_subjects_from_rubric_table <- function(
    df,
    subject_col = "subjectId",
    rubric_col = "rubricId",
    cohort_col = "cohortDefinitionId",
    start_date_col = "cohortStartDate",
    total_col = "total",
    yes_col = "yesScore",
    no_col = "noScore"
) {
  data.table::setDT(df)

  needed_cols <- c(subject_col, rubric_col, cohort_col, start_date_col, total_col, yes_col, no_col)
  missing_cols <- setdiff(needed_cols, names(df))

  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check duplicates
  dupes <- df[
    ,
    .N,
    by = c(cohort_col, subject_col, rubric_col, start_date_col)
  ][N > 1]

  if (nrow(dupes) > 0) {
    stop("Duplicate rows found for cohortDefinitionId x subjectId x rubricId x cohortStartDate")
  }

  # Rubric-level
  rubric_level <- data.table::copy(df)

  rubric_level[
    ,
    rubric_assessment := derive_rubric_assessment(
      total = get(total_col),
      yesScore = get(yes_col),
      noScore = get(no_col)
    )
  ]

  rubric_level[
    ,
    rubric_case_numeric := data.table::fcase(
      rubric_assessment == "case", 1L,
      rubric_assessment == "non_case", -1L,
      default = 0L
    )
  ]

  # Subject-level summary
  subject_level <- rubric_level[
    ,
    .(
      n_case = sum(rubric_assessment == "case", na.rm = TRUE),
      n_non_case = sum(rubric_assessment == "non_case", na.rm = TRUE),
      n_insufficient = sum(rubric_assessment == "insufficient", na.rm = TRUE),
      n_total = .N
    ),
    by = c(cohort_col, subject_col, start_date_col)
  ]

  # Apply classifier rowwise
  subject_level[
    ,
    c("n_observed", "evidence_density", "positive_rate", "net_score", "subject_assessment") :=
      classify_subject_4level(
        n_case = n_case,
        n_non_case = n_non_case,
        n_insufficient = n_insufficient,
        n_total = n_total
      )
  ]

  list(
    rubric_level = rubric_level,
    subject_level = subject_level
  )
}

vectorSearchStandard <- function(term, domains = NULL, conceptClasses = NULL, limit = 10, maxRetries = 3, waitTime = 2) {
  #domains: Condition, Procedure, Drug, Visit, Measurement
  #conceptClasses: (c: Disorder, HCPCS, 	Clinical Observation), (p: Procedure, CPT4), (d: Ingredient, Clinical Drug, Branded Drug),
  # (v: Visit) (m: Clinical Observation, CPT4)
  term <- gsub("_",  " ", term)
  params <- list(
    q = term,
    #domain_id = paste(domains, collapse = ","),
    limit = limit
  )

  if (!is.null(domains)) {
    params$domain_id <- paste(domains, collapse = ",")
  }

  if (!is.null(conceptClasses)) {
    params$concept_class_id <- paste(conceptClasses, collapse = ",")
  }
  url <- "https://hecate.pantheon-hds.com/api/search_standard"

  for (attempt in 1:maxRetries) {
    response <- tryCatch(
      {
        httr::GET(url, query = params)
      },
      error = function(e) {
        message(paste("Attempt", attempt, "failed with error:", e$message))
        return(NULL)
      }
    )

    if (!is.null(response) && httr::status_code(response) == 200) {
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content_text)

      if(length(data) > 0) {
        data <- dplyr::bind_rows(data$concepts) |>
          SqlRender::snakeCaseToCamelCaseNames()
        return(cbind(data.frame(searchTerm = term, data)))
      } else {
        return(data.frame())
      }
    }
    if (attempt < maxRetries) {
      message(sprintf(
        "Search failed for '%s' (Status: %s). Retrying in %s seconds...",
        term,
        if (is.null(response)) "Connection Error" else httr::status_code(response),
        waitTime
      ))
      Sys.sleep(waitTime)
    }
  }
  stop(sprintf("All %s attempts failed for term '%s'.", maxRetries, term))
}

vectorSearch <- function(term,
                         domains,
                         conceptClasses,
                         vocabularyId,
                         standardConcept,
                         limit = 10,
                         maxRetries = 3,
                         waitTime = 2) {
  params <- list(
    q = term,
    domain_id = paste(domains, collapse = ","),
    # standard_concept = "S",
    vocabulary_id = vocabularyId,
    standard_concept = standardConcept,
    limit = limit
  )
  if (!is.null(conceptClasses)) {
    params$concept_class_id <- paste(conceptClasses, collapse = ",")
  }
  url <- "https://hecate.pantheon-hds.com/api/search"

  for (attempt in 1:maxRetries) {
    response <- tryCatch(
      {
        httr::GET(url, query = params)
      },
      error = function(e) {
        message(paste("Attempt", attempt, "failed with error:", e$message))
        return(NULL)
      }
    )


    if (!is.null(response) && httr::status_code(response) == 200) {
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content_text)

      if(length(data) > 0) {
        scoreData <- data[,c("concept_name", "score")]
        data <- dplyr::bind_rows(data$concepts)
        data <- merge(data, scoreData)

        data <- SqlRender::snakeCaseToCamelCaseNames(data)
      }

      return(data)
    }

    if (attempt < maxRetries) {
      message(sprintf(
        "Search failed for '%s' (Status: %s). Retrying in %s seconds...",
        term,
        if (is.null(response)) "Connection Error" else httr::status_code(response),
        waitTime
      ))
      Sys.sleep(waitTime)
    }
  }
  stop(sprintf("All %s attempts failed for term '%s'.", maxRetries, term))
}

vectorSearchAtc <- function(term,
                            domains,
                            vocabularyId,
                            limit = 10,
                            maxRetries = 3,
                            waitTime = 2) {
  params <- list(
    q = term,
    domain_id = paste(domains, collapse = ","),
    vocabulary_id = vocabularyId,
    standard_concept = "C",
    limit = limit
  )
  url <- "https://hecate.pantheon-hds.com/api/search"

  for (attempt in 1:maxRetries) {
    response <- tryCatch(
      {
        httr::GET(url, query = params)
      },
      error = function(e) {
        message(paste("Attempt", attempt, "failed with error:", e$message))
        return(NULL)
      }
    )


    if (!is.null(response) && httr::status_code(response) == 200) {
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content_text)

      if(length(data) > 0) {
        scoreData <- data[,c("concept_name", "score")]
        data <- dplyr::bind_rows(data$concepts)
        data <- merge(data, scoreData)

        data <- SqlRender::snakeCaseToCamelCaseNames(data)
      }

      return(data)
    }

    if (attempt < maxRetries) {
      message(sprintf(
        "Search failed for '%s' (Status: %s). Retrying in %s seconds...",
        term,
        if (is.null(response)) "Connection Error" else httr::status_code(response),
        waitTime
      ))
      Sys.sleep(waitTime)
    }
  }
  stop(sprintf("All %s attempts failed for term '%s'.", maxRetries, term))
}

bulkLoadCohortNewTable <- function(database, cohortFile, cohortTableName) {
  library(stringi)
  library(DatabaseConnector)
  library(SqlRender)

  # preTempTable <- stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]")
  # preTempTable <- "bulkTempTable"
  connection <- connect(database$connectionDetails)

  Sys.setenv(
    "AZR_STORAGE_ACCOUNT" = "episharedcentralbackup",
    "AZR_ACCOUNT_KEY" = Sys.getenv("AZR_ACCOUNT_KEY"),
    "AZR_CONTAINER_NAME" = "bulkloading")

  insertTable(
    connection = connection,
    databaseSchema = database$workDatabaseSchema,
    tableName = cohortTableName,
    data = cohortFile,
    dropTableIfExists = T,
    createTable = T,
    tempTable = FALSE,
    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
    bulkLoad = T,
    progressBar = T,
    camelCaseToSnakeCase = T
  )

  disconnect(connection)
}

bulkLoadCohortExistingTable <- function(database, cohortFile, cohortTableName) {
  library(stringi)
  library(DatabaseConnector)
  library(SqlRender)

  connection <- connect(database$connectionDetails)

  Sys.setenv(
    "AZR_STORAGE_ACCOUNT" = "episharedcentralbackup",
    "AZR_ACCOUNT_KEY" = Sys.getenv("AZR_ACCOUNT_KEY"),
    "AZR_CONTAINER_NAME" = "bulkloading")

  #delete cohort from primary table, copy the first temp table to the real temp table, and delete the first temp table
  sql <- paste0("delete from ", database$workDatabaseSchema, ".", cohortTableName," where cohort_definition_id = ",
                cohortFile$cohortDefinitionId[1], "; ")

  sql <- SqlRender::translate(sql, database$connectionDetails$dbms)
  DatabaseConnector::executeSql(connection, sql)

  insertTable(
    connection = connection,
    databaseSchema = database$workDatabaseSchema,
    tableName = cohortTableName,
    data = cohortFile,
    dropTableIfExists = F,
    createTable = F,
    tempTable = FALSE,
    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
    bulkLoad = T,
    progressBar = T,
    camelCaseToSnakeCase = T
  )

  disconnect(connection)
}

checkForExclusions <- function(term) {
  ellmerTypeObject <- ellmer::type_array(ellmer::type_object(
    term = ellmer::type_string(),
    yesNo = ellmer::type_enum(values = c("YES","NO")),
    rationale = ellmer::type_string(),
    exclusions = ellmer::type_string()
  ))

  prompt <- paste0("Does the term: ", term, " EXPLICITLY suggest there should be exclusions? ",
                   "Examples: 'psoriasis without arthritis' explicitly excludes arthritis whereas 'psoriatic arthritis' does not ",
                   "'nonrheumatic valve disorder' exclude rheumatic valve disorder ",
                   "If yes, create a COMPREHENSIVE set of Excluded terms - DO NOT include any associated billing codes, e.g., ICD10 codes",
                   "If yes, what are the exclusions? ",
                   "  {
                            \"term\": \"Name of term\",
                            \"yesNo\": \"YES or NO\",
                            \"rationale\": \"Rationale for exclusion\",
                            \"exclusions\": \"a COMPREHENSIVE set of Excluded terms\"
                            }")

  exclusions <- queryLLM(llmClient = getLlmo3Client(), prompt = prompt, ellmerTypeObject = ellmerTypeObject)
  return(exclusions)
}

getConceptSet <- function(conceptName,
                          conceptIds,
                          domains,
                          conceptClasses = "",
                          baseDirectory,
                          database,
                          quickRun = FALSE,
                          fullString,
                          exclusions = "TEST", #default is to test for exclusions - call with set to "none" if no testing wanted
                          additionalInformation = "",
                          clinicalContext =  "",
                          bucketSize = 20) {
  if(!getOption("databaseConnectorInteger64AsNumeric")) { #if set to false, change to true and put back at the end
    options(databaseConnectorInteger64AsNumeric = TRUE)
    on.exit(options(databaseConnectorInteger64AsNumeric = FALSE))
  }

  #test for likely very large (> 2K) concept sets
  sql <- paste0("select count(distinct c.concept_id) ",
                "from ", database$cdmDatabaseSchema, ".concept_ancestor ca ",
                "join ", database$cdmDatabaseSchema, ".concept c ",
                "on ca.descendant_concept_id  = c.concept_id ",
                "where ca.ancestor_concept_id in (", paste0(c(conceptIds), collapse = ","), ") ",
                "and c.domain_id in (", paste0("'", domains, "'", collapse = "," ), ") ",
                "and c.concept_class_id in (", paste0("'", conceptClasses, "'", collapse = "," ), ") ",
                "and c.standard_concept = \"S\";")

  connection <- connect(database$connectionDetails)
  recordCount <-DatabaseConnector:: querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  disconnect(connection)

  if(recordCount <= 2000) { # reasonable size concept set to test
    #test to see if any concepts should be explicitly excluded
    if(exclusions == "TEST") {
      exclusions <- checkForExclusions(term = conceptName)
      if(exclusions$yesNo == "YES") {
        excludedConditions <- exclusions$exclusions
      } else {
        excludedConditions <- "none"
      }
    } else {
      excludedConditions <- exclusions
    }

    #create the concept set
    conceptSet <- RubricConcepts::createConceptSet(conceptName = conceptName,
                                              originalConceptList = conceptIds,
                                              llmClient = getLlm4oClient(),
                                              excludedConditions = excludedConditions,
                                              connectionDetails = database$connectionDetails,
                                              cdmDatabaseSchema = database$cdmDatabaseSchema,
                                              outputDirectory = file.path(baseDirectory, fullString),
                                              additionalInformation = additionalInformation,
                                              minCount = 0,
                                              belowMinimumCountApproach = "TEST ALL",
                                              quickRun = quickRun,
                                              condenseConceptSet = F,
                                              bucketSize = bucketSize,
                                              clinicalContext = clinicalContext,
                                              tries = 1,
                                              successes = 1)

    finalConceptSet <- conceptSet[[2]]
    finalConceptSet <- finalConceptSet[finalConceptSet$items$concept$DOMAIN_ID %in% domains]
  } else { #too large to test - send message and use descendants only
    finalConceptSet <- createJsonforConceptsPlusDescendants(conceptIds, database)$expression
  }

  # return(conceptSet)
  return(finalConceptSet)
}

saveConceptSet <- function(cleanName, conceptSetName, finalConceptSet, csLibraryName) {
  cat(paste0("\rAdding ", cleanName, " to concept set library..."))
  conceptSetLibrary <- csLibraryName

  # create embedded vector for concept set name
  embeddedVectorDF <- data.frame(
    conceptSetName = conceptSetName,
    cleanName = cleanName,
    embed = I(getEmbeddingVectors(tolower(cleanName), reportTime = FALSE, silent = TRUE)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  #save embedded vector name
  conceptSetLibrary[[2]] <- rbind(conceptSetLibrary[[2]], embeddedVectorDF)

  if(!is.null(finalConceptSet)) {
    finalConceptSet <- I(finalConceptSet)
  } else { #save as null concept set
    finalConceptSet = ""
  }

  newConceptSet <- data.frame(conceptSetName = conceptSetName,
                              cleanName = cleanName,
                              conceptSet = finalConceptSet,
                              stringsAsFactors = FALSE,
                              row.names = NULL)

  #save concept set to library
  conceptSetLibrary[[1]] <- rbind(conceptSetLibrary[[1]], newConceptSet)
  cat("done\n")
  return(conceptSetLibrary)
}

createJsonforConceptsPlusDescendants <- function(conceptIds, database) {
  library(DatabaseConnector)
  library(SqlRender)
  library(jsonlite)

  # --- Params ----
  connectionDetails <- database$connectionDetails
  vocab_schema <- database$cdmDatabaseSchema
  concept_ids <- c(conceptIds)  # replace with your list

  # --- Fetch metadata for the concept_ids (only valid concepts) ---
  sql <- "
SELECT concept_id,
       concept_name,
       domain_id,
       vocabulary_id,
       concept_class_id,
       standard_concept
FROM @vocab_schema.concept
WHERE concept_id IN (@concept_ids)
  AND (invalid_reason IS NULL OR invalid_reason = '')
"
  # SqlRender: pass the comma-separated list of ids
  sql <- SqlRender::render(sql,
                           vocab_schema = vocab_schema,
                           concept_ids = paste(concept_ids, collapse = ","))
  # Optional: translate to target dialect (if needed)
  # sql <- SqlRender::translate(sql, targetDialect = "postgresql")

  conn <- DatabaseConnector::connect(connectionDetails)
  meta <- DatabaseConnector::querySql(conn, sql)
  DatabaseConnector::disconnect(conn)

  # --- Build items: one item per input concept id, includeDescendants = TRUE ---
  items <- lapply(concept_ids, function(cid) {
    row <- meta[meta$concept_id == cid, , drop = FALSE]
    if (nrow(row) == 1) {
      concept_meta <- list(
        CONCEPT_ID = as.integer(row$concept_id),
        CONCEPT_NAME = row$concept_name,
        DOMAIN_ID = row$domain_id,
        VOCABULARY_ID = row$vocabulary_id,
        CONCEPT_CLASS_ID = row$concept_class_id,
        STANDARD_CONCEPT = row$standard_concept
      )
    } else {
      # fallback if metadata missing (still valid for ATLAS import)
      concept_meta <- list(CONCEPT_ID = as.integer(cid),
                           CONCEPT_NAME = paste0("concept ", cid))
    }

    list(
      concept = concept_meta,
      isExcluded = FALSE,
      includeDescendants = TRUE,
      includeMapped = FALSE
    )
  })

  concept_set <- list(
    id = NULL,
    name = paste0("ConceptSet_with_descendants_", Sys.Date()),
    expression = list(items = items)
  )

  # convert to JSON (NA -> null)
  # json_string <- RJSONIO::toJSON(concept_set, auto_unbox = TRUE, pretty = TRUE, na = "null")
  json_string <- jsonlite::toJSON(concept_set, pretty = TRUE, simplifyVector = FALSE, auto_unbox = TRUE)

  # return(json_string)
  return(concept_set)
}


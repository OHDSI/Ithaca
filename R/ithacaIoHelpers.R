ithListToBullets <- function(x, bullets = c("-", "*", "+", "•"), depth = 1) {
  bullet <- bullets[((depth - 1) %% length(bullets)) + 1]

  if (is.null(x)) {
    return(character())
  }

  if (is.atomic(x) && !is.list(x)) {
    vals <- as.character(x)
    vals <- vals[!is.na(vals)]
    return(ifelse(nzchar(vals), paste0(strrep("  ", depth - 1), bullet, " ", vals), character()))
  }

  out <- character()
  if (is.list(x)) {
    nm <- names(x)
    if (is.null(nm)) {
      nm <- rep("", length(x))
    }

    for (i in seq_along(x)) {
      nameI <- nm[[i]]
      valI <- x[[i]]

      if (nzchar(nameI)) {
        out <- c(out, paste0(strrep("  ", depth - 1), bullet, " ", nameI, ":"))
      }

      nextDepth <- if (nzchar(nameI)) depth + 1 else depth
      out <- c(out, ithListToBullets(valI, bullets = bullets, depth = nextDepth))
    }
  }

  out[nzchar(out)]
}

ithWriteListDoc <- function(x, file = NULL, bullets = c("-", "*", "+", "•"), header = NULL) {
  body <- ithListToBullets(x, bullets = bullets, depth = 1)
  lines <- if (!is.null(header)) c(as.character(header), "", body) else body

  if (!is.null(file)) {
    writeLines(lines, file)
  }

  paste(lines, collapse = "\n")
}

ithBulkLoadCohortNewTable <- function(database, cohortFile, cohortTableName) {
  connection <- DatabaseConnector::connect(database$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = database$workDatabaseSchema,
    tableName = cohortTableName,
    data = cohortFile,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE,
    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
    bulkLoad = TRUE,
    progressBar = TRUE,
    camelCaseToSnakeCase = TRUE
  )
}

ithBulkLoadCohortExistingTable <- function(database, cohortFile, cohortTableName) {
  connection <- DatabaseConnector::connect(database$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

  sql <- paste0(
    "DELETE FROM ", database$workDatabaseSchema, ".", cohortTableName,
    " WHERE cohort_definition_id = ", cohortFile$cohortDefinitionId[[1]]
  )

  sql <- SqlRender::translate(sql, targetDialect = database$connectionDetails$dbms)
  DatabaseConnector::executeSql(connection, sql)

  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = database$workDatabaseSchema,
    tableName = cohortTableName,
    data = cohortFile,
    dropTableIfExists = FALSE,
    createTable = FALSE,
    tempTable = FALSE,
    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
    bulkLoad = TRUE,
    progressBar = TRUE,
    camelCaseToSnakeCase = TRUE
  )
}

#' Convert nested lists to bulleted text lines
#'
#' @param x Input list/vector.
#' @param bullets Bullet symbols to rotate by depth.
#' @param depth Starting indentation depth.
#'
#' @return Character vector of bulleted lines.
#' @examples
#' lines <- listToBullets(list(section = c("a", "b")))
#' @export
listToBullets <- function(x, bullets = c("-", "*", "+", "•"), depth = 1) {
  ithListToBullets(x, bullets = bullets, depth = depth)
}

#' Write or return a bulleted list document
#'
#' @param x Input list/vector.
#' @param file Optional output file path.
#' @param bullets Bullet symbols.
#' @param header Optional header text.
#'
#' @return Single character string containing the document text.
#' @examples
#' text <- writeListDoc(list(a = 1, b = 2), header = "Summary")
#' @export
writeListDoc <- function(x, file = NULL, bullets = c("-", "*", "+", "•"), header = NULL) {
  ithWriteListDoc(x, file = file, bullets = bullets, header = header)
}

#' Bulk-load cohort data into a new table
#'
#' @param database Database specification.
#' @param cohortFile Data frame of cohort rows.
#' @param cohortTableName Target table name.
#'
#' @return Invisibly performs insert operation.
#' @examples
#' \dontrun{
#' bulkLoadCohortNewTable(CCAE_OHDA_DBSpec, cohortFile = data.frame(cohortDefinitionId = 1, subjectId = 1), cohortTableName = "pv_table")
#' }
#' @export
bulkLoadCohortNewTable <- function(database, cohortFile, cohortTableName) {
  ithBulkLoadCohortNewTable(database = database, cohortFile = cohortFile, cohortTableName = cohortTableName)
}

#' Bulk-load cohort data into an existing table
#'
#' @param database Database specification.
#' @param cohortFile Data frame of cohort rows.
#' @param cohortTableName Target table name.
#'
#' @return Invisibly performs delete/insert operation.
#' @examples
#' \dontrun{
#' bulkLoadCohortExistingTable(CCAE_OHDA_DBSpec, cohortFile = data.frame(cohortDefinitionId = 1, subjectId = 1), cohortTableName = "pv_table")
#' }
#' @export
bulkLoadCohortExistingTable <- function(database, cohortFile, cohortTableName) {
  ithBulkLoadCohortExistingTable(database = database, cohortFile = cohortFile, cohortTableName = cohortTableName)
}

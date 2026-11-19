library(Ithaca)
library(DatabaseConnector)

configPath <- "config.yaml"
databaseId <- "OptumDod"  # change if needed

maskToken <- function(x) {
  if (!nzchar(x)) {
    return("<empty>")
  }
  if (nchar(x) <= 8) {
    return(paste0(substr(x, 1, 1), "***", substr(x, nchar(x), nchar(x))))
  }
  paste0(substr(x, 1, 4), "...", substr(x, nchar(x) - 3, nchar(x)))
}

normalizeHost <- function(host) {
  resolved <- trimws(as.character(host[[1]]))
  resolved <- sub("^https?://", "", resolved, ignore.case = TRUE)
  resolved <- sub("/+$", "", resolved)
  resolved
}

normalizeHttpPath <- function(httpPath) {
  resolved <- trimws(as.character(httpPath[[1]]))
  if (nzchar(resolved) && !startsWith(resolved, "/")) {
    resolved <- paste0("/", resolved)
  }
  resolved
}

splitHttpPathAndExtras <- function(httpPath) {
  rawValue <- trimws(as.character(httpPath[[1]]))
  pieces <- strsplit(rawValue, ";", fixed = TRUE)[[1]]
  basePath <- normalizeHttpPath(pieces[[1]])
  extras <- character(0)
  if (length(pieces) > 1) {
    extras <- trimws(pieces[2:length(pieces)])
    extras <- extras[nzchar(extras)]
  }
  list(httpPath = basePath, extraSettings = extras)
}

resolveJdbcUser <- function() {
  envUser <- trimws(Sys.getenv("DATABRICKS_USER"))
  if (nzchar(envUser)) {
    return(envUser)
  }

  "token"
}

resolvedProvider <- Ithaca:::getDatabricksConnectionDetails()
resolvedToken <- trimws(as.character(resolvedProvider$password[[1]]))
resolvedHost <- trimws(as.character(resolvedProvider$host[[1]]))
resolvedHttpPathRaw <- trimws(as.character(resolvedProvider$httpPath[[1]]))

httpPathParts <- splitHttpPathAndExtras(resolvedHttpPathRaw)
resolvedConnection <- list(
  host = normalizeHost(resolvedHost),
  httpPath = httpPathParts$httpPath,
  token = resolvedToken,
  extraSettings = httpPathParts$extraSettings,
  user = resolveJdbcUser()
)

if (!nzchar(resolvedConnection$host) || !nzchar(resolvedConnection$httpPath) || !nzchar(resolvedConnection$token)) {
  stop("DATABRICKS_HOST, DATABRICKS_HTTP_PATH, and DATABRICKS_TOKEN must all be set")
}

cat("Resolved Databricks host: ", resolvedConnection$host, "\n", sep = "")
cat("Resolved Databricks httpPath: ", resolvedConnection$httpPath, "\n", sep = "")
cat("Resolved Databricks token: ", maskToken(resolvedConnection$token), "\n", sep = "")
cat("Resolved Databricks JDBC user: ", resolvedConnection$user, "\n", sep = "")

config <- Ithaca:::ithReadIthacaWorkflowConfig(configPath)
databases <- Ithaca:::ithResolveDatabasesFromConfig(config = config, envir = .GlobalEnv)

if (!(databaseId %in% names(databases))) {
  stop("Database ID not found in config: ", databaseId)
}

db <- databases[[databaseId]]
cat("Testing database: ", databaseId, "\n", sep = "")
cat("CDM schema: ", db$cdmDatabaseSchema, "\n", sep = "")
cat("Scratch schema: ", db$scratchSchema, "\n", sep = "")
connectionString <- paste0(
  "jdbc:databricks://",
  resolvedConnection$host,
  ":443/default;transportMode=http;ssl=1;AuthMech=3;httpPath=",
  resolvedConnection$httpPath
)
connectionExtras <- unique(c("EnableArrow=0", resolvedConnection$extraSettings))
if (length(connectionExtras) > 0) {
  connectionString <- paste0(connectionString, ";", paste(connectionExtras, collapse = ";"))
}
cdList <- list(
  dbms = "spark",
  connectionString = connectionString,
  user = resolvedConnection$user,
  password = resolvedConnection$token
)
cat("JDBC URL: ", cdList$connectionString, "\n", sep = "")

connectionDetails <- do.call(DatabaseConnector::createConnectionDetails, cdList)
conn <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(conn), add = TRUE)

ping <- DatabaseConnector::querySql(conn, "SELECT current_timestamp() AS ts")
print(ping)

showGrantsSql <- "SHOW GRANTS ON CATALOG system"
grants <- tryCatch(DatabaseConnector::querySql(conn, showGrantsSql), error = function(e) NULL)
if (!is.null(grants)) {
  cat("Retrieved grants sample rows: ", nrow(grants), "\n", sep = "")
}

personSql <- paste0("SELECT COUNT(*) AS n FROM ", db$cdmDatabaseSchema, ".person")
personCount <- DatabaseConnector::querySql(conn, personSql)
print(personCount)

cat("Connection and CDM access look good.\n")

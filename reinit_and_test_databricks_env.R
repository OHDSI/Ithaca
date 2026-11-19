library(httr)
library(jsonlite)
library(DatabaseConnector)

# Re-initialize Databricks environment variables for this R session.
# Edit host/httpPath if your target warehouse changes.
host <- "adb-7228388582267561.1.azuredatabricks.net"
httpPath <- "/sql/1.0/warehouses/768ce28781fd35dc"

token <- trimws(Sys.getenv("DATABRICKS_TOKEN"))
if (!nzchar(token)) {
  stop("DATABRICKS_TOKEN is empty. Set it first, then rerun.")
}

warehouseId <- sub("^/sql/1.0/(warehouses|endpoints)/([^/;]+).*$", "\\2", httpPath)
if (!nzchar(warehouseId) || identical(warehouseId, httpPath)) {
  stop("Could not parse warehouse ID from httpPath: ", httpPath)
}

Sys.setenv(
  DATABRICKS_HOST = host,
  DATABRICKS_HTTP_PATH = httpPath,
  DATABRICKS_TOKEN = token
)

# Remove any session override so package code uses env-driven defaults.
if (exists("getDatabricksConnectionDetails", envir = .GlobalEnv, inherits = FALSE)) {
  rm(getDatabricksConnectionDetails, envir = .GlobalEnv)
}

cat("Databricks env re-initialized\n")
cat("host: ", Sys.getenv("DATABRICKS_HOST"), "\n", sep = "")
cat("httpPath: ", Sys.getenv("DATABRICKS_HTTP_PATH"), "\n", sep = "")
cat(
  "token fingerprint: ",
  substr(token, 1, 4), "...", substr(token, nchar(token) - 3, nchar(token)),
  "\n",
  sep = ""
)

# 1) REST check
runRestCheck <- function(forceHttp11 = FALSE) {
  cfg <- if (isTRUE(forceHttp11)) httr::config(http_version = 2L) else NULL
  httr::POST(
    url = paste0("https://", host, "/api/2.0/sql/statements"),
    httr::add_headers(Authorization = paste("Bearer", token)),
    cfg,
    encode = "json",
    body = list(
      statement = "SELECT current_timestamp() AS ts",
      warehouse_id = warehouseId
    )
  )
}

restRes <- tryCatch(
  runRestCheck(forceHttp11 = FALSE),
  error = function(e) {
    cat("REST check failed with default HTTP mode: ", conditionMessage(e), "\n", sep = "")
    cat("Retrying REST check with forced HTTP/1.1...\n")
    tryCatch(
      runRestCheck(forceHttp11 = TRUE),
      error = function(e2) {
        cat("REST check failed with forced HTTP/1.1 as well: ", conditionMessage(e2), "\n", sep = "")
        NULL
      }
    )
  }
)

if (!is.null(restRes)) {
  cat("REST status: ", httr::status_code(restRes), "\n", sep = "")
  cat("REST body: ", httr::content(restRes, as = "text", encoding = "UTF-8"), "\n", sep = "")
} else {
  cat("Skipping REST result details due to transport failure; continuing with JDBC checks.\n")
}

# 2) JDBC checks (try multiple known-valid variants)
warehouseId <- sub("^/sql/1.0/(warehouses|endpoints)/([^/;]+).*$", "\\2", httpPath)
jdbcUser <- "token"

buildJdbcUrl <- function(pathVariant, includeArrow) {
  url <- paste0(
    "jdbc:databricks://",
    host,
    ":443/default;transportMode=http;ssl=1;AuthMech=3;httpPath=",
    pathVariant
  )
  if (isTRUE(includeArrow)) {
    url <- paste0(url, ";EnableArrow=0")
  }
  url
}

pathVariants <- c(
  paste0("/sql/1.0/warehouses/", warehouseId),
  paste0("/sql/1.0/endpoints/", warehouseId)
)

attempts <- expand.grid(
  pathVariant = pathVariants,
  includeArrow = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

jdbcSuccess <- FALSE
jdbcError <- NULL
successfulJdbcUrl <- NULL

for (attemptUp in seq_len(nrow(attempts))) {
  pathVariant <- attempts$pathVariant[[attemptUp]]
  includeArrow <- isTRUE(attempts$includeArrow[[attemptUp]])
  jdbcUrl <- buildJdbcUrl(pathVariant = pathVariant, includeArrow = includeArrow)

  cat("Trying JDBC URL: ", jdbcUrl, "\n", sep = "")
  cat("Using JDBC user: ", jdbcUser, "\n", sep = "")

  attemptResult <- tryCatch({
    connectionDetails <- DatabaseConnector::createConnectionDetails(
      dbms = "spark",
      connectionString = jdbcUrl,
      user = jdbcUser,
      password = token
    )

    conn <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(conn), add = TRUE)

    jdbcResult <- DatabaseConnector::querySql(conn, "SELECT current_timestamp() AS ts")
    print(jdbcResult)
    TRUE
  }, error = function(e) {
    jdbcError <<- conditionMessage(e)
    cat("JDBC attempt failed: ", jdbcError, "\n", sep = "")
    FALSE
  })

  if (isTRUE(attemptResult)) {
    jdbcSuccess <- TRUE
    successfulJdbcUrl <- jdbcUrl
    break
  }
}

if (!jdbcSuccess) {
  stop("All JDBC attempts failed. Last error: ", jdbcError)
}

cat("JDBC query succeeded with URL: ", successfulJdbcUrl, "\n", sep = "")

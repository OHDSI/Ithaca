# One-time migration script: remove project-local Ithaca library usage
# and standardize package installs on a single canonical/original R library.
#
# Run in RStudio:
# source("E:/Ithaca/extras/migrateToOriginalRLibrary.R")
#
# Notes:
# - This script creates .bak backups before editing files.
# - It reinstalls required packages into the canonical library only.
# - It verifies key package loads.
# - It can optionally delete E:/Ithaca/.Rlibs after successful verification.

projectDir <- "E:/Ithaca"
projectRprofile <- file.path(projectDir, ".Rprofile")
repairRunner <- file.path(projectDir, "tmp_run_repair_full.R")
localIthacaLib <- file.path(projectDir, ".Rlibs")

# Set this if you want to force a specific canonical library path.
# If NULL, script uses the first non-project library in .libPaths(),
# or falls back to R_LIBS_USER.
canonicalLibraryPath <- "E:/R/R-4.4.3/library"

requiredPackages <- c(
  "checkmate",
  "curl",
  "openssl",
  "httr",
  "httr2",
  "jsonlite",
  "Rcpp",
  "data.table",
  "DatabaseConnector",
  "SqlRender"
)

repos <- c("https://ohdsi.r-universe.dev", "https://cloud.r-project.org")

makeBackup <- function(path) {
  if (!file.exists(path)) return(NULL)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backupPath <- paste0(path, ".", timestamp, ".bak")
  ok <- file.copy(path, backupPath, overwrite = TRUE)
  if (!ok) stop("Failed to create backup for: ", path)
  backupPath
}

resolveCanonicalLibrary <- function(projectDir, configuredPath = NULL) {
  if (!is.null(configuredPath) && nzchar(configuredPath)) {
    dir.create(configuredPath, recursive = TRUE, showWarnings = FALSE)
    return(normalizePath(configuredPath, winslash = "/", mustWork = FALSE))
  }

  current <- normalizePath(.libPaths(), winslash = "/", mustWork = FALSE)
  projectLib <- normalizePath(file.path(projectDir, ".Rlibs"), winslash = "/", mustWork = FALSE)

  nonProject <- current[!startsWith(current, projectLib)]
  if (length(nonProject) > 0) {
    return(nonProject[[1]])
  }

  rLibsUser <- Sys.getenv("R_LIBS_USER", unset = "")
  if (nzchar(rLibsUser)) {
    dir.create(rLibsUser, recursive = TRUE, showWarnings = FALSE)
    return(normalizePath(rLibsUser, winslash = "/", mustWork = FALSE))
  }

  stop("Could not resolve a canonical library path. Set canonicalLibraryPath manually.")
}

rewriteProjectRprofile <- function(path) {
  if (!file.exists(path)) {
    message("No project .Rprofile found at: ", path)
    return(FALSE)
  }

  lines <- readLines(path, warn = FALSE)
  original <- lines

  startLine <- "projectLib <- file.path(getwd(), \".Rlibs\")"
  endPattern <- "^\\s*\\.libPaths\\(c\\(projectLib, \\.libPaths\\(\\)\\)\\)\\s*$"

  updated <- character()
  skipping <- FALSE

  for (line in lines) {
    if (!skipping && trimws(line) == startLine) {
      skipping <- TRUE
      next
    }

    if (skipping) {
      if (grepl(endPattern, line)) {
        skipping <- FALSE
      }
      next
    }

    updated <- c(updated, line)
  }

  lines <- updated

  # Clean up repeated blank lines
  while (length(lines) > 1 && any(lines[-1] == "" & lines[-length(lines)] == "")) {
    idx <- which(lines[-1] == "" & lines[-length(lines)] == "")[1] + 1
    lines <- lines[-idx]
  }

  changed <- !identical(lines, original)
  if (changed) {
    writeLines(lines, path, useBytes = TRUE)
  }

  changed
}

rewriteRepairRunner <- function(path) {
  if (!file.exists(path)) {
    message("No repair runner file found at: ", path)
    return(FALSE)
  }

  lines <- readLines(path, warn = FALSE)
  original <- lines

  lines <- lines[lines != "userLib <- \"E:/Ithaca/.Rlibs\""]
  lines <- lines[lines != "dir.create(userLib, recursive = TRUE, showWarnings = FALSE)"]
  lines <- lines[lines != ".libPaths(c(userLib, .libPaths()))"]

  updated <- character()
  skipping <- FALSE
  for (line in lines) {
    trimmed <- trimws(line)

    if (!skipping && startsWith(trimmed, "if (!exists(\"userLib\", inherits = TRUE)")) {
      skipping <- TRUE
      next
    }

    if (skipping) {
      if (trimmed == "}") {
        skipping <- FALSE
      }
      next
    }

    updated <- c(updated, line)
  }

  updated <- gsub("lib = userLib", "lib = .libPaths()[1]", updated, fixed = TRUE)

  if (!any(grepl("message\\(\"Using canonical library:\"", updated))) {
    sourceLine <- "source(\"E:/Ithaca/extras/repairConceptSetLibraryFormat.R\")"
    sourceIdx <- which(updated == sourceLine)
    if (length(sourceIdx) > 0) {
      insertAt <- sourceIdx[[1]]
      updated <- append(updated, "message(\"Using canonical library: \", .libPaths()[1])", after = insertAt)
    }
  }

  changed <- !identical(updated, original)
  if (changed) {
    writeLines(updated, path, useBytes = TRUE)
  }

  changed
}

installAndVerify <- function(libraryPath, packageNames, repos) {
  dir.create(libraryPath, recursive = TRUE, showWarnings = FALSE)

  # Make canonical library first for this session only
  .libPaths(c(libraryPath, setdiff(.libPaths(), libraryPath)))
  message("Active library order:")
  print(.libPaths())

  for (pkg in packageNames) {
    message("Installing/reinstalling: ", pkg)
    install.packages(pkg, repos = repos, lib = libraryPath, dependencies = TRUE)
  }

  for (pkg in c("checkmate", "curl", "DatabaseConnector", "SqlRender", "data.table")) {
    message("Verifying load: ", pkg)
    suppressPackageStartupMessages(library(pkg, character.only = TRUE, lib.loc = libraryPath))
  }

  TRUE
}

removeLocalLibrary <- function(localPath) {
  if (!dir.exists(localPath)) {
    message("Local project library does not exist: ", localPath)
    return(invisible(FALSE))
  }

  answer <- readline(prompt = paste0("Delete local project library at ", localPath, " ? [y/N]: "))
  if (!tolower(trimws(answer)) %in% c("y", "yes")) {
    message("Skipped deleting local project library.")
    return(invisible(FALSE))
  }

  unlink(localPath, recursive = TRUE, force = TRUE)
  if (dir.exists(localPath)) {
    warning("Could not fully delete local project library: ", localPath)
    return(invisible(FALSE))
  }

  message("Deleted local project library: ", localPath)
  invisible(TRUE)
}

message("--- Ithaca library migration starting ---")

canonical <- resolveCanonicalLibrary(projectDir, canonicalLibraryPath)
message("Canonical/original library resolved to: ", canonical)

if (file.exists(projectRprofile)) {
  backup <- makeBackup(projectRprofile)
  message("Backup created: ", backup)
  changed <- rewriteProjectRprofile(projectRprofile)
  message("Updated .Rprofile: ", changed)
}

if (file.exists(repairRunner)) {
  backup <- makeBackup(repairRunner)
  message("Backup created: ", backup)
  changed <- rewriteRepairRunner(repairRunner)
  message("Updated tmp_run_repair_full.R: ", changed)
}

installAndVerify(
  libraryPath = canonical,
  packageNames = requiredPackages,
  repos = repos
)

message("Migration complete. Restart RStudio now so startup files are re-read.")
removeLocalLibrary(localIthacaLib)
message("--- Ithaca library migration finished ---")

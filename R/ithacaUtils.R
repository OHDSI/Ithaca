ithPromptDir <- function(promptDir = NULL) {
  if (!is.null(promptDir)) {
    return(promptDir)
  }

  pkg_dir <- system.file("prompts", package = "Ithaca")
  if (nzchar(pkg_dir)) {
    return(pkg_dir)
  }

  if (dir.exists("inst/prompts")) {
    return("inst/prompts")
  }

  "docs"
}

ithMaybeMessage <- function(verbose, ...) {
  if (isTRUE(verbose)) {
    message(...)
  }
}

ithReadPrompt <- function(promptFile, promptDir = NULL) {
  finalPromptDir <- ithPromptDir(promptDir)
  prompt_path <- file.path(finalPromptDir, promptFile)
  if (!file.exists(prompt_path)) {
    stop("Prompt file not found: ", prompt_path)
  }

  paste(readLines(prompt_path, warn = FALSE), collapse = "\n")
}

ithParseConceptSets <- function(conceptSetString) {
  if (is.null(conceptSetString) || is.na(conceptSetString) || conceptSetString == "") {
    return(character())
  }

  parts <- unlist(strsplit(gsub(";", ",", conceptSetString), ",", fixed = TRUE))
  unique(trimws(parts[nzchar(trimws(parts))]))
}

ithEscapeRegex <- function(x) {
  gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", x)
}

ithFindLatestVersion <- function(saveDirectory, fileName) {
  if (!dir.exists(saveDirectory)) {
    return(NULL)
  }

  ext <- tolower(tools::file_ext(fileName))
  name <- basename(tools::file_path_sans_ext(fileName))
  pattern <- paste0("^", ithEscapeRegex(name), "_V([0-9]+)\\.", ithEscapeRegex(ext), "$")

  files <- list.files(path = saveDirectory, pattern = pattern, ignore.case = TRUE)
  if (length(files) == 0L) {
    return(NULL)
  }

  matches <- regexec(pattern, files, perl = TRUE, ignore.case = TRUE)
  versions <- vapply(regmatches(files, matches), function(m) as.integer(m[[2]]), integer(1))
  files[[which.max(versions)]]
}

ithSaveByExt <- function(x, file, overwrite = FALSE) {
  if (file.exists(file) && !overwrite) {
    stop("File exists: ", file)
  }

  ext <- tolower(tools::file_ext(file))
  if (ext == "rds") {
    saveRDS(x, file)
  } else if (ext == "csv") {
    utils::write.csv(x, file, row.names = FALSE)
  } else if (ext %in% c("txt", "text")) {
    if (length(x) > 1 && !is.character(x)) {
      x <- capture.output(print(x))
    }
    writeLines(as.character(x), con = file)
  } else {
    saveRDS(x, file)
  }
}

ithSaveVersioned <- function(saveDirectory, objectToSave, fileName, replace = FALSE) {
  if (!dir.exists(saveDirectory)) {
    dir.create(saveDirectory, recursive = TRUE)
  }

  ext <- tolower(tools::file_ext(fileName))
  name <- basename(tools::file_path_sans_ext(fileName))

  if (replace) {
    latest <- ithFindLatestVersion(saveDirectory, fileName)
    if (is.null(latest)) {
      latest <- paste0(name, "_V1.", ext)
    }
    fullPath <- file.path(saveDirectory, latest)
    ithSaveByExt(objectToSave, fullPath, overwrite = TRUE)
    return(fullPath)
  }

  # Always max + 1 so a deleted version (gap) is never reused.
  latest <- ithFindLatestVersion(saveDirectory, fileName)
  nextVersion <- if (is.null(latest)) {
    1L
  } else {
    as.integer(sub("^.*_V([0-9]+)\\.[^.]+$", "\\1", latest, ignore.case = TRUE)) + 1L
  }
  while (file.exists(file.path(saveDirectory, paste0(name, "_V", nextVersion, ".", ext)))) {
    nextVersion <- nextVersion + 1L
  }

  fullPath <- file.path(saveDirectory, paste0(name, "_V", nextVersion, ".", ext))
  ithSaveByExt(objectToSave, fullPath, overwrite = FALSE)
  fullPath
}

ithLoadLatest <- function(saveDirectory, fileName) {
  latest <- ithFindLatestVersion(saveDirectory, fileName)
  if (is.null(latest)) {
    return(NULL)
  }

  fullPath <- file.path(saveDirectory, latest)
  ext <- tolower(tools::file_ext(fullPath))

  if (ext == "rds") {
    return(readRDS(fullPath))
  }

  if (ext == "csv") {
    return(utils::read.csv(fullPath, stringsAsFactors = FALSE))
  }

  if (ext %in% c("txt", "text")) {
    return(readLines(fullPath, warn = FALSE))
  }

  readRDS(fullPath)
}

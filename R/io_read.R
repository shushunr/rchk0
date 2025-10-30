# R/io_read.R
#' Load raw datasets into a named list, and also assign each to environment
#' @param path Directory to search
#' @param pattern Regex for files (default .sas7bdat)
#' @param exclude_prefixes Character vector of prefixes to drop, e.g., c("sys", "cdms")
#' @param exclude_names Character vector of dataset names to drop (without extension)
#' @param envir Environment to assign datasets into (default: .GlobalEnv)
#' @return Named list of data.frames/tibbles
#' @export
load_dataset <- function(path,
                         pattern = "\\.sas7bdat$",
                         exclude_prefixes = c("sys", "cdms"),
                         exclude_names = c("record_count", "rtsm_sbjct_data"),
                         verbose = TRUE) {
  stopifnot(dir.exists(path))
  all_files <- list.files(path, pattern = pattern, full.names = TRUE)
  dataset_names <- gsub("^.*/|\\.sas7bdat$", "", all_files)
  
  # exclude
  pref_hit <- Reduce(`|`, lapply(exclude_prefixes, function(pref) {
    grepl(paste0("^", pref), dataset_names, ignore.case = TRUE)
  }))
  keep <- !(pref_hit | dataset_names %in% exclude_names)
  
  files_keep <- all_files[keep]
  names_keep <- dataset_names[keep]
  
  out <- vector("list", length(files_keep))
  names(out) <- names_keep
  
  # internal helper that tries multiple encodings/readers
  try_read_sas <- function(file, name) {
    encodings <- c("utf-8", "latin1", "windows-1252")
    for (enc in encodings) {
      df <- tryCatch(
        haven::read_sas(file, encoding = enc),
        error = function(e) NULL
      )
      if (!is.null(df)) {
        # if used a non-default encoding, notify
        if (enc != "utf-8" && verbose) {
          message(sprintf("'%s' loaded using encoding = '%s'", name, enc))
        }
        return(df)
      }
    }
    
    if (requireNamespace("arrow", quietly = TRUE)) {
      df <- tryCatch(arrow::read_sas(file), error = function(e) NULL)
      if (!is.null(df)) {
        if (verbose) message(sprintf("'%s' loaded using arrow::read_sas()", name))
        return(df)
      }
    }
    
    if (requireNamespace("sas7bdat", quietly = TRUE)) {
      df <- tryCatch(sas7bdat::read.sas7bdat(file), error = function(e) NULL)
      if (!is.null(df)) {
        if (verbose) message(sprintf("'%s' loaded using sas7bdat::read.sas7bdat()", name))
        return(df)
      }
    }
    
    return(NULL)
  }
  
  # main loop
  for (i in seq_along(files_keep)) {
    file <- files_keep[i]
    name <- names_keep[i]
    if (verbose) message(sprintf("Reading: %s ...", basename(file)))
    df <- try_read_sas(file, name)
    if (is.null(df)) {
      warning(sprintf("Failed to read dataset: %s", name))
    } else if (nrow(df) > 0) {  ## only assign the environment if nrow of dataset > 0
      out[[i]] <- df
      if (verbose) message(sprintf("Loaded %s (%d rows)", name, nrow(df)))
    }
  }
  
  out <- out[!vapply(out, is.null, logical(1))]
  
  # --- summary section ---
  is_null <- vapply(out, is.null, logical(1))
  is_zero <- vapply(out[!is_null], function(x) nrow(x) == 0, logical(1))
  n_failed <- sum(is_null)
  n_zero   <- sum(is_zero)
  n_nonzero <- sum(!is_zero)
  n_total <- length(out)
  
  message("\n Load summary:")
  message(sprintf("  • Total datasets processed: %d", length(files_keep)))
  message(sprintf("  • Successfully loaded: %d", n_total))
  message(sprintf("      - With data (nrow > 0): %d", n_nonzero))
  message(sprintf("      - Empty (nrow = 0): %d", n_zero))
  message(sprintf("  • Failed to load: %d", n_failed))
  
  return(out)
}
    
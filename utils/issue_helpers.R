# R/check_common.R
# Common issue check functions for raw data tracker

#' Wrap summary and raw issue outputs
#'
#' @param df_summary A summary data.frame
#' @param df_raw A raw data.frame
#' @param idx Optional index
#' @param label Optional label
#' @return A list with `summary` and `raw`
#' @keywords internal
issue_envelope <- function(df_summary, df_raw, idx = NULL, label = NULL) {
  structure(
    list(summary = df_summary, raw = df_raw, idx = idx, label = label),
    class = "issue_result"
  )
}

#' Add issue metadata
#' @param df_summary A summary data.frame
#' @param df_raw A raw data.frame
#' @param idx Optional index
#' @param label Optional label
add_issue_meta <- function(df, issue, type = c("raw", "summary"),
                           dataset = NULL, id_cols = c("SITEID","SUBJECT_ID"),
                           status = "New", first = Sys.Date(), last = Sys.Date()) {
  type <- match.arg(type)
  if (type == "raw") {
    dplyr::mutate(df,
                  Issue_type = "Automatic",
                  Issue_noted_by_Lilly_Stats = issue,
                  DM_Comment_or_resolution = "",
                  Status = status,
                  Importance = ""
    )
  } else {
    # collapse to one row per subject/site
    df |>
      dplyr::distinct(dplyr::across(dplyr::all_of(id_cols))) |>
      dplyr::mutate(
        DATASET = dataset, ISSUE = issue,
        FIRST_DETECTED_DATE = first, LAST_CHECKED_DATE = last,
        STATUS = status
      ) |>
      dplyr::select(DATASET, dplyr::all_of(c(setdiff(id_cols, "SUBJECT_ID"), "SUBJECT_ID")),
                    ISSUE, FIRST_DETECTED_DATE, LAST_CHECKED_DATE, STATUS)
  }
}

##### Helper functions #####
normalize_empty_to_na <- function(df, cols = names(df)) {
  chr <- intersect(cols, names(df)[vapply(df, is.character, TRUE)]) ## check all character type columns
  dplyr::mutate(df, dplyr::across(dplyr::all_of(chr), ~ dplyr::na_if(.x, ""))) ## Turn all empty strings "" to NA
}

filter_exclude_unscheduled <- function(df, visit_label_col = NULL) {
  if (!is.null(visit_label_col) && visit_label_col %in% names(df)) {
    dplyr::filter(df, !grepl("unscheduled", .data[[visit_label_col]], ignore.case = TRUE)) ## filter out unscheduled visits
  } else df
}

derive_visit_num <- function(df, visit_label_col = NULL) {
  if ("VISITNUM" %in% names(df)) {
    dplyr::mutate(df, VISIT_NUM = suppressWarnings(as.numeric(.data$VISITNUM))) ## get VISITNUM from data, else get visit_label_col
  } else if (!is.null(visit_label_col) && visit_label_col %in% names(df)) {
    dplyr::mutate(
      df,
      VISIT_NUM = suppressWarnings(as.numeric(stringr::str_extract(.data[[visit_label_col]], "\\d+")))
    )
  } else df
}

##### Excel I/O #####
.wb_row_cache <- new.env(parent = emptyenv())

wb_append <- function(wb, sheet, x, gap = 1, write_header_if_new = TRUE) {
  stopifnot(inherits(wb, "Workbook"))
  if (!(sheet %in% openxlsx::sheets(wb))) { ## create new sheet if not exist
    openxlsx::addWorksheet(wb, sheet)
    start <- 1
    openxlsx::writeData(wb, sheet, x, startRow = start, colNames = write_header_if_new)
    .wb_row_cache[[sheet]] <- nrow(x) + if (write_header_if_new) 1 else 0
  } else { ## Append data if sheet exists
    start <- (get0(sheet, envir = .wb_row_cache, ifnotfound = 0) %||% 0) + gap + 1
    openxlsx::writeData(wb, sheet, x, startRow = start, colNames = FALSE)
    .wb_row_cache[[sheet]] <- start + nrow(x) - 1
  }
  invisible(TRUE)
}

##### Dataset pre-check #####
per_dataset <- function(datasets_pool, dataset_no = NULL, visit_info_df = NULL,
                        .fn, id_cols = c("SITEID","SUBJECT_ID"), ...) {
  out <- list()
  datasets <- setdiff(names(datasets_pool), dataset_no)
  for (ds in datasets) {
    df <- datasets_pool[[ds]]
    if (is.null(df) || nrow(df) == 0) { message("Dataset is empty: ", ds); next }
    
    ## Get dataset info from data_info
    info <- if (!is.null(visit_info_df))
      dplyr::filter(visit_info_df, .data$dataset == ds) else NULL
    
    vdate <- if (!is.null(info) && nrow(info)) info$visit_date_col[1] else NA_character_
    vlab  <- if (!is.null(info) && nrow(info)) info$visit_label_col[1] else NA_character_
    
    res <- .fn(df = df, ds = ds, visit_date_col = vdate, visit_label_col = vlab,
               id_cols = id_cols, ...)
    if (!is.null(res) && nrow(res)) out[[ds]] <- res
  }
  out
}
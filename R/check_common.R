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


#' Check for missing key variables
#'
#' @param datasets_pool Named list of datasets
#' @param wb An openxlsx Workbook
#' @param target_vars Key variables to check for some/all datasets
#' @param dataset_no Names of datasets to exclude
#' @param visit_info_df Visit info metadata
#' @return Writes results into workbook, invisibly returns NULL
#' @export
compute_missing_key_vars <- function(df, ds, keys) {
  common <- intersect(keys, names(df)) 
  if (!length(common)) return(NULL) ## If keys are not in dataset, return NULL
  
  df2 <- normalize_empty_to_na(df, common)
  miss <- dplyr::filter(df2, dplyr::if_any(dplyr::all_of(common), ~ is.na(.x)))
  if (!nrow(miss)) return(NULL)
  
  # Find missing rows
  miss <- miss |>
    dplyr::rowwise() |>
    dplyr::mutate(
      missing_vars = {
        vals <- dplyr::pick(dplyr::all_of(common))
        nm   <- names(vals)
        miss <- vapply(vals, function(x) is.na(x)[1], logical(1))
        paste(nm[miss], collapse = ", ")
      }
    ) |>
    dplyr::group_by(SUBJECT_ID, SITEID) |>
    dplyr::slice(1) |> ## Only one record per subject_id/siteid
    dplyr::ungroup() |>
    dplyr::filter(missing_vars != "") |>
    dplyr::select(SUBJECT_ID, SITEID, missing_vars) |> ## only keep subject_id, siteid columns per issue
    dplyr::mutate(`.__issue_msg` = paste("Missing values in", missing_vars)) |>
    (\(x) add_issue_meta(x, issue = x$`.__issue_msg`, type = "raw"))() |>
    dplyr::select(-`.__issue_msg`, -missing_vars) 
  
  miss[] <- lapply(miss, as.character)
  miss
}

check_missing_key_vars <- function(datasets_pool, wb,
                                   target_vars = NULL, dataset_no = NULL,
                                   visit_info_df = NULL) {
  res <- per_dataset(
    datasets_pool, dataset_no, visit_info_df,
    .fn = function(df, ds, ...) compute_missing_key_vars(df, ds, target_vars)
  )
  
  purrr::iwalk(res, ~ wb_append(wb, .y, .x))
  res
}

#' Check for duplicate visits with the same date
#'
#' @param datasets_pool Named list of datasets
#' @param wb Workbook
#' @param target_vars Optional
#' @param dataset_no Datasets to exclude
#' @param visit_info_df Visit info
#' @param output_tab Output tab
#' @param custom_code_list Named list of custom check functions
#' @export
compute_duplicate_visit_date <- function(df, ds, visit_date_col, visit_label_col, id_cols) {
  if (is.na(visit_date_col) || !(visit_date_col %in% names(df))) return(NULL) ## If visit date column doesn't exist, skip
  
  df2 <- df |>
    dplyr::filter(!is.na(.data[[visit_date_col]])) |> 
    filter_exclude_unscheduled(visit_label_col) ## filter unscheduled visits
  
  dups <- df2 |>
    dplyr::group_by(.data[[id_cols[2]]], .data[[visit_date_col]]) |>  
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()
  if (!nrow(dups)) return(NULL)
  
  order_cols <- c(id_cols[2], visit_date_col, intersect(visit_label_col, names(dups)))
  dups <- dplyr::arrange(dups, dplyr::across(dplyr::all_of(order_cols))) |>
    dplyr::group_by(.data[[id_cols[2]]], .data[[visit_date_col]]) |>
    dplyr::summarise(
      SUBJECT_ID = dplyr::first(.data[[id_cols[2]]]),
      SITEID     = dplyr::first(.data[[id_cols[1]]]),
      visit_label = {
        if (!is.na(visit_label_col) && visit_label_col %in% names(cur_data_all())) {
          vals <- .data[[visit_label_col]]
          if (all(is.na(vals))) NA_character_ else paste(unique(vals), collapse = ", ")
        } else {
          NA_character_
        }
      },
      .groups = "drop"
    )
  
  dups <- dups |>
    dplyr::mutate(
      `.__issue_msg` = dplyr::if_else(
        is.na(.data$visit_label) | .data$visit_label == "",
        "Duplicate records within the same date",
        paste("Duplicate records within the same date for", .data$visit_label)
      )
    ) |>
    (\(x) add_issue_meta(x, issue = x$`.__issue_msg`, type = "raw"))() |>
    dplyr::select(-`.__issue_msg`)  ## Duplicate visits within the same date for Visit 2, Visit 3
} 

check_duplicate_visit_date <- function(datasets_pool, wb,
                                       target_vars = NULL, dataset_no = NULL,
                                       visit_info_df = NULL,
                                       output_tab = NULL, custom_code_list = NULL,
                                       id_cols = c("SITEID", "SUBJECT_ID")) {
  res <- per_dataset(
    datasets_pool, dataset_no, visit_info_df,
    .fn = function(df, ds, visit_date_col, visit_label_col, id_cols) {
      if (!is.null(custom_code_list) && ds %in% names(custom_code_list)) {
        out <- custom_code_list[[ds]](df, visit_date_col)
        if (is.null(out) || !nrow(out)) return(NULL)
        return(add_issue_meta(out, "Duplicate visits with the same date", type = "raw"))
      }
      compute_duplicate_visit_date(df, ds, visit_date_col, visit_label_col, id_cols)
    },
    id_cols = id_cols
  )
  purrr::iwalk(res, ~ wb_append(wb, .y, .x))
  res
}

#' Check for visits after ED
#'
#' @param datasets_pool Named list of datasets
#' @param wb Workbook
#' @param dataset_no Excluded datasets
#' @param visit_info_df Visit info metadata
#' @param output_tab Output tab
#' @param other_datasets Character vector of datasets to check
#' @return A named list, each element is a data frame of records
#'   with visits after ED for the corresponding dataset.
#'   If no datasets have violations, returns an empty list.
#' @export
get_ed_dates <- function(ed_ds,
                         id_col = "SUBJECT_ID",
                         date_col = "DSSTDAT",
                         form_col = "FORMEID",
                         forms = c("DS6001_LV6","DS6001_LV5")) {
  if (is.null(ed_ds) || !(date_col %in% names(ed_ds))) return(NULL)
  ed_ds |>
    dplyr::filter(.data[[form_col]] %in% forms, .data[[date_col]] != "") |>
    dplyr::mutate(ED_DATE = as.Date(.data[[date_col]])) |>
    dplyr::select(dplyr::all_of(c(id_col, "ED_DATE")))
}

compute_visit_after_ed <- function(df, ds, visit_date_col, visit_label_col, ed_dates) {
  if (is.na(visit_date_col) || !(visit_date_col %in% names(df))) return(NULL)
  
  df2 <- df |>
    dplyr::mutate(VISIT_DATE = as.Date(.data[[visit_date_col]])) |>
    dplyr::filter(!is.na(.data$VISIT_DATE)) |>
    dplyr::inner_join(ed_dates, by = "SUBJECT_ID") |>
    dplyr::filter(.data$VISIT_DATE > .data$ED_DATE)
  
  if ("VISITNUM" %in% names(df2))
    df2 <- dplyr::filter(df2, !(.data$VISITNUM %in% c("999","997","801","802","803")))
  if (!nrow(df2)) return(NULL)
  
  add_issue_meta(dplyr::select(df2, -VISIT_DATE, -ED_DATE),
                 "Visit occurred after ED date", type = "raw")
}

check_visit_after_ED <- function(datasets_pool, wb,
                                 dataset_no = NULL, visit_info_df = NULL,
                                 output_tab = NULL, other_datasets = NULL) {
  ed_dates <- get_ed_dates(datasets_pool[["ds6001"]])
  if (is.null(ed_dates)) {
    warning("ED dataset missing or DSSTDAT not found.")
    return(NULL)
  }
  keep <- intersect(other_datasets %||% names(datasets_pool), names(datasets_pool))
  
  res <- per_dataset(
    datasets_pool[keep], dataset_no = NULL, visit_info_df,
    .fn = function(df, ds, visit_date_col, visit_label_col, ...) {
      compute_visit_after_ed(df, ds, visit_date_col, visit_label_col, ed_dates)
    }
  )
  purrr::iwalk(res, ~ wb_append(wb, .y, .x))
  res
}



#' Check for overlapped visits
#'
#' @param datasets_pool Named list of datasets
#' @param wb Workbook
#' @param dataset_no Excluded datasets
#' @param visit_info_df Visit info metadata
#' @param output_tab Output tab
#'
#' @return A named list, each element is a data frame of records
#'   with overlapped visits for the corresponding dataset.
#'   If no datasets have overlaps, returns an empty list.
#'
#' @export
compute_visit_overlap <- function(df, ds, visit_date_col, visit_label_col,
                                  id_cols = c("SITEID","SUBJECT_ID")) {
  if (is.na(visit_date_col) || !(visit_date_col %in% names(df))) return(NULL)
  
  ds2 <- df |>
    filter_exclude_unscheduled(visit_label_col) |>
    derive_visit_num(visit_label_col) |>
    dplyr::mutate(VISIT_DATE = as.Date(.data[[visit_date_col]])) |>
    dplyr::filter(!is.na(.data$VISIT_DATE)) |>
    dplyr::filter(!is.na(.data$VISIT_NUM), !(.data$VISIT_NUM %in% c(999, 997))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::arrange(.data$VISIT_NUM, .by_group = TRUE) |>
    dplyr::mutate(PREV_DATE = dplyr::lag(.data$VISIT_DATE),
                  OVERLAP_FL = .data$VISIT_DATE < .data$PREV_DATE) |>
    dplyr::ungroup()
  
  overlap_rows <- dplyr::filter(ds2, .data$OVERLAP_FL)
  if (!nrow(overlap_rows)) return(NULL)
  
  prior_rows <- dplyr::inner_join(
    ds2,
    dplyr::select(overlap_rows, dplyr::all_of(id_cols),
                  CUR_VISITNUM = .data$VISIT_NUM, PREV_DATE = .data$PREV_DATE),
    by = setNames(c(id_cols, "VISIT_DATE"), c(id_cols, "PREV_DATE"))
  )
  
  raw0 <- dplyr::bind_rows(overlap_rows, prior_rows) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(id_cols, "VISIT_NUM", "VISIT_DATE")))) |>
    dplyr::select(-.data$PREV_DATE, -.data$OVERLAP_FL, -.data$VISIT_DATE, -.data$CUR_VISITNUM)
  
  add_issue_meta(raw0, "Overlapped visits: later visit has earlier date", type = "raw")
}

check_visit_overlap <- function(datasets_pool, wb,
                                dataset_no = NULL, visit_info_df = NULL,
                                output_tab = NULL,
                                id_cols = c("SITEID","SUBJECT_ID")) {
  res <- per_dataset(
    datasets_pool, dataset_no, visit_info_df,
    .fn = function(df, ds, visit_date_col, visit_label_col, id_cols) {
      compute_visit_overlap(df, ds, visit_date_col, visit_label_col, id_cols)
    },
    id_cols = id_cols
  )
  purrr::iwalk(res, ~ wb_append(wb, .y, .x))
  res
}




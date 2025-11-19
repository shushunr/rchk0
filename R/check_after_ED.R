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
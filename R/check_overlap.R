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




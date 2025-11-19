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

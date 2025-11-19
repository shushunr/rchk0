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

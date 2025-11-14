#' Check subjects occurred in multiple sites
#'
#' This function identifies subjects who appear in more than one site across datasets.
#'
#' @param datasets_pool Named list of raw datasets
#' @param wb An openxlsx Workbook object
#' @param output_tab Name of Excel worksheet to create
#' @param visit_info_df Metadata with dataset, subject_var, site_var
#'
#' @return A data frame with the following key columns:
#' \item{SUBJECT_ID}{Subject Identifier}
#' \item{n_sites}{Number of sites that this subject occurred in}
#' \item{site_dataset_map}{Site-dataset pairs of subjects that occurred in multiple sites}
#' @export
multi_site <- function(datasets_pool, wb, output_tab = NULL,
                             visit_info_df = NULL) {
  available_ds <- names(datasets_pool)
  
  ## Find all subject id/site id pairs across all datasets
  all_subject_sites <- purrr::map_dfr(available_ds, function(ds_name) {
    df <- datasets_pool[[ds_name]]
    info_row <- visit_info_df %>% filter(dataset == ds_name)
    
    ## skip if dataset was not found
    if (nrow(info_row) == 0) {
      message("Dataset ", ds_name, " not found in visit_info_df. Skipped.")
      return(NULL)
    }
    
    subject_col <- info_row$subject_var
    site_col    <- info_row$site_var
    
    # skip if either subject id or site id was not found
    if (is.na(subject_col) || is.na(site_col) ||
        subject_col == "" || site_col == "") {
      message("Dataset ", ds_name, " missing subject/site column info. Skipped.")
      return(NULL)
    }
    
    df %>% 
      transmute(SUBJECT_ID = as.character(.data[[subject_col]]),
             SITEID = as.character(.data[[site_col]]),
             dataset_name = ds_name) %>% 
      filter(!is.na(.data[[subject_col]]), !is.na(.data[[site_col]]))
  })
  
  if (nrow(all_subject_sites) == 0) {
    message("No subject/site information available across datasets.")
    return(invisible(NULL))
  }
  
  multi_site_summary <- all_subject_sites %>% 
    group_by(SUBJECT_ID, SITEID) %>% 
    summarise(
      dataset_list = paste(sort(unique(dataset_name)), collapse = ", "),
      .groups = "drop"
    )  %>% 
    group_by(SUBJECT_ID) %>% 
    summarise(n_sites = dplyr::n(),
              site_dataset_map = paste(
              paste0("Site ", SITEID, ": ", dataset_list),
              collapse = "\n"),
              .groups = "drop") %>% 
    dplyr::filter(n_sites > 1)
  
  if (nrow(multi_site_summary) == 0) {
    message("No subjects found across multiple sites.")
    return(invisible(NULL))
  }
  
  issues <- multi_site_summary |>
    dplyr::mutate(
      Issue_type = "Automatic",
      Issue_noted_by_Lilly_Stats = "Subject appears in multiple sites",
      PPD_Comment_or_resolution = "",
      Status = "New"
    )
  
  if (!is.null(output_tab)) {
    openxlsx::addWorksheet(wb, output_tab, tabColour = "#FFFF99")
    openxlsx::writeData(wb, sheet = output_tab, x = issues)
  }
  
  return(issues)
}


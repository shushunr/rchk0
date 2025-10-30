# R/preprocess.R
#' Preprocess raw datasets based on spec
#'
#' This function standardizes subject and site identifiers, applies
#' preprocessing rules defined in the spec, and captures visit-related
#' metadata for each dataset in the pool. The main steps include:
#' \enumerate{
#'   \item Identify subject and site variables and create unified columns
#'         \code{SUBJECT_ID} and \code{SITEID}.
#'   \item Apply row- and column-level filtering rules from the spec
#'         (sheet "PREPROCESSING").
#'   \item Detect visit-related columns (visit date and visit label).
#'   \item Return updated datasets and a summary data frame of metadata.
#' }
#'
#' @param datasets_pool A named list of raw datasets, typically the output of \code{load_dataset()}.
#' @param spec_path File path to the preprocessing spec (Excel, with sheet "PREPROCESSING").
#'
#' @return A list with two elements:
#' \describe{
#'   \item{\code{visit_info_df}}{A data frame with one row per dataset,
#'     containing subject/site variable names and detected visit columns.}
#'   \item{\code{datasets_pool}}{The updated datasets after preprocessing.}
#' }
#'
#' @examples
#' \dontrun{
#'   res <- preprocess(datasets_pool, "spec.xlsx")
#'   res$visit_info_df
#'   head(res$datasets_pool$ds6001)
#' }
#'
#' @export
preprocess <- function(datasets_pool, spec_path, export_to_env = FALSE) {
  visit_info_list <- list()
  preprocess_spec <- openxlsx::read.xlsx(spec_path, sheet = "PREPROCESSING")
  
  for (nm in names(datasets_pool)) {
    df <- datasets_pool[[nm]]
    
    ## Read in spec rows (keep rows where REMOVE != "Y")
    spec_row <- preprocess_spec %>%
      dplyr::filter(DATASET == nm & (is.na(REMOVE) | REMOVE != "Y"))
    
    # ====================
    # 1) SUBJECT_ID variable creation
    # ====================
    if ("SUBJECT" %in% names(df)) {
      df <- df %>% dplyr::mutate(SUBJECT_ID = sub(".*-", "", SUBJECT))
      subject_var <- "SUBJECT"
    } else if ("SUBJID" %in% names(df)) {
      df <- df %>% dplyr::mutate(SUBJECT_ID = sub(".*-", "", SUBJID))
      subject_var <- "SUBJID"
    } else if ("USUBJID" %in% names(df)) {
      df <- df %>% dplyr::mutate(SUBJECT_ID = sub(".*-", "", USUBJID))
      subject_var <- "USUBJID"
    } else if ("SUBJECT_NUMBER" %in% names(df)) {
      df <- df %>% dplyr::mutate(SUBJECT_ID = sub(".*-", "", SUBJECT_NUMBER))
      subject_var <- "USUBJID"
    } else if ("SUBJECTNAME" %in% names(df)) {
      df <- df %>% dplyr::mutate(SUBJECT_ID = sub(".*-", "", SUBJECTNAME))
      subject_var <- "SUBJECTNAME"
    } else {
      df <- df %>% dplyr::mutate(SUBJECT_ID = NA_character_)
      subject_var <- ""
    }
    
    # ====================
    # 2) SITEID variable creation
    # ====================
    if ("SITEID" %in% names(df)) {
      df <- df %>% dplyr::mutate(SITEID = as.character(SITEID))
      site_var <- "SITEID"
    } else if ("SITE_ID" %in% names(df)) {
      df <- df %>% dplyr::mutate(SITEID = as.character(SITE_ID))
      site_var <- "SITENUM"
    } else if ("SITENUM" %in% names(df)) {
      df <- df %>% dplyr::mutate(SITEID = as.character(SITENUM))
      site_var <- "SITENUM"
    } else if ("USUBJID" %in% names(df)) {
      df <- df %>% dplyr::mutate(SITEID = substr(USUBJID, 1, 5))
      site_var <- "USUBJID"
    } else if ("SITE" %in% names(df)) {
      df <- df %>% dplyr::mutate(SITEID = as.character(SITE))
      site_var <- "SITE"
    } else if ("SITENUMBER" %in% names(df)) {
      df <- df %>% dplyr::mutate(SITEID = as.character(SITENUMBER))
      site_var <- "SITENUMBER"
    } else if ("INVID" %in% names(df)) {
      df <- df %>% dplyr::mutate(SITEID = as.character(INVID))
      site_var <- "INVID"
    } else if ("SUBJID" %in% names(df)) {
      df <- df %>% dplyr::mutate(SITEID = substr(SUBJID, 1, 5))  ## SUBJID='10019-20099', no site variables found
      site_var <- "SUBJID"
    } else {
      df <- df %>% dplyr::mutate(SITEID = NA_character_)
      site_var <- ""
    }
    
    # ====================
    # 3) Customized row filter from EXCLUDE_ROWS (filter out)
    # ====================
    if (nrow(spec_row) > 0 && !is.na(spec_row$EXCLUDE_ROWS)) {
      custom_filter <- spec_row$EXCLUDE_ROWS
      cond <- eval(parse(text = custom_filter), envir = df)

      if (is.logical(cond) && length(cond) == nrow(df)) {
        df <- df[cond, , drop = FALSE]
      } else {
        warning("EXCLUDE_ROWS did not evaluate to a logical vector; no rows removed.")
      }
    }

    # ====================
    # 4) Customized columns filter from EXCLUDE_COLS (drop columns)
    # ====================
    if (nrow(spec_row) > 0 && !is.na(spec_row$EXCLUDE_COLS)) {
      selected_cols <- strsplit(spec_row$EXCLUDE_COLS, ",")[[1]] %>% trimws()
      existing_cols <- intersect(selected_cols, names(df))
      if (length(existing_cols) > 0) {
        df <- df %>% select(-all_of(existing_cols))
      }
    }
    
    # ====================
    # 5) Visit date column
    # ====================
    visit_date_col <- NULL
    if ("EVENTDT" %in% names(df)) {
      visit_date_col <- "EVENTDT"
    } else if (nm == "lab" && "LBDTM" %in% names(df)) {
      visit_date_col <- "LBDTM"
    } else if (nm == "sv1001") {
      visit_date_col <- "VISDAT"
    } else if (nm == "vs1001") {
      visit_date_col <- "VSDAT"
    } else if ("ECOAASMDT" %in% names(df)) {
      df <- df %>% dplyr::mutate(ECOAASMDT = as.Date(ECOAASMDT))
      visit_date_col <- "ECOAASMDT"
    } else if (nm == "ec1001") {
      visit_date_col <- "ECSTDAT"
    }
    
    # ====================
    # 6) Visit label column
    # ====================
    visit_label_col <- if ("VISIT" %in% names(df)) {
      "VISIT"
    } else if ("EVENT" %in% names(df)) {
      "EVENT"
    } else if ("VISITNAME" %in% names(df)){
      "VISITNAME"
    }
    else {
      NA
    }
    
    visit_info_list[[nm]] <- data.frame(
      dataset = nm,
      subject_var = subject_var,
      site_var = site_var,
      visit_date_col = ifelse(is.null(visit_date_col), NA, visit_date_col),
      visit_label_col = ifelse(is.null(visit_label_col), NA, visit_label_col),
      stringsAsFactors = FALSE
    )
    
    datasets_pool[[nm]] <- df
    
    if (export_to_env) {
      assign(nm, df, envir = .GlobalEnv)
    }
  }
  
  visit_info_df <- dplyr::bind_rows(visit_info_list)
  
  return(list(
    visit_info_df = visit_info_df,
    datasets_pool = datasets_pool
  ))
}

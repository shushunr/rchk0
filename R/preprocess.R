# R/preprocess.R
#' Preprocess raw datasets according to spec
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
#'   res <- preprocess_data(datasets_pool, "spec.xlsx")
#'   res$visit_info_df
#'   head(res$datasets_pool$ds6001)
#' }
#'
#' @export
preprocess <- function(datasets_pool, spec_path) {
  preprocess_spec <- read.xlsx(spec_path, sheet = "PREPROCESSING")

  map(names(datasets_pool), function(nm) {
    df <- datasets_pool[[nm]]
    spec_row <- preprocess_spec %>% filter(DATASET == nm, is.na(REMOVE) | REMOVE != "Y") ## spec preprocessing rows

    # subject_var
    subject_candidates <- c("SUBJECT", "SUBJID", "USUBJID", "SUBJECTNAME")
    subject_var <- intersect(subject_candidates, names(df))[1] %||% NA_character_
    if (!is.na(subject_var)) {
      df <- df %>% mutate(SUBJECT_ID = as.character(sub(".*-", "", .data[[subject_var]])))
    } else {
      df <- df %>% mutate(SUBJECT_ID = NA_character_)
    }

    # site_var
    site_candidates <- c("SITEID", "SITENUM", "USUBJID", "SITE", "SITENUMBER", "INVID")
    site_var <- intersect(site_candidates, names(df))[1] %||% NA_character_
    if (!is.na(site_var)) {
      df <- df %>% mutate(SITEID = as.character(.data[[site_var]]))
      if (site_var == "USUBJID"){
        df <- df %>% mutate(SITEID = substr(USUBJID, 1, 5))
      }
    } else {
      df <- df %>% mutate(SITEID = NA_character_)
    }

    # row filter
    if (nrow(spec_row) == 1 && !is.na(spec_row$EXCLUDE_ROWS)) {
      expr <- rlang::parse_expr(spec_row$EXCLUDE_ROWS)
      df <- df %>% filter(!!expr)
    }

    # column filter
    if (nrow(spec_row) == 1 && !is.na(spec_row$EXCLUDE_COLS)) {
      cols <- strsplit(spec_row$EXCLUDE_COLS, ",")[[1]] %>% trimws()
      df <- df %>% select(-any_of(cols))
    }

    # visit_date_col
    visit_date_col <- intersect(c("EVENTDT", "LBDTM", "VISDAT", "VSDAT", "ECOAASMDT", "ECSTDAT"), names(df))[1] %||% NA_character_

    # visit_label_col
    visit_label_col <- intersect(c("VISIT", "EVENT"), names(df))[1] %||% NA_character_

    list(
      dataset = nm,
      subject_var = subject_var,
      site_var = site_var,
      visit_date_col = visit_date_col,
      visit_label_col = visit_label_col,
      df = df
    )
  }) %>%
    {
      info <- bind_rows(lapply(., \(x) tibble(
        dataset = x$dataset,
        subject_var = x$subject_var,
        site_var = x$site_var,
        visit_date_col = x$visit_date_col,
        visit_label_col = x$visit_label_col
      )))
      pool <- setNames(lapply(., `[[`, "df"), map_chr(., "dataset"))
      list(visit_info_df = info, data_list = pool)
    }
}


#' Disposition Dataset Check
#'
#' This function performs two checks:
#' 1. For subjects having both ED and V801, ED date is on or after V801.
#' 2. Regulatory visits (V1–V10) occurring after ED across all datasets (except diary data).
#'
#' @param ds Disposition dataset name
#' @param dataset_no A character vector of dataset names to exclude (e.g., diary data).
#' @param visit_info_df A data frame describing dataset, subject, site, visit/date column info. Please set
#' @param output_tab The name of the worksheet to create in the Excel output.
#'
#' @return The workbook `wb` with a new sheet added if issues are found.
#' @export
#'
#' @import dplyr purrr
#'

check_ds <- function(ds, formeid,
                     dataset_no = NULL,
                     visit_info_df = NULL) {

  ## Screening discontinuation
  lv4 <- ds6001 %>%
    filter(FORMEID == "DS6001_LVxx") %>%
    transmute(SITENUM, SUBJID,
              scrn_disc_dat  = DSSTDAT2,
              scrn_disc_reas = DSDECOD_13)

  ## Treatment discontinuation
  lv5 <- ds6001 %>%
    filter(FORMEID == "DS6001_LV5") %>%
    transmute(SITENUM, SUBJID,
              trt_disc_dat  = DSSTDAT3,
              trt_disc_reas = DSDECOD_7)

  ## Treatment phase discontinuation
  lv6 <- ds6001 %>%
    filter(FORMEID == "DS6001_LV6") %>%
    transmute(SITENUM, SUBJID,
              trtph_disc_dat  = DSSTDAT6,
              trtph_disc_reas = DSDECOD_9)

  ## Follow-up discontinuation
  lv7 <- ds6001 %>%
    filter(FORMEID == "DS6001_LV7") %>%
    transmute(SITENUM, SUBJID,
              fup_disc_dat  = DSSTDAT9,
              fup_disc_reas = DSDECOD_15)

  ## Overall study discontinuation
  lv12 <- ds6001 %>%
    filter(FORMEID == "DS6001_LV12") %>%
    transmute(SITENUM, SUBJID,
              overall_disc_dat = DSSTDAT)


  ds_summary <- list(lv4, lv5, lv6, lv7, lv12) %>%
    reduce(full_join, by = c("SITENUM", "SUBJID"))

  return(ds_summary)

}



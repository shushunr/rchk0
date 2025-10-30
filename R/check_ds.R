#' Disposition Dataset Check
#'
#' This function checks the ds6001 dataset (for out-sourced study)
#'
#' @param ds Disposition dataset name
#' @param output_tab The name of the worksheet to create in the Excel output.
#'
#' @return A dataframe that specified each subject's disposition information.
#' @export
#'
#' @import dplyr purrr
#'

check_ds <- function(ds = ds6001, formeid = FORMEID,
                     scr_dt = DSSTDAT2, scr_reas = DSDECOD_13,
                     trt_dt = DSSTDAT3, trt_reas = DSDECOD_7,
                     trtp_dt = DSSTDAT6, trtp_reas = DSDECOD_9,
                     fup_dt = DSSTDAT9, fup_reas = DSDECOD_15,
                     visit_info_df = visit_info_df) {

  ## Screening discontinuation
  lv4 <- ds6001 %>%
    filter(FORMEID == "DS6001_LV4") %>%
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


  ds_summary <- list(lv4, lv5, lv6, lv7) %>%
    reduce(full_join, by = c("SITENUM", "SUBJID"))

  return(ds_summary)

}



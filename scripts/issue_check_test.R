
rm(list = ls())
library(dplyr)
library(openxlsx)
library(haven)
library(stringr)
library(purrr)
library(tidyr)

library(rchk0)

##### Example study: DSAF #####

# --- Important: Change all the directories ---
# setwd("~/lillyce/qa/ly3041658/i7p_mc_dsaf/intrm2/programs/component_modules/data issue/") ## set working directory
rm(list = ls())
MODULE_PATH <- "/lillyce/qa/ly3041658/i7p_mc_dsaf/intrm2/programs/component_modules/data issue/" ## data issue parent folder
SPEC_PATH <- paste0(MODULE_PATH, "documentation/DSAF_raw_data_check_issue_spec.xlsx") ## issue spec location
DATA_PATH <- "/lillyce/qa/ly3041658/i7p_mc_dsaf/prelock/data/raw/shared/" # where raw data have been located
STUDY_NAME <- "I7P-MC-DSAF" ## Please enter your full study name
MASTER_PATH  <- paste0(MODULE_PATH, "output/DSAF_Stats_issues_log_cumulative.xlsx") ## final excel output file

run_tracker(DATA_PATH, SPEC_PATH, MASTER_PATH, study_name = "I7P-MC-DSAF",
            exclude_prefixes = c("cdms", "sys"),
            exclude_names = c("record_count", "rtsm_sbjct_data", "sd", "mhpresp1001"))


##### Example study: KIAC #####

# --- Important: Change all the directories ---
rm(list = ls())
MODULE_PATH <- "/lillyce/qa/ly3541860/j3k_mc_kiac/intrm1/programs/component_modules/data issue/" ## data issue parent folder
SPEC_PATH <- paste0(MODULE_PATH, "documentation/KIAC_raw_data_check_issue_spec.xlsx") ## issue spec location
DATA_PATH <- "/lillyce/qa/ly3541860/j3k_mc_kiac/prelock/data/raw/shared/" # where raw data have been located
STUDY_NAME <- "J3K-MC-KIAC" ## Please enter your full study name
MASTER_PATH  <- paste0(MODULE_PATH, "output/KIAC_Stats_issues_log_cumulative.xlsx") ## final excel output file
# 
# ##### Example study: DSAG #####
# 
# # --- Important: Change all the directories ---
rm(list = ls())
MODULE_PATH <- "/lillyce/qa/ly3041658/i7p_mc_dsag/prelock/programs/data issue/DSAG/" ## data issue parent folder
SPEC_PATH <- paste0(MODULE_PATH, "documentation/DSAG_raw_data_check_issue_spec.xlsx") ## issue spec location
DATA_PATH <- "/lillyce/qa/ly3041658/i7p_mc_dsag/prelock/data/raw/shared/" # where raw data have been located
STUDY_NAME <- "I7P-MC-DSAG" ## Please enter your full study name
MASTER_PATH  <- paste0(MODULE_PATH, "output/DSAG_Stats_issues_log_cumulative.xlsx") ## final excel output file
# 
# ##### Using FVAA raw data as an example #####
# # --- Important: Change all the directories ---
# rm(list = ls())
# MODULE_PATH <- "/lillyce/qa/ly3972406/j4h_mc_fvaa/final/programs/component_modules/data issue/" ## data issue parent folder
# SPEC_PATH <- paste0(MODULE_PATH, "FVAA_raw_data_check_issue_spec.xlsx") ## issue spec location
# DATA_PATH <- "/lillyce/qa/ly3972406/j4h_mc_fvaa/prelock/data/raw/shared/" # where raw data have been located
# STUDY_NAME <- "J4H-MC-FVAA" ## Please enter your full study name
# # MASTER_PATH  <- paste0(MODULE_PATH, "output/KIAC_Stats_issues_log_cumulative.xlsx") ## final excel output file


# --- Confirm: All the directories have been changed ---

# extra_cols_list <- list(
#   ae3001 = c("EVENT", "FORMEID", "FORMILB", "PT"),
#   cm1001 = c("EVENT", "CMTRT", "CMSTDAT", "PREFLABEL"),
#   cm2001 = c("EVENT", "CMTRT", "CMSTDAT", "EVENTDT"),
#   ds6001 = c("EVENT", "EVENTDT", "FORMEID"),
#   ex1001 = c("EVENT", "EVENTDT"),
#   i7pmcdsaf_cssrssla = c("CSSASMDAT", "VISIT", "VISITNUM"),
#   i7pmcdsaf_dlqi = c("DLQIASMDT", "QSCAT", "QSEVLINT", "VISIT", "VISITNUM"),
#   i7pmcdsaf_hads = c("HADSASMDAT", "QSCAT", "QSEVLINT", "VISIT", "VISITNUM"),
#   i7pmcdsaf_hisqol = c("HISQOLDAT", "QSCAT", "QSEVLINT", "VISIT", "VISITNUM"),
#   i7pmcdsaf_hsnrs = c("HSNRSDAT", "QSCAT", "QSEVLINT", "VISIT", "VISITNUM",
#                       "HSNRSRES4", "HSNRSRES3", "HSNRSRES1", "HSNRSRES2"),
#   i7pmcdsaf_itchnrs = c("INRSASMDT", "INRSASMTIM", "VISIT", "VISITNUM", "INRSRES1"),
#   i7pmcdsaf_lesion = c("DLADAT", "VISIT", "VISITNUM", "DLALOC", "DLALAT",
#                        "DLADIR", "DLARES1", "DLARES2", "DLARES3", "DLARES4", "DLARES5"),
#   i7pmcdsaf_pgic = c("PGICDAT", "VISIT", "VISITNUM"),
#   i7pmcdsaf_pgis = c("PGISASMDAT", "VISIT", "VISITNUM"),
#   i7pmcdsaf_promis7a = c("PROMISFA7DAT", "QSEVLINT", "VISIT", "VISITNUM"),
#   i7pmcdsaf_promis7b = c("PROMISSDSFBDAT", "PROMISSDSFBTIM","QSEVLINT", "VISIT",
#                          "VISITNUM"),
#   lili7pmcdsaf_pkhead_p = c("VISIT", "VISITNUM", "LBDTM", "PTMELTX"),
#   pr1001 = c("EVENT", "EVENTDT", "FORMDEF"),
#   sv1001 = c("EVENT", "EVENTDT", "VISITOCCUR")
# )

##### Read raw data #####
data_list <- load_dataset(DATA_PATH, ## raw data path
                          pattern = "\\.sas7bdat$",
                          exclude_prefixes = c("sys", "cdms"),  ## exclude datasets that starting with sys, cdms
                          exclude_names = c("record_count", "rtsm_sbjct_data", "sd", 
                                            "mhpresp1001")) ## exclude certain datasets

##### Preprocess the data based on spec #####
data_info0 <- preprocess(data_list, SPEC_PATH, export_to_env = TRUE) ## preprocessed dataset should be in the environment at this stage


## output dataset info and preprocessed dataset
visit_info_df <- data_info0$visit_info_df ## Please check this visit info
datasets_pool <- data_info0$datasets_pool ## preprocessed datasets as a list



# -----------------------------------------------------

##### Create workbook and start data checking #####
wb <- createWorkbook()

## test multi site function
multi_site <- multi_site(datasets_pool, wb, output_tab = "multi-site", visit_info_df = visit_info_df)

## test check_sv_not_in_ds6001
sv_not_in_ds6001 <- sv_ds(datasets_pool, wb, sv_visit = c(1:28, 801, 802), 
                          visit_info_df = visit_info_df, output_tab = "sv_ds",
                          sv_pattern = "Visit x", ds_pattern = "Visit x")

## test checking_missing_key_vars function
# Note: this function is able to check any column that has missing values
missing_visitnum <- check_missing_key_vars(datasets_pool, wb, target_vars = c("EVENT", "EVENTDEF", "EVENTEID", "VISIT", "VISITNUM"),
                       dataset_no = NULL, visit_info_df = visit_info_df)


custom_code_list = list(
  
  lab = function(df, visit_date_col) {
    df[[visit_date_col]] <- as.Date(substr(df[[visit_date_col]], 1, 10))
    
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col), TSTDTM, LBSPEC, LBTESTCD) %>%
      mutate(has_retest = any(VISIT == "Retest")) %>%
      filter((n_distinct(VISIT) > 1 | n() > 1) & !(VISIT == "Retest" & has_retest)) %>%
      select(-has_retest) %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col), LBTESTCD) %>%
      filter(n() > 1) %>%
      ungroup()
    
  },
  
  i7pmcdsaf_lesion = function(df, visit_date_col) {
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col), DLALOC, DLALAT, DLADIR, DLARES1) %>%
      filter(n() > 1) %>%
      ungroup()
  },
  
  ds2001 = function(df, visit_date_col) {
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col), FORMEID) %>%
      filter(n() > 1) %>%
      ungroup()
  },
  
  sys_form = function(df, visit_date_col) {
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col), FORMEID) %>%
      filter(n() > 1) %>%
      ungroup()
  },
  
  ds6001 = function(df, visit_date_col) {
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col), FORMEID) %>%
      filter(n() > 1) %>%
      ungroup()
  },
  
  cm2001 = function(df, visit_date_col) {
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col), FORMEID, CMYN, CMTRT, CMSTDAT, IGSEQ) %>%
      filter(n() > 1) %>%
      ungroup()
  },
  
  i7pmcdsaf_itchnrs = function(df, visit_date_col) {
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col), INRSASMTIM) %>%
      filter(n() > 1 & VISIT == "DAILY DIARY") %>%
      ungroup()
  },
  
  mhpresp1001 = function(df, visit_date_col) {
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col), FORMEID, HLT, PT, LLT) %>%
      filter(n() > 1) %>%
      ungroup()
  },
  
  pr1001 = function(df, visit_date_col) {
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col), FORMEID, PRLOC_PR1001_F1_D, LOCATION_SIDE_D, PRDIR_D) %>%
      filter(n() > 1) %>%
      mutate(
        f1_all_not_na = all(!is.na(PRSTDAT_PR1001_F1)),
        f1_has_duplicates = length(unique(PRSTDAT_PR1001_F1)) < n(),
        f2_all_not_na = all(!is.na(PRSTDAT_PR1001_F2)),
        f2_has_duplicates = length(unique(PRSTDAT_PR1001_F2)) < n()
      ) %>%
      filter(
        case_when(
          f1_all_not_na ~ f1_has_duplicates,
          !f1_all_not_na & f2_all_not_na ~ f2_has_duplicates,
          TRUE ~ TRUE
        )
      ) %>%
      select(-f1_all_not_na, -f1_has_duplicates, -f2_all_not_na, -f2_has_duplicates) %>%
      ungroup()
  },
  
  sv1001 = function(df, visit_date_col) {
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col)) %>%
      filter(!(VISITOCCUR == "N")) %>%
      filter(n() > 1) %>%
      ungroup()
  },
  
  vs1001 = function(df, visit_date_col) {
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col)) %>%
      filter(FORMILB == "N" & VSPERF == "Y") %>%
      filter(n() > 1) %>%
      ungroup()
  },
  
  i7pmcdsaf_hsnrs = function(df, visit_date_col) {
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col)) %>%
      filter(
        n() > 1 &
          (any(VISIT != "DAILY DIARY", na.rm = TRUE) |
             all(VISIT != "DAILY DIARY", na.rm = TRUE))
      ) %>%
      ungroup()
  },
  
  lb9001 = function(df, visit_date_col) {
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col)) %>%
      filter(!(EVENT == "Hepatic Event 1" & n() > 1)) %>%
      ungroup()
  },
  
  apmh1001 = function(df, visit_date_col) {
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col)) %>%
      filter(!(EVENT == "Hepatic Event 1" & n() > 1)) %>%
      ungroup()
  },
  
  hmpr1001 = function(df, visit_date_col) {
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col)) %>%
      filter(!(EVENT == "Hepatic Event 1" & n() > 1)) %>%
      ungroup()
  }
)

## test check_duplicate_visit_date function
custom_code_list = NULL
duplicate_visit <- check_duplicate_visit_date(datasets_pool, wb,
                                              dataset_no = c("mh7001", "sys_evt", "sys_form"),
                                              visit_info_df = visit_info_df,
                                              custom_code_list = custom_code_list)


##### check_ds function #####
ds_check <- check_ds(ds = ds6001, formeid = FORMEID,
                     scr_dt = DSSTDAT2, scr_reas = DSDECOD_13,
                     trt_dt = DSSTDAT3, trt_reas = DSDECOD_7,
                     trtp_dt = DSSTDAT6, trtp_reas = DSDECOD_9,
                     fup_dt = DSSTDAT9, fup_reas = DSDECOD_15,
                     visit_info_df = visit_info_df)

## what if used for KIAC study?
ds_check <- check_ds(ds = ds6001, formeid = FORMEID,
                     scr_dt = DSSTDAT, scr_reas = DSDECOD,
                     trt_dt = DSSTDAT, trt_reas = DSDECOD,
                     trtp_dt = DSSTDAT, trtp_reas = DSDECOD,
                     fup_dt = DSSTDAT, fup_reas = DSDECOD,
                     visit_info_df = visit_info_df)




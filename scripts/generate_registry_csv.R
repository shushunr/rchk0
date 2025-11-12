# --- Developer script to generate checkpoints_registry.csv ---
library(readr)
library(digest)
library(jsonlite)
library(tibble)

registry <- tibble(
  checkpoint_name = c(
    "check_missing_key_vars",
    "check_duplicate_visit_date",
    "check_visit_after_ED"
  ),
  function_name = c(
    "compute_missing_key_vars",
    "compute_duplicate_visit_date",
    "compute_visit_after_ed"
  ),
  function_version = "0.1.0",
  datasets_required = c("ANY", "ANY", "ds6001,ANY"),
  required_columns = c(
    "SUBJECT_ID,SITEID,VISITNUM",
    "SUBJECT_ID,SITEID,EVENTDT,EVENT",
    "SUBJECT_ID,DSSTDAT"
  ),
  parameters_schema = c(
    toJSON(list(target_vars = list(type="character", default=c("VISIT","VISITNUM","EVENT"))), auto_unbox=TRUE),
    toJSON(list(visit_date_col=list(type="character", default="EVENTDT"),
                visit_label_col=list(type="character", default="EVENT")), auto_unbox=TRUE),
    toJSON(list(ed_forms=list(type="character", default=c("DS6001_LV6","DS6001_LV5"))), auto_unbox=TRUE)
  ),
  parameters_json = c(
    toJSON(list(target_vars=c("VISIT","VISITNUM","EVENT")), auto_unbox=TRUE),
    toJSON(list(visit_date_col="EVENTDT", visit_label_col="EVENT"), auto_unbox=TRUE),
    toJSON(list(ed_forms=c("DS6001_LV6","DS6001_LV5")), auto_unbox=TRUE)
  ),
  severity = c("Warning", "Warning", "Critical"),
  logic_digest = c(
    "Flag rows where any target key is NA after empty string",
    "Find duplicate visits with same date, excluding unscheduled",
    "Flag visits occurring after ED date (exclude 801/802/803/997/999)"
  ),
  logic_hash = sapply(c(
    "Flag rows where any target key is NA after empty string",
    "Find duplicate visits with same date, excluding unscheduled",
    "Flag visits occurring after ED date (exclude 801/802/803/997/999)"
  ), digest, algo = "md5"),
  owner = "shushunr",
  last_updated = as.character(Sys.Date()),
  enabled_default = TRUE
)

# ensure directory exists
dir.create("inst/registry", recursive = TRUE, showWarnings = FALSE)

# write csv
write_csv(registry, "inst/registry/checkpoints_registry.csv")
message("✅ checkpoints_registry.csv generated successfully.")

# Disposition Dataset Check

This function checks the ds6001 dataset (for out-sourced study)

## Usage

``` r
check_ds(
  ds = ds6001,
  formeid = FORMEID,
  scr_dt = DSSTDAT2,
  scr_reas = DSDECOD_13,
  trt_dt = DSSTDAT3,
  trt_reas = DSDECOD_7,
  trtp_dt = DSSTDAT6,
  trtp_reas = DSDECOD_9,
  fup_dt = DSSTDAT9,
  fup_reas = DSDECOD_15,
  visit_info_df = visit_info_df
)
```

## Arguments

- ds:

  Disposition dataset name

- output_tab:

  The name of the worksheet to create in the Excel output.

## Value

A dataframe that specified each subject's disposition information.

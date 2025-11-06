# Check for visits after ED

Check for visits after ED

## Usage

``` r
get_ed_dates(
  ed_ds,
  id_col = "SUBJECT_ID",
  date_col = "DSSTDAT",
  form_col = "FORMEID",
  forms = c("DS6001_LV6", "DS6001_LV5")
)
```

## Arguments

- datasets_pool:

  Named list of datasets

- wb:

  Workbook

- dataset_no:

  Excluded datasets

- visit_info_df:

  Visit info metadata

- output_tab:

  Output tab

- other_datasets:

  Character vector of datasets to check

## Value

A named list, each element is a data frame of records with visits after
ED for the corresponding dataset. If no datasets have violations,
returns an empty list.

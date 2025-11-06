# Check for overlapped visits

Check for overlapped visits

## Usage

``` r
compute_visit_overlap(
  df,
  ds,
  visit_date_col,
  visit_label_col,
  id_cols = c("SITEID", "SUBJECT_ID")
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

## Value

A named list, each element is a data frame of records with overlapped
visits for the corresponding dataset. If no datasets have overlaps,
returns an empty list.

# Check consistency between ED and V801 visits

This function performs two checks:

1.  For subjects having both ED and V801, ED date is on or after V801.

2.  Regulatory visits (V1–V10) occurring after ED across all datasets
    (except diary data).

## Usage

``` r
check_ed_v801(
  datasets_pool,
  wb,
  dataset_no = NULL,
  visit_info_df = NULL,
  output_tab = NULL
)
```

## Arguments

- datasets_pool:

  A named list of datasets (data frames).

- wb:

  An openxlsx workbook object to which results will be written.

- dataset_no:

  A character vector of dataset names to exclude (e.g., diary data).

- visit_info_df:

  A data frame describing dataset, subject, site, visit/date column
  info.

- output_tab:

  The name of the worksheet to create in the Excel output.

## Value

The workbook `wb` with a new sheet added if issues are found.

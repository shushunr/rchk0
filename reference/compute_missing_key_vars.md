# Check for missing key variables

Check for missing key variables

## Usage

``` r
compute_missing_key_vars(df, ds, keys)
```

## Arguments

- datasets_pool:

  Named list of datasets

- wb:

  An openxlsx Workbook

- target_vars:

  Key variables to check for some/all datasets

- dataset_no:

  Names of datasets to exclude

- visit_info_df:

  Visit info metadata

## Value

Writes results into workbook, invisibly returns NULL

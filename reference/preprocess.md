# Preprocess raw datasets based on spec

This function standardizes subject and site identifiers, applies
preprocessing rules defined in the spec, and captures visit-related
metadata for each dataset in the pool. The main steps include:

1.  Identify subject and site variables and create unified columns
    `SUBJECT_ID` and `SITEID`.

2.  Apply row- and column-level filtering rules from the spec (sheet
    "PREPROCESSING").

3.  Detect visit-related columns (visit date and visit label).

4.  Return updated datasets and a summary data frame of metadata.

## Usage

``` r
preprocess(datasets_pool, spec_path, export_to_env = FALSE)
```

## Arguments

- datasets_pool:

  A named list of raw datasets, typically the output of
  [`load_dataset()`](load_dataset.md).

- spec_path:

  File path to the preprocessing spec (Excel, with sheet
  "PREPROCESSING").

## Value

A list with two elements:

- `visit_info_df`:

  A data frame with one row per dataset, containing subject/site
  variable names and detected visit columns.

- `datasets_pool`:

  The updated datasets after preprocessing.

## Examples

``` r
if (FALSE) { # \dontrun{
  res <- preprocess(datasets_pool, "spec.xlsx")
  res$visit_info_df
  head(res$datasets_pool$ds6001)
} # }
```

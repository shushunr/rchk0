# Check SV1001 visits not present in DS6001

This function checks for regulatory visits (1–28, 801, 802) recorded in
`sv1001` but not in `ds6001`.

## Usage

``` r
sv_ds(
  datasets_pool,
  wb,
  other_datasets = NULL,
  output_tab = NULL,
  visit_info_df = NULL,
  sv_visit = c(1:28, 801, 802),
  sv_pattern = NULL,
  ds_pattern = NULL
)
```

## Arguments

- datasets_pool:

  Named list of raw datasets

- wb:

  An openxlsx Workbook object

- other_datasets:

  Not used (kept for compatibility)

- output_tab:

  Name of Excel worksheet to create

- visit_info_df:

  Metadata with dataset, visit_label_col, visit_date_col

- sv_visit:

  Numeric vector of VISIT numbers to check (default:
  `c(1:28, 801, 802)`)

- sv_pattern:

  Character vector of sv visit patterns, where "x" is a placeholder for
  digits, e.g., "evVx", "Visit x"

- ds_pattern:

  Character vector of ds visit patterns, where "x" is a placeholder for
  digits, e.g., "evVx", "Visit x"

## Value

Writes a worksheet with detected issues into `wb`

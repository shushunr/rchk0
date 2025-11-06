# Check visit discrepancies across EX/EC, SV, and TRTDSPN

This function compares visits across `ec1001` (or EX), `sv1001`, and
`trtdspn` datasets by VISID, to identify:

1.  Missing visit records in one or more datasets

2.  Visit date mismatches between TRTDSPN and EX/SV

## Usage

``` r
check_visit_discrepancy(
  datasets_pool,
  wb,
  seq = NULL,
  output_tab = NULL,
  visit_info_df = NULL,
  visit_excl = c(1, 28, 801, 802, 803)
)
```

## Arguments

- datasets_pool:

  Named list of raw datasets

- wb:

  An openxlsx Workbook object

- seq:

  Integer. If 1, run missing visits check; if 2, run date mismatch
  check.

- output_tab:

  Name of Excel worksheet to create

- visit_info_df:

  Metadata with dataset, visit_label_col, visit_date_col

- visit_excl:

  Numeric vector of VISIDs to exclude (default: `c(1,28,801,802,803)`)

## Value

Writes a worksheet with detected issues into `wb`

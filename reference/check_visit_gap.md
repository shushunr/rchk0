# Check visit gaps between consecutive visits

This function checks that consecutive scheduled visits are more than
`day_lob` days (default: 7 days / 1 week) and less than `day_hib` days
(default: 21 days / 3 weeks) apart.

## Usage

``` r
check_visit_gap(
  datasets_pool,
  wb,
  visit_info_df = NULL,
  output_tab = NULL,
  day_lob = 7,
  day_hib = 21
)
```

## Arguments

- datasets_pool:

  Named list of raw datasets

- wb:

  An openxlsx Workbook object

- visit_info_df:

  Metadata with dataset, visit_label_col, visit_date_col

- output_tab:

  Name of Excel worksheet to create

- day_lob:

  Minimum day gap (default: 7)

- day_hib:

  Maximum day gap (default: 21)

## Value

Writes a worksheet with detected issues into `wb`

## Details

Visits such as 1, 801, 802, 803 are excluded from the check.

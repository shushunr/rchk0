# Check ED visit consistency across datasets

Patients having ED visit in DS6001, but no ED visits in lab or eCOA
datasets. Compares ED dates across ds6001 and other datasets, and
reports missing or inconsistent values.

## Usage

``` r
check_ED(
  datasets_pool,
  wb,
  other_datasets = NULL,
  output_tab = NULL,
  visit_info_df = NULL
)
```

## Arguments

- datasets_pool:

  Named list of raw datasets

- wb:

  An openxlsx Workbook object

- other_datasets:

  Character vector of datasets to check (e.g., c("lab", "ecoa"))

- output_tab:

  Name of Excel worksheet to create

- visit_info_df:

  Metadata with dataset, visit_label_col, visit_date_col

## Value

Writes a worksheet with detected issues into `wb`

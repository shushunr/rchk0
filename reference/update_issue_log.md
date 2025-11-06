# Update issue log by merging new and old Excel sheets

This function updates the issue log workbook by comparing new issues
detected in the current run with historical issues recorded in a master
Excel file.

## Usage

``` r
update_issue_log(wb, master_path)
```

## Arguments

- wb:

  An openxlsx Workbook object containing the newly generated issue
  sheets

- master_path:

  Path to the master Excel file (historical issue log)

## Value

The updated workbook object

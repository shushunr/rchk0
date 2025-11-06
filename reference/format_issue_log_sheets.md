# Format all issue log sheets in a workbook

This function applies consistent formatting (styles, column widths,
filters, status highlighting, and tab ordering) to all sheets in an
issue log workbook.

## Usage

``` r
format_issue_log_sheets(wb, extra_cols_list)
```

## Arguments

- wb:

  An openxlsx Workbook object to be formatted.

- extra_cols_list:

  A list including column names retained in the output excel file for
  each sheet

## Value

The formatted workbook object (in-place).

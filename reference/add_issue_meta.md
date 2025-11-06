# Add issue metadata

Add issue metadata

## Usage

``` r
add_issue_meta(
  df,
  issue,
  type = c("raw", "summary"),
  dataset = NULL,
  id_cols = c("SITEID", "SUBJECT_ID"),
  status = "New",
  first = Sys.Date(),
  last = Sys.Date()
)
```

## Arguments

- df_summary:

  A summary data.frame

- df_raw:

  A raw data.frame

- idx:

  Optional index

- label:

  Optional label

# Convert Date/POSIX columns in a data.frame to character

Convert Date/POSIX columns in a data.frame to character

## Usage

``` r
convert_dates_to_char(
  df,
  date_format = "%Y-%m-%d",
  dttm_format = "%Y-%m-%d %H:%M:%S"
)
```

## Arguments

- df:

  A data.frame

- date_format:

  Format for Date columns

- dttm_format:

  Format for POSIXct/POSIXlt columns

## Value

data.frame with converted columns

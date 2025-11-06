# Load raw datasets into a named list, and also assign each to environment

Load raw datasets into a named list, and also assign each to environment

## Usage

``` r
load_dataset(
  path,
  pattern = "\\.sas7bdat$",
  exclude_prefixes = c("sys", "cdms"),
  exclude_names = c("record_count", "rtsm_sbjct_data"),
  verbose = TRUE
)
```

## Arguments

- path:

  Directory to search

- pattern:

  Regex for files (default .sas7bdat)

- exclude_prefixes:

  Character vector of prefixes to drop, e.g., c("sys", "cdms")

- exclude_names:

  Character vector of dataset names to drop (without extension)

- envir:

  Environment to assign datasets into (default: .GlobalEnv)

## Value

Named list of data.frames/tibbles

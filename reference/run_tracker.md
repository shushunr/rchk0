# Run Raw Issue Tracker at one time

Run Raw Issue Tracker at one time

## Usage

``` r
run_tracker(
  data_path,
  spec_path,
  out_xlsx,
  study_name = NULL,
  exclude_prefixes = c("cdms"),
  exclude_names = c("record_count", "rtsm_sbjct_data", "sd", "mhpresp1001")
)
```

## Arguments

- data_path:

  directory of raw datasets

- spec_path:

  xlsx spec path

- out_xlsx:

  output Excel path

- study_name:

  optional study label

- exclude_prefixes:

  prefixes to drop

- exclude_names:

  exact basenames to drop

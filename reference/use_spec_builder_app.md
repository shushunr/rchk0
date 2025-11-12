# Launch the Spec Builder App

This Shiny app allows Study Leads to:

- Load raw datasets

- Review and edit dataset metadata (visit_info_df)

- Configure checkpoints and parameters

- Generate standardized spec.xlsx for issue tracker

## Usage

``` r
use_spec_builder_app(data_path = NULL, spec_path = NULL, registry_path = NULL)
```

## Arguments

- data_path:

  Path to the folder containing raw data (.sas7bdat)

- spec_path:

  Optional path to existing spec (for loading old parameters)

- registry_path:

  Path to checkpoints registry CSV

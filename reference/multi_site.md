# Check subjects occurred in multiple sites

This function identifies subjects who appear in more than one site
across datasets.

## Usage

``` r
multi_site(datasets_pool, wb, output_tab = NULL, visit_info_df = NULL)
```

## Arguments

- datasets_pool:

  Named list of raw datasets

- wb:

  An openxlsx Workbook object

- output_tab:

  Name of Excel worksheet to create

- visit_info_df:

  Metadata with dataset, subject_var, site_var

## Value

A data frame with the following key columns:

- SUBJECT_ID:

  Subject Identifier

- n_sites:

  Number of sites that this subject occurred in

- site_dataset_map:

  Site-dataset pairs of subjects that occurred in multiple sites

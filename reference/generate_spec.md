# Generate spec workbook (and optional YAML) for raw-data checks

Generate spec workbook (and optional YAML) for raw-data checks

## Usage

``` r
generate_spec(config, path, include_yaml = FALSE, yaml_path = NULL)
```

## Arguments

- config:

  list. Required fields: study_id, generated_by. Optional fields:
  visit_info_df (data.frame), checkpoints_enabled (data.frame),
  datasets_pool (list).

- path:

  character. Output .xlsx path.

- include_yaml:

  logical. If TRUE, also write a YAML spec next to the Excel.

- yaml_path:

  character. Optional YAML path. If NULL and include_yaml=TRUE,
  auto-derive from `path`.

## Value

(invisible) path (xlsx path)

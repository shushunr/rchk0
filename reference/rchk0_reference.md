# rchk0_reference

Curated list of exported functions, grouped by topic.

## Common visit checks (applied to all datasets)

- `check_missing_key_vars()` - Check observations that having missing
  visit numbers (e.g., VISITNUM)

- `check_duplicate_visit_date()` - Check duplicate visits on the same
  date (e.g., Visit 14 and Visit 15 on the same date 2025-07-21)

- `check_visit_after_ED()` — Visits after an ED date

- `check_visit_overlap()` — detect overlapped visits

- `check_multi_site()` — Check subjects occurred in multiple sites

## Export & reporting

- `export_excel()` — write formatted issue logs

## Core workflow

- [`run_tracker()`](run_tracker.md) — run all standard checks and export

## Utilities

- [`compress_ranges()`](compress_ranges.md),
  [`convert_dates_to_char()`](convert_dates_to_char.md),
  [`issue_envelope()`](issue_envelope.md)

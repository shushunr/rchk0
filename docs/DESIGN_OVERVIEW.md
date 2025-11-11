# Raw Data Issue Tracker — Design Overview

-   **Author**：Shushun Ren
-   **Last Updated**：2025-11-06
-   **Status**：Draft

## 1. System Architecture

---
Raw Data Transfer (SAS7BDAT)
        ↓
Program 1: Spec Generator
        ↓
spec.xlsx (read-only)
        ↓
Program 2: Issue Tracker
        ↓
issue_log.xlsx / SQLite DB
        ↓
Dashboard (Shiny / HTML report)
---

## 2. Core Modules

| Module | Description |
|------------------------------------|------------------------------------|
| Program 1: Spec Generator | Reads raw data and metadata to create a standardized Excel spec (`spec.xlsx`) |
| Program 2: Issue Tracker | Executes QC checkpoints based on the spec and produces outputs with issue persistence tracking. |
| Shiny Config App | Builds study_config.yml (Study ID, dataset mapping, params) |
| Registry | Catalog of checkpoints (name/version/defaults/param schema/logic_digest) |
| State Store | SQLite/DuckDB for issues history & longitudinal tracking |

## 3. Data Flow (End-to-End)

| Step | Input |
|------|-------|

## 4. Spec Structure

| Sheet | Description | Editable |
|------------------------|------------------------|-------------------------|
| README | Meta：generated_at/by, engine_version, spec_schema_version | No |
| Raw Snapshot | Per-dataset：rows/cols/load status/dates/schema hash | No |
| Data Info | Mapping：Subject ID / Site ID / Visit Date / Visit Label | Yes（via Shiny） |
| Checkpoints | `checkpoint_name, function_name, function_version, datasets_required, required_columns, parameters_json, enabled, logic_digest` | Parameters only（via Shiny） |
| Errors/Notes | Validation / warnings（STAMP msg） | No |

## 5. Issue Tracking Logic

### Identifiers

-   `instance_id` = hash(checkpoint_name, function_version, dataset, siteid, subject_id, stabilizer, …)
-   `issue_group_id` = hash(checkpoint_name, siteid, subject_id, stabilizer)
-   `logic_digest` = hash(function body) for drift detection

### Issue Status Updates (For automatic checks)

Note: Issue will be matched based on unique identifiers.

| Old status of the issue | New status of the issue | Indication | Updated Status |
|------------------|------------------|------------------|------------------|
| Contains "permanent" | ANY | Old issue was determined as a permanent issue, **whether or not the issue still exists in the new data transfer**, user will add "permanent" to the status | Old status remained |
| Contains "non-issue" | ANY | Old issue was detected, but is not an actual issue (fake issue from automatic checks), user will add "non-issue" to the status | Old status remained |
| NA | New | Issue doesn't exist in old tracker, yet occurs in the new tracker | New |
| New/Open | New | Issue was found in the old tracker and not resolved previously, and still pops up in the new tracker | Open |
| New/Open | NA | Issue was found in the old tracker and not resolved, but not popping up in the new tracker | Closed |
| Closed | New | Issue was found in the old tracker and closed previously, yet pops up in the new tracker | Recurred |

## 6. Configuration Files

### study_config.yml (User-Defined via Shiny)

```{yaml}
study_id: "I7P_MC_DSAF"
raw_path: "/lillyce/qa/i7p_mc_dsaf/intrm2/"
generated_by: "shushunr"
dataset_info:
  adsl:
    subject_id: "USUBJID"
    site_id: "SITEID"
  sv1001:
    subject_id: "SUBJID"
    site_id: "SITE"
checkpoints_enabled:
  - check_missing_key_vars
  - check_duplicate_visit_date
parameters_overrides:
  check_missing_key_vars:
    target_vars: ["VISIT", "VISITNUM", "EVENT"]
```

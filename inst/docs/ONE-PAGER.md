# Raw Data Issue Tracker — High-level Design (One-pager)

-   **Author**：Shushun Ren
-   **Last Updated**：2025-11-06
-   **Status**：Draft

## 1. Background / Problem

Clinical raw datasets are frequently transferred (weekly/biweekly), and issues such as missing keys, visit inconsistencies, and post-ED records must be revalidated each time.

Manual checks and Excel-based tracking are:

-   Time-consuming and error-prone
-   Hard to synchronize across studies
-   Difficult to audit longitudinally

We need a modular, spec-driven, reproducible data issue tracking framework that minimizes manual maintenance while preserving transparency and traceability.

Even though DM had their issue detection tools (e.g., pinnacle 21), we still see considerable amounts of data issue across multiple transfers. This tool will be developed as a supplement to DM's issue detection tools, and will be applied to various studies (both out-sourced and in-house)

## 2. Goal

Two-program workflow:

| Program | Description | Output |
|----|----|----|
| Program 1 – Spec Generator | Read raw data –\> auto-generate study params –\> standardized spec | `spec.xlsx` |
| Program 2 – Issue Tracker | Read spec –\> run QC checkpoints –\> issue outputs & history | `issue_log.xlsx` |

**Key objectives**

-   Minimize manual edits across transfers
-   Centralize configuration (YAML + Shiny)
-   Versioning + persistence（`instance_id`, `issue_group_id`）
-   Pre-defined & user-defined checkpoints

# 3. Non-Goals

-   No manual data correction
-   No study-specific hard-coding
-   No strict dependency on CDISC flavors

# 4. Solution Overview

Workflow（Spec is a read-only snapshot，the parameters are derived from user entries from a Shiny app –\> `study_config.yml`）：

-   Program 1：Generate `spec.xlsx`（README / raw_snapshot / data_info / checkpoints / errors）
-   Program 2：Check issues based on spec, and generate issue summary and individual data Sheet

# 5. Technical Highlights

-   R functions modularization；Paramaterized YAML –\> Generate spec dymamically
-   Hash-based tracing：`instance_id` (strict)、`issue_group_id` (across multiple datasets)
-   Long-term tracing：New / Persisting / Resolved / Updated
-   Output Excel/HTML，or into repositories（SQLite / DuckDB）

# 6. Configuration Files

study_config.yml (User-Defined via Shiny)

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

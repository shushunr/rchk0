# Checkpoints Reference Guide

## Overview

This vignette documents all checkpoints implemented in the rchk0
package. Each checkpoint represents a self-contained data validation
rule applied to raw clinical datasets.

- Audience: Statistical Analysts, Data Managers, QC Programmers
- Purpose: Understand the purpose, parameters, and outputs of each
  check.
- Last Updated: 2025-11-19

## Checkpoints list

Below are detailed descriptions for each checkpoint.  
Each section includes: overview, parameters, logic, and example usage.

### CHK01 - Check missing key variables

**Function**: `check_missing_key_vars()`  
**Applicable Datasets**: ANY  
**Category**: Data Completeness

#### Description

Detects missing values in key variables.

#### Parameters

| Name          | Type      | Required | Description                                                                |
|---------------|-----------|----------|----------------------------------------------------------------------------|
| `target_vars` | character | √        | Variables to be checked across all the datasets (except for `dataset_no` ) |
| `dataset_no`  | character | √        | Datasets not to be checked                                                 |

#### Output

| Issue ID (Hash)                  | …   | Issue_noted_by_Lilly_Stats                        | …   |
|----------------------------------|-----|---------------------------------------------------|-----|
| 897ed905f28cb9aea188235a985f54da | …   | Missing values in \<var1\>, \<var2\>, …, \<varn\> | …   |

#### Notes

This checkpoint is for all-dataset issues. If there’s only a few
datasets that need to be checked, use customized code instead of this
function.

### CHK02 - Check duplicate visits within the same date

**Function**: `check_duplicate_visit_date()`  
**Applicable Datasets**: ANY  
**Category**: Data Duplicates

#### Description

Detects duplicate visits within the same date.

#### Parameters

| Name          | Type      | Required | Description                                                                |
|---------------|-----------|----------|----------------------------------------------------------------------------|
| `target_vars` | character | √        | Variables to be checked across all the datasets (except for `dataset_no` ) |
| `dataset_no`  | character | √        | Datasets not to be checked                                                 |

#### Output

| Issue ID (Hash)                  | …   | Issue_noted_by_Lilly_Stats                                                      | …   |
|----------------------------------|-----|---------------------------------------------------------------------------------|-----|
| 897ed905f28cb9aea188235a985f54da | …   | Duplicate records within the same date for \<visit_label_x\>, \<visit_label_y\> | …   |

#### Notes

This checkpoint needs a customized code list to find real duplicates.

### CHK03 - Check visits occurred after Early Discontinuation (ED)

**Function**: `check_visit_after_ED()`  
**Applicable Datasets**: Datasets that include regulatory visits  
**Category**: Data Deviation

#### Description

Detects regulatory visits occurred after **Early Discontinuation (Or
completion date)**.

#### Parameters

| Name         | Type      | Required | Description                |
|--------------|-----------|----------|----------------------------|
| `dataset_no` | character | √        | Datasets not to be checked |

#### Output

| Issue ID (Hash)                  | …   | Issue_noted_by_Lilly_Stats                         | …   |
|----------------------------------|-----|----------------------------------------------------|-----|
| 897ed905f28cb9aea188235a985f54da | …   | Visit occurred after ED/Completion date YYYY-MM-DD | …   |

#### Notes

In `DS6001`, DSSTDAT6 (might vary) is “Treatment phase completion /
discontinuation date”, which is the designated ED date. If the subject
proceeds to follow-up phase (DSCONT_PROFU = “Y”, might vary), DSSTDAT9
(“Follow-up discontinuation / completion date”, might vary) is the
designated ED date.

### CHK04 - Check visits overlap

**Function**: `check_visit_overlap()`  
**Applicable Datasets**: ANY  
**Category**: Data Deviation

#### Description

This check identifies subjects whose later visits have earlier dates,
indicating overlapping or out-of-order visit records.

#### Output

| Issue ID (Hash)                  | …   | Issue_noted_by_Lilly_Stats                                 | …   |
|----------------------------------|-----|------------------------------------------------------------|-----|
| 897ed905f28cb9aea188235a985f54da | …   | Overlapped visits for \<visit_label_x\>, \<visit_label_y\> | …   |

### CHK05 - Check subjects in multiple sites

**Function**: [`multi_site()`](../reference/multi_site.md)  
**Applicable Datasets**: ANY datasets with SITEID variable  
**Category**: Data Deviation

#### Description

This check identifies subjects that occurred in multiple sites.

#### Output

[TABLE]

#### Notes

Datasets to be checked must have subject ID and site ID columns. If any
of the two are missing, dataset will be skipped.

### CHK06 - Check SV1001 visits not present in DS6001

**Function**: [`sv_ds()`](../reference/sv_ds.md)  
**Applicable Datasets**: ANY datasets with SITEID variable  
**Category**: Data Deviation

#### Description

This check identifies subjects that occurred in multiple sites.

# test/testthat/test-check-multi-site.R

## 1. Subject appears in multiple sites
## 2. no multi-site subjects returns NULL
## 3. dataset skipped when subject/site mapping missing
## 4. worksheet is added to workbook when output_tab provided

test_that("subject appears in multiple sites is detected", {
  datasets_pool <- list(
    ds1 = data.frame(SUBJECT_ID = c("S1", "S2"), SITEID = c("100", "200")),
    ds2 = data.frame(SUBJECT_ID = c("S1", "S3"), SITEID = c("200", "300"))
  )
  
  visit_info_df <- data.frame(
    dataset = c("ds1", "ds2"),
    subject_var = "SUBJECT_ID",
    site_var = "SITEID",
    stringsAsFactors = FALSE
  )
  
  wb <- openxlsx::createWorkbook()
  issues <- check_multi_site(datasets_pool, wb, "multi", visit_info_df)
  
  expect_s3_class(issues, "data.frame")
  expect_true("SUBJECT_ID" %in% names(issues))
  expect_true("site_dataset_map" %in% names(issues))
  expect_true("S1" %in% issues$SUBJECT_ID)
  expect_true(any(grepl("Site 100", issues$site_dataset_map)))
  expect_true(any(grepl("Site 200", issues$site_dataset_map)))
})

test_that("no multi-site subjects returns NULL", {
  datasets_pool <- list(
    ds1 = data.frame(SUBJECT_ID = c("S1", "S2"), SITEID = c("100", "100"))
  )
  
  visit_info_df <- data.frame(
    dataset = "ds1",
    subject_var = "SUBJECT_ID",
    site_var = "SITEID",
    stringsAsFactors = FALSE
  )
  
  wb <- openxlsx::createWorkbook()
  issues <- check_multi_site(datasets_pool, wb, "multi", visit_info_df)
  
  expect_null(issues)
})

test_that("worksheet is added to workbook when output_tab provided", {
  datasets_pool <- list(
    ds1 = data.frame(SUBJECT_ID = c("S1", "S1"), SITEID = c("100", "200"))
  )
  
  visit_info_df <- data.frame(
    dataset = "ds1",
    subject_var = "SUBJECT_ID",
    site_var = "SITEID",
    stringsAsFactors = FALSE
  )
  
  wb <- openxlsx::createWorkbook()
  check_multi_site(datasets_pool, wb, "multi", visit_info_df)
  
  expect_true("multi" %in% names(wb$worksheets))
})



test_that("check_missing_key_vars detects missing values", {
  df1 <- data.frame(
    SUBJECT_ID = c("S1", "S2"),
    VISITNUM   = c(1, NA),     
    VISIT      = c("V1", ""), 
    stringsAsFactors = FALSE
  )
  datasets_pool <- list(ds1 = df1)
  wb <- openxlsx::createWorkbook()
  
  res <- check_missing_key_vars(datasets_pool, wb,
                                target_vars = c("VISITNUM", "VISIT"))
  
  expect_true("ds1" %in% names(res))
  expect_gt(nrow(res$ds1), 0)
  expect_true(all(c("Issue_type","Status") %in% names(res$ds1)))
  expect_true(any(is.na(res$ds1$VISITNUM) | res$ds1$VISIT == ""))
})

test_that("sv_ds works with evVx pattern", {
  sv1001 <- data.frame(
    SUBJECT_ID = c("S1","S2"),
    SITEID     = c("100","200"),
    VISIT      = c("evV2","evV5"),
    VISITOCCUR = c("Y","Y")
  )
  ds6001 <- data.frame(
    SUBJECT_ID = c("S1"),
    SITEID     = c("100"),
    VISIT      = c("evV2")
  )
  
  datasets_pool <- list(sv1001=sv1001, ds6001=ds6001)
  visit_info_df <- data.frame(dataset=c("sv1001","ds6001"),
                              subject_var="SUBJECT_ID",
                              site_var="SITEID",
                              visit_date_col=NA,
                              visit_label_col="VISIT")
  
  wb <- openxlsx::createWorkbook()
  
  res <- sv_ds(datasets_pool, wb,
               sv_visit = c(2,5),
               visit_info_df = visit_info_df,
               output_tab = "sv_ds",
               sv_pattern = "evVx",
               ds_pattern = "evVx")
  
  expect_true("S2" %in% res$SUBJECT_ID)  # evV5 only in SV
})

test_that("sv_ds works with Visit x pattern", {
  sv1001 <- data.frame(
    SUBJECT_ID = c("S3","S4"),
    SITEID     = c("300","400"),
    VISIT      = c("Visit 10","Visit 15"),
    VISITOCCUR = c("Y","Y")
  )
  ds6001 <- data.frame(
    SUBJECT_ID = c("S3"),
    SITEID     = c("300"),
    VISIT      = c("Visit 10")
  )
  
  datasets_pool <- list(sv1001=sv1001, ds6001=ds6001)
  visit_info_df <- data.frame(dataset=c("sv1001","ds6001"),
                              subject_var="SUBJECT_ID",
                              site_var="SITEID",
                              visit_date_col=NA,
                              visit_label_col="VISIT")
  
  wb <- openxlsx::createWorkbook()
  
  res <- sv_ds(datasets_pool, wb,
               sv_visit = c(10,15),
               visit_info_df = visit_info_df,
               output_tab = "sv_ds",
               sv_pattern = "Visit x",
               ds_pattern = "Visit x")
  
  expect_true("S4" %in% res$SUBJECT_ID)  # Visit 15 only in SV
})
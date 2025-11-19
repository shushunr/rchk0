install.packages("DBI")
install.packages("RSQLite")

library(DBI)
conn <- dbConnect(RSQLite::SQLite(), "issue_tracker.db")

dbExecute(conn, "
  CREATE TABLE IF NOT EXISTS issue_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    
    transfer_id   TEXT NOT NULL,
    study_id      TEXT,
    dataset       TEXT NOT NULL,
    subject_id    TEXT,
    site_id       TEXT,
    
    issue_type    TEXT NOT NULL,
    issue         TEXT NOT NULL,
    row_reference INTEGER,
    key_vars      TEXT,
    
    old_status    TEXT,
    new_status    TEXT NOT NULL,
    manual_comment TEXT,
    
    hash          TEXT NOT NULL,
    
    created_at    TEXT NOT NULL,
    updated_at    TEXT NOT NULL
  );
")

issue_df <- data.frame(
  transfer_id = "TRF20251112",
  dataset     = c("ds6001", "ds6001", "lab"),
  subject_id  = c("1001-001", "1002-003", "1003-002"),
  site_id     = c("1001", "1002", "1003"),
  issue_type  = c("status_vs_AE", "missing_visit", "dup_record"),
  issue       = c(
    "DSDECOD_7 was updated to 'Completed' but subject had an AE.",
    "VISITNUM missing for a record.",
    "Duplicate record detected with same TESTCD and date."
  ),
  row_reference = c(12, 48, 88),
  key_vars      = c("DSDECOD_7, AEFLAG", "VISITNUM", "LBTESTCD, LBDTC"),
  
  old_status  = c(NA, "Open", "Closed"),
  new_status  = c("New", "Open", "Recurred"),
  
  manual_comment = c("", "DM previously contacted site", ""),
  
  created_at = Sys.time(),
  updated_at = Sys.time(),
  
  hash = c("897ed905f28cb9aea188235a985f54da", "0827a5ccfcaa0155b4c13d98248791ed", "2dca42d57cead7cf2839e6806f52e5ec")
)


dbWriteTable(conn, "issue_log", issue_df, append = TRUE, row.names = FALSE)





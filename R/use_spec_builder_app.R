#' Launch the Spec Builder App
#'
#' This Shiny app allows Study Leads to:
#' - Load raw datasets
#' - Review and edit dataset metadata (visit_info_df)
#' - Configure checkpoints and parameters
#' - Generate standardized spec.xlsx for issue tracker
#'
#' @param data_path Path to the folder containing raw data (.sas7bdat)
#' @param spec_path Optional path to existing spec (for loading old parameters)
#' @param registry_path Path to checkpoints registry CSV
#' @export
use_spec_builder_app <- function(data_path = NULL, spec_path = NULL, registry_path = NULL) {
  
  # ---- source UI and server modules manually ----
  app_dir <- system.file("shiny/spec_builder_app", package = "rchk0")
  source(file.path(app_dir, "ui.R"), local = TRUE)
  source(file.path(app_dir, "server.R"), local = TRUE)
  
  # ---- launch ----
  shiny::shinyApp(
    ui = spec_builder_ui(),
    server = function(input, output, session) {
      spec_builder_server(input, output, session)
    }
  )
}


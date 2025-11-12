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
use_spec_builder_app <- function(
    data_path = NULL,
    spec_path = NULL,
    registry_path = system.file("registry/checkpoints_registry.csv", package = "rchk0"),
    study_id = NULL
) {
  shiny::shinyApp(
    ui = spec_builder_ui(data_path = data_path, spec_path = spec_path, study_id = study_id),
    server = function(input, output, session) {
      spec_builder_server(input, output, session, data_path, spec_path, registry_path,
                          datasets_pool = get("datasets_pool", envir = .GlobalEnv))
    }
  )
}
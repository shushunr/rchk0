library(rchk0)

# Detect from global env if exists
if (!exists("DATA_PATH", envir = .GlobalEnv)) DATA_PATH <- ""
if (!exists("SPEC_PATH", envir = .GlobalEnv)) SPEC_PATH <- ""
if (!exists("REGISTRY_PATH", envir = .GlobalEnv)) {
  REGISTRY_PATH <- system.file("app", "checkpoints_registry.csv", package = "rchk0")
}

shinyApp(
  ui = spec_builder_ui(
    data_path = DATA_PATH,
    spec_path = SPEC_PATH
  ),
  server = function(input, output, session) {
    spec_builder_server(input, output, session,
                        data_path = DATA_PATH,
                        spec_path = SPEC_PATH,
                        registry_path = REGISTRY_PATH,
                        datasets_pool =  get("datasets_pool", envir = .GlobalEnv))
  }
)

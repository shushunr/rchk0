library(DT)

spec_builder_server <- function(input, output, session) {
  
  # --- Step 1. Load datasets ----
  datasets_pool <- reactiveVal(NULL)
  
  observeEvent(input$load_data, {
    shiny::req(input$data_path)
    
    params <- list(
      path = input$data_path,
      exclude_prefixes = strsplit(input$exclude_prefixes, ",\\s*")[[1]],
      exclude_names = strsplit(input$exclude_names, ",\\s*")[[1]]
    )
    
    message("📂 Loading datasets with parameters: ", paste(names(params), collapse=", "))
    data_list <- do.call(load_dataset, params)
    
    datasets_pool(data_list)
  })
  
  
  # --- Step 2. Show dataset summary ----
  output$dataset_summary <- renderDT({
    shiny::req(datasets_pool())
    data_list <- datasets_pool()
    
    info <- lapply(names(data_list), function(nm) {
      df <- data_list[[nm]]
      tibble(
        dataset = nm,
        nrow = nrow(df),
        ncol = ncol(df),
        last_edit = file.info(file.path(input$data_path, paste0(nm, ".sas7bdat")))$mtime
      )
    }) |> bind_rows()
    
    datatable(info)
  })
  
  
  # --- Step 3. Visit info (from data_info0$visit_info_df) ----
  visit_info <- reactiveVal(NULL)
  
  observe({
    shiny::req(datasets_pool())
    
    if (exists("data_info0", envir = .GlobalEnv)) {
      visit_info(get("data_info0", envir = .GlobalEnv)$visit_info_df)
      message("✅ visit_info loaded from global data_info0")
    } else {
      message("⚠️ data_info0 not found in global env")
    }
  })
  
  output$visit_info <- renderDT({
    shiny::req(visit_info())
    datatable(visit_info(), editable = TRUE)
  })
  
  observeEvent(input$visit_info_cell_edit, {
    info <- input$visit_info_cell_edit
    df <- visit_info()
    df[info$row, info$col] <- info$value
    visit_info(df)
  })
  
  
  # --- Step 4. Checkpoints registry table (inst/registry/checkpoints_registry.csv) ----
  registry_path <- system.file("registry/checkpoints_registry.csv", package = "rchk0")
  registry_data <- reactiveVal(read.csv(registry_path))
  
  output$checkpoints_registry <- DT::renderDT({
    DT::datatable(
      registry_data(),
      editable = TRUE,
      options = list(
        scrollX = TRUE   # feel free to scroll
      )
    )
  })
  
  observeEvent(input$checkpoints_registry_cell_edit, {
    info <- input$checkpoints_registry_cell_edit
    df <- registry_data()
    df[info$row, info$col] <- info$value
    registry_data(df)
  })
  
  
  # --- Step 5. Generate spec ----
  observeEvent(input$generate_spec, {
    cfg <- list(
      study_id           = input$study_id,
      generated_by       = input$generated_by,
      visit_info_df      = if (!is.null(visit_info())) visit_info() else NULL,
      checkpoints_enabled= if (!is.null(registry_data())) registry_data() else NULL,
      datasets_pool      = if (!is.null(datasets_pool())) datasets_pool() else NULL
    )
    
    shiny::req(cfg$study_id, cfg$generated_by)
    
    out_xlsx <- file.path(tempdir(), paste0("study_spec_", Sys.Date(), ".xlsx"))
    include_yaml <- isTRUE(input$include_yaml)   
    out_yaml <- if (include_yaml) sub("\\.xlsx$", ".yml", out_xlsx, ignore.case = TRUE) else NULL
    
    generate_spec(
      config       = cfg,
      path         = out_xlsx,
      include_yaml = include_yaml,
      yaml_path    = out_yaml
    )
    
    output$download_spec <- downloadHandler(
      filename = function() basename(out_xlsx),
      content  = function(file) file.copy(out_xlsx, file, overwrite = TRUE)
    )
    
    if (include_yaml && !is.null(out_yaml)) {
      output$download_yaml <- downloadHandler(
        filename = function() basename(out_yaml),
        content  = function(file) file.copy(out_yaml, file, overwrite = TRUE)
      )
    }
  })
  
}

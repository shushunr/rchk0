spec_builder_server <- function(input, output, session,
                                data_path = NULL,
                                spec_path = NULL,
                                registry_path = NULL,
                                visit_info = NULL,
                                datasets_pool = NULL) {
  
  # --- Step 4. Registry ----
  registry_df <- reactive({
    shiny::req(registry_path)
    readr::read_csv(registry_path, show_col_types = FALSE)
  })
  
  output$checkpoints_table <- DT::renderDataTable({
    DT::datatable(registry_df(), editable = TRUE, options = list(pageLength = 10, dom = "t"))
  })
  
  # --- Step 5. Dataset overview ----
  output$dataset_overview <- DT::renderDataTable({
    req(datasets_pool)
    DT::datatable(
      data.frame(
        Dataset = names(datasets_pool),
        Rows = purrr::map_int(datasets_pool, nrow),
        Cols = purrr::map_int(datasets_pool, ncol)
      ),
      options = list(dom = "t")
    )
  })
  
  # --- Step 6. Data Info ----
  output$data_info_table <- DT::renderDT({
    req(preprocess_res())
    DT::datatable(preprocess_res()$visit_info_df, editable = TRUE, options = list(dom = "t"))
  })
  
  observeEvent(input$data_info_table_cell_edit, {
    info <- input$data_info_table_cell_edit
    df <- preprocess_res()$visit_info_df
    df[info$row, info$col] <- info$value
    preprocess_res(list(visit_info_df = df, datasets_pool = preprocess_res()$datasets_pool))
  })
  
  # --- Step 7. Download spec ----
  output$download_spec <- shiny::downloadHandler(
    filename = function() paste0("spec_", input$study_id, ".xlsx"),
    content = function(file) {
      config <- list(
        study_id = input$study_id,
        generated_by = input$generated_by,
        checkpoints_enabled = registry_df(),
        visit_info_df = preprocess_res()$visit_info_df
      )
      rchk0::generate_spec(config, file)
    }
  )
  
  # --- Step 8. Preview ----
  output$preview_spec <- shiny::renderPrint({
    list(
      Study_ID = input$study_id,
      Data_Path = current_data_path(),
      Spec_Path = current_spec_path(),
      Datasets = names(datasets_pool()),
      Visit_Info = preprocess_res()$visit_info_df,
      Checkpoints = registry_df()
    )
  })
}

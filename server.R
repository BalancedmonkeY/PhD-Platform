server <- function(input, output, session) {
  
  ### Reload Demo Data ###
  #----------------------#
  
  observe({
    
    # Demo date #
    base_demo <- as.Date("2023-12-31")
    saveRDS(base_demo, "Saved_dfs/demo_date.rds")
    
    # Saved search date #
    search_date_base <- readRDS("Saved_dfs/search_date_base.rds")
    saveRDS(search_date_base, "Saved_dfs/search_date.rds")
    
    # Saved published data #
    published_updates_base <- readRDS("Saved_dfs/published_updates_base.rds")
    saveRDS(published_updates_base, "Saved_dfs/published_updates.rds")
    
    # Saved latest update #
    non_published_update_base <- readRDS("Saved_dfs/non_published_update_base.rds")
    saveRDS(non_published_update_base, "Saved_dfs/non_published_update.rds")
    
    # Saved activity log #
    activity_log_base <- readRDS("Saved_dfs/activity_log_base.rds")
    saveRDS(activity_log_base, "Saved_dfs/activity_log.rds")
    
    # Saved protocol updates #
    protocol_updates_base <- readRDS("Saved_dfs/protocol_updates_base.rds")
    saveRDS(protocol_updates_base, "Saved_dfs/protocol_updates.rds")
    
    # Saves summary of results #
    results_summary_base <- readRDS("Saved_dfs/results_summary_base.rds")
    saveRDS(results_summary_base, "Saved_dfs/results_summary.rds")
    
    # Lock status
    lock_base <- readRDS("Saved_dfs/lock_base.rds")
    saveRDS(lock_base, "Saved_dfs/lock.rds")
    
    # Saved thresholds
    thresholds_base <- readRDS("Saved_dfs/thresholds_base.rds")
    saveRDS(thresholds_base, "Saved_dfs/thresholds.rds")
    
    # Notification of reloading session
    showModal(
      modalDialog(
        title = "Resetting Demo",
        footer = NULL,
        size = "l",
        div(
          h5("To reset the demo data the app needs to completely close. To begin the demo again, please relaunch the app."),
          actionButton("accept_reset", "Accept")
        )
      )
    )

  }) |>
    bindEvent(input$reset_demo)
  
  observe({
    stopApp()
  }) |>
    bindEvent(input$accept_reset)
  
  
  ## Save & Pass Demo Date Through ##
  #---------------------------------#
  
  # Save demo date whenever user changes date
  observe({
    if (!is.null(input$demo_date)) {
      saveRDS(input$demo_date, "Saved_dfs/demo_date.rds")
    }
  }) |>
    bindEvent(input$demo_date, ignoreInit = TRUE)
  
  # Keep demo date up to date
  saved_demo_date <- reactive({readRDS("Saved_dfs/demo_date.rds")})
  observe({
    updateDateInput(session, "demo_date", value = saved_demo_date())
  })
  
  # Save as a reactive to then pass through into other modules
  current_date <- reactive({input$demo_date})
  
  
  ## Initialise Reactive Activity Log ##
  #------------------------------------#
  
  latest_activity <- reactiveVal(
    readRDS('Saved_dfs/activity_log.rds')
  )
  
  ## Initialise Summary of Results ##
  #---------------------------------#
  
  results_summary <- reactiveVal(
    readRDS("Saved_dfs/results_summary.rds")
  )
  

  ### Load Data ###
  #---------------#
  
  data_object <- data_page_server(
    id = "data",
    demo_date = current_date,
    activity = latest_activity
  )
  
  
  ### Home Page Server ###
  #----------------------#
  
  home_obj <- home_page_server(
    id = "home",
    demo_date = current_date,
    activity = latest_activity,
    last_search_date = data_object$last_search_date,
    studies = data_object$studies,
    results_summary = results_summary,
    parent_session = session)
  
  
  ### Present Results ###
  #---------------------#
  
  results_page_server(
    id = "results",
    data = data_object$data,
    studies = data_object$studies,
    latest_data_df = home_obj$latest_data_df
  )
  
  ### Activity Log Server ###
  #--------------------------#
  
  activity_page_server(
    id = "activity",
    activity_log = latest_activity
  )
  
  # Save activity log for future loads
  observe({
    saveRDS(latest_activity(), 'Saved_dfs/activity_log.rds')
  })
  
  
  ### LSRUpdateR Server ###
  #-----------------------#
  
  LSRUpdateR_page_server(
    id = "LSRUpdateR",
    parent_session = session,
    activity = latest_activity,
    latest_data_df = home_obj$latest_data_df,
    data_object = data_object,
    results_summary = results_summary,
    demo_date = current_date
  )
  
  # Save summary of results for future loads
  observe({
    saveRDS(results_summary(), "Saved_dfs/results_summary.rds")
  })
  

}

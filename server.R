server <- function(input, output, session) {
  
  ### Reload Demo Data ###
  #----------------------#
  
  observe({
    
    # Demo date #
    base_demo <- as.Date("2023-12-01")
    saveRDS(base_demo, "demo_date.rds")
    
    # Saved search date #
    search_date_base <- readRDS("search_date_base.rds")
    saveRDS(search_date_base, "search_date.rds")
    
    # Saved published data #
    published_updates_base <- readRDS("published_updates_base.rds")
    saveRDS(published_updates_base, "published_updates.rds")
    
    # Saved latest update #
    non_published_update_base <- readRDS("non_published_update_base.rds")
    saveRDS(non_published_update_base, "non_published_update.rds")
    
    # Saved activity log #
    activity_log_base <- readRDS("activity_log_base.rds")
    saveRDS(activity_log_base, "activity_log.rds")
    
    # Saved protocol updates #
    protocol_updates_base <- readRDS("protocol_updates_base.rds")
    saveRDS(protocol_updates_base, "protocol_updates.rds")
    
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
      saveRDS(input$demo_date, "demo_date.rds")
    }
  }) |>
    bindEvent(input$demo_date, ignoreInit = TRUE)
  
  # Keep demo date up to date
  saved_demo_date <- reactive({readRDS("demo_date.rds")})
  observe({
    updateDateInput(session, "demo_date", value = saved_demo_date())
  })
  
  # Save as a reactive to then pass through into other modules
  current_date <- reactive({input$demo_date})
  
  
  ## Initialise Reactive Activity Log ##
  #------------------------------------#
  
  latest_activity <- reactiveVal(
    readRDS('activity_log.rds')
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
  
  home_page_server(
    id = "home",
    demo_date = current_date,
    activity = latest_activity,
    last_search_date = data_object$last_search_date,
    studies = data_object$studies,
    parent_session = session)
  
  
  ### Present Results ###
  #---------------------#
  
  results_page_server(
    id = "results",
    data = data_object$data
  )
  
  ### Activity Log Server ###
  #--------------------------#
  
  activity_page_server(
    id = "activity",
    activity_log = latest_activity
  )
  
  # Save activity log for future loads
  observe({
    saveRDS(latest_activity(), 'activity_log.rds')
  })
  

}

function(input, output, session) {
  
  ### Reload Demo Data ###
  #----------------------#
  
  observe({
    
    # Saved search data #
    saved_date_base <- readRDS("saved_date_base.rds")
    saveRDS(saved_date_base, "saved_date.rds")
    
    # Saved published data #
    published_updates_base <- readRDS("published_updates_base.rds")
    saveRDS(published_updates_base, "published_updates.rds")
    
    # Saved latest update #
    non_published_update_base <- readRDS("non_published_update_base.rds")
    saveRDS(non_published_update_base, "non_published_update.rds")
    
    # Saved activity log #
    activity_log_base <- readRDS("activity_log_base.rds")
    saveRDS(activity_log_base, "activity_log.rds")
    
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
  
  
  ## Pass Demo Date Through ##
  #--------------------------#
  
  current_date <- reactive({input$demo_date})
  

  ### Load Data ###
  #---------------#
  
  data_object <- data_page_server(
    id = "data",
    demo_date = current_date
  )
  
  
  ### Home Page Server ###
  #----------------------#
  
  home_page_server(
    id = "home",
    last_search_date = data_object$last_search_date,
    studies = data_object$studies)
  
  
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
    activity_log = data_object$latest_activity
  )

}

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
    
    # Reload session
    session$reload()
    

  }) |>
    bindEvent(input$reset_demo)
  

  ### Load Data ###
  #---------------#
  
  data_object <- data_page_server(id = "data")
  
  
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
    id = "activity"
  )

}

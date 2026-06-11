function(input, output, session) {
  

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

}

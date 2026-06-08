function(input, output, session) {

  ### Load Data ###
  #---------------#
  
  data <- data_page_server(id = "data")
  
  
  ### Present Results ###
  #---------------------#
  
  results_page_server(
    id = "results",
    data = data
  )

}

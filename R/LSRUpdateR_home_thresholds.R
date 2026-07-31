#' UI for the threshold settings on home page of LSRUpdateR page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
LSRUpdateR_home_thresholds_ui <- function(id) {
  ns = NS(id)
  
  div()
  
}

#' Server for the threshold settings on home page for LSRUpdateR page.
#'
#' @param id ID of the module
#'
#' @return 
LSRUpdateR_home_thresholds_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}
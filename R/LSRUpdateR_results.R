#' UI for the results page of LSRUpdateR page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
LSRUpdateR_results_ui <- function(id) {
  ns = NS(id)
  
  div(
    h3("Results page"),
  
    actionButton(ns("go_home"), label = "Home")
  )
  
}

#' Server for the results page for LSRUpdateR page.
#'
#' @param id ID of the module
#' @param LSRUpdateR_session Session from LSRUpdateR page (needed for navbar_select)
#'
#' @return 
LSRUpdateR_results_server <- function(id, LSRUpdateR_session) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$go_home, {
        bslib::nav_select("LSRUpdateR_navbar", selected = "home", session = LSRUpdateR_session)
      })
      
    }
  )
}
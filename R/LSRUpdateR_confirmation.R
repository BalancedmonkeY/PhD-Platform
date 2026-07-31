#' UI for the confirmation page of LSRUpdateR page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
LSRUpdateR_confirmation_ui <- function(id) {
  ns = NS(id)
  
  div(
    h3("Confirmation page"),
  
    actionButton(ns("go_home"), label = "Home")
  )
  
}

#' Server for the confirmation page for LSRUpdateR page.
#'
#' @param id ID of the module
#' @param LSRUpdateR_session Session from LSRUpdateR page (needed for navbar_select)
#'
#' @return 
LSRUpdateR_confirmation_server <- function(id, LSRUpdateR_session) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$go_home, {
        bslib::nav_select("LSRUpdateR_navbar", selected = "home", session = LSRUpdateR_session)
      })
      
    }
  )
}
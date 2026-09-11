#' UI for the results page of LSRUpdateR page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
LSRUpdateR_results_ui <- function(id) {
  ns = NS(id)
  
  div(
    h3("Results page"),
    
    uiOutput(ns("test_result")),
  
    actionButton(ns("go_home"), label = "Home")
  )
  
}

#' Server for the results page for LSRUpdateR page.
#'
#' @param id ID of the module
#' @param LSRUpdateR_session Session from LSRUpdateR page (needed for navbar_select)
#' @param LSRUpdateR_results Results object from running LSRUpdateR
#' @param activity_record Beginnings of an activity record from running LSRUpdateR
#'
#' @return 
LSRUpdateR_results_server <- function(id, LSRUpdateR_session, LSRUpdateR_results, activity_record) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      output$test_result <- renderUI({
        HTML(paste0(
          LSRUpdateR_results()[["NRT_cess"]]$text_result, "<br>",
          activity_record()))
      })
      
      
      observeEvent(input$go_home, {
        bslib::nav_select("LSRUpdateR_navbar", selected = "home", session = LSRUpdateR_session)
      })
      
    }
  )
}
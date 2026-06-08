#' UI for the results page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
results_page_ui <- function(id) {
  ns = NS(id)
  div(
    reactableOutput(ns("data_table"))
  )
}


#' Server for the results page.
#'
#' @param id ID of the module.
#' @param data Latest data from LSR
#'
#' @return nothing yet
results_page_server <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$data_table <- renderReactable({
        req(data()) #ensures data is available
        reactable(data())})
      
    }
  )
}
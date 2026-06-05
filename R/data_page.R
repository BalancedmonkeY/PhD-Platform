#' UI for the data page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
data_page_ui <- function(id) {
  ns = NS(id)
  div(
    reactableOutput(ns("data_table")) 
  )
}


#' Server for the data page.
#'
#' @param id ID of the module.
#'
#' @return loaded data frame
data_page_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Load data from google sheet
      data <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1DddDEB5TUQh2MccOvOhcrZ_hOe7DOEvhFosQ-ataaCI/edit?gid=0#gid=0")
      output$data_table <- renderReactable({reactable(data)})
      
    }
  )
}
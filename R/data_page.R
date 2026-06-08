#' UI for the data page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
data_page_ui <- function(id) {
  ns = NS(id)
  div(
    fluidRow(
      box(
        div(
          style = "display: flex; align-items: center; gap: 12px;", # lines up widgets side by side
          actionButton(ns("refresh_button"), "Sync Data"),
          textOutput(ns("refresh_text"))
        )
      )
    ),
    htmlOutput(ns("google_sheets_data"))
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
      
      # Load iframe of google sheet
      output$google_sheets_data <- renderUI({
        tags$iframe(
          src="https://docs.google.com/spreadsheets/d/1DddDEB5TUQh2MccOvOhcrZ_hOe7DOEvhFosQ-ataaCI/edit?usp=sharing",
          style='height:100vh;',
          width = "100%",
          frameBorder = "0")
      })
      
      # Load data from google sheet and refresh
      
      data <- eventReactive(input$refresh_button, {
          df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1DddDEB5TUQh2MccOvOhcrZ_hOe7DOEvhFosQ-ataaCI/edit?usp=sharing")
          df$Date <- as.Date(df$Date)
          df
        },
        ignoreNULL = FALSE   # ensures it runs once at startup
      )
      
      output$refresh_text <- renderText({
        data()
        paste0("Data last synced ", format(Sys.time(), "%a %b %d %Y %H:%M"))
      })
      
      return(data)
      
    }
  )
}
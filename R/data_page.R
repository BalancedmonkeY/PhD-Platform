#' UI for the data page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
data_page_ui <- function(id) {
  ns = NS(id)
  
  # Load saved date
  saved_last_search_date <- readRDS("saved_date.rds")
  
  div(
    fluidRow(
      box(
        div(
          style = "display: flex; align-items: center; gap: 12px;", # lines up widgets side by side
          actionButton(ns("refresh_button"), "Sync Data"),
          textOutput(ns("refresh_text")),
          dateInput(ns("last_search_date"), label = "Last Search Date", format = "dd MM yyyy", value = saved_last_search_date)
        ),
      )
    ),
    htmlOutput(ns("google_sheets_data"))
  )
}


#' Server for the data page.
#'
#' @param id ID of the module.
#'
#' @return list containing
#' - loaded data frame(s) (each outcome)
#' - last search date
#' - unique studies
data_page_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Load iframe of google sheet
      output$google_sheets_data <- renderUI({
        tags$iframe(
          src="https://docs.google.com/spreadsheets/d/1IBE4yIVd67-8bcLEqptfglM75CWGz6SOE6D8LS4FULY/edit?usp=sharing",
          style='height:100vh;',
          width = "100%",
          frameBorder = "0")
      })
      
      # Load data from google sheet and refresh
      data <- eventReactive(input$refresh_button, {
        # Cessation data
          cess_df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1IBE4yIVd67-8bcLEqptfglM75CWGz6SOE6D8LS4FULY/edit?usp=sharing",
                                          sheet = "Cessation")
          cess_df$Included_Date <- as.Date(cess_df$Included_Date)
          cess_df$Excluded_Date <- as.Date(cess_df$Excluded_Date)
        # Adverse events data
          AE_df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1IBE4yIVd67-8bcLEqptfglM75CWGz6SOE6D8LS4FULY/edit?usp=sharing",
                                               sheet = "Adverse_Events")
          AE_df$Included_Date <- as.Date(AE_df$Included_Date)
          AE_df$Excluded_Date <- as.Date(AE_df$Excluded_Date)
        # Serious adverse events data
          SAE_df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1IBE4yIVd67-8bcLEqptfglM75CWGz6SOE6D8LS4FULY/edit?usp=sharing",
                                             sheet = "Serious_Adverse_Events")
          SAE_df$Included_Date <- as.Date(SAE_df$Included_Date)
          SAE_df$Excluded_Date <- as.Date(SAE_df$Excluded_Date)
          
        list(
          cess_df = cess_df,
          AE_df = AE_df,
          SAE_df = SAE_df
        )
          
        },
        ignoreNULL = FALSE   # ensures it runs once at startup
      )
      
      studies <- eventReactive(input$refresh_button, {
        # Unique studies
        studies <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1IBE4yIVd67-8bcLEqptfglM75CWGz6SOE6D8LS4FULY/edit?usp=sharing",
                                             sheet = "Unique_Studies")
        studies$Included_Date <- as.Date(studies$Included_Date)
        studies$Excluded_Date <- as.Date(studies$Excluded_Date)
        
        return(studies)
        
      },
      ignoreNULL = FALSE   # ensures it runs once at startup
      )
      
      
      output$refresh_text <- renderText({
        data()
        paste0("Data last synced ", format(Sys.time(), "%a %b %d %Y %H:%M"))
      })
      
      
      # Save search date whenever user changes date
      observeEvent(input$last_search_date, {
        if (!is.null(input$last_search_date)) {
          saveRDS(input$last_search_date, "saved_date.rds")
        }
      })
      
      # Extract last search date and save as a reactive
      last_search_date <- reactive({
        input$last_search_date
      })
      
      return(list(
        data = data,
        last_search_date = last_search_date,
        studies = studies)
      )
    }
  )
}
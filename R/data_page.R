#' UI for the data page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
data_page_ui <- function(id) {
  ns = NS(id)
  
  div(
    fluidRow(
      div(
        style = "display: flex; align-items: center; gap: 12px;", # lines up widgets side by side
        dateInput(ns("last_search_date"), label = "Last Search Date", format = "dd MM yyyy", value = readRDS("Saved_dfs/search_date.rds")),
        textOutput(ns("refresh_text")),
        actionButton(ns("refresh_button"), "Sync Data"),
        p("Please sync data before moving on to ensure everything is saved")
      ),
    ),
    htmlOutput(ns("google_sheets_data"))
  )
}


#' Server for the data page.
#'
#' @param id ID of the module.
#' @param demo_date Current demo date
#' @param activity Reactive activity log
#'
#' @return list containing
#' - loaded data frame(s) (each outcome)
#' - last search date
#' - unique studies
data_page_server <- function(id, demo_date, activity) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Update input for last search date for when demo is reset
      saved_last_search_date <- reactive({readRDS("Saved_dfs/search_date.rds")})
      observe({
        updateDateInput(session, "last_search_date", value = saved_last_search_date())
      })
      
      # Load iframe of google sheet
      output$google_sheets_data <- renderUI({
        tags$iframe(
          src="https://docs.google.com/spreadsheets/d/1IBE4yIVd67-8bcLEqptfglM75CWGz6SOE6D8LS4FULY/edit?usp=sharing",
          style='height:100vh;',
          width = "100%",
          frameBorder = "0")
      })
      
      # Load data from google sheet and refresh
      data <- eventReactive(list(input$refresh_button), {
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
      
      studies <- eventReactive(list(input$refresh_button), {
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
      observe({
        if (!is.null(input$last_search_date)) {
          saveRDS(input$last_search_date, "Saved_dfs/search_date.rds")
        }
      }) |>
        bindEvent(input$last_search_date, ignoreInit = TRUE)
      

      # Add new data activity logs #
      observe({
        # Extract last data activity (and what search date was reported)
        latest_data_entry_log <- activity() %>% filter(Category == "Data") %>% filter(Date == max(Date))
        previous_search_date <- as.Date(gsub(".*last search date to (\\d{2} [a-zA-Z]{3} \\d{4}).*", "\\1", latest_data_entry_log$Details), "%d %b %Y")
        # Calculate number of new studies
        current_number_studies <- nrow(studies() %>% filter(Included_Date <= input$last_search_date))
        previous_number_studies <- nrow(studies() %>% filter(Included_Date <= previous_search_date))
        number_new_studies <- current_number_studies - previous_number_studies

        # Update activity log
        new_activity_df <- activity() %>%
          add_row(Date = demo_date(), Category = "Data",
                  Details = paste0("User updated the last search date to ", format(input$last_search_date, "%d %b %Y"), ". This gave ", number_new_studies, " new study/ies added."))
        activity(new_activity_df)
      })  |>
        bindEvent(input$refresh_button, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      return(list(
        data = data,
        last_search_date = reactive({input$last_search_date}),
        studies = studies
        )
      )
    }
  )
}
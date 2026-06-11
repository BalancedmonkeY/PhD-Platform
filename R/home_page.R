#' UI for the home page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
home_page_ui <- function(id) {
  ns = NS(id)
  div(
    
    # UI components
    column(7,
           value_box( 
             title = tags$p("Latest Data", style = "font-size: 1.6em; margin-bottom: 5px;"), 
             value = uiOutput(ns("latest_data_table")),
             showcase = bsicons::bs_icon("clipboard2-data"),
             showcase_layout = showcase_left_center(width = 0.1),
             theme = "text-success" 
           )),
    column(5)
  )
}


#' Server for the home page.
#'
#' @param id ID of the module
#' @param last_search_date Reactive date for most recent search
#' @param studies Reactive dataframe of unique studies that are included in the review
#'
#' @return nothing yet
home_page_server <- function(id, last_search_date, studies) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Create base reactive table of published updates
      published_updates <- reactive(
        data.frame(
          Publish_date = as.Date(c("17-Nov-2022", "14-Sep-2021", "29-Apr-2021", "14-Oct-2020"), "%d-%b-%Y"),
          Search_date = as.Date(c("01-Jul-2022", "01-May-2021", "01-Feb-2021", "01-Jan-2020"), "%d-%b-%Y"),
          No_studies = c(25, 19, 14, 13),
          Reason = c("Change in GRADE rating and interpretation. Cessation and adverse events outcomes for NRT comparison moved from Moderate to High and Low to Moderate respectively. Interpretation of serious adverse events outcome for non-nicotine EC and behavioural comparisons both changed from beneficial and harmful respectively to unknown.",
                     "New device data",
                     "New device data and change in GRADE rating. Adverse events outcome for non-nicotine EC and behavioural comparison moved from Low to Moderate and Moderate to High respectively.",
                     "Received funding to create living review."),
          DOI = c("https://doi.org/10.1002/14651858.CD010216.pub7", "https://doi.org/10.1002/14651858.CD010216.pub6", "https://doi.org/10.1002/14651858.CD010216.pub5", "https://doi.org/10.1002/14651858.CD010216.pub4"))
      )
      
      # Take most recent published update
      latest_published_update <- reactive({
        df <- published_updates() %>% filter(Publish_date == max(published_updates()$Publish_date)) %>% select(c("Search_date", "No_studies"))
        df$Status <- paste0("Most recent peer-reviewed update: <a href='", published_updates()$DOI[published_updates()$Publish_date == max(published_updates()$Publish_date)], "'>link here</a>")
        df
      })
      
      # Initialise reactive entry for latest update subject to peer-review
      non_peer_review_update <- reactive(
        data.frame(
          Publish_date = as.Date("08-Jan-2024", "%d-%b-%Y"), 
          Search_date = as.Date("01-Jul-2023", "%d-%b-%Y"), 
          No_studies = 28, 
          Reason = "Change in GRADE rating and interpretation. Cessation outcome for behavioural comparison moved from Moderate to High. Interpretation of serious adverse events outcome for behavioural comparison changed from unknown to clinically beneficial."
        )
      )
      
      # Top table entry for latest data
      latest_data_row <- reactive({
        current_studies <- nrow(studies()[studies()$Included_Date <= last_search_date() & studies()$Excluded_Date > last_search_date(),])
        non_peer_review_studies <- non_peer_review_update()$No_studies
        status_text <- if (current_studies - non_peer_review_studies == 0) {
          "All studies are included in latest analysis."
        } else {
          paste0(current_studies - non_peer_review_studies, 
                 ifelse(current_studies - non_peer_review_studies == 1, " study has", " studies have"),
                 " data extracted but not yet included in the analysis")
        }
        df <- data.frame(
          Search_date = as.Date(last_search_date(), "%d-%b-%Y"),
          No_studies = nrow(studies()[studies()$Included_Date <= last_search_date() & studies()$Excluded_Date > last_search_date(),]),
          Status = status_text
        )
        df
      })
      
      
      # Table of latest data
      output$latest_data_table <- renderUI({
        
        # Build table #
        df <- rbind(latest_data_row(),
          non_peer_review_update() %>% select(c("Search_date", "No_studies")) %>% mutate(Status = "Latest update (subject to peer-review)"),
          latest_published_update())
        
        # Rename columns
        colnames(df) <- c("Search Date", "No. Studies", "Status")
        
        # Reformat dates
        df[["Search Date"]] <- format(df[["Search Date"]], "%d %B %Y")
        
        
        # Build HTML table manually
        tags$table(
          style = "width:100%; border-collapse: collapse;",
          
          # Header
          tags$thead(
            tags$tr(
              lapply(names(df), function(col) {
                tags$th(col,
                        style = "text-align:left; padding:6px; font-size:15px; border-bottom:1px solid #ddd;"
                )
              })
            )
          ),
          
          # Body
          tags$tbody(
            lapply(seq_len(nrow(df)), function(i) {
              tags$tr(
                tags$td(df[i, 1], style = "padding:6px; font-size:13px;"),
                tags$td(df[i, 2], style = "padding:6px; text-align:center; font-size:13px;"),
                tags$td(HTML(df[i, 3]), style = "padding:6px; font-size:13px;") # HTML needed for the link
              )
            })
          )
        )
      })
    }
  )
}
#' UI for the home page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
home_page_ui <- function(id) {
  ns = NS(id)
  div(
    
    # UI components
    fluidRow(
      column(7,
             bslib::value_box( 
               title = tags$p("Latest Data", style = "font-size: 1.6em; margin-bottom: 5px;"), 
               value = uiOutput(ns("latest_data_table")),
               showcase = bsicons::bs_icon("clipboard2-data"),
              showcase_layout = showcase_left_center(width = 0.1),
              theme = "text-success" 
            )),
      column(5,
             bslib::value_box(
               title = tags$p("Version History", style = "font-size: 1.6em; margin-bottom: 5px;"),
               value = uiOutput(ns("version_history_table")),
               showcase = bsicons::bs_icon("clock-history"),
              showcase_layout = showcase_left_center(width = 0.1),
              theme = "text-success"
            ))
    )
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
      
      # Read in recorded table of published updates
      published_updates <- reactive({
        readRDS("published_updates.rds")
      })
      
      # Take most recent published update
      latest_published_update <- reactive({
        df <- published_updates() %>% filter(Publish_date == max(published_updates()$Publish_date)) %>% select(c("Search_date", "No_studies"))
        df$Status <- paste0("Most recent peer-reviewed update: <a href='", published_updates()$DOI[published_updates()$Publish_date == max(published_updates()$Publish_date)], "'>link here</a>")
        df
      })
      
      # Read in recorded non-peer-review update status
      non_peer_review_update <- reactive({
        readRDS("non_published_update.rds")
      })
      
      # Summary of latest data regardless of update
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
        return(df)
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
      
      
      # Version history table
      # Build table
      version_history <- reactive({
        # Published updates
        published <- published_updates() %>%
          mutate(Publish_link = paste0("<a href='", DOI, "'>", format(Publish_date, "%d %b %Y"), "</a>")) %>%
          select("Publish_link", "Search_date", "Reason") 
        # Latest non-published update
        latest <- data.frame(
          Publish_link = "In peer-review",
          Search_date = non_peer_review_update()$Search_date,
          Reason = non_peer_review_update()$Reason
        )
        # Combine
        df <- rbind(latest, published)
        df$Search_date <- format(df$Search_date, "%d %B %Y")
        df <- df %>%
          rename("Publication Date" = "Publish_link", "Search Date" = "Search_date", "Reason for Update" = "Reason")
        return(df)
      })
      
      # Render version history table
      output$version_history_table <- renderUI({
        
        df <- version_history()
        
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
                tags$td(HTML(df[i, 1]), style = "padding:6px; font-size:13px;"), # HTML needed for the link
                tags$td(df[i, 2], style = "padding:6px; font-size:13px;"),
                tags$td(df[i, 3], style = "padding:6px; font-size:13px;") 
              )
            })
          )
        )
        
        
      })
    }
  )
}
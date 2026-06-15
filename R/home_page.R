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
        home_latest_data_ui(id = ns("latest_data"))
      ),
      column(5,
        home_version_history_ui(id = ns("version_history"))
      )
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
        df$Status <- paste0("Most recent peer-reviewed update: <a href='", published_updates()$DOI[published_updates()$Publish_date == max(published_updates()$Publish_date)], "' target='_blank'>link here</a>")
        df
      })
      
      # Read in recorded non-peer-review update status
      non_peer_review_update <- reactive({
        readRDS("non_published_update.rds")
      })
      
      # Summary of latest data regardless of update
      latest_data_row <- reactive({
        current_studies <- nrow(studies()[studies()$Included_Date <= last_search_date() & studies()$Excluded_Date > last_search_date(),])
        previous_studies <- ifelse(is.null(non_peer_review_update()),
                                   latest_published_update()$No_studies,
                                   non_peer_review_update()$No_studies)
        status_text <- if (current_studies - previous_studies == 0) {
          "All studies are included in latest analysis."
        } else {
          paste0(current_studies - previous_studies, 
                 ifelse(current_studies - previous_studies == 1, " study has", " studies have"),
                 " data extracted but not yet included in the analysis")
        }
        df <- data.frame(
          Search_date = as.Date(last_search_date(), "%d-%b-%Y"),
          No_studies = nrow(studies()[studies()$Included_Date <= last_search_date() & studies()$Excluded_Date > last_search_date(),]),
          Status = status_text
        )
        return(df)
      })
      
      
      # Build latest data table #
      latest_data_df <- reactive({
        if (is.null(non_peer_review_update())) {
          rbind(latest_data_row(),
                latest_published_update())
        } else {
          rbind(latest_data_row(),
            non_peer_review_update() %>% select(c("Search_date", "No_studies")) %>% mutate(Status = "Latest update (subject to peer-review)"),
            latest_published_update())
        }
      })
      
      # Render latest data table #
      new_publication <- home_latest_data_server(
        id = "latest_data",
        df = latest_data_df,
        non_published_update = non_peer_review_update,
        published = published_updates
      )
      
      
      # Build version history table #
      version_history_df <- reactive({
        # Published updates
        published <- published_updates() %>%
          mutate(Publish_link = paste0("<a href='", DOI, "' target = '_blank'>", format(Publish_date, "%d %b %Y"), "</a>")) %>%
          select("Publish_link", "Search_date", "Reason") 
        # Latest non-published update (if exists)
        if (!is.null(non_peer_review_update())) {
          latest <- data.frame(
            Publish_link = "In peer-review",
            Search_date = non_peer_review_update()$Search_date,
            Reason = non_peer_review_update()$Reason
          )
        }
        # Combine
        if (is.null(non_peer_review_update())) {
          df <- published
        } else {
          df <- rbind(latest, published)
        }
        df$Search_date <- format(df$Search_date, "%d %B %Y")
        df <- df %>%
          rename("Publication Date" = "Publish_link", "Search Date" = "Search_date", "Reason for Update" = "Reason")
        return(df)
      })
      
      # Render version history table #
      home_version_history_server(
        id = "version_history",
        df = version_history_df
      )
    }
  )
}
#' UI for the home page of LSRUpdateR page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
LSRUpdateR_home_ui <- function(id) {
  ns = NS(id)
  
  fluidRow(
    column(4, 
           
      # Update buttons
      div(style = "display: flex; flex-direction: column; align-items: center; gap: 20px; margin-bottom: 20px;",
        actionButton(ns("run_LSRUpdateR"), label = "Run LSRUpdateR", icon = shiny::icon("play")),
        actionButton(ns("run_update"), label = "Conduct Update", icon = shiny::icon("paper-plane"))
      ),
      
      # Latest activity
      bslib::value_box(
        title = tags$p("Latest LSRUpdateR Activity", style = "font-size: 1.6em; margin-bottom: 5px;"),
        value = div(style = "font-size: 16px;",
                    br(),
                    # Latest activity
                    htmlOutput(ns("latest_activity_content")),
                    br(),
                    # Link to see more items
                    actionLink(ns("more_link"), "See next 5 items."),
                    br(),
                    # Link to activity log
                    actionLink(ns("activity_link"), "See entire activity log here.")
        ),
        showcase = bsicons::bs_icon("activity"),
        showcase_layout = showcase_left_center(width = 0.1),
        theme = "text-success"
      )
    ),
    column(8,
           
      # Threshold settings
      LSRUpdateR_home_thresholds_ui(id = ns("thresholds")),
      
      # Latest data
      htmlOutput(ns("latest_data_content"))
           
    )
  )
  
}

#' Server for the home page for LSRUpdateR page.
#'
#' @param id ID of the module
#' @param LSRUpdateR_session Session from LSRUpdateR page (needed for navbar_select)
#' @param parent_session Session from home page (needed for navbar_select)
#' @param activity_df Reactive dataframe of the activity log
#'
#' @return 
LSRUpdateR_home_server <- function(id, LSRUpdateR_session, parent_session, activity_df) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ### Run LSRUpdateR ###
      #--------------------#
      
      
      
      # Move user to results page
      observeEvent(input$run_LSRUpdateR, {
        bslib::nav_select("LSRUpdateR_navbar", selected = "results", session = LSRUpdateR_session)
      })
      
      
      ### Conduct manual update ###
      #---------------------------#
      
      
      
      # Move user to manual update page
      observeEvent(input$run_update, {
        bslib::nav_select("LSRUpdateR_navbar", selected = "manual_update", session = LSRUpdateR_session)
      })
      
      
      ### Latest LSRUpdateR Activity ###
      #--------------------------------#
      
      # Show activity #
      output$latest_activity_content <- renderText({
        
        # Filter to relevant activities & sort by date
        LSRUpdateR_activity <- activity_df() %>% 
          filter(Category == "LSRUpdateR") %>%
          arrange(desc(Date))
        
        if (nrow(LSRUpdateR_activity) == 0) {
          
          # If no activity, say so
          paste0("There is currently no LSRUpdateR related activity")
          
        } else {
          
          # Calculate number of rows to show and reduce dataframe
          show_rows <- 1 + 5*input$more_link
          displayed_activity <- LSRUpdateR_activity %>% slice_head(n = show_rows)
        
          # Display activity
          paste0("<b>", format(displayed_activity$Date, "%d %b %Y"), "</b>: ", displayed_activity$Details,
            collapse = "<br>"
          )

        }
        
      })
      
      # Take user to activity log #
      observeEvent(input$activity_link, {
        bslib::nav_select("app_navbar", selected = "activity_page", session = parent_session)
      })
      
      
      ### Threshold server ###
      #----------------------#
      
      LSRUpdateR_home_thresholds_server(
        id = "thresholds"
      )
      
    }
  )
}
#' UI for the latest activity section of the home page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
home_latest_activity_ui <- function(id) {
  ns = NS(id)
  div(
    bslib::value_box(
      title = tags$p("Latest Activity", style = "font-size: 1.6em; margin-bottom: 5px;"),
      value = div(style = "font-size: 16px;",
        br(),
        # Latest activity
        htmlOutput(ns("latest_activity_content")),
        br(),
        # Link to activity log
        actionLink(ns("activity_link"), "See entire activity log here")
      ),
      showcase = bsicons::bs_icon("activity"),
      showcase_layout = showcase_left_center(width = 0.1),
      theme = "text-success"
    )
  )
}

#' Server for the latest activity section of the home page.
#'
#' @param id ID of the module
#' @param activity_df Reactive dataframe of the activity log
#' @param parent_session Parameter to pass the app session through to the other modules
#'
#' @return nothing
home_latest_activity_server <- function(id, activity_df, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$latest_activity_content <- renderText({
        
        activity_df <- activity_df()
        
        # Get most recent activity
        activity_df <- activity_df %>% arrange(desc(Date))
        latest_entry <- activity_df[1,]
        
        # Text of most recent activity
        paste0("<b>", format(latest_entry$Date, "%d %b %Y"), "</b>: ", latest_entry$Details, " (", latest_entry$Category, ")")
      })
      
      # Link for going to activity log page
      observeEvent(input$activity_link, {
        bslib::nav_select("app_navbar", selected = "activity_page", session = parent_session)
      })
      
      
    }
  )
}
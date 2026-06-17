#' UI for the activity log page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
activity_page_ui <- function(id) {
  ns = NS(id)
  div(
    # UI components
    textOutput(ns("activity_intro")),
    DT::DTOutput(ns("activity_log"))
  )
}


#' Server for the activity log page.
#'
#' @param id ID of the module
#' @param activity_log Log of activities taken from throughout the app
#'
#' @return nothing yet
activity_page_server <- function(id, activity_log) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Activity log intro
      output$activity_intro <- renderText({
        paste("Below is the activity log. It contains all decisions made and activities conducting whilst using this LivingStudio.")
      })
      
      # Render activity log
      output$activity_log <- DT::renderDT({
        print("Received latest activity")
        print(activity_log())
        df <- activity_log() %>% arrange(desc(Date)) # arrange by default as most recent first
        df$Date <- format(df$Date, "%d %b %Y")
        DT::datatable(df, filter = 'top')
      })
      
    }
  )
}
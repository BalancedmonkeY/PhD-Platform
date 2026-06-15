#' UI for the activity log page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
activity_page_ui <- function(id) {
  ns = NS(id)
  div(
    # UI components
    h5("Below is the activity log. It contains all decisions made and activities conducting whilst using this LivingStudio."),
    DT::DTOutput(ns("activity_log"))
  )
}


#' Server for the activity log page.
#'
#' @param id ID of the module
#'
#' @return nothing yet
activity_page_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Render activity log
      output$activity_log <- DT::renderDT({
        df <- readRDS("activity_log.rds")
        df$Date <- format(df$Date, "%d %b %Y")
        df <- df %>% arrange(desc(Date)) # arrange by default as most recent first
        DT::datatable(df, filter = 'top')
      })
      
    }
  )
}
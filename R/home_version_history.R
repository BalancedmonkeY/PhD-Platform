#' UI for the version history section of the home page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
home_version_history_ui <- function(id) {
  ns = NS(id)
  div(
    bslib::value_box(
      title = tags$p("Version History", style = "font-size: 1.6em; margin-bottom: 5px;"),
      value = uiOutput(ns("version_history_table")),
      showcase = bsicons::bs_icon("clock-history"),
      showcase_layout = showcase_left_center(width = 0.1),
      theme = "text-primary"
    )
  )
}

#' Server for the version history section of the home page.
#'
#' @param id ID of the module
#' @param df Dataframe of the version history
#'
#' @return nothing
home_version_history_server <- function(id, df) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$version_history_table <- renderUI({
        
        df <- df()
        
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
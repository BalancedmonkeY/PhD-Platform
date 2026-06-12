#' UI for the latest data section of the home page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
home_latest_data_ui <- function(id) {
  ns = NS(id)
  div(
    bslib::value_box( 
      title = tags$p("Latest Data", style = "font-size: 1.6em; margin-bottom: 5px;"), 
      value = uiOutput(ns("latest_data_table")),
      showcase = bsicons::bs_icon("clipboard2-data"),
      showcase_layout = showcase_left_center(width = 0.1),
      theme = "text-success" 
    )
  )
}


#' Server for the latest data section of the home page.
#'
#' @param id ID of the module
#' @param df Dataframe of the latest data
#'
#' @return published checkbox input value
home_latest_data_server <- function(id, df) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$latest_data_table <- renderUI({
        
        df <- df()
        
        # Rename columns
        colnames(df) <- c("Search Date", "No. Studies", "Status")
        
        # Reformat dates
        df[["Search Date"]] <- format(df[["Search Date"]], "%d %B %Y")
        
        div(
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
          ),
          
          br(),
          
          # Checkbox for stating new published update
          div(
            style = "font-size: 15px;",
            checkboxInput("new_published", "New Published Update?", value = FALSE, width = '100%')
          )
        )
      })
      
      # Capture result from 'new_published' and return
      new_publication <- reactive({input$new_published})
      return(new_publication)
      
    }
  )
}
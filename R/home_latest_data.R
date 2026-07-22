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
      value = div(
        # Table of latest data
        uiOutput(ns("latest_data_table")),
        # Action button for stating new published update
        div(
          style = "font-size: 15px;",
          actionButton(ns("new_published"), "New Published Update?")
        )),
      showcase = bsicons::bs_icon("clipboard2-data"),
      showcase_layout = showcase_left_center(width = 0.1),
      theme = "text-success" 
    )
  )
}


#' Server for the latest data section of the home page.
#'
#' @param id ID of the module
#' @param demo_date Current date of demo
#' @param activity Reactive activity log
#' @param df Dataframe of the latest data
#' @param non_published_update Reactive dataframe summary of latest non peer-reviewed update
#' @param published Reactive dataframe of all published updates
#'
#' @return published checkbox input value
home_latest_data_server <- function(id, demo_date, activity, df, non_published_update, published) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
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
          
          
        )
      })
      
      # Pop-up window for entering information regarding new published update
      observe({
        showModal(
          modalDialog(
            title = "Add New Published Update",
            footer = modalButton("Cancel"),
            size = "l",
            easy_close = TRUE,
            if (is.null(non_published_update())) {
              p("A non-published update doesn't currently exist")
            } else {
              div(
                h4("Summary Information"),
                paste0("Search date: ", format(non_published_update()$Search_date, "%d-%b-%Y")),
                br(),
                paste0("Number of studies: ", non_published_update()$No_studies),
                br(),
                paste0("Reason for update: ", non_published_update()$Reason),
                br(), br(),
                dateInput(ns("new_published_date"), "Publication Date", value = demo_date()),
                textInput(ns("new_DOI"), "URL", placeholder = "Enter URL for update here..."),
                actionButton(ns("submit_new_published_update"), "Submit Update")
              )
            }
          )
        )
      }) |>
        bindEvent(input$new_published)
      
      # Update datasets once new published update has been entered
      observe({
        
        # Create new published row
        new_published <- reactive({
          data.frame(
            Publish_date = input$new_published_date,
            Search_date = non_published_update()$Search_date,
            No_studies = non_published_update()$No_studies,
            Reason = non_published_update()$Reason,
            DOI = input$new_DOI
          )
        })
        
        # Add new published row to top entry of published dataframe
        new_published_df <- reactive({
          rbind(new_published(), published())
        })
        
        # Clear latest update
        latest_unpublished <- NULL
        
        # Add to activity log
        activity_log <- activity() %>%
          add_row(Date = input$new_published_date, Category = "Publications", Details = "User recorded that latest update has now been published in peer-reviewed journal.")
        activity(activity_log)
        
        # Save new dataframes to reactive values
        published(new_published_df())
        non_published_update(latest_unpublished)
        
        # Close Modal
        removeModal()
        
      }) |>
        bindEvent(input$submit_new_published_update)
      
    }
  )
}
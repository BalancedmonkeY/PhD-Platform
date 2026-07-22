#' UI for the protocol section of the home page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
home_protocol_ui <- function(id) {
  ns = NS(id)
  div(
    bslib::value_box(
      title = tags$p("Protocol", style = "font-size: 1.6em; margin-bottom: 5px;"),
      value = div(style = "font-size: 16px;",
                  br(),
                  # Original Protocol
                  p("The original protocol can be found ",
                    tags$a(href = "https://doi.org/10.1002/14651858.CD010216", "here",target = "_blank")
                  ),
                  br(),
                  # List of changes to protocol
                  p("Since then, the following changes have been made:"),
                  htmlOutput(ns("protocol_changes_content"),
                             style = "font-size: 12px;"),
                  br(),
                  # Option to add changes to protocol
                  div(
                    style = "font-size: 15px;",
                    actionButton(ns("protocol_change_request"), "Change to protocol?")
                  )
      ),
      showcase = bsicons::bs_icon("file-earmark-medical"),
      showcase_layout = showcase_left_center(width = 0.1),
      theme = "text-success"
    )
  )
}

#' Server for the latest activity section of the home page.
#'
#' @param id ID of the module
#' @param demo_date Current date of the demo
#' @param activity Reactive activity log
#' @param protocol_df Dataframe of the protocol changes
#'
#' @return new protocol change input value
home_protocol_server <- function(id, demo_date, activity, protocol_df) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$protocol_changes_content <- renderText({
        
        # Sort protocol details
        protocol_df <- protocol_df()
        protocol_df <- protocol_df %>% arrange(desc(Date))
        
        # Print protocol changes
        paste0("<b>", format(protocol_df$Date, "%b %Y"), "</b>: ", protocol_df$Update, collapse = "<br>")

      })
        
      # Add new updates as needed
      # Pop-up window for entering information regarding new protocol update
      observe({
        showModal(
          modalDialog(
            title = "Add New Protocol Change",
            footer = modalButton("Cancel"),
            size = "l",
            easy_close = TRUE,
            div(
               dateInput(ns("new_protocol_date"), "Protocol Change Date", value = demo_date()),
               textInput(ns("new_protocol_change"), "Details of Change", placeholder = "Please add details of the protocol change(s) here..."),
               actionButton(ns("submit_protocol_update"), "Submit Update")
            )
          )
        )
      }) |>
          bindEvent(input$protocol_change_request)
        
      # Update dataset with new protocol change
      observe({
        # Create new row
        new_protocol_update <- reactive({
          data.frame(
            Date = input$new_protocol_date,
            Update = input$new_protocol_change
          )
        })
        # Add new row to top entry of dataframe
        new_protocol_df <- reactive({
          rbind(new_protocol_update(), protocol_df())
        })
        # Add to activity log
        activity_log <- activity() %>%
          add_row(Date = input$new_protocol_date, Category = "Protocol", Details = paste0("User added a change to the protocol. ", input$new_protocol_change))
        activity(activity_log)
          
        # Save new dataframes and reload app
        saveRDS(new_protocol_df(), "protocol_updates.rds")
        session$reload()
          
        }) |>
          bindEvent(input$submit_protocol_update)
        
    }
  )
}
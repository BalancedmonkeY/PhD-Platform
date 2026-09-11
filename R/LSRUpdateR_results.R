#' UI for the results page of LSRUpdateR page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
LSRUpdateR_results_ui <- function(id) {
  ns = NS(id)
  
  div(
    
    bslib::card(
      card_header("LSRUpdateR Suggestion"),
      card_body(
        uiOutput(ns("overall_text"))
      )
    ),
    
    fluidRow(
      column(4,
             bslib::card(
               card_header("Suggestion Details"),
               card_body(
                 uiOutput(ns("detail_text"))
               )
             )
      ),
      column(8,
             bslib::card(
               card_header(
                 bslib::toolbar(
                   align = "left",
                   bslib::toolbar_input_select(
                     id = ns("comparison_filter"),
                     label = "Comparator",
                     choices = c("NRT", "Non-nicotine EC", "Behavioural or none"),
                     icon = icon("hand-point-right")
                   ),
                   bslib::toolbar_input_select(
                     id = ns("outcome_filter"),
                     label = "Outcome",
                     choices = c("Cessation", "Adverse Events", "Serious Adverse Events"),
                     icon = icon("hand-point-right")
                   )
                  )
                ),
              navset_card_tab(
                 nav_panel("GRADE",
                           "I'll display GRADE results"),
                nav_panel("Point Estimate",
                           "I'll display Update Funnel Plot"),
                id = ns("GRADE_or_Estimate")
              )
             ),
             div(style = "display: flex; flex-direction: row; align-items: center; gap: 20px; margin-bottom: 20px;",
                 actionButton(ns("continue"), label = "Continue", icon = shiny::icon("right-long")),
                 actionButton(ns("cancel"), label = "Cancel", icon = shiny::icon("circle-xmark"))
             )
      )
    )
  )
  
}

#' Server for the results page for LSRUpdateR page.
#'
#' @param id ID of the module
#' @param LSRUpdateR_session Session from LSRUpdateR page (needed for navbar_select)
#' @param LSRUpdateR_results Results object from running LSRUpdateR
#' @param activity_record Beginnings of an activity record from running LSRUpdateR
#'
#' @return 
LSRUpdateR_results_server <- function(id, LSRUpdateR_session, LSRUpdateR_results, activity_record) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ### Ascertain which scenarios LSRUpdateR suggested a update ###
      #-------------------------------------------------------------#
      
      predicts_update <- reactive({
        sapply(
          LSRUpdateR_results(),
          function(x) grepl("LSRUpdateR predicts that the review should be updated for the following reasons.", x$text_result, fixed = TRUE)
        )
      })
      
      ### Fill overall suggestion text ###
      #----------------------------------#
      
      outcome_lookup <- c(cess = "cessation", AE = "adverse events", SAE = "serious adverse events")
      comparator_lookup <- c(NRT = "NRT", `Non-nic` = "non-nicotine ECs", Behavioural = "behavioural support (or none)")
      
      output$overall_text <- renderUI({
        # Extract which scenarios suggested an update
        scenarios <- names(LSRUpdateR_results())[predicts_update()]
        
        # If no updates
        if (length(scenarios) == 0) {
          HTML("LSRUpdateR predicts that the review should <b>not</b> be updated.<br>
               The addition of new studies did not change the GRADE rating or interpretation of point estimate.")
        } else {
        # If updates
          sentences <- sapply(scenarios, function(x) {
            scenario_parts <- strsplit(x, "_")[[1]]
            comparator <- scenario_parts[1]
            outcome <- scenario_parts[2]
            paste0(
              "LSRUpdateR predicts that the review should be updated for the following reasons:<br>Regarding the ", 
              outcome_lookup[outcome], " outcome in the ",
              comparator_lookup[comparator], " vs nicotine ECs comparison:"
            )
          })
          HTML(paste(sentences, collapse = "<br>"))
        }
      })
      
      
      ### Action buttons ###
      #--------------------#
      
      observeEvent(input$continue, {
        bslib::nav_select("LSRUpdateR_navbar", selected = "confirmation", session = LSRUpdateR_session)
      })
      
      observeEvent(input$cancel, {
        bslib::nav_select("LSRUpdateR_navbar", selected = "home", session = LSRUpdateR_session)
      })
      
    }
  )
}
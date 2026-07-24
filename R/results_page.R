#' UI for the results page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
results_page_ui <- function(id) {
  ns = NS(id)
  div(
    bslib::card(
      class = "bg-success bg-opacity-10 text-dark",
      htmlOutput(ns("update_summary_text"))
    ),
    uiOutput(ns("peer_review_warning")),
    bslib::card(
      card_header(
        "Forest Plots",
        bslib::toolbar(
          align = "right",
          bslib::toolbar_input_select(
            id = ns("comparison_filter"),
            label = "Comparator",
            choices = c("NRT", "Non-nicotine EC", "Behavioural or none"),
            icon = icon("filter")
          ),
          bslib::toolbar_input_select(
            id = ns("outcome_filter"),
            label = "Outcome",
            choices = c("Cessation", "Adverse Events", "Serious Adverse Events"),
            icon = icon("filter")
          )
        )
      ),
      card_body(
        plotOutput(ns("forest_plot"))
      )
    )
  )
}


#' Server for the results page.
#'
#' @param id ID of the module.
#' @param data Reactive latest data from LSR
#' @param studies Reactive latest studies from LSR
#' @param latest_data_df Reactive dataframe of information about the latest data
#'
#' @return nothing yet
results_page_server <- function(id, data, studies, latest_data_df) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ### Summarise key information of latest update ###
      #------------------------------------------------#
      
      # Obtain row of latest update
      latest_update <- reactive({
        update_rows <- latest_data_df() %>% 
          filter(stringr::str_detect(Status, "peer-review"))
        update_rows %>% 
          filter(Search_date == max(update_rows$Search_date))
      })
      
      output$update_summary_text <- renderText({
        
        # Extract wanted elements
        no_included_studies <- latest_update()$No_studies
        update_search_date <- format(latest_update()$Search_date, "%d %B %Y")
        published <- stringr::str_detect(latest_update()$Status, "peer-reviewed update")
        
        # Obtain row of data not yet included (if exists)
        row_new_studies <- latest_data_df() %>% 
          filter(stringr::str_detect(Status, "not yet included"))
        
        # Extract wanted elements
        if (nrow(row_new_studies) != 0) {
          no_new_studies <- row_new_studies$No_studies - no_included_studies
        } else {
          no_new_studies <- NA
        }
        
        # Text for latest update #
        update_text <- paste0("These results are based on the latest ", 
                              ifelse(published, "peer-reviewed update", "unpublished update"), 
                              " which includes ", no_included_studies, " studies identified in searches up to ",
                              update_search_date, ".")
        
        # Text for new studies (if exist) #
        if (is.na(no_new_studies)) {
          new_text <- ""
        } else {
          new_text <- paste0("There are ", no_new_studies, 
                             " new study/ies identified that will be included in a future update.")
        }
        
        # HTML collated statements
        paste0(update_text, "<br>", new_text)
        
      })
      
      output$peer_review_warning <- renderUI({
        
        # Only continue if under peer-review
        under_peer_review <- stringr::str_detect(latest_update()$Status, "subject to peer-review")
        req(under_peer_review)
        
        card(
          class = "bg-warning bg-gradient text-center fs-4",
          p("Subject to peer-review")
        )
        
      })
      

      ### Conduct meta-analyses ###
      #---------------------------#
      
      output$forest_plot <- renderPlot({
        
        # Extract search date for latest update #
        update_search_date <- latest_update()$Search_date

        # Filter to chosen dataset
        if (input$outcome_filter == "Cessation") {
          data_df <- data()$cess_df
        } else if (input$outcome_filter == "Adverse Events") {
          data_df <- data()$AE_df
        } else if (input$outcome_filter == "Serious Adverse Events") {
          data_df <- data()$SAE_df
        }
        data_df <- data_df %>% filter(Comparator == input$comparison_filter) %>%
          filter(Included_Date <= update_search_date & (is.na(Excluded_Date) | Excluded_Date > update_search_date))

        # Run meta-analysis
        MA_obj <- metafor::rma.mh(ai = Experimental_cases, ci = Control_cases, n1i = Experimental_N, n2i = Control_N, slab = Study, data = data_df, 
                        measure = "RR", drop00 = c(TRUE, TRUE), add = c(0.5, 0.5), to = c("only0", "only0"))
        
        # Create forest plot
        metafor::forest(MA_obj, atransf = exp,
                        xlim = c(-8, 5), alim = c(log(0.25), log(8)), at = c(log(0.25), log(0.5), log(1), log(2), log(4), log(8)), shade = TRUE,
                        ilab = cbind(Experimental_cases, Experimental_N, Control_cases, Control_N), ilab.xpos = c(-5.75, -4.75, -3.25, -2.25),
                        ilab.lab = c("Events", "Total", "Events", "Total"), cex = 1.25)
        text(c(-5.25, -2.75), MA_obj$k + 2.8, c("EC", input$comparison_filter), font = 2, cex = 1.3)
  
      })
      
    }
  )
}
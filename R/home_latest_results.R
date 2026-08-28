#' UI for the latest results section of the home page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
home_latest_results_ui <- function(id) {
  ns = NS(id)
  div(
    bslib::value_box(
      title = tags$p("Latest Results", style = "font-size: 1.6em; margin-bottom: 5px;"),
      value = div(style = "font-size: 16px;",
                  br(),
                  htmlOutput(ns("latest_results_content"))
              ),
      showcase = bsicons::bs_icon("graph-up-arrow"),
      showcase_layout = showcase_left_center(width = 0.1),
      theme = "text-primary"
    )
  )
}

#' Server for the latest activity section of the home page.
#'
#' @param id ID of the module
#' @param results_summary Reactive dataframe of the summary of results
#' @param latest_data_df Reactive dataframe of the latest data
#'
#' @return nothing
home_latest_results_server <- function(id, results_summary, latest_data_df) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$latest_results_content <- renderText({
        
        ## Intro text ##
        
        # Obtain row of latest update
        update_rows <- latest_data_df() %>% 
            filter(stringr::str_detect(Status, "peer-review"))
        latest_update <- update_rows %>% 
            filter(Search_date == max(update_rows$Search_date))
        
        # Extract wanted elements
        no_included_studies <- latest_update$No_studies
        update_search_date <- format(latest_update$Search_date, "%d %B %Y")
        published <- stringr::str_detect(latest_update$Status, "peer-reviewed update")
          
        # Text for latest update #
        update_text <- paste0("These results are based on the latest ", 
                                ifelse(published, "peer-reviewed update", "unpublished update"), 
                                " which includes <u>", no_included_studies, "</u> studies identified in searches up to <u>",
                                update_search_date, "</u>.")
        
        ## Results for each outcome/comparator ##
        
        results_summary <- results_summary()
        
        # Add conclusion text to table
        results_summary$conclusion <- mapply(
          cochrane_conclusion,
          effect = results_summary$RR_clin,
          certainty = results_summary$GRADE,
          outcome = results_summary$Outcome,
          intervention = "nicotine EC"
        )
        
        # Each comparator text
        NRT_text <- paste(results_summary %>% filter(Comparator == "NRT") %>% pull(conclusion), 
                          collapse = "<br>")
        Non_nicotine_text <- paste(results_summary %>% filter(Comparator == "non-nicotine EC") %>% pull(conclusion), 
                          collapse = "<br>")
        Behavioural_text <- paste(results_summary %>% filter(Comparator == "behavioural or none") %>% pull(conclusion), 
                          collapse = "<br>")
        

        ## Complete text ##
        paste0(update_text, "<br><br><b>NRT</b><br>", 
               NRT_text, "<br><br><b>Non-nicotine EC</b><br>",
               Non_nicotine_text, "<br><br><b>Behavioural or no support</b><br>",
               Behavioural_text)
        
      })
      
    }
  )
}
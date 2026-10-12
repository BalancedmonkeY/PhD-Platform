#' UI for the home page of LSRUpdateR page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
LSRUpdateR_home_ui <- function(id) {
  ns = NS(id)
  
  fluidRow(
    column(4, 
           
      # Update buttons
      div(style = "display: flex; flex-direction: column; align-items: center; gap: 20px; margin-bottom: 20px;",
        actionButton(ns("run_LSRUpdateR"), label = "Run LSRUpdateR", icon = shiny::icon("play")),
        actionButton(ns("run_update"), label = "Conduct Update", icon = shiny::icon("paper-plane"))
      ),
      
      # Latest activity
      bslib::value_box(
        title = tags$p("Latest LSRUpdateR Activity", style = "font-size: 1.6em; margin-bottom: 5px;"),
        value = div(style = "font-size: 16px;",
                    br(),
                    # Latest activity
                    htmlOutput(ns("latest_activity_content")),
                    br(),
                    # Link to see more items
                    actionLink(ns("more_link"), "See next 5 items."),
                    br(),
                    # Link to activity log
                    actionLink(ns("activity_link"), "See entire activity log here.")
        ),
        showcase = bsicons::bs_icon("activity"),
        showcase_layout = showcase_left_center(width = 0.1),
        theme = "text-primary"
      )
    ),
    column(8,
           
      # Threshold settings
      div(style = "margin-bottom: 20px;",
        LSRUpdateR_home_thresholds_ui(id = ns("thresholds"))
      ),
      
      # Latest data
      bslib::value_box( 
        title = tags$p("Latest Data", style = "font-size: 1.6em; margin-bottom: 5px;"), 
        value = div(
          # Table of latest data
          uiOutput(ns("latest_data_content"))),
        showcase = bsicons::bs_icon("clipboard2-data"),
        showcase_layout = showcase_left_center(width = 0.1),
        theme = "text-primary" 
      )
           
    )
  )
  
}

#' Server for the home page for LSRUpdateR page.
#'
#' @param id ID of the module
#' @param LSRUpdateR_session Session from LSRUpdateR page (needed for navbar_select)
#' @param parent_session Session from home page (needed for navbar_select)
#' @param activity_df Reactive dataframe of the activity log
#' @param latest_data_df Reactive dataframe of summarised latest data
#' @param data_object Data object from data page containing reactive objects (includes datasets, last search date, and unique studies)
#' @param results_summary Reactive dataframe of summary of results
#'
#' @return 
#' thresholds - Reactive dataframe of threshold currently used
LSRUpdateR_home_server <- function(id, LSRUpdateR_session, parent_session, activity_df, latest_data_df, data_object, results_summary) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ### Initialise empty objects ###
      #------------------------------#
      
      LSRUpdateR_results <- reactiveVal(NULL)
      
      
      ### Threshold server ###
      #----------------------#
      
      thresholds <- LSRUpdateR_home_thresholds_server(
        id = "thresholds"
      )
      
      
      ### Run LSRUpdateR ###
      #--------------------#
      
      observe({
        
        # Check whether there are any new studies - if not, notify the user and return to page
        if (latest_data_df()$No_studies[1] == latest_data_df()$No_studies[2]) {
          
          showModal(
            modalDialog(
              title = "No New Studies",
              footer = modalButton("Understand"),
              size = "l",
              easyClose = TRUE,
              div(
                h5("No new studies have been identified since the last update. Therefore there is no need to run LSRUpdateR.")
              )
            )
          )
          
        } else {
          
          # Start spinner
          shinycssloaders::showPageSpinner(type = 7, size = 1.5, caption = "Running LSRUpdateR")
        
          # Import LSRUpdateR
          source("C:/Users/crn4/OneDrive - University of Leicester/PhD/WP2/3. Create tool/UpdatePredictor.R")
      
          # Unpack data objects
          data <- data_object$data()
          most_recent_search_date <- max(latest_data_df()$Search_date)
          last_update_search_date <- latest_data_df() %>% filter(stringr::str_detect(Status, "update")) %>% pull(Search_date) %>% max()
        
          # Remove any expired studies
          if(nrow(filter(data$cess_df, Excluded_Date < most_recent_search_date)) != 0) {
            data$cess_df <- data$cess_df %>%
              filter(Excluded_Date >= most_recent_search_date | is.na(Excluded_Date))
          }
          if(nrow(filter(data$AE_df, Excluded_Date < most_recent_search_date)) != 0) {
            data$AE_df <- data$AE_df %>%
              filter(Excluded_Date >= most_recent_search_date | is.na(Excluded_Date))
          }
          if(nrow(filter(data$SAE_df, Excluded_Date < most_recent_search_date)) != 0) {
            data$SAE_df <- data$SAE_df %>%
              filter(Excluded_Date >= most_recent_search_date | is.na(Excluded_Date))
          }
        
          # Create combination grid (i.e., all 9 scenarios)
          combinations <- expand.grid(
            dataset = c("cess_df", "AE_df", "SAE_df"),
            comparator = c("NRT", "Non-nicotine EC", "Behavioural or none")
          )
          combinations$outcome <- rep(c("Cessation", "Adverse Events", "Serious Adverse Events"), 3)
          combinations <- combinations %>%
            mutate(
              dataset_short = sub("_df$", "", dataset),
              comparator_short = case_when(
                comparator == "NRT" ~ "NRT",
                comparator == "Non-nicotine EC" ~ "Non-nic",
                comparator == "Behavioural or none" ~ "Behavioural"
              ),
            result_name = paste(comparator_short, dataset_short, sep = "_")
            )
        
          # Run LSRUpdateR for each scenario
          LSRUpdateR_results <- purrr::pmap(
            combinations,
            ~ run_LSRUpdateR(df_name = ..1, comparator = ..2, outcome = ..3,
                             last_update_search_date, thresholds(), results_summary())
          )
        
          # Assign names to named list
          names(LSRUpdateR_results) <- combinations$result_name
        
          LSRUpdateR_results(LSRUpdateR_results)
        
          # End spinner
          shinycssloaders::hidePageSpinner()
          
          # Move to results page
          bslib::nav_select("LSRUpdateR_navbar", selected = "results", session = LSRUpdateR_session)
          
        }
        
      }) |>
        bindEvent(input$run_LSRUpdateR)
      
      
      
      ### Conduct manual update ###
      #---------------------------#
      
      
      
      # Move user to manual update page
      observeEvent(input$run_update, {
        bslib::nav_select("LSRUpdateR_navbar", selected = "manual_update", session = LSRUpdateR_session)
      })
      
      
      ### Latest LSRUpdateR Activity ###
      #--------------------------------#
      
      # Show activity #
      output$latest_activity_content <- renderText({
        
        # Filter to relevant activities & sort by date
        LSRUpdateR_activity <- activity_df() %>% 
          filter(Category == "LSRUpdateR") %>%
          arrange(desc(Date))
        
        if (nrow(LSRUpdateR_activity) == 0) {
          
          # If no activity, say so
          paste0("There is currently no LSRUpdateR related activity")
          
        } else {
          
          # Calculate number of rows to show and reduce dataframe
          show_rows <- 1 + 5*input$more_link
          displayed_activity <- LSRUpdateR_activity %>% slice_head(n = show_rows)
        
          # Display activity
          paste0("<b>", format(displayed_activity$Date, "%d %b %Y"), "</b>: ", displayed_activity$Details,
            collapse = "<br>"
          )
        }
      })
      
      # Take user to activity log #
      observeEvent(input$activity_link, {
        bslib::nav_select("app_navbar", selected = "activity_page", session = parent_session)
      })
      
      
      
      ### Latest Data Summary ###
      #-------------------------#
      
      output$latest_data_content <- renderUI({
        
        df <- latest_data_df()
        
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
          )
        )
        
      })
      
      
      ## Return objects needed for other pages
      return(list(
        LSRUpdateR_results = LSRUpdateR_results,
        thresholds = thresholds,
        data = data
      ))
      
    }
  )
}
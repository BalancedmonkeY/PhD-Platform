#' UI for the confirmation page of LSRUpdateR page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
LSRUpdateR_confirmation_ui <- function(id) {
  ns = NS(id)
  
  div(
    fluidRow(
      column(6, h4("Current Data Summary"), dataTableOutput(ns("current_data_table"))),
      column(6, h4("Proposed New Data Summary"), dataTableOutput(ns("new_data_table")))
      ),
    uiOutput(ns("reason_text")),
    div(style = "float: right;",
      actionButton(ns("finish"), label = "Complete Update", class = "btn-success", icon = shiny::icon("circle-check"))
    )
  )
  
}

#' Server for the confirmation page for LSRUpdateR page.
#'
#' @param id ID of the module
#' @param LSRUpdateR_session Session from LSRUpdateR page (needed for navbar_select)
#' @param active_page Which page is active within LSRUpdateR page
#' @param LSRUpdateR_results Results from running LSRUpdateR function
#' @param activity The latest activity log
#' @param suggested_scenarios List of which scenarios led to an update being suggested
#' @param results_summary Reactive dataframe of summary of results
#' @param thresholds Reactive dataframe of currently chosen thresholds
#'
#' @return 
LSRUpdateR_confirmation_server <- function(id, LSRUpdateR_session, active_page, LSRUpdateR_results, activity, suggested_scenarios, results_summary, thresholds) {
  moduleServer(
    id,
    function(input, output, session) {

      
      ### Create new results_summary object from LSRUpdateR results ###
      #---------------------------------------------------------------#
      
      # Filter lookups and keys
      outcome_lookup <- c(cess = "Cessation", AE = "Adverse Events", SAE = "Serious Adverse Events")
      comparator_lookup <- c(NRT = "NRT", `Non-nic` = "Non-nicotine EC", Behavioural = "Behavioural or none")
      
      proposed_new_results <- reactiveVal()
      
      # Move details from LSRUpdateR results to format of results summary
      observe({
        req(active_page() == "confirmation")
        
        # Initialise dataframe
        initial_df <- results_summary() %>% select("Outcome", "Comparator")
        initial_df <- initial_df %>% mutate(GRADE = NA_character_, RoB = NA_integer_, Imprecision = NA_integer_, Inconsistency = NA_integer_, 
                                             Indirectness = NA_integer_, Pubbias = NA_integer_, RR = NA_real_, RR_clin = NA_character_)
        
        # Take out of reactive
        results_list <- LSRUpdateR_results()
        
        # For loop to go through each result and add the relevant data
        for (scenario_name in names(results_list)) {
          
          # Extract the individual result
          x <- results_list[[scenario_name]]
          
          # Split item name into components 
          name_parts <- strsplit(scenario_name, "_")[[1]]
          comparator <- name_parts[1]
          outcome <- name_parts[2]
          
          # Identify relevant row
          comparator_label <- unname(comparator_lookup[comparator])
          outcome_label <- unname(outcome_lookup[outcome])
          row_index <- initial_df$Outcome == outcome_label &
            initial_df$Comparator == comparator_label
          
          # Assign GRADE elements
          initial_df$GRADE[row_index] <- tolower(x$GRADE_results$result)
          initial_df$RoB[row_index] <- x$GRADE_results$RoB
          initial_df$Imprecision[row_index] <- x$GRADE_results$Imprecision
          initial_df$Inconsistency[row_index] <- x$GRADE_results$Inconsistency
          initial_df$Indirectness[row_index] <- x$GRADE_results$Indirectness
          initial_df$Pubbias[row_index] <- x$GRADE_results$Pubbias
          
          # Assign estimate
          RR <- as.numeric(exp(x$pooled_results$new_ma$b))
          initial_df$RR[row_index] <- round(RR, 2)
          
          # Obtain clinical thresholds for this scenario
          sub_thresholds <- thresholds() %>% filter(Comparator == comparator_label & Outcome == outcome_label)
          clin_thresholds <- sort(c(sub_thresholds$Value[sub_thresholds$Input == 'pos_clin'],
                                    sub_thresholds$Value[sub_thresholds$Input == 'neg_clin']))
          
          # Add clinical interpretation
          RR_clin <- if (RR > clin_thresholds[2]) {"increase"} else if (RR < clin_thresholds[1]) {"decrease"} else {"no_difference"}
          initial_df$RR_clin[row_index] <- RR_clin
          
        }
        
       proposed_new_results(initial_df)
        
      }) |> bindEvent(active_page())
      
      
      ### Identify changes ###
      #----------------------#
      
      old_and_new <- reactive({
        
        # Obtain existing results
        old_results <- results_summary() %>%
          dplyr::transmute(Outcome, Comparator, Old_GRADE = stringr::str_to_title(GRADE),
                           Old_interpretation = RR_clin)
        
        # Obtain new results
        new_results <- proposed_new_results() %>%
          dplyr::transmute(Outcome, Comparator, New_GRADE = stringr::str_to_title(GRADE),
                           New_interpretation = RR_clin)
        
        # Join together
        old_results %>% 
          dplyr::inner_join(new_results, by = c("Outcome", "Comparator"))
        
      })
      
      bold_combinations <- reactive({
        
        old_and_new() %>%
          filter((!is.na(Old_GRADE) & !is.na(New_GRADE) & Old_GRADE != New_GRADE) |
                   (!is.na(Old_interpretation) & !is.na(New_interpretation) & Old_interpretation != New_interpretation)) %>%
          select(c("Outcome", "Comparator"))
        
      })
      
      
      ### Present current data ###
      #--------------------------#
      
      output$current_data_table <- renderDataTable({
        reduced_table <- results_summary() %>% select(c("Outcome", "Comparator", "RR", "GRADE"))
        bold_rows <- which(
          paste(reduced_table$Outcome, reduced_table$Comparator) %in%
            paste(bold_combinations()$Outcome, bold_combinations()$Comparator)
        )
        DT::datatable(reduced_table, class = list(stripe = FALSE), rownames = FALSE,
                      options = list(info = FALSE,
                                     paging = FALSE,
                                     searching = FALSE,
                                     ordering = FALSE)
                      ) |>
          # Colour rows by GRADE rating
          DT::formatStyle(
            columns = names(reduced_table),
            valueColumns = "GRADE",
            backgroundColor = DT::styleEqual(
              c("high", "moderate", "low", "very low"),
              c("var(--bs-success-bg-subtle)",
                "var(--bs-warning-bg-subtle)",
                "var(--bs-danger-bg-subtle)",
                "var(--bs-dark-bg-subtle)")
            )
          ) |>
          # Bolden rows that changed
          DT::formatStyle(
            columns = names(reduced_table),
            fontWeight = DT::styleRow(
              rows = bold_rows,
              values = "bold"
            )
          )
      })
      
      
      ### Present new results ###
      #-------------------------#
      
      output$new_data_table <- renderDataTable({
        reduced_table <- proposed_new_results() %>% select(c("Outcome", "Comparator", "RR", "GRADE"))
        bold_rows <- which(
          paste(reduced_table$Outcome, reduced_table$Comparator) %in%
            paste(bold_combinations()$Outcome, bold_combinations()$Comparator)
        )
        DT::datatable(reduced_table, class = list(stripe = FALSE), rownames = FALSE,
                      options = list(info = FALSE,
                                     paging = FALSE,
                                     searching = FALSE,
                                     ordering = FALSE)
                      )|>
          # Colour rows by GRADE rating
          DT::formatStyle(
            columns = names(reduced_table),
            valueColumns = "GRADE",
            backgroundColor = DT::styleEqual(
              c("high", "moderate", "low", "very low"),
              c("var(--bs-success-bg-subtle)",
                "var(--bs-warning-bg-subtle)",
                "var(--bs-danger-bg-subtle)",
                "var(--bs-dark-bg-subtle)")
            )
          ) |>
          # Bolden rows that changed
          DT::formatStyle(
            columns = names(reduced_table),
            fontWeight = DT::styleRow(
              rows = bold_rows,
              values = "bold"
            )
          )
      })
      
      
      ### Reason for change text ###
      #----------------------------#
      
      update_text <- reactive({
        
        # Identify changes
        grade_changes <- old_and_new() %>%
          filter(!is.na(Old_GRADE), !is.na(New_GRADE), Old_GRADE != New_GRADE)
        interpretation_changes <- old_and_new() %>%
          filter(!is.na(Old_interpretation), !is.na(New_interpretation), Old_interpretation != New_interpretation)
        
        # Create text
        interpretation_changes <- interpretation_changes %>%
          dplyr::mutate(
            Old_interpretation = case_when(
              Old_interpretation == "no_difference" ~ "no difference",
              Outcome == "Cessation" & Old_interpretation == "increase" ~ "beneficial",
              Outcome == "Cessation" & Old_interpretation == "decrease" ~ "harmful",
              Outcome %in% c("Adverse Events", "Serious Adverse Events") & Old_interpretation == "increase" ~ "harmful",
              Outcome %in% c("Adverse Events", "Serious Adverse Events") & Old_interpretation == "decrease" ~ "beneficial",
            ),
            New_interpretation = case_when(
              New_interpretation == "no_difference" ~ "no difference",
              Outcome == "Cessation" & New_interpretation == "increase" ~ "beneficial",
              Outcome == "Cessation" & New_interpretation == "decrease" ~ "harmful",
              Outcome %in% c("Adverse Events", "Serious Adverse Events") & New_interpretation == "increase" ~ "harmful",
              Outcome %in% c("Adverse Events", "Serious Adverse Events") & New_interpretation == "decrease" ~ "beneficial",
            )
          )
        grade_changed <- nrow(grade_changes) > 0
        interpretation_changed <- nrow(interpretation_changes) > 0
        change_types <- c(
          if (grade_changed) "GRADE rating",
          if (interpretation_changed) "interpretation"
        )
        opening_sentence <- paste0(
          "Change in ",
          join_words(change_types),
          "."
        )
        grade_sentence <- if (grade_changed) {
          make_grade_reason(grade_changes)
        } else {
          NULL
        }
        interpretation_sentence <- if (interpretation_changed) {
          make_interpretation_reason(interpretation_changes)
        } else {
          NULL
        }
        paste(
          c(
            opening_sentence,
            grade_sentence,
            interpretation_sentence
          ),
          collapse = " "
        )
      })
      
      output$reason_text <- renderUI({
        div(
          bslib::value_box(
            title = tags$span("Reason for update", style = "font-size: 22px; margin-bottom: 3px;"),
            value = tags$span(update_text(), style = "font-size: 16px;"),
            showcase = bsicons::bs_icon("pencil-square"),
            showcase_layout = showcase_left_center(width = 0.1),
            theme = "info"
          )
        )
      })
      
      
      ### Confirm and update dataframes and activity logs ###
      #-----------------------------------------------------#
      
      observe({
        
        # Update results_summary
        results_summary(proposed_new_results())
        
        # Return home
        bslib::nav_select("LSRUpdateR_navbar", selected = "home", session = LSRUpdateR_session)
        
      }) |> bindEvent(input$finish)
      
    }
  )
}
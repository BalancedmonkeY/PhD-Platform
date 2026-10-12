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
                  fluidRow(
                    column(4,
                           h5("Suggestion Details"),
                           uiOutput(ns("detail_text_GRADE"))
                          ),
                    column(8,
                           bslib::accordion(
                             accordion_panel("Risk of Bias", value = "RoB", htmlOutput(ns("RoB_details"))),
                             accordion_panel("Imprecision", value = "imprecision", htmlOutput(ns("imprecision_details"))),
                             accordion_panel("Inconsistency", value = "inconsistency", htmlOutput(ns("inconsistency_details"))),
                             accordion_panel("Publication Bias", value = "pubbias", htmlOutput(ns("pubbias_details"))),
                             accordion_panel("Indirectness", value = "indirectness", htmlOutput(ns("indirectness_details"))),
                             id = ns("GRADE_domains"),
                             open = FALSE
                           ))
                    )
                  ),
        nav_panel("Point Estimate",
                  fluidRow(
                    column(4,
                           h5("Suggestion Details"),
                           uiOutput(ns("detail_text_estimate"))
                          ),
                    column(8,
                           h5("Update Funnel Plot"),
                           plotOutput(ns("funnel_plot")))
                    )
                  ),
        id = ns("GRADE_or_Estimate")
      )
    ),
    
    div(style = "display: flex; flex-direction: row; align-items: center; gap: 20px; margin-bottom: 20px;",
        uiOutput(ns("action_buttons"))
    )
  )
  
}

#' Server for the results page for LSRUpdateR page.
#'
#' @param id ID of the module
#' @param LSRUpdateR_session Session from LSRUpdateR page (needed for navbar_select)
#' @param active_page Reactive indicator of which navpage LSRUpdateR tab is currently on
#' @param LSRUpdateR_results Results object from running LSRUpdateR
#' @param activity ReactiveVal of the entire current activity log
#' @param thresholds Reactive dataframe of currently chosen thresholds
#' @param latest_data_df Reactive dataframe of summarised latest data
#' @param cleaned_data Datasets with old studies excluded
#' @param results_summary Reactive dataframe that summarises latest results
#' @param demo_date The current date for the demo
#'
#' @return 
LSRUpdateR_results_server <- function(id, LSRUpdateR_session, active_page, LSRUpdateR_results, activity, thresholds, latest_data_df, cleaned_data, results_summary, demo_date) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns

      
      ### Ascertain which scenarios LSRUpdateR suggested a update ###
      #-------------------------------------------------------------#
      
      predicts_update <- reactive({
        sapply(
          LSRUpdateR_results(),
          function(x) grepl("LSRUpdateR predicts that the review should be updated for the following reasons.", x$text_result, fixed = TRUE)
        )
      })
      
      ### Create filter keys and indicators of suggested updates ###
      #------------------------------------------------------------#
      
      # Quantitative information regarding current update
      base_quant_info <- reactiveVal(readRDS("Saved_dfs/quant_results.rds"))
      
      # Initialising reactiveVals
      filter_key <- reactiveVal()
      suggested_scenarios <- reactiveVal()
      
      observe({
        req(active_page() == "results")
        
        # Create filter key
        outcome_filter_lookup <- c(Cessation = "cess", `Adverse Events` = "AE", `Serious Adverse Events` = "SAE")
        comparator_filter_lookup <- c(NRT = "NRT", `Non-nicotine EC` = "Non-nic", `Behavioural or none` = "Behavioural")
        
        filter_key(paste0(comparator_filter_lookup[input$comparison_filter], "_", outcome_filter_lookup[input$outcome_filter]))
        
        # Extract which scenarios suggested an update
        suggested_scenarios(names(LSRUpdateR_results())[predicts_update()])
        
        # # Create combination grid (i.e., all 9 scenarios)
        # combinations <- expand.grid(
        #   dataset = c("cess_df", "AE_df", "SAE_df"),
        #   comparator = c("NRT", "Non-nicotine EC", "Behavioural or none")
        # )
        # combinations$outcome <- rep(c("Cessation", "Adverse Events", "Serious Adverse Events"), 3)
        # combinations <- combinations %>%
        #   mutate(
        #     dataset_short = sub("_df$", "", dataset),
        #     comparator_short = case_when(
        #       comparator == "NRT" ~ "NRT",
        #       comparator == "Non-nicotine EC" ~ "Non-nic",
        #       comparator == "Behavioural or none" ~ "Behavioural"
        #     ),
        #     result_name = paste(comparator_short, dataset_short, sep = "_")
        #   )
        # 
        # # Run function that calculates quantitative results for each scenario
        # quant_results <- purrr::pmap(
        #   combinations,
        #   ~ run_quant_info(df_name = ..1, comparator = ..2, outcome = ..3,
        #                    data_obj = data_og, thresholds_df = thresholds())
        # )
        
      }) |>
        bindEvent(c(active_page(), input$outcome_filter, input$comparison_filter))
      
      
      ### Fill overall suggestion text ###
      #----------------------------------#
      
      outcome_lookup <- c(cess = "cessation", AE = "adverse events", SAE = "serious adverse events")
      comparator_lookup <- c(NRT = "NRT", `Non-nic` = "non-nicotine ECs", Behavioural = "behavioural support (or none)")
      
      output$overall_text <- renderUI({
        
        # If no updates
        if (length(suggested_scenarios()) == 0) {
          HTML("LSRUpdateR predicts that the review should <b>not</b> be updated.<br>
               The addition of new studies did not change the GRADE rating or interpretation of point estimate.")
        } else {
        # If updates
          sentences <- sapply(suggested_scenarios(), function(x) {
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
      

      
      ### Auto present tabs where there are changes ###
      #-----------------------------------------------#
      
      # Where there is no suggested update, at least present outcome-comparator that had some new data
      
      
      ### Fill GRADE details ###
      #------------------------#
      
     # Filter datasets #
      viewed_new_df <- reactive({
        LSRUpdateR_results()[[filter_key()]] 
      })
      
      viewed_old_df <- reactive({
        base_quant_info()[[filter_key()]]
      })
      
      viewed_thresholds <- reactive({
        thresholds() %>% filter(Comparator == input$comparison_filter & Outcome == input$outcome_filter)
      })
      
      viewed_summaries <- reactive({
        results_summary() %>% filter(Comparator == input$comparison_filter & Outcome == input$outcome_filter)
      })
      
      # Risk of bias #
      
      output$RoB_details <- renderUI({
        
        RoB_avg_old <- viewed_old_df()$RoB_avg
        RoB_avg_new <- viewed_new_df()$GRADE_results$RoB_avg
        RoB_one <- viewed_thresholds()$Value[viewed_thresholds()$Input == 'RoB_one_level']
        RoB_two <- viewed_thresholds()$Value[viewed_thresholds()$Input == 'RoB_two_level']
        
        HTML(paste0(
        "<table style='width:100%'>
          <tr>
            <th colspan = '2'>Weighted average RoB score</th>
          </tr>
          <tr>
            <th style='width:50%'>Before</th>
            <th>Now</th>
          </tr>
          <tr>
            <td>", round(RoB_avg_old, 2), "</td>
            <td>", round(RoB_avg_new, 2), "</td>
          </tr>
          <tr>
            <td colspan = '2' style='font-size: 80%;'><i>Based on thresholds of ", RoB_one," and ", RoB_two," to downgrade 1 or 2 levels respectively</i></td>
          </tr>
        </table>"))
        
      })
      
      # Imprecision #
      
      output$imprecision_details <- renderUI({
        
        events_old <- viewed_old_df()$events
        events_new <- viewed_new_df()$GRADE_results$events
        
        CI_old <- viewed_old_df()$CI
        CI_new <- viewed_new_df()$GRADE_results$CI
        
        clin_no_old <- viewed_old_df()$interp_no
        clin_no_new <- viewed_new_df()$GRADE_results$interp_no

        events_one <- viewed_thresholds()$Value[viewed_thresholds()$Input == 'events_one_level']
        events_two <- viewed_thresholds()$Value[viewed_thresholds()$Input == 'events_two_level']
        
        region_upper <- viewed_thresholds()$Value[viewed_thresholds()$Input == 'region_upper_bin']
        region_lower <- viewed_thresholds()$Value[viewed_thresholds()$Input == 'region_lower_bin']
        
        HTML(paste0(
          "<table style='width:100%'>
          <tr>
            <th colspan = '2'>Total number of events</th>
            <th colspan = '2'>Confidence interval</th>
            <th colspan = '2'>Number of clinical interpretations</th>
          </tr>
          <tr>
            <th style='width:15%'>Before</th><th style='width:15%'>Now</th>
            <th style='width:20%'>Before</th><th style='width:20%'>Now</th>
            <th style='width:15%'>Before</th><th style='width:15%'>Now</th>
          </tr>
          <tr>
            <td>", events_old, "</td>
            <td>", events_new, "</td>
            <td>", CI_old, "</td>
            <td>", CI_new, "</td>
            <td>", clin_no_old, "</td>
            <td>", clin_no_new, "</td>
          </tr>
          <tr>
            <td colspan = '2' style='font-size: 80%;'><i>Based on thresholds of ", events_one," and ", events_two," events to downgrade 1 or 2 levels respectively</i></td>
            <td colspan = '4' style='font-size: 80%;'><i>Regions of clinical benefit/harm and no clinical effects were created from values of ", region_lower, " and ", region_upper, ". Evidence was downgraded 1 or 2 levels if the 95% CI covered 2 or 3 regions.</i></td>
          </tr>
        </table>"))
        
      })
      
      # Inconsistency #
      
      output$inconsistency_details <- renderUI({
        
        var_est_old <- viewed_old_df()$var_est
        var_est_new <- viewed_new_df()$GRADE_results$var_est
        
        Jaccard_old <- viewed_old_df()$Jaccard
        Jaccard_new <- viewed_new_df()$GRADE_results$Jaccard
        
        variation_thresh <- viewed_thresholds()$Value[viewed_thresholds()$Input == 'variation_estimates']
        point_est_thresh <- viewed_thresholds()$Value[viewed_thresholds()$Input == 'pe_threshold']
        Jaccard_thresh <- viewed_thresholds()$Value[viewed_thresholds()$Input == 'Jaccard']
        
        HTML(paste0(
          "<table style='width:100%'>
          <tr>
            <th colspan = '2'>Variation of estimates</th>
            <th colspan = '2'>Jaccard Index</th>
          </tr>
          <tr>
            <th style='width:25%'>Before</th><th style='width:25%'>Now</th>
            <th style='width:25%'>Before</th><th style='width:25%'>Now</th>
          </tr>
          <tr>
            <td>", round(var_est_old, 2)*100, "%</td>
            <td>", round(var_est_new, 2)*100, "%</td>
            <td>", round(Jaccard_old, 2), "</td>
            <td>", round(Jaccard_new, 2), "</td>
          </tr>
          <tr>
            <td colspan = '2' style='font-size: 80%;'><i>Proportion of point estimates that are on one side of the value of ", point_est_thresh, ". Do not downgrade if the proportion is ", as.numeric(variation_thresh)*100, "% or above, otherwise the average pairwise Jaccard index was considered.</i></td>
            <td colspan = '2' style='font-size: 80%;'><i>If the average pairwise Jaccard index needs consulting, downgrade 1 level based on threshold of ", Jaccard_thresh, " or lower.</i></td>
          </tr>
        </table>"))
        
      })
      
      # Publication bias #
      
      output$pubbias_details <- renderUI({
        
        studies_old <- viewed_old_df()$studies_no
        studies_new <- viewed_new_df()$GRADE_results$studies_no
        
        Eggers_old <- viewed_old_df()$Eggers
        Eggers_new <- viewed_new_df()$GRADE_results$Eggers
        if (Eggers_new == 1) {Eggers_new <- NA}
        
        industry_old <- viewed_old_df()$industry
        industry_new <- viewed_thresholds()$Value[viewed_thresholds()$Input == 'industry']
        
        search_old <- viewed_old_df()$search
        search_new <- viewed_thresholds()$Value[viewed_thresholds()$Input == 'search']
        
        no_studies_thresh <- viewed_thresholds()$Value[viewed_thresholds()$Input == 'min_studies']
        Eggers_thresh <- viewed_thresholds()$Value[viewed_thresholds()$Input == 'Eggers_pvalue']
        
        HTML(paste0(
          "<table style='width:100%'>
          <tr>
            <th colspan = '2'>Number of studies</th>
            <th colspan = '2'>Egger's test p-value</th>
            <th colspan = '2'>Industry influence</th>
            <th colspan = '2'>Search integrity concerns</th>
          </tr>
          <tr>
            <th style='width:15%'>Before</th><th style='width:15%'>Now</th>
            <th style='width:15%'>Before</th><th style='width:15%'>Now</th>
            <th style='width:10%'>Before</th><th style='width:10%'>Now</th>
            <th style='width:10%'>Before</th><th style='width:10%'>Now</th>
          </tr>
          <tr>
            <td>", studies_old, "</td>
            <td>", studies_new, "</td>
            <td>", round(Eggers_old, 2), "</td>
            <td>", round(Eggers_new, 2), "</td>
            <td>", industry_old, "</td>
            <td>", industry_new, "</td>
            <td>", search_old, "</td>
            <td>", search_new, "</td
          </tr>
          <tr>
            <td colspan = '4' style='font-size: 80%;'><i>The Egger's test for indication of publication bias was only conducted is there were at least ", no_studies_thresh, " studies. A threshold of ", as.numeric(Eggers_thresh), " was then used to indicate a concern regarding publication bias that was worth downgrading 1 level.</i></td>
            <td colspan = '4' style='font-size: 80%;'><i>Any concerns regarding industry influence and search integrity lead to downgrading of 1 level for publication bias</i></td>
          </tr>
        </table>"))
        
      })
      
      # Indirectness #
      
      output$indirectness_details <- renderUI({
        
        indirect_old <- viewed_old_df()$indirectness
        indirect_new <- viewed_thresholds()$Value[viewed_thresholds()$Input == 'indirectness']
        
        HTML(paste0(
          "<table style='width:100%'>
          <tr>
            <th colspan = '2'>Concerns regarding indirectness</th>
          </tr>
          <tr>
            <th style='width:50%'>Before</th><th style='width:50%'>Now</th>
          </tr>
          <tr>
            <td>", indirect_old, "</td>
            <td>", indirect_new, "</td>
          </tr>
          <tr>
            <td colspan = '2' style='font-size: 80%;'><i>Any concerns regarding indirectness lead to downgrading of 1 level for indirectness.</i></td>
          </tr>
        </table>"))
        
      })
      
      # Overall text relating to GRADE #
      
      output$detail_text_GRADE <- renderUI({
        
        total_levels <- viewed_summaries() %>% select(c("Imprecision", "Inconsistency", "Indirectness", "Pubbias", "RoB")) %>% sum()
        reasons <- c(
          if (viewed_summaries()$Imprecision  != 0) "imprecision",
          if (viewed_summaries()$Inconsistency != 0) "inconsistency",
          if (viewed_summaries()$Indirectness  != 0) "indirectness",
          if (viewed_summaries()$Pubbias       != 0) "publication bias",
          if (viewed_summaries()$RoB           != 0) "risk of bias"
        )
        
        if (total_levels != 0) {
          # If there is some downgrading
          reason_text <- if (length(reasons) == 1) {
            reasons
          } else if (length(reasons) == 2) {
            paste(reasons, collapse = " and ")
          } else {
            paste0(
              paste0(reasons[-length(reasons)], collapse = ", "),
              ", and ",
              reasons[length(reasons)]
            )
          }
          
          downgrading_sentence <- paste0("downgrading ", total_levels, " level(s) due to ", reason_text, ".")
          
        } else {
          # If there is no downgrading
          downgrading_sentence <- "no downgrading across all domains."
        }
        
        current_state_paragraph <- paste0("Under the latest public update which included studies identified in searches up to <i>", 
                              format(latest_data_df() %>% filter(stringr::str_detect(Status, "update")) %>% pull(Search_date) %>% max(), "%d %B %Y"),
                              "</i>, the certainty of evidence was graded as <b>",
                              viewed_summaries()$GRADE,
                              "</b> - ", downgrading_sentence
        )
        
        
        if (filter_key() %in% suggested_scenarios()) {
          addition_change <- "Some other details that needs calculating when it does suggest any update..."
        } else {
          if (viewed_new_df()$GRADE_results$studies_no - viewed_old_df()$studies_no != 0) {
            # If there were new studies
            addition_change <- paste0("Addition of the new study(ies) did not lead to a change in the certainty of evidence.")
          } else {
            # If there were no new studies
            addition_change <- ""
          }
          
        }

        additional_data_paragraph <- paste0("By including search results from <i>",
                                            format(latest_data_df() %>% pull(Search_date) %>% max(), "%d %B %Y"),
                                            "</i>, this gave <i>",
                                            viewed_new_df()$GRADE_results$studies_no - viewed_old_df()$studies_no,
                                            " new study(ies)</i> for the ",
                                            input$outcome_filter, " outcome in the ", input$comparison_filter, " comparison. ",
                                            addition_change
                                            )
        
        
        HTML(paste0(
          current_state_paragraph, "<br><br>",
          additional_data_paragraph)
        )
        
      })
      
      
      ### Add Estimate details ###
      #--------------------------#
      
      # Funnel plot
      
      output$funnel_plot <- renderPlot({
        
        if (viewed_new_df()$GRADE_results$studies_no - viewed_old_df()$studies_no == 0) {
          
          # Where there are no new studies, state, but still print plot
          viewed_data_frame <- data[[paste0(outcome_filter_lookup[input$outcome_filter], "_df")]] %>% filter(Comparator == input$comparison_filter)
          
          ExtendedFunnelPlot(
            events_trt = viewed_data_frame$Experimental_cases,
            events_ctrl = viewed_data_frame$Control_cases,
            n_trt = viewed_data_frame$Experimental_N,
            n_ctrl = viewed_data_frame$Control_N,
            method = 'MH',
            ma_outcome = 'RR',
            est_pos = as.numeric(viewed_thresholds()$Value[viewed_thresholds()$Input == 'pos_clin']),
            est_neg = as.numeric(viewed_thresholds()$Value[viewed_thresholds()$Input == 'neg_clin']),
          ) + labs(title = "Note that there are no new studies.")
          
          
        } else {
          
          viewed_new_df()$pooled_results$threshold_plot
          
        }
        
        
        
      })
      
      # Overall text relating to point estimate #
      
      output$detail_text_estimate <- renderUI({
        
        clin_thresholds <- sort(c(viewed_thresholds()$Value[viewed_thresholds()$Input == 'pos_clin'],
                             viewed_thresholds()$Value[viewed_thresholds()$Input == 'neg_clin']))
        
        
        current_state_paragraph <- paste0("Under the latest public update which included studies identified in searches up to <i>", 
                                          format(latest_data_df() %>% filter(stringr::str_detect(Status, "update")) %>% pull(Search_date) %>% max(), "%d %B %Y"),
                                          "</i>, the pooled RR was <b>",
                                          viewed_summaries()$RR, " ", viewed_old_df()$CI, 
                                          " </b> - ", 
                                          clinical_threshold_wording(effect = viewed_summaries()$RR_clin,
                                                                     threshold_higher = clin_thresholds[2],
                                                                     threshold_lower = clin_thresholds[1])
        )
        
        
        if (filter_key() %in% suggested_scenarios()) {
          addition_change <- paste0("Addition of the new study(ies) gave a pooled RR of <b>",
                                    round(exp(viewed_new_df()$pooled_results$new_ma$beta), 2), " ", viewed_new_df()$GRADE_results$CI,
                                    "</b> - ",
                                    clinical_threshold_wording(effect = viewed_summaries()$RR_clin,
                                                               threshold_higher = clin_thresholds[2],
                                                               threshold_lower = clin_thresholds[1]))
        } else {
          if (viewed_new_df()$GRADE_results$studies_no - viewed_old_df()$studies_no != 0) {
            # If there were new studies
            addition_change <- paste0("Addition of the new study(ies) gave a pooled RR of <b>",
                                      round(exp(viewed_new_df()$pooled_results$new_ma$beta), 2), " ", viewed_new_df()$GRADE_results$CI,
                                      "</b> - this does not give a new clinical interpretation.")
          } else {
            # If there were no new studies
            addition_change <- ""
          }
          
        }
        
        additional_data_paragraph <- paste0("By including search results from <i>",
                                            format(latest_data_df() %>% pull(Search_date) %>% max(), "%d %B %Y"),
                                            "</i>, this gave <i>",
                                            viewed_new_df()$GRADE_results$studies_no - viewed_old_df()$studies_no,
                                            " new study(ies)</i> for the ",
                                            input$outcome_filter, " outcome in the ", input$comparison_filter, " comparison. ",
                                            addition_change
        )
        
        
        HTML(paste0(
          current_state_paragraph, "<br><br>",
          additional_data_paragraph)
        )
        
      })
      
      
      ### Update Activity Log ###
      #-------------------------#
      
      observe({
        req(active_page() == "results")
      
        if (length(suggested_scenarios()) == 0) {
          
          activity_log <- activity() %>%
            add_row(Date = demo_date(), Category = "LSRUpdateR",
                    Details = paste0("LSRUpdateR was run on ", format(demo_date(), "%d %B %Y"), ". 
                                     LSRUpdateR did not suggest an update and so no update was conducted."))
          activity(activity_log)

        } else {
        
          # To be added
        
        }
        
      }) |>
        bindEvent(active_page())
      
      
      ### Action buttons ###
      #--------------------#
      
      output$action_buttons <- renderUI({
        
        if (length(suggested_scenarios()) == 0) {
          
          # When no updated suggested
          div(
            actionButton(ns("return"), label = "Return", icon = shiny::icon("right-long"))
          )
          
        } else {
          
          div(
            actionButton(ns("continue"), label = "Continue", icon = shiny::icon("right-long")),
            actionButton(ns("cancel"), label = "Cancel", icon = shiny::icon("circle-xmark"))
          )
        }

      })
      
      observeEvent(input$continue, {
        bslib::nav_select("LSRUpdateR_navbar", selected = "confirmation", session = LSRUpdateR_session)
      })
      
      observeEvent(input$cancel, {
        bslib::nav_select("LSRUpdateR_navbar", selected = "home", session = LSRUpdateR_session)
      })
      
      observeEvent(input$return, {
        bslib::nav_select("LSRUpdateR_navbar", selected = "home", session = LSRUpdateR_session)
      })
      
      ### Return objects ###
      #--------------------#
      
      return(list(
        
      ))
      
    }
  )
}
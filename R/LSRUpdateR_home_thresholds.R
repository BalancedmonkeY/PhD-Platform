#' UI for the threshold settings on home page of LSRUpdateR page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
LSRUpdateR_home_thresholds_ui <- function(id) {
  ns = NS(id)
  
  div(
    
    bslib::accordion(
      accordion_panel(
        title = "Threshold Settings",
        icon = bsicons::bs_icon("gear-fill"),
        
        # Card with options on the toolbar
        bslib::card(
          card_header(
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
              ),
              bslib::toolbar_input_button(
                id = ns("apply_all"),
                label = "Apply to all"
              ),
              bslib::toolbar_divider(),
              bslib::toolbar_input_button(
                id = ns("info"),
                label = NULL,
                icon = icon("circle-info")
              ),
              bslib::toolbar_input_button(
                id = ns("save"),
                label = NULL,
                icon = icon("floppy-disk")
              ),
              bslib::toolbar_input_button(
                id = ns("lock_thresholds"),
                label = NULL,
                icon = icon("lock")
              )
            )
          ),
          
          # Inputs
          card_body(
            uiOutput(ns("threshold_inputs_ui"))
          )
        )
      ),
      open = FALSE
    )
    
  )
  
}

#' Server for the threshold settings on home page for LSRUpdateR page.
#'
#' @param id ID of the module
#'
#' @return Dataframe of thresholds
LSRUpdateR_home_thresholds_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      ## Lock/unlock settings ##
      #------------------------#
      
      # Compile threshold inputs for locking/unlocking
      threshold_inputs = c("RoB_one_level", "RoB_two_level", "variation_estimates", "Jaccard",
                           "bin_or_cont", "events_one_level", "events_two_level", "region_lower_bin",
                           "region_upper_bin", "region_lower_cont", "region_upper_cont", "Eggers_pvalue",
                           "min_studies", "search", "industry", "indirectness",
                           "pos_clin", "neg_clin", "pe_threshold")

      locked <- reactiveVal(readRDS("Saved_dfs/lock.rds")) 
      
      # Make sure UI features match current lock settings
      observe({
        req(input$RoB_one_level) # ensures that the threshold inputs exist before enabling/disabling them

        #Enable or Disable inputs depending on locked status
        lapply(threshold_inputs, function(id) {
          shinyjs::toggleState(id, condition = !locked())
        })

        if (locked() == FALSE) {
        
          # Change icon
          bslib::update_toolbar_input_button(
            "lock_thresholds",
            icon = icon("lock"),
            label = NULL
          )

        } else if (locked() == TRUE) {
        
          # Change icon
          bslib::update_toolbar_input_button(
            "lock_thresholds",
            icon = icon("lock-open"),
            label = NULL
          )
        }
      })
      
      # Swap value when lock/unlock button is pressed
      observe({
        
        # Moving from locked -> unlocked
        if (locked() == TRUE) {
          
          # Change reactiveVal
          locked(FALSE)
          
        # Moving from unlocked -> locked
        } else if (locked() == FALSE) {
          
          # Change reactiveVal
          locked(TRUE)
          
        }
        
        # Save last lock position
        saveRDS(locked(), "Saved_dfs/lock.rds")
      }) |>
        bindEvent(input$lock_thresholds)
      
      
      ## Modify inputs ##
      #-----------------#
      
      observeEvent(input$bin_or_cont, {
        bslib::nav_select("imprecision_options", selected = input$bin_or_cont)
      })
      
      
      ## Help Wizard ##
      #---------------#
      
      threshold_inputs_bin = c("events_one_level", "events_two_level", "region_lower_bin", "region_upper_bin")
      threshold_inputs_cont = c("region_lower_cont", "region_upper_cont")
      
      threshold_help <- reactive({
        req(input$bin_or_cont)
        data.frame(
        element = paste0("#", ns(c("RoB_one_level", "RoB_two_level", "pe_threshold", "variation_estimates", "Jaccard", "bin_or_cont", 
                                   if(input$bin_or_cont == "bin") {threshold_inputs_bin} else {threshold_inputs_cont},
                                   "Eggers_pvalue", "min_studies", "search", "industry", "indirectness", "pos_clin", "neg_clin"))),
        intro = c("Value X such that if the weighted risk of bias score is bigger than or equal to X, downgrade one level due to risk of bias",
                  "Value X such that if the weighted risk of bias socre is bigger than or equal to X, downgrade two levels due to risk of bias",
                  "Value X such that if two point estimated were either side of X they would be considered inconsistent",
                  "Based on the value that represents differing point estimates, choose the proportion, X, such that if X (or more) of the study effect estimates sit to one 'side' of the desired effect, there is no need to downgrade for imprecision.
                  If less than X study effect estimates sit on the same side, then consider the overlap of confidence intervals using the pairwise Jaccard index.",
                  "The pairwise Jaccard index is a measure of overlap of the confidence intervals, and is only considered when the study effect estimates are deemed to not be in 'agreement'. 
                  Choose a proportion, X, such that if the average pairwise Jaccard index (i.e., measure of overlap) is less than X, then downgrade 1 level for imprecision.",
                  "Indicate whether the outcome is binary or continuous",
                  if(input$bin_or_cont == "bin")
                    {c("Value X such that if the total number of events across all studies and arms is less than X, downgrade 1 level due to imprecision.",
                      "Value X such that if the total number of events across all studies and arms is less than X, downgrade 2 levels due to imprecision.",
                      "The outcome effect is split into three regions, indicating negative effect, no effect, positive effect. If the confidence interval spans two of these regions, downgrade 1 level for imprecision, if it spans all three regions, downgrade 2 levels.
                           Indicate here the value that splits apart the regions of negative effect and no effect.",
                      "Indicate here the value that splits apart the regions of positive effect and no effect.")} else
                        {c("The outcome effect is split into three regions, indicating negative effect, no effect, positive effect. If the confidence interval spans two of these regions, downgrade 1 level for imprecision, if it spans all three regions, downgrade 2 levels.
                           Indicate here the value that splits apart the regions of negative effect and no effect.",
                          "Indicate here the value that splits apart the regions of positive effect and no effect.")},
                  "Egger's test is a statistical test for indicating potential publication bias. Choose value, X, such that if the p-value from Egger's test is less than X, downgrade 1 level for publication bias.",
                  "Egger's test is only suitable if there are sufficient studies. Specify the minimum number of studies, X, needed to carry out Egger's test. If there are less than X studies, there is no need to downgrade for publication bias.",
                  "Indicate whether you think there are concerns regarding search inegrity. Concerns will lead to downgrading 1 level due to publication bias.",
                  "Indicate whether you think there is industry influence present. Industry influence present will lead to downgrading 1 level due to publication bias.",
                  "Indicate whether you think indirectness is present, by considering whether the population, intervention, comparator, and/or outcomes of the studies incorporates what is wanted for the research question.
                  Any indication of indirectness will lead to downgrading 1 level due to indirectness.",
                  "What outcome value indicates a positive (desired) clinical effect.",
                  "What outcome value indicates a negative (undesired) clinical effect.")
      )})
      
      observeEvent(
        input$info, {
          rintrojs::introjs(
            session,
            options = list(
              steps = threshold_help(),
              showBullets = FALSE,
              showProgress = TRUE,
              showStepNumbers = FALSE,
              nextLabel = "Next",
              prevLabel = "Prev",
              skipLabel = "Skip"
            )
          )
        }
      )
      
      ## Assign and save threshold values ##
      #------------------------------------#
      
      threshold_choices <- reactiveVal(readRDS("Saved_dfs/thresholds.rds"))
      
      # UI for threshold inputs
      output$threshold_inputs_ui <- renderUI({
        fluidRow(
          column(4,
                 tags$b("Risk of Bias"),
                 numericInput(ns("RoB_one_level"),
                              label = "Weighted RoB - 1 level",
                              value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "RoB_one_level"], 
                              min = 1, max = 3, step = 0.05),
                 numericInput(ns("RoB_two_level"),
                              label = "Weighted RoB - 2 level",
                              value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "RoB_two_level"], 
                              min = 1, max = 3, step = 0.05),
                 br(),
                 tags$b("Inconsistency"),
                 numericInput(ns("pe_threshold"),
                              label = "Threshold for consistent point estimates",
                              value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "pe_threshold"]),
                 numericInput(ns("variation_estimates"),
                              label = "Variation in estimates",
                              value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "variation_estimates"], 
                              min = 0, max = 1, step = 0.01),
                 numericInput(ns("Jaccard"),
                              label = "Pairwise Jaccard index",
                              value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "Jaccard"], 
                              min = 0, max = 1, step = 0.01),
                 br()
          ),
          column(4,
                 tags$b("Imprecision"),
                 radioButtons(ns("bin_or_cont"), label = NULL, inline = TRUE,
                              choices = list("Binary" = "bin", "Continuous" = "cont"), selected = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "bin_or_cont"]),
                 bslib::navset_hidden(id = ns("imprecision_options"),
                                      nav_panel(title = "binary", value = "bin",
                                                numericInput(ns("events_one_level"),
                                                             label = "Number of events - 1 level",
                                                             value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "events_one_level"], 
                                                             min = 0, step = 1),
                                                numericInput(ns("events_two_level"),
                                                             label = "Number of events - 2 level",
                                                             value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "events_two_level"], 
                                                             min = 0, step = 1),
                                                numericInput(ns("region_lower_bin"),
                                                             label = "Lower region divider",
                                                             value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "region_lower_bin"], 
                                                             min = 0),
                                                numericInput(ns("region_upper_bin"),
                                                             label = "Upper region divider",
                                                             value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "region_upper_bin"], 
                                                             min = 0) 
                                      ),
                                      nav_panel(title = "continuous", value = "cont",
                                                numericInput(ns("region_lower_cont"),
                                                             label = "Lower region divider",
                                                             value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "region_lower_cont"]),
                                                numericInput(ns("region_upper_cont"),
                                                             label = "Upper region divider",
                                                             value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "region_upper_cont"])
                                      )
                 ),
                 br(),
                 tags$b("Publication bias"),
                 numericInput(ns("Eggers_pvalue"),
                              label = "Egger's test p-value",
                              value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "Eggers_pvalue"], 
                              min = 0, max = 1, step = 0.001),
                 numericInput(ns("min_studies"),
                              label = "Minimum number of studies",
                              value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "min_studies"], 
                              min = 0, step = 1),
                 bslib::input_switch(ns("search"),
                                     label = "Concerns with search integrity?",
                                     value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "search"]),
                 bslib::input_switch(ns("industry"),
                                     label = "Concerns with industry influence?",
                                     value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "industry"]),
                 br()
          ),
          column(4,
                 tags$b("Indirectness"),
                 bslib::input_switch(ns("indirectness"),
                                     label = "Concerns with indirectness?",
                                     value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "indirectness"]),
                 br(),
                 tags$b("Outcome features"),
                 numericInput(ns("pos_clin"),
                              label = "Threshold for a positive (desired) clinical effect",
                              value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "pos_clin"]),
                 numericInput(ns("neg_clin"),
                              label = "Threshold for a negative (undesired) clinical effect",
                              value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "neg_clin"]),
                 br()
          )
        )
      })
      
      # Make inputs upon loading match latest df of saved thresholds
      observe({
        
        updateNumericInput(session, "RoB_one_level",
          value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "RoB_one_level"])
        updateNumericInput(session, "RoB_two_level",
          value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "RoB_two_level"])
        updateNumericInput(session, "variation_estimates",
          value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "variation_estimates"])
        updateNumericInput(session, "Jaccard",
          value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "Jaccard"])
        updateRadioButtons(session, "bin_or_cont",
          selected = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "bin_or_cont"])
        updateNumericInput(session, "events_one_level",
          value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "events_one_level"])
        updateNumericInput(session, "events_two_level",
          value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "events_two_level"])
        updateNumericInput(session, "region_lower_bin",
          value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "region_lower_bin"])
        updateNumericInput(session, "region_upper_bin",
          value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "region_upper_bin"])
        updateNumericInput(session, "region_lower_cont",
          value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "region_lower_cont"])
        updateNumericInput(session, "region_upper_cont",
          value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "region_upper_cont"])
        updateNumericInput(session, "Eggers_pvalue",
          value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "Eggers_pvalue"])
        updateNumericInput(session, "min_studies",
          value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "min_studies"])
        bslib::update_switch("search",
          value = as.logical(threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "search"]))
        bslib::update_switch("industry",
          value = as.logical(threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "industry"]))
        bslib::update_switch("indirectness",
          value = as.logical(threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "indirectness"]))
        updateNumericInput(session, "pos_clin",
          value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "pos_clin"])
        updateNumericInput(session, "neg_clin",
          value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "neg_clin"])
        updateNumericInput(session, "pe_threshold",
          value = threshold_choices()$Value[threshold_choices()$Comparator == input$comparison_filter & threshold_choices()$Outcome == input$outcome_filter & threshold_choices()$Input == "pe_threshold"])
        
      }, suspended = TRUE)
      
      # Update dataset if user changes any threshold inputs
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "RoB_one_level", input$RoB_one_level))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$RoB_one_level)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "RoB_two_level", input$RoB_two_level))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$RoB_two_level)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "variation_estimates", input$variation_estimates))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$variation_estimates)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "Jaccard", input$Jaccard))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$Jaccard)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "bin_or_cont", input$bin_or_cont))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$bin_or_cont)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "events_one_level", input$events_one_level))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$events_one_level)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "events_two_level", input$events_two_level))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$events_two_level)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "region_lower_bin", input$region_lower_bin))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$region_lower_bin)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "region_upper_bin", input$region_upper_bin))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$region_upper_bin)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "region_lower_cont", input$region_lower_cont))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$region_lower_cont)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "region_upper_cont", input$region_upper_cont))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$region_upper_cont)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "Eggers_pvalue", input$Eggers_pvalue))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$Eggers_pvalue)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "min_studies", input$min_studies))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$min_studies)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "search", input$search))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$search)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "industry", input$industry))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$industry)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "indirectness", input$indirectness))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$indirectness)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "pos_clin", input$pos_clin))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$pos_clin)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "neg_clin", input$neg_clin))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$neg_clin)
      observe({
        update_thresh <- threshold_choices() |> 
          mutate(Value = base::replace(Value, Comparator == input$comparison_filter & Outcome == input$outcome_filter & Input == "pe_threshold", input$pe_threshold))
        threshold_choices(update_thresh)
      }) |>
        bindEvent(input$pe_threshold)
      
      # Save updated thresholds
      observe({
        saveRDS(threshold_choices(), "Saved_dfs/thresholds.rds")
        showNotification("Thresholds have been saved", type = "message", duration = 2)
      }) |>
        bindEvent(input$save)
      
      
      ## Return thresholds ##
      #---------------------#
      
      return(threshold_choices)

    }
  )
}
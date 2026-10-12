## user specified functions ##

#' Function to give the suggested Cochrane wording of results conclusions
#'
#' @param effect What was the resulting effect (one of 'increase', 'decrease', or 'no_difference')
#' @param certainty What was the certainty of evidence (GRADE result)
#' @param outcome String for what the outcome is
#' @param intervention String for what the intervention is
#'
#' @return string containing wording

cochrane_conclusion <- function(effect,
                             certainty,
                             outcome,
                             intervention) {
  
  # Ensure words are in lower case
  effect <- tolower(effect)
  certainty <- tolower(certainty)
  outcome <- tolower(outcome)
  
  # Create templates for each scenario
  templates <- list(
    high = c(
      increase = "There is <b style='color:ForestGreen;'>high-certainty</b> evidence that %s increases %s.",
      decrease = "There is <b style='color:ForestGreen;'>high-certainty</b> evidence that %s decreases %s.",
      no_difference = "There is <b style='color:ForestGreen;'>high-certainty</b> evidence that %s results in little to no difference in %s."
    ),
    moderate = c(
      increase = "There is <b style='color:YellowGreen;'>moderate-certainty</b> evidence that %s probably increases %s.",
      decrease = "There is <b style='color:YellowGreen;'>moderate-certainty</b> evidence that %s probably decreases %s.",
      no_difference = "There is <b style='color:YellowGreen;'>moderate-certainty</b> evidence that %s probably results in little to no difference in %s."
    ),
    low = c(
      increase = "There is <b style='color:GoldenRod;'>low-certainty</b> evidence that %s may increase %s.",
      decrease = "There is <b style='color:GoldenRod;'>low-certainty</b> evidence that %s may decrease %s.",
      no_difference = "There is <b style='color:GoldenRod;'>low-certainty</b> evidence that %s may result in little to no difference in %s."
    )
  )
  
  # Stopping checks
  if (!certainty %in% c(names(templates), "very low", "very_low")) {
    stop("certainty must be one of: high, moderate, low, very low")
  }
  
  if (!effect %in% c("increase", "decrease", "no_difference")) {
    stop("effect must be one of: increase, decrease, no_difference")
  }
  
  # Create relevant strings
  if (certainty %in% c("very low", "very_low")) {
    return(
      sprintf(
        "The evidence is <b style = 'color:FireBrick;'>very uncertain</b> about the effect of %s on %s.",
        intervention,
        outcome
      )
    )
  } else {
    return(
      sprintf(
        templates[[certainty]][effect],
        intervention,
        outcome
      )
    )
  }
  
}


#' Function to give the suggested wording for interpretation of pooled effect with respect to threshold used
#'
#' @param effect What was the resulting effect (one of 'increase', 'decrease', or 'no_difference')
#' @param threshold_higher The threshold value used for ratios above 1
#' @param threshold_lower The threshold value used for ratios below 1
#'
#' @return string containing wording

clinical_threshold_wording <- function(effect,
                                threshold_higher,
                                threshold_lower) {
  
  # Ensure words are in lower case
  effect <- tolower(effect)
  
  # Create templates for each scenario
  templates <- list(
    increase = paste0("this is higher than the threshold of ", threshold_higher, "."),
    decrease = paste0("this is lower than the threshold of ", threshold_lower, "."),
    no_difference = paste0("this sits in the non-clinical significance range between ", threshold_lower, " and ", threshold_higher, ".")
  )
  
  # Stopping checks
  if (!effect %in% c("increase", "decrease", "no_difference")) {
    stop("effect must be one of: increase, decrease, no_difference")
  }
  
  # Create relevant strings
  return(
    templates[[effect]]
  )
}


#' Function that extracts thresholds from threshold dataframe
#'
#' @param df Dataframe from which to extract
#' @param input What type of threshold
#' @param comparator Which comparator its referring to
#' @param outcome Which outcome its referring to
#'
#' @return Designated value

get_threshold <- function(df, input, comparator, outcome) {
  df %>%
    filter(
      Input == input,
      Outcome == outcome,
      Comparator == comparator
    ) %>%
    pull(Value) %>%
    as.numeric()
}

#' Function that runs LSRUpdateR for a single outcome/comparator
#'
#' @param df_name Name of dataframe subsetting from
#' @param comparator Which comparator its referring to
#' @param outcome Which outcome its referring to
#' @param last_search_date The search date of last update
#' @param thresholds_df Dataframe for thresholds
#' @param results_summary_df Dataframe containing summary of last update results
#'
#' @return LSRUpdateR object

run_LSRUpdateR <- function(df_name, comparator, outcome, last_search_date, thresholds_df, results_summary_df) {
  
  # Define dataset
  data_df <- data[[df_name]] %>%
    filter(Comparator == comparator)
  
  # Run meta-analysis to obtain weights
  MA_obj <- metafor::rma.mh(ai = Experimental_cases, ci = Control_cases, n1i = Experimental_N, n2i = Control_N, slab = Study, data = data_df, 
                            measure = "RR", drop00 = c(TRUE, TRUE), add = c(0.5, 0.5), to = c("only0", "only0"))
  data_df <- data_df %>% mutate(est = NA_real_, est.log = NA_real_, CI.lb = NA_real_, CI.ub = NA_real_, weight = NA_real_)
  # Identify rows where both arms are not zero
  idx <- !(data_df$Experimental_cases == 0 & data_df$Control_cases == 0)
  # Populate only non-zero rows
  data_df[idx, c("est", "est.log", "CI.lb", "CI.ub", "weight")] <-
    list(
      exp(MA_obj$yi), MA_obj$yi, exp(MA_obj$yi - 1.96*sqrt(MA_obj$vi)), exp(MA_obj$yi + 1.96*sqrt(MA_obj$vi)), weights(MA_obj)
      )
  
  # Run LSRUpdateR command
  UpdatePredictor(
    data = data_df,
    search_col = "Included_Date",
    last_search_date = last_search_date,
    overall_rob = "Overall_Bias",
    event_cols = c("Experimental_cases", "Control_cases"),
    CI_lb_col = "CI.lb",
    CI_ub_col = "CI.ub",
    estimates = "est.log",
    events_trt_name = "Experimental_cases",
    events_ctrl_name = "Control_cases",
    n_trt_name = "Experimental_N",
    n_ctrl_name = "Control_N",
    rob_tool = 1,
    ma_outcome = "RR",
    model = "MH",
    pe_threshold = get_threshold(thresholds_df, "pe_threshold", comparator, outcome),
    RoB_1_threshold = get_threshold(thresholds_df, "RoB_one_level", comparator, outcome),
    RoB_2_threshold = get_threshold(thresholds_df, "RoB_two_level", comparator, outcome),
    CI_threshold_pos = get_threshold(thresholds_df, "region_upper_bin", comparator, outcome),
    CI_threshold_neg = get_threshold(thresholds_df, "region_lower_bin", comparator, outcome),
    variation_threshold = get_threshold(thresholds_df, "variation_estimates", comparator, outcome),
    effect_est_pos = get_threshold(thresholds_df, "pos_clin", comparator, outcome),
    effect_est_neg = get_threshold(thresholds_df, "neg_clin", comparator, outcome),
    Jaccard_threshold = get_threshold(thresholds_df, "Jaccard", comparator, outcome),
    events_1_threshold = get_threshold(thresholds_df, "events_one_level", comparator, outcome),
    events_2_threshold = get_threshold(thresholds_df, "events_two_level", comparator, outcome),
    pubbias_min_studies = get_threshold(thresholds_df, "min_studies", comparator, outcome),
    Eggers_threshold = get_threshold(thresholds_df, "Eggers_pvalue", comparator, outcome),
    industry = as.logical(thresholds_df$Value[thresholds_df$Input == "industry" & thresholds_df$Comparator == comparator & thresholds_df$Outcome == outcome]),
    search = as.logical(thresholds_df$Value[thresholds_df$Input == "search" & thresholds_df$Comparator == comparator & thresholds_df$Outcome == outcome]),
    indirectness = as.numeric(is.logical(thresholds_df$Value[thresholds_df$Input == "indirectness" & thresholds_df$Comparator == comparator & thresholds_df$Outcome == outcome])),
    prev_RoB = results_summary_df$RoB[results_summary_df$Outcome == outcome & results_summary_df$Comparator == comparator],
    prev_imprecision = results_summary_df$Imprecision[results_summary_df$Outcome == outcome & results_summary_df$Comparator == comparator],
    prev_inconsistency = results_summary_df$Inconsistency[results_summary_df$Outcome == outcome & results_summary_df$Comparator == comparator],
    prev_pubbias = results_summary_df$Pubbias[results_summary_df$Outcome == outcome & results_summary_df$Comparator == comparator],
    prev_indirectness = results_summary_df$Indirectness[results_summary_df$Outcome == outcome & results_summary_df$Comparator == comparator],
    prev_est = results_summary_df$RR[results_summary_df$Outcome == outcome & results_summary_df$Comparator == comparator],
    draw_plot = TRUE
  )
  
}

#' Function that calculates all the quantitative information that is used for GRADE
#'
#' @param data_obj Main data object with multiple objects
#' @param df_name Name of dataframe subsetting from
#' @param comparator Which comparator its referring to
#' @param outcome Which outcome its referring to
#' @param thresholds_df Dataframe for thresholds
#'
#' @return list of quantitative info

run_quant_info <- function(data_obj, df_name, comparator, outcome, thresholds_df, industry, search, indirectness) {
  
  # Define dataset
  data_df <- data_obj[[df_name]] %>%
    filter(Comparator == comparator)
  
  # Run meta-analysis to obtain weights
  MA_obj <- metafor::rma.mh(ai = Experimental_cases, ci = Control_cases, n1i = Experimental_N, n2i = Control_N, slab = Study, data = data_df, 
                            measure = "RR", drop00 = c(TRUE, TRUE), add = c(0.5, 0.5), to = c("only0", "only0"))
  data_df <- data_df %>% mutate(est = NA_real_, est.log = NA_real_, CI.lb = NA_real_, CI.ub = NA_real_, weight = NA_real_)
  # Identify rows where both arms are not zero
  idx <- !(data_df$Experimental_cases == 0 & data_df$Control_cases == 0)
  # Populate only non-zero rows
  data_df[idx, c("est", "est.log", "CI.lb", "CI.ub", "weight")] <-
    list(
      exp(MA_obj$yi), MA_obj$yi, exp(MA_obj$yi - 1.96*sqrt(MA_obj$vi)), exp(MA_obj$yi + 1.96*sqrt(MA_obj$vi)), weights(MA_obj)
    )
  
  # Risk of bias #
  # RoB_avg
  RoB_avg <- weighted_RoB(
    data = data_df[idx,], 
    rob_tool = 2,
    overall_rob = "Overall_Bias",
    weights = "weight"
  )
  
  # Imprecision
  # Number of events
  events <- sum_events(
    data = data_df,
    event_cols = c("Experimental_cases", "Control_cases")
  )
  # CI
  CI <- paste0("(", round(exp(MA_obj$ci.lb),2), ", ", round(exp(MA_obj$ci.ub),2), ")")
  # Number of interpretations
  interpretation_df <- CI_interpretation(
    CI_lb = MA_obj$ci.lb,
    CI_ub = MA_obj$ci.ub,
    threshold_pos = log(get_threshold(thresholds_df, "region_upper_bin", comparator, outcome)),
    threshold_neg = log(get_threshold(thresholds_df, "region_lower_bin", comparator, outcome)),
    ma_outcome = "RR"
  )
  interp_no <- sum(interpretation_df)
  
  # Inconsistency
  # Variation in estimates
  var_est <- estimate_variation(
    estimates = data_df[["est"]][!is.na(data_df[["est"]])],
    threshold = get_threshold(thresholds_df, "pe_threshold", comparator, outcome)
  )
  # Jaccard
  Jaccard <- pairwise_Jaccard(
    CI_lb_col = data_df[["CI.lb"]][!is.na(data_df[["CI.lb"]])],
    CI_ub_col = data_df[["CI.ub"]][!is.na(data_df[["CI.ub"]])]
  )
  
  # Publication bias
  # Number of studies
  studies_no <- nrow(data_df)
  # Eggers p-value
  if (studies_no >= get_threshold(thresholds_df, "min_studies", comparator, outcome)) {
    test_info <- metafor::regtest(MA_obj, model = "lm", predictor = "sei")
    stat <- test_info$pval
  } else {
    stat <- NA
  }
  
  return(list(
    RoB_avg = RoB_avg,
    events = events,
    CI = CI,
    interp_no = interp_no,
    var_est = var_est,
    Jaccard = Jaccard,
    studies_no = studies_no,
    Eggers = stat,
    industry = industry,
    search = search,
    indirectness = indirectness
  ))
  
}

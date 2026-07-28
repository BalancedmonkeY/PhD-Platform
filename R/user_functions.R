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

#' Predict Probabilities
#'
#' Predicts the key probabilities.
#'
#' @param analysis The jags_analysis object to predict the probabilities for.
#' @return A data frame of probabilities
#' @export
predict_probs <- function(analysis) {
  recaptured <- predict(analysis, parm = "eRecapture", newdata = "")
  moving <- predict(analysis, parm = "eMoving", newdata = "")
  mortality <- predict(analysis, parm = "eMortality", newdata = "")
  mortality_spawning <- predict(analysis, parm = "eMortalitySpawningAnnual", newdata = "")

  probs <- dplyr::bind_rows("Movement Detected" = moving, "Angled Recaptured" = recaptured,
                   "Mortality Inlake" = mortality,
                   "Mortality Spawner" = mortality_spawning, .id = "Parameter")
  . <- NULL
  probs$Parameter %<>% factor(., levels = .)
  probs %<>% dplyr::select_(~Parameter, ~Species, ~Spawned, ~estimate, ~lower, ~upper)
  probs
}


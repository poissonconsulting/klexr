#' Predict Probabilities
#'
#' Predicts the key probabilities.
#'
#' @param analysis The jags_analysis object to predict the probabilities for.
#' @return A data frame of probabilities
#' @export
predict_probs <- function(analysis) {
  recaptured <- stats::predict(analysis, parm = "eRecapture", newdata = "")
  moving <- stats::predict(analysis, parm = "eMoving", newdata = "")
  mortality <- stats::predict(analysis, parm = "eMortality", newdata = "")
  mortality_spawning <- stats::predict(analysis, parm = "eMortalitySpawningAnnual", newdata = "")

  probs <- dplyr::bind_rows("Movement Detected" = moving, "Angled Recaptured" = recaptured,
                   "Mortality Inlake" = mortality,
                   "Mortality Spawner" = mortality_spawning, .id = "Parameter")
  . <- NULL
  probs$Parameter %<>% factor(., levels = .)
  probs %<>% dplyr::select_(~Parameter, ~Species, ~Spawned, ~estimate, ~lower, ~upper)
  probs
}


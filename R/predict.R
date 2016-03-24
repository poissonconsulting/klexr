#' Predict Probabilities
#'
#' Predicts
#'
#' @param analysis The jags_analysis object to predict the probabilities for.
#' @return A data frame of probabilities
#' @export
predict_probs <- function(analysis) {
  angling <- predict(analysis, parm = "eAngling", newdata = "")
  moving <- predict(analysis, parm = "eMoving", newdata = "")
  mortality <- predict(analysis, parm = "eMortality", newdata = "Spawned")

  probs <- dplyr::bind_rows("Movement" = moving, "Reported" = angling,
                   "Mortality" = dplyr::slice(mortality,1),
                   "Spawning Mortality" = dplyr::slice(mortality,2), .id = "Parameter")
  . <- NULL
  probs$Parameter %<>% factor(., levels = .)
  probs %<>% dplyr::select_(~Parameter, ~Species, ~Spawned, ~estimate, ~lower, ~upper)
  probs
}


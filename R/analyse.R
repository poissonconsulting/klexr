#' Subtract 600 Divide 100
#'
#' @param x The numeric vector to transform.
#' @export
#' @examples
#' subtract600divide100(c(400,500,600,750))
subtract600divide100 <- function(x) {
  x %<>% magrittr::subtract(600) %>% magrittr::divide_by(100)
  x
}

#' Survival Model Code
#'
#' Returns a string of the JAGS code
#' defining the survival model.
#'
#' @param species A string specifying the species ("Bull Trout", "Rainbow Trout")
#' @param model A string specifying the model type ("base", "full" and "final")
#' @param comments A flag indicating whether to include comments.
#' @return A string of the JAGS model code.
#' @examples
#' cat(survival_model_code())
#' @export
survival_model_code <- function(species, model = "final", comments = TRUE) {
  check_scalar(species, c("Bull Trout", "Rainbow Trout", "Rainbow Trout"))
  check_scalar(model, c("base", "full", "final"))
  check_flag(comments)

  data <- list()

  data$kI <- switch(model,
         base = 0,
         full = 1,
         final = 0.5,
         stop())

  model_code <- "
data {
  kI <- {{kI}}
  kC <- 10^-2
}
model{
  bSpawning ~ dnorm(0, 3^-2)
  bSpawningLength ~ dnorm(0, 3^-2)
  bMoving ~ dnorm(0, 3^-2)
  bMovingSpawningPeriod ~ dnorm(0, 3^-2)
  bRecapture ~ dnorm(0, 3^-2)
  bSurvival ~ dnorm(0, 3^-2)
  bSurvivalSpawning ~ dnorm(0, 3^-2)

  iRecaptureYear ~ dbern(kI)
  sdRecaptureYear <- iRecaptureYear * 3 + (1-iRecaptureYear) * 3 * kC
  bRecaptureYear ~ dnorm(0, sdRecaptureYear^-2)

  iSurvivalYear ~ dbern(kI)
  sdSurvivalYear <- iSurvivalYear * 3 + (1-iSurvivalYear) * 3 * kC
  bSurvivalYear ~ dnorm(0, sdSurvivalYear^-2)

  for (i in 1:nCapture){
    eAlive[i,PeriodCapture[i]] <- 1

    logit(eSpawning[i,PeriodCapture[i]]) <- bSpawning + bSpawningLength * Length[i,PeriodCapture[i]]

    Spawned[i,PeriodCapture[i]] ~ dbern(eAlive[i,PeriodCapture[i]] * SpawningPeriod[PeriodCapture[i]] * eSpawning[i,PeriodCapture[i]])

    logit(eMoving[i,PeriodCapture[i]]) <- bMoving + bMovingSpawningPeriod * SpawningPeriod[PeriodCapture[i]]

    Moved[i,PeriodCapture[i]] ~ dbern(eAlive[i,PeriodCapture[i]] * Monitored[i,PeriodCapture[i]] * eMoving[i,PeriodCapture[i]])

    logit(eRecapture[i,PeriodCapture[i]]) <- bRecapture + bRecaptureYear * Year[PeriodCapture[i]]

    Recaptured[i,PeriodCapture[i]] ~ dbern(eAlive[i,PeriodCapture[i]] * eRecapture[i,PeriodCapture[i]])

    logit(eSurvival[i, PeriodCapture[i]]) <- bSurvival + bSurvivalSpawning * Spawned[i,PeriodCapture[i]] +
bSurvivalYear * Year[PeriodCapture[i]]

    for(j in (PeriodCapture[i]+1):nPeriod) {
      eAlive[i,j] ~ dbern(eAlive[i,j-1] * eSurvival[i,j-1] * (1-Recaptured[i,j-1]))

      logit(eSpawning[i,j]) <- bSpawning + bSpawningLength * Length[i,j]
      Spawned[i,j] ~ dbern(eAlive[i,j] *  SpawningPeriod[j] * eSpawning[i,j])

      logit(eMoving[i,j]) <- bMoving + bMovingSpawningPeriod * SpawningPeriod[j]
      Moved[i,j] ~ dbern(eAlive[i,j] * Monitored[i,j] * eMoving[i,j])

      logit(eRecapture[i,j]) <- bRecapture + bRecaptureYear * Year[j]
      Recaptured[i,j] ~ dbern(eAlive[i,j] * eRecapture[i,j])
      logit(eSurvival[i,j]) <- bSurvival + bSurvivalSpawning * Spawned[i,j] +
bSurvivalYear * Year[j]
    }
  }
}"
  model_code %<>%  whisker::whisker.render(data)

  ifelse(!comments, juggler::jg_rm_comments(model_code), model_code)
}

survival_model <- function(species, model) {

  jaggernaut::jags_model(survival_model_code(species = species, model = model),
derived_code = "data{
  for(i in 1:length(Capture)) {
    logit(eSpawning[i]) <- bSpawning + bSpawningLength * Length[i]
    logit(eMoving[i]) <- bMovingSeason[Season[i]]
    logit(eRecapture[i]) <- bRecapture
    logit(eMortality[i]) <- bMortality + bMortalityPeriod[Period[i]]
    eMortalitySeason[i] <- 1-(1-eMortality[i])^(1/nSeason)
    logit(eMortalitySpawning[i]) <- bMortality + bMortalitySpawning
    eMortalitySpawningSeason[i] <- 1-(1-eMortalitySpawning[i])^(1/nSeason)
    eMortalitySpawningAnnual[i] <- 1-(1-eMortalitySeason[i])^(nSeason-1) * (1-eMortalitySpawningSeason[i])
  }
}",
             gen_inits = function(data) {

               inits <- list()

               undefined <- array(TRUE, dim = dim(data$Moved))
               for (i in 1:data$nCapture) {
                 undefined[i,data$PeriodCapture[i]:data$nPeriod] <- FALSE
               }

               inits$eAlive <- array(1, dim = dim(data$Recaptured))
               for (i in 1:data$nCapture) {
                 for (j in 2:ncol(inits$eAlive)) {
                   inits$eAlive[i,j] <- inits$eAlive[i,j-1] * (1 - data$Recaptured[i,j-1])
                 }
               }

               is.na(inits$eAlive[undefined]) <- TRUE
               for (i in 1:data$nCapture) {
                 is.na(inits$eAlive[i,data$PeriodCapture[i]]) <- TRUE
               }
               inits
             },
             modify_data_derived = function (data) {
               data
             },
             modify_data = function(data) {

               df <- as.data.frame(data[c("Capture", "Period", "PeriodCapture",
                                          "Monitored", "Moved", "Recaptured", "Year", "Season",
                                          "Spawned", "SpawningPeriod", "Length")])

               data$SpawningPeriod <- reshape2::acast(df, Capture ~ Period, value.var = "SpawningPeriod")
               data$SpawningPeriod <- data$SpawningPeriod[1,]

               data$Year <- reshape2::acast(df, Capture ~ Period, value.var = "Year")
               data$Year <- data$Year[1,]

               data$Season <- reshape2::acast(df, Capture ~ Period, value.var = "Season")
               data$Season <- data$Season[1,]

               data$PeriodCapture <- reshape2::acast(df, Capture ~ Period, value.var = "PeriodCapture")
               data$PeriodCapture <- apply(data$PeriodCapture, MARGIN = 1, FUN = min, na.rm = TRUE)

               data$Monitored <- reshape2::acast(df, Capture ~ Period, value.var = "Monitored")

               data$Recaptured <- reshape2::acast(df, Capture ~ Period, value.var = "Recaptured")
               data$Length <- reshape2::acast(df, Capture ~ Period, value.var = "Length")
               data$Spawned <- reshape2::acast(df, Capture ~ Period, value.var = "Spawned")

               data$Moved <- reshape2::acast(df, Capture ~ Period, value.var = "Moved")

               data$Capture <- NULL
               data$Period <- NULL
               data$nPeriodCapture <- NULL
               data$nPeriodTagExpire <- NULL

               data
             },
             select_data = c("Capture", "PeriodCapture",
                             "Period", "Year*", "Season",
                             "Monitored", "Moved", "Recaptured",
                             "Spawned", "SpawningPeriod", "subtract600divide100(Length)"),
monitor = "^([^de]|.[^A-Z])"
  )
}

#' Analyse Detections and Recaptures
#'
#' Analyses detection and recapture data using a Bayesian individual multistate
#' state-space formulation of the Cormack-Jolly-Seber (CJS) survival model
#'
#' To view the full model description
#' in the JAGS dialect of the BUGS language use \code{\link{survival_model_code}}.
#'
#' The data must be an \code{analysis_data} object as generated by the
#' \code{\link{make_analysis_data}} function.
#'
#' @param data A \code{analysis_data} object of the detection and recapture data to analyse.
#' @param model A string specifying the model type ("base", "full" and "final")
#' @param niters An integer of the minimum number of MCMC iterations to
#' perform.
#' @param mode A character element indicating the mode for the analysis.
#' @return A jags_analysis object.
#' @export
analyse_survival <- function(data, model = "final", niters = 10^5, mode = "current") {
  assert_that(is.data.frame(data))
  assert_that(is.count(niters) && noNA(niters))

  data %<>% check_data3(
    list(
      Species = factor(1),
      Capture = factor(1),
      Period = factor(1),
      Days = 1,
      PeriodCapture = factor(1),
      Year = 1L,
      Month = c(1L, 12L),
      Length = c(0L, 1000L),
      Weight = c(0.5, 10, NA),
      Reward1 = c(0L, 200L),
      Reward2 = c(0L, 200L, NA),
      TBarTag1 = c(TRUE, NA),
      TBarTag2 = c(TRUE, NA),
      Monitored = TRUE,
      Detected = TRUE,
      Moved = TRUE,
      Recaptured = TRUE,
      Public = c(TRUE, NA),
      Removed = c(TRUE, NA),
      Released = c(TRUE, NA),
      SpawningPeriod = TRUE,
      Spawned = c(TRUE, NA),
      Season = factor(1)),
    key = c("Capture", "Period"), select = TRUE)

  jaggernaut::jags_analysis(survival_model(species = as.character(data$Species[1]), model = model),
                            data, niters = niters, mode = mode)
}

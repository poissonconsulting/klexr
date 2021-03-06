#' Subtract 750 Divide 100
#'
#' @param x The numeric vector to transform.
#' @export
#' @examples
#' subtract750divide100(c(400,500,600,750))
subtract750divide100 <- function(x) {
  x %<>% magrittr::subtract(750) %>% magrittr::divide_by(100)
  x
}

#' Survival Model Code
#'
#' Returns a string of the JAGS code
#' defining the survival model.
#'
#' @param model A string specifying the model type ("base", "full" and "final")
#' @param sd A number specifying the standard deviation for the prior distributions.
#' @param comments A flag indicating whether to include comments.
#' @return A string of the JAGS model code.
#' @examples
#' cat(survival_model_code())
#' @export
survival_model_code <- function(model = "final", sd = 3, comments = TRUE) {
  check_scalar(model, c("base", "full", "final"))
  check_scalar(sd, c(1, 10))
  check_flag(comments)

  data <- list()

  data$dKI <- switch(model,
         base = 0,
         full = 1,
         final = 0.5,
         stop())

  data$sd <- sd

  model_code <- "
model{
  bSpawning ~ dnorm(0, {{sd}}^-2)                              # $\\beta_{\\kappa 0}$
  bMoving ~ dnorm(0, {{sd}}^-2)                                # $\\beta_{\\delta 0}$
  bRecapture ~ dnorm(0, {{sd}}^-2)                             # $\\beta_{\\rho 0}$
  bSurvival ~ dnorm(0, {{sd}}^-2)                              # $\\beta_{\\phi 0}$

  iSpawningLength ~ dbern({{dKI}})                             # $\\gamma_{\\kappa L}$
  dSDSpawningLength <- iSpawningLength * {{sd}} + (1-iSpawningLength) * 0.03
  bSpawningLength ~ dnorm(0, dSDSpawningLength^-2)         # $\\beta_{\\kappa L}$

  iMovingSpawningPeriod ~ dbern({{dKI}})                       # $\\gamma_{\\delta S}$
  dSDMovingSpawningPeriod <- iMovingSpawningPeriod * {{sd}} + (1-iMovingSpawningPeriod) * 0.03
  bMovingSpawningPeriod ~ dnorm(0, dSDMovingSpawningPeriod^-2) # $\\beta_{\\delta S}$

  iSurvivalSpawning ~ dbern({{dKI}})                           # $\\gamma_{\\phi \\kappa}$
  dSDSurvivalSpawning <- iSurvivalSpawning * {{sd}} + (1-iSurvivalSpawning) * 0.03
  bSurvivalSpawning ~ dnorm(0, dSDSurvivalSpawning^-2)     # $\\beta_{\\phi \\kappa}$

  iMovingYear ~ dbern({{dKI}})                               # $\\gamma_{\\delta Y}$
  dSDMovingYear <- iMovingYear * {{sd}} + (1-iMovingYear) * 0.03
  bMovingYear ~ dnorm(0, dSDMovingYear^-2)               # $\\beta_{\\delta Y}$

  iRecaptureYear ~ dbern({{dKI}})                              # $\\gamma_{\\rho Y}$
  dSDRecaptureYear <- iRecaptureYear * {{sd}} + (1-iRecaptureYear) * 0.03
  bRecaptureYear ~ dnorm(0, dSDRecaptureYear^-2)           # $\\beta_{\\rho Y}$

  iSpawningYear ~ dbern({{dKI}})                              # $\\gamma_{\\kappa Y}$
  dSDSpawningYear <- iSpawningYear * {{sd}} + (1-iSpawningYear) * 0.03
  bSpawningYear ~ dnorm(0, dSDSpawningYear^-2)            # $\\beta_{\\kappa Y}$

  iSurvivalYear ~ dbern({{dKI}})                              # $\\gamma_{\\phi Y}$
  dSDSurvivalYear <- iSurvivalYear * {{sd}} + (1-iSurvivalYear) * 0.03
  bSurvivalYear ~ dnorm(0, dSDSurvivalYear^-2)            # $\\beta_{\\phi Y}$

  for (i in 1:nCapture){
    eAlive[i,PeriodCapture[i]] <- 1

    logit(eSpawning[i,PeriodCapture[i]]) <- bSpawning + bSpawningLength * Length[i,PeriodCapture[i]] + bSpawningYear * Year[PeriodCapture[i]]

    Spawned[i,PeriodCapture[i]] ~ dbern(eAlive[i,PeriodCapture[i]] * SpawningPeriod[PeriodCapture[i]] * eSpawning[i,PeriodCapture[i]])

    logit(eMoving[i,PeriodCapture[i]]) <- bMoving + bMovingSpawningPeriod * SpawningPeriod[PeriodCapture[i]]

    Moved[i,PeriodCapture[i]] ~ dbern(eAlive[i,PeriodCapture[i]] * Monitored[i,PeriodCapture[i]] * eMoving[i,PeriodCapture[i]])

    logit(eRecapture[i,PeriodCapture[i]]) <- bRecapture + bRecaptureYear * Year[PeriodCapture[i]]

    Recaptured[i,PeriodCapture[i]] ~ dbern(eAlive[i,PeriodCapture[i]] * eRecapture[i,PeriodCapture[i]])

    logit(eSurvival[i, PeriodCapture[i]]) <- bSurvival + bSurvivalSpawning * Spawned[i,PeriodCapture[i]] +
bSurvivalYear * Year[PeriodCapture[i]]

    for(j in (PeriodCapture[i]+1):nPeriod) {
      eAlive[i,j] ~ dbern(eAlive[i,j-1] * eSurvival[i,j-1] * (1-Recaptured[i,j-1]))

      logit(eSpawning[i,j]) <- bSpawning + bSpawningLength * Length[i,j] + bSpawningYear * Year[PeriodCapture[i]]
      Spawned[i,j] ~ dbern(eAlive[i,j] *  SpawningPeriod[j] * eSpawning[i,j])

      logit(eMoving[i,j]) <- bMoving + bMovingSpawningPeriod * SpawningPeriod[j] + bMovingYear * Year[j]
      Moved[i,j] ~ dbern(eAlive[i,j] * Monitored[i,j] * eMoving[i,j])

      logit(eRecapture[i,j]) <- bRecapture + bRecaptureYear * Year[j]
      Recaptured[i,j] ~ dbern(eAlive[i,j] * eRecapture[i,j])
      logit(eSurvival[i,j]) <- bSurvival + bSurvivalSpawning * Spawned[i,j] +
bSurvivalYear * Year[j]
    }
  }
}"
  model_code %<>%  whisker::whisker.render(data)

  ifelse(!comments, jaggernaut::jg_rm_comments(model_code), model_code)
}

survival_model <- function(model, sd) {

  jaggernaut::jags_model(survival_model_code(model = model, sd = sd),
derived_code = "data{
  for(i in 1:length(Capture)) {
    logit(eSpawning[i]) <- bSpawning + bSpawningLength * Length[i] + bSpawningYear * Year[i]
    logit(eMoving[i]) <- bMoving + bMovingSpawningPeriod * SpawningPeriod[i] + bMovingYear * Year[i]
    logit(eRecapture[i]) <- bRecapture + bRecaptureYear * Year[i]
    logit(eSurvival[i]) <- bSurvival + bSurvivalSpawning * Spawned[i] + bSurvivalYear * Year[i]

    logit(eSurvivalSpawner[i]) <- bSurvival + bSurvivalSpawning + bSurvivalYear * Year[i]
    logit(eSurvivalNonspawner[i]) <- bSurvival + bSurvivalYear * Year[i]

    eRecaptureAnnual[i] <- 1 - (1 - eRecapture[i])^4
    eSurvivalAnnual[i] <- eSurvivalNonspawner[i]^3 * (eSurvivalSpawner[i] * Spawned[i] + eSurvivalNonspawner[i] * (1 - Spawned[i]))
    eSurvivalLengthAnnual[i] <- eSurvivalNonspawner[i]^3 * (eSurvivalSpawner[i] * eSpawning[i] + eSurvivalNonspawner[i] * (1 - eSpawning[i]))
    eMortalityLengthAnnual[i] <- -log(eSurvivalLengthAnnual[i])
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
                                          "Monitored", "Moved", "Recaptured", "Year",
                                          "Spawned", "SpawningPeriod", "Length")])

               data$SpawningPeriod <- reshape2::acast(df, Capture ~ Period, value.var = "SpawningPeriod")
               data$SpawningPeriod <- data$SpawningPeriod[1,]

               data$Year <- reshape2::acast(df, Capture ~ Period, value.var = "Year")
               data$Year <- data$Year[1,]

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
                             "Period", "Year*",
                             "Monitored", "Moved", "Recaptured",
                             "Spawned", "SpawningPeriod", "subtract750divide100(Length)"),
monitor = "^([^de]|.[^A-Z])"
  )
}

#' Analyse Survival
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
#' @param sd A number specifying the standard deviation for the prior distributions.
#' @param niters An integer of the minimum number of MCMC iterations to
#' perform.
#' @param mode A character element indicating the mode for the analysis.
#' @return A jags_analysis object.
#' @export
analyse_survival <- function(data, model = "final", sd = 3, niters = 10^5, mode = "current") {
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
      Spawned = c(TRUE, NA)),
    key = c("Capture", "Period"), select = TRUE)

  jaggernaut::jags_analysis(survival_model(model = model, sd = sd),
                            data, niters = niters, mode = mode)
}

#' Tag Loss Model Code
#'
#' Returns a string of the JAGS code
#' defining the tag loss model.
#'
#' @param comments A flag indicating whether to include comments.
#' @return A string of the JAGS model code.
#' @examples
#' cat(tagloss_model_code())
#' @export
tagloss_model_code <- function(comments = TRUE) {
  check_flag(comments)

  model_code <- "
model{
  bTagLoss ~ dunif(0, 1)
  bTagLoss2 <- bTagLoss^2

  for (i in 1:length(Tags)) {
    Tags[i] ~ dbin(1 - bTagLoss, 2) T(1, )
  }
}"
  ifelse(!comments, jaggernaut::jg_rm_comments(model_code), model_code)
}

tagloss_model <- function() {
  jaggernaut::jags_model(tagloss_model_code())
}

#' Analyse Tag Loss
#'
#' Analyses tag loss data using a simple Bayesian zero-truncated binomial tag-loss model.
#'
#' To view the full model description
#' in the JAGS dialect of the BUGS language use \code{\link{tagloss_model_code}}.
#'
#' @param table A table of the number of recaptures with $100 and $10 tags.
#' @param niters An integer of the minimum number of MCMC iterations to
#' perform.
#' @param mode A character element indicating the mode for the analysis.
#' @return A jags_analysis object.
#' @export
analyse_tagloss <- function(table, niters = 10^3, mode = "current") {
  assert_that(is.count(niters) && noNA(niters))
  assert_that(is.table(table))

  data <- dplyr::data_frame(Tags = rep.int(c(2L, 1L), times = c(table[1,1], table[1,2] + table[2,1])))

  jaggernaut::jags_analysis(tagloss_model(), data, niters = niters, mode = mode)
}

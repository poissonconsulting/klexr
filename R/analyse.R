#' Survival Model Code
#'
#' Returns a string of the JAGS code
#' defining the survival model.
#'
#' @param comments A flag indicating whether to include comments.
#' @return A string of the JAGS model code.
#' @examples
#' cat(survival_model_code())
#' @export
survival_model_code <- function(comments = TRUE) {
  assert_that(is.flag(comments))
  assert_that(noNA(comments))

  model_code <- "model{
  bSpawning ~ dnorm(0, 3^-2)
  bMoving ~ dnorm(0, 3^-2)
  bReported ~ dnorm(0, 3^-2)
  bMortality ~ dnorm(0, 3^-2) # $\\beta_{\\lambda 0}$

  bSpawningLength ~ dnorm(0, 3^-2)
  bMortalitySpawning ~ dnorm(0, 3^-2)

  for (i in 1:nCapture){
    eAlive[i,PeriodCapture[i]] <- 1

    logit(eSpawning[i,PeriodCapture[i]]) <- bSpawning + bSpawningLength * Length[i,PeriodCapture[i]]

    Spawned[i,PeriodCapture[i]] ~ dbern(eAlive[i,PeriodCapture[i]] * SpawningPeriod[PeriodCapture[i]] * eSpawning[i,PeriodCapture[i]])

    logit(eMoving[i,PeriodCapture[i]]) <- bMoving

    Moved[i,PeriodCapture[i]] ~ dbern(eAlive[i,PeriodCapture[i]] * Monitored[i,PeriodCapture[i]] * eMoving[i,PeriodCapture[i]])

    logit(eReported[i,PeriodCapture[i]]) <- bReported
    eReportedSeasonal[i,PeriodCapture[i]] <- 1-(1-eReported[i,PeriodCapture[i]])^(1/nSeason)
    Reported[i,PeriodCapture[i]] ~ dbern(eAlive[i,PeriodCapture[i]] * eReportedSeasonal[i,PeriodCapture[i]])

    logit(eMortality[i, PeriodCapture[i]]) <- bMortality + bMortalitySpawning * Spawned[i,PeriodCapture[i]]
    eMortalitySeasonal[i,PeriodCapture[i]] <- 1-(1-eMortality[i,PeriodCapture[i]])^(1/nSeason)

    for(j in (PeriodCapture[i]+1):nPeriod) {
      eAlive[i,j] ~ dbern(eAlive[i,j-1] * (1-eMortalitySeasonal[i,j-1]) * (1-Reported[i,j-1]))

      logit(eSpawning[i,j]) <- bSpawning + bSpawningLength * Length[i,j]
      Spawned[i,j] ~ dbern(eAlive[i,j] *  SpawningPeriod[j] * eSpawning[i,j])

      logit(eMoving[i,j]) <- bMoving
      Moved[i,j] ~ dbern(eAlive[i,j] * Monitored[i,j] * eMoving[i,j])

      logit(eReported[i,j]) <- bReported
      eReportedSeasonal[i,j] <- 1-(1-eReported[i,j])^(1/nSeason)
      Reported[i,j] ~ dbern(eAlive[i,j] * eReportedSeasonal[i,j])
      logit(eMortality[i,j]) <- bMortality + bMortalitySpawning * max(Spawned[i,j],Spawned[i,j-1])
      eMortalitySeasonal[i,j] <- 1-(1-eMortality[i,j])^(1/nSeason)
    }
  }
}"
  ifelse(!comments, juggler::jg_rm_comments(model_code), model_code)
}

survival_model <- function () {
  jaggernaut::jags_model(survival_model_code(),
derived_code = "data{
  for(i in 1:length(Capture)) {
    logit(eSpawning[i]) <- bSpawning + bSpawningLength * Length[i]
    logit(eMoving[i]) <- bMoving
    logit(eReported[i]) <- bReported
    logit(eMortality[i]) <- bMortality + bMortalitySpawning * Spawned[i]
  }
}",
             gen_inits = function(data) {
               inits <- list()

               undefined <- array(TRUE, dim = dim(data$Moved))
               for (i in 1:data$nCapture) {
                 undefined[i,data$PeriodCapture[i]:data$nPeriod] <- FALSE
               }

               inits$eAlive <- array(1, dim = dim(data$Reported))
               for (i in 1:data$nCapture) {
                 for (j in 2:ncol(inits$eAlive)) {
                   inits$eAlive[i,j] <- inits$eAlive[i,j-1] * (1 - data$Reported[i,j-1])
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
                                          "Monitored", "Moved", "Reported", "Year", "Season",
                                          "Spawned", "SpawningPeriod", "Length")])

               data$SpawningPeriod <- reshape2::acast(df, Capture ~ Period, value.var = "SpawningPeriod")
               data$SpawningPeriod <- data$SpawningPeriod[1,]

               data$Year <- reshape2::acast(df, Capture ~ Period, value.var = "Year")
               data$Year <- data$Year[1,]

               data$Season <- reshape2::acast(df, Capture ~ Period, value.var = "Season")
               data$Season <- data$Season[1,]

               data$PeriodCapture <- reshape2::acast(df, Capture ~ Period, value.var = "PeriodCapture")
               data$PeriodCapture <- data$PeriodCapture[,1]

               data$Monitored <- reshape2::acast(df, Capture ~ Period, value.var = "Monitored")
               data$Reported <- reshape2::acast(df, Capture ~ Period, value.var = "Reported")
               data$Length <- reshape2::acast(df, Capture ~ Period, value.var = "Length")
               data$Spawned <- reshape2::acast(df, Capture ~ Period, value.var = "Spawned")

               data$Moved <- reshape2::acast(df, Capture ~ Period, value.var = "Moved")

               data$Capture <- NULL
               data$Period <- NULL
               data$nPeriodCapture <- NULL
               data$nPeriodTagExpire <- NULL
               data$Year <- NULL
               data$Season <- NULL

               data
             },
             select_data = c("Capture", "PeriodCapture",
                             "Period", "Year*", "Season",
                             "Monitored", "Moved", "Reported",
                             "Spawned", "SpawningPeriod", "Length*")
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
#' @param niters An integer of the minimum number of MCMC iterations to
#' perform.
#' @param A character element indicating the mode for the analysis.
#' @return A jags_analysis object.
#' @export
analyse_survival <- function(data, niters = 10^5, mode = "current") {
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
      Reported = TRUE,
      Public = c(TRUE, NA),
      Removed = c(TRUE, NA),
      Released = c(TRUE, NA),
      SpawningPeriod = TRUE,
      Spawned = c(TRUE, NA)),
    key = c("Capture", "Period"), select = TRUE)

  data$Season <- season(data$Month)

  jaggernaut::jags_analysis(survival_model(), data, niters = niters, mode = mode)
}
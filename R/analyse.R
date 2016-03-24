#' Mortality Model Code
#'
#' Returns a string of the JAGS code
#' defining the mortality model.
#'
#' @param comments A flag indicating whether to include comments.
#' @return A string of the JAGS model code.
#' @examples
#' cat(mortality_model_code())
#' @export
mortality_model_code <- function(comments = TRUE) {
  assert_that(is.flag(comments))
  assert_that(noNA(comments))

  model_code <- "model{
  bSpawning ~ dnorm(0, 3^-2)
  bMoving ~ dnorm(0, 3^-2)
  bAngling ~ dnorm(0, 3^-2)
  bMortality ~ dnorm(0, 3^-2) # $\\beta_{\\lambda 0}$

  bSpawningLength ~ dnorm(0, 3^-2)
  bMortalitySpawning ~ dnorm(0, 3^-2)

  for (i in 1:nCapture){
    eAlive[i,PeriodCapture[i]] <- 1

    logit(eSpawning[i,PeriodCapture[i]]) <- bSpawning + bSpawningLength * Length[i,PeriodCapture[i]]

    Spawned[i,PeriodCapture[i]] ~ dbern(eAlive[i,PeriodCapture[i]] * SpawningPeriod[PeriodCapture[i]] * eSpawning[i,PeriodCapture[i]])

    logit(eMoving[i,PeriodCapture[i]]) <- bMoving

    Moved[i,PeriodCapture[i]] ~ dbern(eAlive[i,PeriodCapture[i]] * Monitored[i,PeriodCapture[i]] * eMoving[i,PeriodCapture[i]])

    logit(eAngling[i,PeriodCapture[i]]) <- bAngling
    eAnglingSeasonal[i,PeriodCapture[i]] <- 1-(1-eAngling[i,PeriodCapture[i]])^(1/nSeason)
    Angled[i,PeriodCapture[i]] ~ dbern(eAlive[i,PeriodCapture[i]] * eAnglingSeasonal[i,PeriodCapture[i]])

    logit(eMortality[i, PeriodCapture[i]]) <- bMortality + bMortalitySpawning * Spawned[i,PeriodCapture[i]]
    eMortalitySeasonal[i,PeriodCapture[i]] <- 1-(1-eMortality[i,PeriodCapture[i]])^(1/nSeason)

    for(j in (PeriodCapture[i]+1):nPeriod) {
      eAlive[i,j] ~ dbern(eAlive[i,j-1] * (1-eMortalitySeasonal[i,j-1]) * (1-Angled[i,j-1]))

      logit(eSpawning[i,j]) <- bSpawning + bSpawningLength * Length[i,j]
      Spawned[i,j] ~ dbern(eAlive[i,j] *  SpawningPeriod[j] * eSpawning[i,j])

      logit(eMoving[i,j]) <- bMoving
      Moved[i,j] ~ dbern(eAlive[i,j] * Monitored[i,j] * eMoving[i,j])

      logit(eAngling[i,j]) <- bAngling
      eAnglingSeasonal[i,j] <- 1-(1-eAngling[i,j])^(1/nSeason)
      Angled[i,j] ~ dbern(eAlive[i,j] * eAnglingSeasonal[i,j])
      logit(eMortality[i,j]) <- bMortality + bMortalitySpawning * max(Spawned[i,j],Spawned[i,j-1])
      eMortalitySeasonal[i,j] <- 1-(1-eMortality[i,j])^(1/nSeason)
    }
  }
}"
  ifelse(!comments, juggler::jg_rm_comments(model_code), model_code)
}

mortality_model <- function () {
  jaggernaut::jags_model(mortality_model_code(),
derived_code = "data{
  for(i in 1:length(Capture)) {
    logit(eSpawning[i]) <- bSpawning + bSpawningLength * Length[i]
    logit(eMoving[i]) <- bMoving
    logit(eAngling[i]) <- bAngling
    logit(eMortality[i]) <- bMortality + bMortalitySpawning * Spawned[i]
  }
}",
             gen_inits = function(data) {
               inits <- list()

               undefined <- array(TRUE, dim = dim(data$Moved))
               for (i in 1:data$nCapture) {
                 undefined[i,data$PeriodCapture[i]:data$nPeriod] <- FALSE
               }

               inits$eAlive <- array(1, dim = dim(data$Angled))
               for (i in 1:data$nCapture) {
                 for (j in 2:ncol(inits$eAlive)) {
                   inits$eAlive[i,j] <- inits$eAlive[i,j-1] * (1 - data$Angled[i,j-1])
                 }
               }

               is.na(inits$eAlive[undefined]) <- TRUE
               for (i in 1:data$nCapture) {
                 is.na(inits$eAlive[i,data$PeriodCapture[i]]) <- TRUE
               }
               inits
             },
             modify_data_derived = function (data) {
               data$Length <- subtract600divide100(data$Length)
               data$Year <- data$Year - 2010

               data
             },
             modify_data = function(data) {

               df <- as.data.frame(data[c("Capture", "Period", "PeriodCapture",
                                          "Monitored", "Moved", "Angled", "Year", "Season",
                                          "Spawned", "SpawningPeriod", "Length")])

               df$Length <- subtract600divide100(df$Length)
               df$Year <- df$Year - 2010

               data$SpawningPeriod <- reshape2::acast(df, Capture ~ Period, value.var = "SpawningPeriod")
               data$SpawningPeriod <- data$SpawningPeriod[1,]

               data$Year <- reshape2::acast(df, Capture ~ Period, value.var = "Year")
               data$Year <- data$Year[1,]

               data$Season <- reshape2::acast(df, Capture ~ Period, value.var = "Season")
               data$Season <- data$Season[1,]

               data$PeriodCapture <- reshape2::acast(df, Capture ~ Period, value.var = "PeriodCapture")
               data$PeriodCapture <- data$PeriodCapture[,1]

               data$Monitored <- reshape2::acast(df, Capture ~ Period, value.var = "Monitored")
               data$Angled <- reshape2::acast(df, Capture ~ Period, value.var = "Angled")
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
                             "Period", "Year", "Season",
                             "Monitored", "Moved", "Angled",
                             "Spawned", "SpawningPeriod", "Length")
  )
}


#' Analyse Detections and Recaptures
#'
#' Analyses detection and recapture data using a mortality model.
#'
#' To view the full model description
#' in the JAGS dialect of the BUGS language use \code{\link{mortality_model_code}}.
#'
#' The data must be a \code{analysis_data} object as generated by the
#' \code{\link{make_analysis_data}} function.
#'
#' @param data A \code{analysis_data} object of the detection and recapture data to analyse.
#' @param niters An integer of the minimum number of MCMC iterations to
#' perform.
#' @return A jags_analysis object.
#' @export
analyse_mortality <- function(data, niters = 10^5) {
  assert_that(is.count(niters) && noNA(niters))

  data %<>% process_data()

  jaggernaut::jags_analysis(mortality_model(), data, niters = niters)
}

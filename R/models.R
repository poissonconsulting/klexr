#' #' Mark-Recapture Model Code
#' #'
#' #' Returns a string of the JAGS code
#' #' defining the mark-recapture model.
#' #'
#' #' @return A string of the JAGS model code.
#' #' @seealso \code{\link{ranmr}}
#' #' @examples
#' #' cat(mr_model_code())
#' #' @export
#' mr_model_code <- function() {
#' "model {
#'
#'   psi ~ dunif(0,1) # inclusion parameter
#'   S ~ dunif(0,1) # annual survival
#'   p ~ dunif(0,1) # probability of (re)capture
#'   rho1 ~ dunif(0, 1) # probability recruit in first year
#'
#'   b2 <- (1 - rho1) / (nYear - 1)
#'   rho[1] <- rho1
#'   for (yr in 2:nYear) {
#'     rho[yr] <- b2 / (1 - b2 * (yr - 1))
#'   }
#'
#'   for (i in 1:nFish) {
#'     w[i] ~ dbern(psi)
#'     z[i, 1] ~ dbern(rho[1])
#'     u[i, 1] <- z[i, 1] * w[i]
#'     y[i, 1] ~ dbern(u[i, 1] * p)
#'     for (j in 2:nYear) {
#'       q[i, j-1] <- 1 - z[i, j-1]
#'       z[i, j] ~ dbern(z[i, j-1] * S + rho[j] * prod(q[i, 1:(j-1)]))
#'       u[i, j] <- z[i, j] * w[i]
#'       y[i, j] ~ dbern(u[i, j] * p)
#'     }
#'   }
#'   for (i in 1:nYear) {
#'     N[i] <- sum(u[1:nFish, i])
#'   }
#'   B <- nFish * psi * (1 - rho1) / (nYear - 1)
#' }"
#' }
#'
#' jmodels <- function() {
#'   model1 <- jaggernaut::jags_model(
#'     mr_model_code(),
#' derived_code = "data {
#'     for(i in 1:nYear) {
#'       prediction[i] <- N[i] * p
#'       observation[i] <- sum(y[1:nFish, i])
#'       simulation[i] ~ dbin(p, N[i])
#'
#'       D_observation[i] <- pow(pow(observation[i], 0.5) - pow(prediction[i], 0.5), 2)
#'       D_simulation[i] <- pow(pow(simulation[i], 0.5) - pow(prediction[i], 0.5), 2)
#'     }
#'     discrepancy <- sum(D_simulation) - sum(D_observation)
#'   }",
#' modify_data = function(data) {
#'   dat <- data.frame(Fish = data$Fish, Year = data$Year)
#'   dat$Recapture <- duplicated(paste(dat$ID, dat$Year))
#'
#'   data$y <- reshape2::acast(dat, Fish ~ Year,
#'                             fun.aggregate = function(x) as.integer(length(x) != 0),
#'                             value.var = "Recapture")
#'
#'   data$y <- rbind(data$y,matrix(0, ncol = data$nYear, nrow = data$nFish - nrow(data$y)))
#'
#'   data$Fish <- NULL
#'   data$Year <- NULL
#'
#'   data
#'   },
#' gen_inits = function(data) {
#'   inits <- list()
#'   inits$z <- array(1,dim(data$y))
#'   inits$w <- rep(1,data$nFish)
#'   inits
#' },
#' random_effects = list(u = c("Fish", "Year")),
#' select_data = c("Fish", "Year"),
#' monitor = c("psi", "p", "S", "rho1", "N", "B")
#' )
#'   model1
#' }
#'
#' #' Analyse Mark-Recapture Data
#' #'
#' #' Analyses mark-recapture data using a Bayesian Jolly-Seber
#' #' model to estimate the abundance, annual survival and (re)capture probability.
#' #' The data must be in the same format as the \code{\link{ferox}}
#' #' data provided with this package (only the Fish and Date columns are required).
#' #'
#' #' @param x A data.frame of the mark-recapture data to analyse.
#' #' @param niters An integer of the minimum number of MCMC iterations to
#' #' perform.
#' #' @return A jags_analysis object.
#' #' @seealso \code{\link{ranmr}}
#' #' @export
#' analyse_mr <- function(x, niters = 10^5) {
#'   assert_that(is.data.frame(x))
#'   assert_that(is.count(niters) && noNA(niters))
#'   check_columns(x, c("Fish", "Date"))
#'
#'   x <- process_mr_data(x)
#'
#'   x$Year <- lubridate::year(x$Date)
#'   x$Year <- factor(x$Year, levels = min(x$Year):max(x$Year))
#'
#'   x <- dplyr::select_(x, ~Fish, ~Year)
#'
#'   jaggernaut::jags_analysis(jmodels(), x, niters = niters)
#' }

dayte <- function (x) {
  as.Date(paste(2000, lubridate::month(x), lubridate::day(x), sep = "-"))
}

#' Season
#'
#' @param x A date-time object.
#'
#' @return An ordered factor with levels Winter, Spring, Summer and Autumn
#' @export
#' @examples
#' season(1:12)
#' season(Sys.Date())
season <- function(x) {
  x %<>% lubridate::month(label = TRUE)
  x %<>% factor()
  levels(x) <- list(Winter = c("Jan", "Feb", "Mar"),
                    Spring = c("Apr", "May", "Jun"),
                    Summer = c("Jul", "Aug", "Sep"),
                    Autumn = c("Oct", "Nov", "Dec"))
  x
}

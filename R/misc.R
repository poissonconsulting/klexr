#' Season
#'
#' @param x A date-time object.
#'
#' @return An ordered factor with levels Spring, Summer, Fall and Winter
#' @export
#' @examples
#' season(1:12)
#' season(Sys.Date())
season <- function(x) {
  x %<>% lubridate::month(label = TRUE)
  x %<>% factor()
  levels(x) <- list(Spring = c("Jan", "Feb", "Mar"),
                    Summer = c("Apr", "May", "Jun"),
                    Fall = c("Jul", "Aug", "Sep"),
                    Winter = c("Oct", "Nov", "Dec"))
  x
}

subtract600divide100 <- function(x) {
  x %<>% magrittr::subtract(600) %>% magrittr::divide_by(100)
  x
}

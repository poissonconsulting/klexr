section_polygon <- function(section) {
  suppressMessages(polygon <- broom::tidy(section))
  polygon %<>% dplyr::rename_(.dots = list(Section = "id", EastingSection = "long",
                                           NorthingSection = "lat"))
  polygon$Section %<>% factor(levels = levels(section@data$Section))
  polygon %<>% dplyr::inner_join(dplyr::select_(section@data, ~-EastingSection, ~-NorthingSection), by = "Section")
  polygon
}

waterbody <- function() {
  waterbody <- dplyr::data_frame(Easting = 480000, Northing = 5605000, Waterbody = "Trout Lake")
  waterbody %<>% dplyr::bind_rows(dplyr::data_frame(Easting = 517500, Northing = 5582500, Waterbody = "Duncan Reservoir"))
  waterbody %<>% dplyr::bind_rows(dplyr::data_frame(Easting = 480000, Northing = 5582500, Waterbody = "Lardeau River"))
  waterbody %<>% dplyr::bind_rows(dplyr::data_frame(Easting = 492500, Northing = 5562500, Waterbody = "Duncan River"))
  waterbody %<>% dplyr::bind_rows(dplyr::data_frame(Easting = 492500, Northing = 5525000, Waterbody = "North Arm\n(Kootenay Lake)"))
  waterbody %<>% dplyr::bind_rows(dplyr::data_frame(Easting = 470000, Northing = 5494000, Waterbody = "West Arm\n(Kootenay Lake)"))
  waterbody %<>% dplyr::bind_rows(dplyr::data_frame(Easting = 500000, Northing = 5475000, Waterbody = "South Arm\n(Kootenay Lake)"))
  waterbody %<>% dplyr::bind_rows(dplyr::data_frame(Easting = 510000, Northing = 5450000, Waterbody = "Kootenay River"))
  waterbody
}

#' Plot Section
#'
#' Maps and labels the sections for the klexdatr \code{lex_data} object.
#'
#' @param data The \code{lex_data} object to plot.
#' @return A ggplot2 object.
#' @export
plot_section <- function(data) {
  section <- data$section
  station <- data$station

  polygon <- section_polygon(section)

  ggplot2::ggplot(data = section@data, ggplot2::aes_(x = ~EastingSection / 1000,
                                                     y = ~NorthingSection / 1000)) +
    ggplot2::geom_polygon(data = dplyr::filter_(polygon, ~!hole),
                          fill = "blue", ggplot2::aes_(group = ~Section)) +
    ggplot2::geom_polygon(data = dplyr::filter_(polygon, ~hole),
                          ggplot2::aes_(group = ~Section), fill = "transparent") +
    ggplot2::geom_point(data = station, ggplot2::aes_(x = ~EastingStation / 1000,
                                                     y = ~NorthingStation / 1000),
                        color = "red", alpha = 1/2, size = 1) +
    ggplot2::geom_label(data = waterbody(), size = 2,
                        ggplot2::aes_(label = ~Waterbody, x = ~Easting / 1000,
                                                     y = ~Northing / 1000)) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}

#' Plot Spawning
#'
#' Plots the probability of spawning by length.
#'
#' @param data The data to plot.
#' @return A ggplot2 object.
#' @export
plot_spawning <- function(data) {
  data %<>% check_data3(values = list(
    Length = c(500L, 800L),
    estimate = c(0, 1),
    lower = c(0, 1),
    upper = c(0, 1)), select = TRUE, min_row = 2)

   ggplot2::ggplot(data = data, ggplot2::aes_(x = ~Length, y = ~estimate)) +
   ggplot2::geom_line() +
   ggplot2::geom_line(ggplot2::aes_(y = ~lower), linetype = "dotted") +
   ggplot2::geom_line(ggplot2::aes_(y = ~upper), linetype = "dotted") +
   ggplot2::scale_x_continuous(name = "Fork Length (mm)") +
   ggplot2::scale_y_continuous(name = "Annual Spawning  Probability (%)", labels = scales::percent) +
   ggplot2::expand_limits(y = c(0, 1))
}

#' Plot Probabilities
#'
#' Plots the key probabilities.
#'
#' @param data The data to plot.
#' @return A ggplot2 object.
#' @export
plot_probs <- function(data) {
  data %<>% check_data3(values = list(
    Parameter = factor(1),
    estimate = c(0, 1),
    lower = c(0, 1),
    upper = c(0, 1)), select = TRUE, min_row = 4)

   ggplot2::ggplot(data = data, ggplot2::aes_(x = ~Parameter, y = ~estimate)) +
   ggplot2::geom_pointrange(ggplot2::aes_(ymin = ~lower, ymax = ~upper)) +
   ggplot2::scale_x_discrete(name = "Parameter") +
   ggplot2::scale_y_continuous(name = "Probability (%)", labels = scales::percent) +
   ggplot2::expand_limits(y = c(0, 1))
}

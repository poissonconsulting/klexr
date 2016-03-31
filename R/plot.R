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

plot_logical_matrix <- function(x) {
  title <- deparse(substitute(x))

  x %<>% reshape2::melt()
  x$value <- x$value == 1
  colnames(x) <- c("Var1", "Var2", "value")

  ggplot2::ggplot(data = x, ggplot2::aes_(x = ~Var2, y = ~Var1)) +
          ggplot2::geom_point(ggplot2::aes_string(shape = "value", color = "value")) +
          ggplot2::scale_color_manual(values = c("red", "black")) +
          ggplot2::scale_shape_manual(values = c(17, 16))
}

#' Plot Analysis Data
#'
#' Plots analysis data.
#'
#' @param data The data to plot.
#' @return A ggplot2 object.
#' @export
plot_analysis_data <- function(data, years = 2008:2013) {

  stopifnot(is.data.frame(data))

  data$Year %<>% factor(, levels = years)

  monitored <- dplyr::filter_(data, ~Monitored)
  capture <- dplyr::filter_(data, ~Period == PeriodCapture)
  spawned <- dplyr::filter_(data, ~Spawned)
  moved <- dplyr::filter_(data, ~Moved)
  reported <- dplyr::filter_(data, ~Reported)

  ggplot2::ggplot(data = data, ggplot2::aes_(x = ~Season, y = ~Capture)) +
    ggplot2::facet_grid(.~Year, drop = FALSE) +
    ggplot2::geom_raster(data = monitored, fill = "grey80") +
    ggplot2::geom_point(data = capture, color = "red", position = position_nudge(x = -0.38)) +
    ggplot2::geom_point(data = moved, color = "grey50", position = position_nudge(x = -0.1333), shape = 18) +
    ggplot2::geom_point(data = spawned, color = "blue", position = position_nudge(x = 0.1333), shape = 17) +
    ggplot2::geom_point(data = reported, position = position_nudge(x = 0.38), shape = 15) +
    ggplot2::scale_x_discrete(name = "Season", expand = c(0, 0.5)) +
    ggplot2::scale_y_discrete(name = "Capture", expand = c(0, 0.5)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = ggplot2::rel(0.6))) +
    ggplot2::theme(panel.margin = ggplot2::unit(0, "in"))
}

drop_post_recaps <- function (x) {
  x %<>% dplyr::arrange_(~Period)
  recap <- which(x$Reported)
  stopifnot(length(recap) < 2)
  if (length(recap))
    x %<>% dplyr::slice(1:recap)
  x
}

#' Plot Analysis Length
#'
#' Plots analysis length.
#'
#' @param data The data to plot.
#' @return A ggplot2 object.
#' @export
plot_analysis_length <- function(data, years = 2008:2013) {

  stopifnot(is.data.frame(data))

  data$Year %<>% factor(, levels = years)

  data %<>% dplyr::filter_(~as.integer(Period) >= as.integer(PeriodCapture))
  capture <- dplyr::filter_(data, ~Period == PeriodCapture)
  recapture <- dplyr::filter_(data, ~Reported)

  data %<>% plyr::ddply(c("Capture"), drop_post_recaps)

  ggplot2::ggplot(data = data, ggplot2::aes_(x = ~Date, y = ~Length)) +
    ggplot2::geom_line(ggplot2::aes_(group = ~Capture)) +
    ggplot2::geom_point(data = capture, color = "red") +
    ggplot2::geom_point(data = recapture, shape = 15) +
    ggplot2::scale_x_date(name = "Year") +
    ggplot2::scale_y_continuous(name = "Fork Length (mm)") +
    ggplot2::expand_limits(x = as.Date("2008-01-01", "2013-12-31"))
}

#' Plot Spawning
#'
#' Plots the probability of spawning by length.
#'
#' @param data The data to plot.
#' @return A ggplot2 object.
#' @export
plot_spawning <- function(data1, data2) {
  data1 %<>% check_data3(values = list(
    Species = factor(1),
    Length = c(500L, 800L),
    estimate = c(0, 1),
    lower = c(0, 1),
    upper = c(0, 1)), select = TRUE, min_row = 2)

  data2 %<>% check_data3(values = list(
    Species = factor(1),
    Length = c(500L, 800L),
    estimate = c(0, 1),
    lower = c(0, 1),
    upper = c(0, 1)), select = TRUE, min_row = 2)

  data1$Species %<>% as.character()
  data2$Species %<>% as.character()

  data <- dplyr::bind_rows(data1, data2)
  data$Species %<>% factor(levels = c(data1$Species[1], data2$Species[1]))

  ggplot2::ggplot(data = data, ggplot2::aes_(x = ~Length, y = ~estimate,
                                             group = ~Species, color = ~Species)) +
    ggplot2::geom_line() +
    ggplot2::geom_line(ggplot2::aes_(y = ~lower), linetype = "dotted") +
    ggplot2::geom_line(ggplot2::aes_(y = ~upper), linetype = "dotted") +
    ggplot2::scale_x_continuous(name = "Fork Length (mm)") +
    ggplot2::scale_y_continuous(name = "Annual Spawning  Probability (%)", labels = scales::percent) +
    ggplot2::scale_color_manual(values = c("black", "red")) +
    ggplot2::expand_limits(y = c(0, 1))
}

#' Plot Probabilities
#'
#' Plots the key probabilities.
#'
#' @param data The data to plot.
#' @return A ggplot2 object.
#' @export
plot_probs <- function(data1, data2) {
  data1 %<>% check_data3(values = list(
    Species = factor(1),
    Parameter = factor(1),
    estimate = c(0, 1),
    lower = c(0, 1),
    upper = c(0, 1)), select = TRUE, min_row = 4)

  data2 %<>% check_data3(values = list(
    Species = factor(1),
    Parameter = factor(1),
    estimate = c(0, 1),
    lower = c(0, 1),
    upper = c(0, 1)), select = TRUE, min_row = 4)

  data1$Species %<>% as.character()
  data2$Species %<>% as.character()

  data <- dplyr::bind_rows(data1, data2)
  data$Species %<>% factor(levels = c(data1$Species[1], data2$Species[1]))

  ggplot2::ggplot(data = data, ggplot2::aes_(x = ~Parameter, y = ~estimate)) +
    ggplot2::geom_pointrange(ggplot2::aes_(ymin = ~lower, ymax = ~upper, color = ~Species),
                             position = ggplot2::position_dodge(width = 0.25)) +
    ggplot2::scale_x_discrete(name = "Parameter") +
    ggplot2::scale_y_continuous(name = "Probability (%)", labels = scales::percent) +
    ggplot2::scale_color_manual(values = c("black", "red")) +
    ggplot2::expand_limits(y = c(0, 1)) +
    ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

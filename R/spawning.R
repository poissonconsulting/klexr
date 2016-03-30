spawning_bt_year <- function(detection, spawning_window) {
  if (!nrow(detection))
    return(NULL)

  detection %<>% dplyr::arrange_(~Dayte)

  if (!detection$Dayte[1] < spawning_window[1])
    return(NULL)
  if (!detection$Dayte[nrow(detection)] > spawning_window[2])
    return(NULL)

  detection$Days <- c(0, diff(detection$Dayte))

  spawning_days <- 28
  detection %<>% dplyr::filter_(~Days >= spawning_days)
  detection %<>% dplyr::filter_(~Dayte >= (spawning_window[1] + spawning_days))
  detection %<>% dplyr::filter_(~(spawning_window[2] - (Dayte - Days)) >= spawning_days)
  return(dplyr::data_frame(Spawning = nrow(detection) >= 1))
}

#' Bull Trout Spawning
#'
#' Identifies rainbow trout spawning events.
#'
#' @param detection A data.frame of the detection data for the capture.
#' @param period A data.frame of the periods.
#' @export
spawning_bt <- function(detection, period) {

  period %<>% dplyr::mutate_(.dots = list(Year = ~lubridate::year(Date)),
                             EndDayte = ~dayte(Date + Days - 1),
                             Dayte = ~dayte(Date))

  period %<>% dplyr::select_(~Period, ~Dayte, ~EndDayte, ~Year)

  spawning_window <- as.Date(paste0("2000-", c("08-01", "09-30")))

  period$Spawning <- NA
  period$Spawning[period$EndDayte < spawning_window[1] |  period$Dayte > spawning_window[2]] <- FALSE

  if (!nrow(detection))
    return(period$Spawning)

  detection %<>% dplyr::mutate_(.dots = list(Year = ~lubridate::year(Date)),
                                Dayte = ~dayte(Date))

  detection %<>% dplyr::select_(~Dayte, ~Year, ~Section, ~Period)

  detection %<>% plyr::ddply("Year", spawning_bt_year, spawning_window)

  for (i in seq_len(nrow(detection))) {
    period$Spawning[is.na(period$Spawning) & period$Year == detection$Year[i]] <- detection$Spawning[i]
  }
  return(period$Spawning)
}

spawning_rb_year <- function(detection, spawning_window) {
  if (!nrow(detection))
    return(NULL)

  detection %<>% dplyr::arrange_(~Dayte)

  gerrard <- dplyr::filter_(detection, ~Section == "S02",
                            ~Dayte >= spawning_window[1],
                            ~Dayte <= spawning_window[2])

  if (nrow(gerrard)) {
    return(dplyr::data_frame(Spawning = TRUE))
  }

  detection %<>% dplyr::filter_(~Section != "S02")

  if (!detection$Dayte[1] < spawning_window[1])
    return(NULL)
  if (!detection$Dayte[nrow(detection)] > spawning_window[2])
    return(NULL)

  detection$Days <- c(0, diff(detection$Dayte))
  detection$From <- c(NA, detection$Section[1:(nrow(detection)-1)])
  detection %<>% dplyr::filter_(~Section %in% c("S07", "S09"))
  detection %<>% dplyr::filter_(~From %in% c("S07", "S09"))

  spawning_days <- 21
  detection %<>% dplyr::filter_(~Days >= spawning_days)
  detection %<>% dplyr::filter_(~Dayte >= (spawning_window[1] + spawning_days))
  detection %<>% dplyr::filter_(~(spawning_window[2] - (Dayte - Days)) >= spawning_days)
  return(dplyr::data_frame(Spawning = nrow(detection) >= 1))
}

#' Rainbow Trout Spawning
#'
#' Identifies rainbow trout spawning events.
#'
#' @param detection A data.frame of the detection data for the capture.
#' @param period A data.frame of the periods.
#' @export
spawning_rb <- function(detection, period) {

  period %<>% dplyr::mutate_(.dots = list(Year = ~lubridate::year(Date)),
                             EndDayte = ~dayte(Date + Days - 1),
                             Dayte = ~dayte(Date))
  period %<>% dplyr::select_(~Period, ~Dayte, ~EndDayte, ~Year)

  spawning_window <- as.Date(paste0("2000-", c("04-01", "05-31")))

  period$Spawning <- NA
  period$Spawning[period$EndDayte < spawning_window[1] |  period$Dayte > spawning_window[2]] <- FALSE

  if (!nrow(detection))
    return(period$Spawning)

  detection %<>% dplyr::mutate_(.dots = list(Year = ~lubridate::year(Date)),
                                Dayte = ~dayte(Date))

  detection %<>% dplyr::select_(~Dayte, ~Year, ~Section, ~Period)

  detection %<>% plyr::ddply("Year", spawning_rb_year, spawning_window)

  for (i in seq_len(nrow(detection))) {
    period$Spawning[is.na(period$Spawning) & period$Year == detection$Year[i]] <- detection$Spawning[i]
  }
  return(period$Spawning)
}

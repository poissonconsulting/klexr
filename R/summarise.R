tags2 <- function (x) {
  tags_bt <- dplyr::group_by_(x, ~Year, ~Reward2) %>%
    dplyr::summarise_(n = ~n()) %>% dplyr::ungroup()

  tags_bt$Year %<>% factor(levels = 2008:2013)
  tags_bt$Reward2 %<>% factor(levels = c("100", "10", "0", "No"))
  tags_bt$Reward2[is.na(tags_bt$Reward2)] <- "No"
  levels(tags_bt$Reward2) <- list("$100" = "100", "$10" = "10", "$0" = "0", "No" = "No")

  tags_bt %<>% tidyr::spread_("Reward2", "n", drop = FALSE)
  tags_bt[is.na(tags_bt)] <- 0
  tags_bt
}

#' Summarise Results
#'
#' Summarises key results as a named list.
#'
#' @param lex The hourly raw data
#' @param detect The daily detection data
#' @param bull_trout The bull trout analysis object
#' @param rainbow_trout The rainbow trout analysis object
#'
#' @return A named list of key results.
#' @export
summarise_results <- function(lex, detect, bull_trout, rainbow_trout) {
  assert_that(is.lex_data(lex))
  assert_that(is.detect_data(detect))
  assert_that(is.jags_analysis(bull_trout))
  assert_that(is.jags_analysis(rainbow_trout))

  sections <- detect$section@data
  stations <- dplyr::inner_join(lex$station, sections, by = "Section")
  captures <- filter(lex$capture, Length >= 500, Reward1 == 100)

  lightest <- dplyr::filter_(captures, ~!is.na(Weight))
  lightest <- captures[which.min(lightest$Weight),]

  captures_bt <- jaggernaut::dataset(bull_trout) %>%
    dplyr::filter_(~Period == PeriodCapture)

  captures_rb <- jaggernaut::dataset(rainbow_trout) %>%
    dplyr::filter_(~Period == PeriodCapture)

  recaptures_bt <- jaggernaut::dataset(bull_trout) %>%
    dplyr::filter_(~Reported)

  recaptures_rb <- jaggernaut::dataset(rainbow_trout) %>%
    dplyr::filter_(~Reported)

  results <- list()
  results$nSections <- nrow(sections)

  results$MainLakeArea <- sum(sections$Area[sections$Habitat == "Lentic" & !sections$Section %in% c("S03", "S04")]) %>% round() %>% as.integer()

  results$S7to9Area <- sum(sections$Area[sections$Section %in% c("S07", "S08", "S09")]) %>% round() %>%
    as.integer()

  results$TotalStations <- nrow(stations)
  results$MainLakeStations <- nrow(stations[!stations$Section %in% c("S02", "S19", "S20"),])
  results$GerrardStations <- nrow(stations[stations$Section == "S02",])
  results$WestArmStations <- nrow(stations[stations$Section %in% c("S19", "S20"),])

  results$Simultaneous <- summary(detect)$simultaneous

  results$LightestLength <- lightest$Length
  results$LightestWeight <- lightest$Weight
  results$LightestSpecies <- lightest$Species %>% as.character()

  results$BullTroutCapture <- nrow(captures[captures$Species == "Bull Trout",])
  results$RainbowTroutCapture <- nrow(captures[captures$Species == "Rainbow Trout",])

  results$BullTroutRewardOnly <- nrow(captures[captures$Species == "Bull Trout" & captures$DateTimeCapture == captures$DateTimeTagExpire,])
  results$RainbowTroutRewardOnly <- nrow(captures[captures$Species == "Rainbow Trout" & captures$DateTimeCapture == captures$DateTimeTagExpire,])

  results$BullTroutSurvive <- nlevels(captures_bt$Capture)
  results$RainbowTroutSurvive <- nlevels(captures_rb$Capture)

  results$tags_bt <- tags2(captures_bt)
  results$tags_rb <- tags2(captures_rb)

  results
}


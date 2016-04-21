tags2 <- function (x) {

  x$Year <- lubridate::year(x$DateTimeCapture)
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
  assert_that(lexr::is.lex_data(lex))
  assert_that(lexr::is.detect_data(detect))
  assert_that(jaggernaut::is.jags_analysis(bull_trout))
  assert_that(jaggernaut::is.jags_analysis(rainbow_trout))

  sections <- detect$section@data
  stations <- dplyr::inner_join(lex$station, sections, by = "Section")
  captures_all <- dplyr::filter_(lex$capture, ~Length >= 500, ~Reward1 == 100)

  captures_all_bt <- dplyr::filter_(captures_all, ~Species == "Bull Trout")
  captures_all_rb <- dplyr::filter_(captures_all, ~Species == "Rainbow Trout")

  lightest <- dplyr::filter_(captures_all, ~!is.na(Weight))
  lightest <- captures_all[which.min(lightest$Weight),]

  captures_bt <- jaggernaut::dataset(bull_trout) %>%
    dplyr::filter_(~Period == PeriodCapture)

  captures_rb <- jaggernaut::dataset(rainbow_trout) %>%
    dplyr::filter_(~Period == PeriodCapture)

  recaptures_bt <- jaggernaut::dataset(bull_trout) %>%
    dplyr::filter_(~Recaptured)

  recaptures_rb <- jaggernaut::dataset(rainbow_trout) %>%
    dplyr::filter_(~Recaptured)

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

  results$BullTroutCapture <- nrow(captures_all_bt)
  results$RainbowTroutCapture <- nrow(captures_all_rb)

  results$BullTroutRewardOnly <- nrow(captures_all_bt[captures_all_bt$DateTimeCapture == captures_all_bt$DateTimeTagExpire,])
  results$RainbowTroutRewardOnly <- nrow(captures_all_rb[captures_all_rb$DateTimeCapture == captures_all_rb$DateTimeTagExpire,])

  results$BullTroutSurvive <- nlevels(captures_bt$Capture)
  results$RainbowTroutSurvive <- nlevels(captures_rb$Capture)

  results$BullTroutRecaptures <- nrow(recaptures_bt)
  results$RainbowTroutRecaptures <- nrow(recaptures_rb)

  recaptures_bt %<>% dplyr::filter_(~Reward1 == 100, ~Reward2 == 10)
  recaptures_rb %<>% dplyr::filter_(~Reward1 == 100, ~Reward2 == 10)

  recaptures_bt$TBarTag1 %<>% factor(levels = c(TRUE, FALSE))
  recaptures_bt$TBarTag2 %<>% factor(levels = c(TRUE, FALSE))

  recaptures_rb$TBarTag1 %<>% factor(levels = c(TRUE, FALSE))
  recaptures_rb$TBarTag2 %<>% factor(levels = c(TRUE, FALSE))

  results$recap_bt <- table(recaptures_bt$TBarTag1, recaptures_bt$TBarTag2, dnn = c("$100", "$10"))
  results$recap_rb <- table(recaptures_rb$TBarTag1, recaptures_rb$TBarTag2, dnn = c("$100", "$10"))

  results$tags_bt <- tags2(captures_all_bt)
  results$tags_rb <- tags2(captures_all_rb)

  results$coef_bt <- coef(bull_trout)
  results$coef_rb <- coef(rainbow_trout)

  results$probs <- plot_probs(predict_probs(bull_trout), predict_probs(rainbow_trout))$data

  results
}


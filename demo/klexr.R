#' ---
#' title: "Kootenay Lake Exploitation Analysis"
#' author: "Joe Thorley"
#' date: "March 23rd, 2015"
#' ---
#'
#' ensure required packages are loaded
library(magrittr)
library(dplyr)
library(jaggernaut)
library(foreach)
library(doParallel)
library(ggplot2)
library(scales)
library(lexr)
library(klexr)

#' for additional information on a function enter: ?function_name

#' create directory to store results
dir.create("results", showWarnings = FALSE, recursive = TRUE)

#' load hourly detection dataset
lex <- input_lex_data("klexdatr")

#' plot a map of the study area
png("results/Figure_1.png", width = 3, height = 6, units = "in", res = 900)
plot_section(lex)
dev.off()

#' aggregate hourly receiver detection data into daily sectional detections
#' drop mortalities within 30 days of release
#' treat all recaptures as if harvested
recapture <- lex$recapture
recapture$Released <- FALSE
detect <- make_detect_data(lex, recapture = recapture,
                           start_date = as.Date("2008-04-01"),
                           end_date = as.Date("2013-12-31"), hourly_interval = 24L,
                           recovery_days = 30L)

#' plot Kootenay Lake by color-coded section
png("results/Figure_2.png", width = 3, height = 6, units = "in", res = 900)
plot_detect_section(detect)
dev.off()

#' plot percent receiver coverage by color-coded section
png("results/Figure_3.png", width = 4, height = 4, units = "in", res = 900)
plot_detect_coverage(detect)
dev.off()

#' plot detections by fish, species, date and color-coded section.
png("results/Figure_4.png", width = 6, height = 8, units = "in", res = 900)
plot_detect_overview(detect)
dev.off()

#' divide captures by species
bull_trout <- filter(detect$capture, Species == "Bull Trout")
rainbow_trout <- filter(detect$capture, Species == "Rainbow Trout")

#' define seasonal periods
interval_period <- mutate(detect$interval, Season = season(Month),
                          Period = paste(Year, Season))$Period
interval_period %<>% factor(levels = unique(.))

#' aggregate bull trout daily detections into monthly detections
bull_trout %<>% make_analysis_data(
  detect, capture = ., interval_period = interval_period,
  spawning = spawning_bt, growth = growth_vb, linf = 1000, k = 0.19
)

#' aggregate rainbow trout daily detections into monthly detections
rainbow_trout %<>% make_analysis_data(
  detect, capture = ., interval_period = interval_period,
  spawning = spawning_rb, growth = growth_vb, linf = 1000, k = 0.19
)

# print JAGS model code for mortality model
cat(mortality_model_code())

#' analyse bull trout data using mortality model
bull_trout %<>% analyse_mortality()

#' print bull trout coefficient table
summary(bull_trout)

probs_bt <- predict_probs(bull_trout)
png("results/Figure_5.png", width = 3, height = 3, units = "in", res = 900)
plot_probs(probs_bt)
dev.off()

# predict and plot probability of bull trout spawning by length
spawning_bt <- predict(bull_trout, parm = "eSpawning",
                       newdata = data_frame(Length = seq(500L, 800L, by = 10L)))
png("results/Figure_6.png", width = 3, height = 3, units = "in", res = 900)
plot_spawning(spawning_bt)
dev.off()

#' analyse rainbow trout data using mortality model
rainbow_trout %<>% analyse_mortality()

#' print rainbow trout coefficient table
summary(rainbow_trout)

probs_rb <- predict_probs(rainbow_trout)
png("results/Figure_7.png", width = 3, height = 3, units = "in", res = 900)
plot_probs(probs_rb)
dev.off()

# predict and plot probability of rainbow trout spawning by length
spawning_rb <- predict(rainbow_trout, parm = "eSpawning",
                       newdata = data_frame(Length = seq(500L, 800L, by = 10L)))
png("results/Figure_8.png", width = 3, height = 3, units = "in", res = 900)
plot_spawning(spawning_rb)
dev.off()

#' save bull trout traceplots
pdf("results/traceplots_bt.pdf")
plot(bull_trout)
dev.off()

#' save rainbow trout traceplots
pdf("results/traceplots_rb.pdf")
plot(rainbow_trout)
dev.off()





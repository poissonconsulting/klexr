#' ---
#' title: "Kootenay Lake Exploitation Analysis"
#' author: "Joe Thorley"
#' date: "March 30th, 2015"
#' ---
#'
#' ensure required packages are loaded
library(magrittr)
library(dplyr)
library(jaggernaut)
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
#' keep captures with a fork length of 500 or more with a $100 reward tag
#' drop mortalities within 30 days of release
#' and treat all recaptures as if harvested
capture <- filter(lex$capture, Length >= 500, Reward1 == 100)
recapture <- lex$recapture
recapture$Released <- FALSE
detect <- make_detect_data(lex, capture = capture, recapture = recapture,
                           start_date = as.Date("2008-04-01"),
                           end_date = as.Date("2013-12-31"),
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
  spawning = spawning_bt, growth = growth_vb, linf = 1000, k = 0.10
)

#' aggregate rainbow trout daily detections into monthly detections
rainbow_trout %<>% make_analysis_data(
  detect, capture = ., interval_period = interval_period,
  spawning = spawning_rb, growth = growth_vb, linf = 1000, k = 0.19
)

#' convert to data frames ready for analysis
bull_trout %<>% as.data.frame()
rainbow_trout %<>% as.data.frame()

#' add season column
bull_trout$Season <- season(bull_trout$Month)
rainbow_trout$Season <- season(rainbow_trout$Month)

#' plot analysis data for bull trout
png("results/Figure_5.png", width = 6, height = 6, units = "in", res = 900)
plot_analysis_data(bull_trout)
dev.off()

#' plot analysis data for rainbow trout
png("results/Figure_6.png", width = 6, height = 8, units = "in", res = 900)
plot_analysis_data(rainbow_trout)
dev.off()

#' plot analysis lengths for bull trout
png("results/Figure_7.png", width = 3, height = 3, units = "in", res = 900)
plot_analysis_length(bull_trout)
dev.off()

#' plot analysis lengths for rainbow trout
png("results/Figure_8.png", width = 3, height = 3, units = "in", res = 900)
plot_analysis_length(rainbow_trout)
dev.off()

# print JAGS model code for mortality model
cat(survival_model_code())

#' analyse bull trout data using mortality model
bull_trout %<>% analyse_survival()

#' save rainbow trout analysis to results
saveRDS(bull_trout, "results/bull_trout.rds")

#' print bull trout coefficient table
summary(bull_trout)

#' save bull trout traceplots
pdf("results/traceplots_bt.pdf")
plot(bull_trout)
dev.off()

#' analyse rainbow trout data using mortality model
rainbow_trout %<>% analyse_survival()

#' save rainbow trout analysis to results
saveRDS(rainbow_trout, "results/rainbow_trout.rds")

#' print rainbow trout coefficient table
summary(rainbow_trout)

#' save rainbow trout traceplots
pdf("results/traceplots_rb.pdf")
plot(rainbow_trout)
dev.off()

# predict and plot key parameters
probs_bt <- predict_probs(bull_trout)
probs_rb <- predict_probs(rainbow_trout)
png("results/Figure_9.png", width = 4, height = 4, units = "in", res = 900)
plot_probs(probs_bt, probs_rb)
dev.off()

# predict and plot probability of spawning by length
spawn_bt <- predict(bull_trout, parm = "eSpawning",
                       newdata = data_frame(Length = seq(500L, 800L, by = 10L)))
spawn_rb <- predict(rainbow_trout, parm = "eSpawning",
                       newdata = data_frame(Length = seq(500L, 800L, by = 10L)))
png("results/Figure10.png", width = 4, height = 3, units = "in", res = 900)
plot_spawning(spawn_bt, spawn_rb)
dev.off()

#' save list of summary information
summary <- summarise_results(lex, detect, bull_trout, rainbow_trout)
saveRDS(summary, "results/summary.rds")

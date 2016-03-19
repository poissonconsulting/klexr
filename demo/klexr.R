# ensure required packages are loaded
library(magrittr)
library(dplyr)
library(lubridate)
library(jaggernaut)
library(foreach)
library(doParallel)
library(ggplot2)
library(klexdatr)
library(lexr)
library(klexr)

# for additional information on a function enter: ?function_name

# create directory to store results
dir.create("results", showWarnings = FALSE, recursive = TRUE)

# load hourly detection dataset
lex <- input_lex_data("klexdatr")

# only select those individuals with a fork length of 500 mm or more
capture <- filter(lex$capture, Length >= 500)

# aggregate hourly receiver detection data into 6 hourly interval by section
detect <- make_detect_data(lex, capture = capture, start_date = as.Date("2008-04-01"),
                           end_date = as.Date("2013-12-31"), hourly_interval = 6L)

# save detection data
saveRDS(detect, "results/detect.RDS")

# plot Kootenay Lake by color-coded section
png("results/Figure_2.png", width = 3, height = 6, units = "in", res = 900)
plot_detect_section(detect)
dev.off()

# plot percent receiver coverage by color-coded section
png("results/Figure_3.png", width = 4, height = 4, units = "in", res = 900)
plot_detect_coverage(detect)
dev.off()

# plot detections by fish, species, date and color-coded section.
png("results/Figure_4.png", width = 6, height = 6, units = "in", res = 900)
plot_detect_overview(detect)
dev.off()

# divide capture into species
bull_trout <- filter(detect$capture, Species == "Bull Trout")
rainbow_trout <-  filter(detect$capture, Species == "Rainbow Trout")

# group six hour intervals into monthly periods
interval_period <- mutate(detect$interval, Month = month(Month, label = TRUE),
                          Period = paste(Year, Month))$Period
interval_period %<>% factor(levels = unique(.))

# # produce monthly bull trout data ready for analysis
# bull_trout %<>% make_analysis_data(
#   detect, capture = ., interval_period = interval_period, spawning = spawning_bt,
#   growth = growth_bt, linf = 1000, k = 0.19
# )
#
# # produce monthly rainbow trout data ready for analysis
# rainbow_trout %<>% make_analysis_data(
#   detect, capture = ., interval_period = interval_period, spawning = spawning_rb,
#   growth = growth_rb, linf = 1000, k = 0.19
# )
#
# # print and save summaries of data
# summary(ferox)
# save_rds(summarise_mr(ferox), "data")
# tabulate_mr(ferox)
# save_rds(tabulate_mr(ferox), "table")
#
# # set number of genuine and pseudoindividuals to be 1000
# levels(ferox$Fish) %<>% c(paste0("Pseudo", 1:(1000 - nlevels(ferox$Fish))))
#
# # print JAGS model code
# cat(mr_model_code())
#
# # perform mark-recapture analysis and save results
# analysis <- analyse_mr(ferox)
# summary(analysis)
#
# save_rds(analysis, "analysis")
# save_rds(coef(analysis), "coef")
#
# pdf(file = "results/pdf/")
# plot(bull_trout)
# dev.off()
#
# # save pdf of analysis traceplots
# save_pdf(analysis, "traceplots")

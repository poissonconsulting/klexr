#' ---
#' title: "Kootenay Lake Exploitation Analysis"
#' author: "Joe Thorley"
#' ---
#'
#' ensure required packages are loaded
library(stats)
library(magrittr)
library(dplyr)
library(jaggernaut)
library(ggplot2)
library(scales)
library(lexr)
library(klexr)

#jaggernaut::opts_jagr(mode = "debug")
#doParallel::registerDoParallel(jaggernaut::opts_jagr()$nchains)
#jaggernaut::opts_jagr(parallel = TRUE)

#' for additional information on a function enter: ?function_name

#' create directory to store results
dir.create("results", showWarnings = FALSE, recursive = TRUE)

#' load hourly detection dataset
lex <- input_lex_data("klexdatr")

lex %<>% combine_sections_lex_data(list("S01" = c("S01", "S05", "S06"),
                                        "S18" = c("S18", "S21")))

#' plot a map of the study area
png("results/area.png", width = 3, height = 6, units = "in", res = getOption("res", 150))
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
png("results/section.png", width = 3, height = 6, units = "in", res = getOption("res", 150))
plot_detect_section(detect)
dev.off()

#' plot percent receiver coverage by color-coded section
png("results/coverage.png", width = 4, height = 4, units = "in", res = getOption("res", 150))
plot_detect_coverage(detect)
dev.off()

#' plot daily detections by fish, species, date and color-coded section.
png("results/daily.png", width = 6, height = 8, units = "in", res = getOption("res", 150))
plot_detect_overview(detect)
dev.off()

#' divide captures by species
bull_trout <- filter(detect$capture, Species == "Bull Trout")
rainbow_trout <- filter(detect$capture, Species == "Rainbow Trout")

# just keep main lake sections for plotting use and last detections
section <- detect$section[!detect$section@data$Section %in% paste0("S", c(paste0("0", 1:6),19,33)),]

#' plot habitat use by color-coded section for Bull Trout
png("results/use_bt.png", width = 3, height = 6, units = "in", res = getOption("res", 150))
plot_use_detect(filter_detect_data(detect, capture = bull_trout, section = section))
dev.off()

#' plot habitat use by color-coded section for Rainbow Trout
png("results/use_rb.png", width = 3, height = 6, units = "in", res = getOption("res", 150))
plot_use_detect(filter_detect_data(detect, capture = rainbow_trout, section = section))
dev.off()

#' get number of fish last detected at each main lake section
last_bt <- last_section_data(filter_detect_data(detect, capture = bull_trout, section = section), delay_days = 30L)
last_rb <- last_section_data(filter_detect_data(detect, capture = rainbow_trout, section = section), delay_days = 30L)

last_bt %<>% group_by(Section) %>% summarise(Fish = n())
last_rb %<>% group_by(Section) %>% summarise(Fish = n())

#' save bull trout last detection sections to results
saveRDS(last_bt, "results/last_bt.rds")
#' save rainbow trout last detection sections to results
saveRDS(last_rb, "results/last_rb.rds")

#' define seasonal periods
interval_period <- mutate(detect$interval, Season = season(Month),
                          Period = paste(Year, Season))$Period
interval_period %<>% factor(levels = unique(.))

#' aggregate bull trout daily detections into monthly detections
bull_trout %<>% make_analysis_data(
  detect, capture = ., interval_period = interval_period,
  spawning = spawning_bt, growth = growth_vb, linf = 1000, k = 0.14
)

#' aggregate rainbow trout daily detections into monthly detections
rainbow_trout %<>% make_analysis_data(
  detect, capture = ., interval_period = interval_period,
  spawning = spawning_rb, growth = growth_vb, linf = 1000, k = 0.19
)

#' convert to data frames ready for analysis
bull_trout %<>% as.data.frame()
rainbow_trout %<>% as.data.frame()

# rename Reported as Recaptured and add Season column
bull_trout %<>% rename(Recaptured = Reported) %>% mutate(Season = season(Month))
rainbow_trout %<>% rename(Recaptured = Reported) %>% mutate(Season = season(Month))

#' plot analysis data for bull trout
png("results/seasonal_bt.png", width = 6, height = 6, units = "in", res = getOption("res", 150))
plot_analysis_data(bull_trout)
dev.off()

#' plot analysis data for rainbow trout
png("results/seasonal_rb.png", width = 6, height = 8, units = "in", res = getOption("res", 150))
plot_analysis_data(rainbow_trout)
dev.off()

#' plot analysis lengths for bull trout
png("results/lengths_bt.png", width = 3, height = 3, units = "in", res = getOption("res", 150))
plot_analysis_length(bull_trout)
dev.off()

#' plot analysis lengths for rainbow trout
png("results/lengths_rb.png", width = 3, height = 3, units = "in", res = getOption("res", 150))
plot_analysis_length(rainbow_trout)
dev.off()

# print JAGS model code for final survival model
cat(survival_model_code(model = "final"))

#' analyse bull trout data using final survival model
survival_bt <- analyse_survival(bull_trout, model = "final")

#' save bull trout survival analysis to results
saveRDS(survival_bt, "results/survival_bt.rds")

#' print bull trout survival coefficient table
summary(survival_bt)

#' save bull trout survival traceplots
pdf("results/traceplots_survival_bt.pdf")
plot(survival_bt)
dev.off()

#' analyse rainbow trout data using final survival model
survival_rb <- analyse_survival(rainbow_trout, model = "final")

#' save rainbow trout survival analysis to results
saveRDS(survival_rb, "results/survival_rb.rds")

#' print rainbow trout survival coefficient table
summary(survival_rb)

#' save rainbow trout traceplots
pdf("results/traceplots_survival_rb.pdf")
plot(survival_rb)
dev.off()

values = data_frame(Year = 2010L, Length = 650L, SpawningPeriod = FALSE, Spawned = FALSE)

pred_bt <- predict(survival_bt, parm = "eMoving", newdata = "SpawningPeriod", values = values)
pred_rb <- predict(survival_rb, parm = "eMoving", newdata = "SpawningPeriod", values = values)
movement_spawningseason <- plot_probability(pred_bt, pred_rb, x = "SpawningPeriod", xlab = "Spawning Season", ylab = "Seasonal Movement (%)")
png("results/movement_spawningseason.png", width = 4, height = 3, units = "in", res = getOption("res", 150))
movement_spawningseason
dev.off()

pred_bt <- predict(survival_bt, parm = "eSpawning", newdata = data_frame(Length = seq(500L, 800L, by = 10L)), values = values)
pred_rb <- predict(survival_rb, parm = "eSpawning", newdata = data_frame(Length = seq(500L, 800L, by = 10L)), values = values)
spawning_length <- plot_probability(pred_bt, pred_rb, x = "Length", xlab = "Fork Length (mm)", ylab = "Spawning (%)")
png("results/spawning_length.png", width = 4, height = 3, units = "in", res = getOption("res", 150))
spawning_length
dev.off()

pred_bt <- predict(survival_bt, parm = "eRecaptureAnnual", newdata = "Year", values = values)
pred_rb <- predict(survival_rb, parm = "eRecaptureAnnual", newdata = "Year", values = values)
recapture_year <- plot_probability(pred_bt, pred_rb, x = "Year", ylab = "Annual Recapture (%)")
png("results/recapture_year.png", width = 4, height = 3, units = "in", res = getOption("res", 150))
recapture_year
dev.off()

pred_bt <- predict(survival_bt, parm = "eSurvivalAnnual", newdata = "Year", values = values)
pred_rb <- predict(survival_rb, parm = "eSurvivalAnnual", newdata = "Year", values = values)
pred_bt$Year %<>% factor()
pred_rb$Year %<>% factor()
pred_bt %<>% filter(Year != "2008")
survival_year <- plot_probability(pred_bt, pred_rb, x = "Year", ylab = "Annual Survival (%)")
png("results/survival_year.png", width = 4, height = 3, units = "in", res = getOption("res", 150))
survival_year
dev.off()

pred_bt <- predict(survival_bt, parm = "eSurvivalAnnual", newdata = "Spawned", values = values)
pred_rb <- predict(survival_rb, parm = "eSurvivalAnnual", newdata = "Spawned", values = values)
survival_spawning <- plot_probability(pred_bt, pred_rb, x = "Spawned", ylab = "Annual Survival (%)")
png("results/survival_spawning.png", width = 4, height = 3, units = "in", res = getOption("res", 150))
survival_spawning
dev.off()

pred_bt <- predict(survival_bt, parm = "eMortalityLengthAnnual", newdata = data_frame(Length = seq(500L, 800L, by = 10L)), values = values)
pred_rb <- predict(survival_rb, parm = "eMortalityLengthAnnual", data_frame(Length = seq(500L, 800L, by = 10L)), values = values)
mortality_length <- plot_mortality(pred_bt, pred_rb, x = "Length", xlab = "Fork Length (mm)", ylab = "Natural Mortality (M)")
png("results/mortality_length.png", width = 4, height = 3, units = "in", res = getOption("res", 150))
mortality_length
dev.off()

#' create list of summary information
summary <- summarise_results(lex, detect, survival_bt, survival_rb)

# add plot information to summary
summary$movement_spawningseason <- movement_spawningseason$data
summary$spawning_length <- spawning_length$data
summary$recapture_year <- recapture_year$data
summary$survival_year <- survival_year$data
summary$survival_spawning <- survival_spawning$data
summary$mortality_length <- mortality_length$data

# print JAGS model code for tag loss model
cat(tagloss_model_code())

#' analyse bull trout data using tagloss model
tagloss_bt <- analyse_tagloss(summary$recap_bt)
#' analyse rainbow trout data using tagloss model
tagloss_rb <- analyse_tagloss(summary$recap_rb)

# add tagloss coefficent estimates to summary information
summary$tagloss_bt <- coef(tagloss_bt)
summary$tagloss_rb <- coef(tagloss_rb)

#' save list of summary information
saveRDS(summary, "results/summary.rds")

#' save bull trout tagloss analysis to results
saveRDS(tagloss_bt, "results/tagloss_bt.rds")

#' print bull trout tagloss coefficient table
summary(tagloss_bt)

#' save bull trout tagloss traceplots
pdf("results/traceplots_tagloss_bt.pdf")
plot(tagloss_bt)
dev.off()

#' save rainbow trout tagloss analysis to results
saveRDS(tagloss_rb, "results/tagloss_rb.rds")

#' print rainbow trout tagloss coefficient table
summary(tagloss_rb)

#' save rainbow trout tagloss traceplots
pdf("results/traceplots_tagloss_rb.pdf")
plot(tagloss_rb)
dev.off()

# Recompute the numbers behind the "Seasonal ET patterns..." Results
# paragraph, using the RECONSTRUCTED (observed + gap-filled) monthly ET
# series from predict_ET_full_2004_2016.R (`monthly_final`), and adding
# mean +/- SD for every period reported.
#
# Season/period definitions follow the existing factors used elsewhere in
# the project (R/Pre_proc_data.R "season"):
#   flooded          = Feb-May   (months 2-5)
#   dry season       = Jun-Sep   (months 6-9)
#   rainy non-flooded = Oct-Jan  (months 10-12, 1)
# "early rainy season" (used only for the 2017 anomaly) = Sep-Dec, a
# separate ad-hoc window used in the original text, distinct from the
# general "rainy non-flooded" period above.
#
# ASSUMPTION to flag: "2015-2016 dry season" / "September" values are read
# here as the dry season that follows the Sep/2015-May/2016 wet season,
# i.e. calendar Jun-Sep 2016 (and September 2016). If you meant Jun-Sep
# 2015 instead, change `dry_year <- 2016` below to 2015.

library(tidyverse)
library(lubridate)

if (!exists("monthly_final") || !exists("df")) {
  stop("Rode predict_ET_full_2004_2016.R primeiro - este script depende de ",
       "`monthly_final` e `df` gerados por ele.")
}

pct_diff <- function(value, clim_mean) 100 * (value - clim_mean) / clim_mean

et_data <- monthly_final %>%
  mutate(
    season_grp = case_when(
      month %in% 2:5           ~ "flooded",
      month %in% 6:9           ~ "dry",
      month %in% c(10, 11, 12, 1) ~ "rainy_nonflooded"
    )
  )

# 1) Long-term climatology per season group (mean ACROSS YEARS of each
# year's period-mean ET, so the SD reflects real interannual variability,
# not just within-period monthly spread) ------------------------------------
yearly_period_means <- et_data %>%
  group_by(year, season_grp) %>%
  summarise(period_mean = mean(value, na.rm = TRUE), .groups = "drop")

clim_by_season <- yearly_period_means %>%
  group_by(season_grp) %>%
  summarise(clim_mean = mean(period_mean, na.rm = TRUE),
            clim_sd   = sd(period_mean, na.rm = TRUE),
            n_years   = sum(!is.na(period_mean)),
            .groups = "drop")

cat("============================================================\n")
cat("1) Long-term climatology by season (mean +/- SD across years, mm/month)\n")
print(clim_by_season)

# 2) Calendar-month climatology (needed for September / January / February /
# October specifically) ------------------------------------------------------
clim_by_month <- et_data %>%
  group_by(month) %>%
  summarise(clim_mean = mean(value, na.rm = TRUE),
            clim_sd   = sd(value, na.rm = TRUE),
            n_years   = sum(!is.na(value)),
            .groups = "drop")

cat("------------------------------------------------------------\n")
cat("2) Long-term climatology by calendar month (mean +/- SD, mm/month):\n")
print(clim_by_month, n = 12)

sep_clim <- clim_by_month %>% filter(month == 9)
jan_clim <- clim_by_month %>% filter(month == 1)
feb_clim <- clim_by_month %>% filter(month == 2)
oct_clim <- clim_by_month %>% filter(month == 10)

cat("------------------------------------------------------------\n")
cat("Rewrite for the paragraph (general seasonal pattern):\n")
cat(sprintf("  Flooded period: %.0f +/- %.0f mm (n=%d years)\n",
            clim_by_season$clim_mean[clim_by_season$season_grp == "flooded"],
            clim_by_season$clim_sd[clim_by_season$season_grp == "flooded"],
            clim_by_season$n_years[clim_by_season$season_grp == "flooded"]))
cat(sprintf("  Dry season: %.0f +/- %.0f mm (n=%d years)\n",
            clim_by_season$clim_mean[clim_by_season$season_grp == "dry"],
            clim_by_season$clim_sd[clim_by_season$season_grp == "dry"],
            clim_by_season$n_years[clim_by_season$season_grp == "dry"]))
cat(sprintf("  September (minimum): %.0f +/- %.0f mm\n", sep_clim$clim_mean, sep_clim$clim_sd))
cat(sprintf("  Rainy non-flooded period: %.0f +/- %.0f mm (n=%d years)\n",
            clim_by_season$clim_mean[clim_by_season$season_grp == "rainy_nonflooded"],
            clim_by_season$clim_sd[clim_by_season$season_grp == "rainy_nonflooded"],
            clim_by_season$n_years[clim_by_season$season_grp == "rainy_nonflooded"]))
cat(sprintf("  Jan -> Feb decline: %.1f mm (Jan %.0f +/- %.0f -> Feb %.0f +/- %.0f)\n",
            jan_clim$clim_mean - feb_clim$clim_mean,
            jan_clim$clim_mean, jan_clim$clim_sd, feb_clim$clim_mean, feb_clim$clim_sd))

# 2b) Significance of the Jan -> Feb decline: PAIRED t-test across years ----
# Paired (not independent-sample) because January and February of the SAME
# year are correlated (shared interannual climate signal, e.g. that year's
# ENSO state) - pairing removes that shared variance and is the
# statistically appropriate test here.
jan_feb_paired <- et_data %>%
  filter(month %in% c(1, 2)) %>%
  select(year, month, value) %>%
  pivot_wider(names_from = month, values_from = value, names_prefix = "m") %>%
  filter(!is.na(m1) & !is.na(m2))

cat("------------------------------------------------------------\n")
cat("Jan -> Feb decline: paired t-test across", nrow(jan_feb_paired), "years\n")
jan_feb_test <- t.test(jan_feb_paired$m1, jan_feb_paired$m2, paired = TRUE)
print(jan_feb_test)
cat(sprintf("  Mean difference (Jan - Feb): %.1f mm | 95%% CI [%.1f, %.1f] | t(%d) = %.2f | p = %.4f\n",
            jan_feb_test$estimate, jan_feb_test$conf.int[1], jan_feb_test$conf.int[2],
            jan_feb_test$parameter, jan_feb_test$statistic, jan_feb_test$p.value))

# 3) 2015-2016 ENSO event -----------------------------------------------------
# Flooded phase = Feb-May 2016 (still part of the Sep/2015-May/2016 wet
# season referenced in the text); dry season + September = calendar 2016.
dry_year <- 2016

flood_2016 <- yearly_period_means %>% filter(year == dry_year, season_grp == "flooded") %>% pull(period_mean)
dry_2016   <- yearly_period_means %>% filter(year == dry_year, season_grp == "dry") %>% pull(period_mean)
sep_2016   <- et_data %>% filter(year == dry_year, month == 9) %>% pull(value)

cat("------------------------------------------------------------\n")
cat("3) 2015-2016 ENSO event (calendar", dry_year, "for dry season/September):\n")
cat(sprintf("  Flooded phase (Feb-May %d): %.0f mm\n", dry_year, flood_2016))
cat(sprintf("  Dry season (Jun-Sep %d): %.1f mm -> %.1f%% vs long-term mean (%.0f +/- %.0f)\n",
            dry_year, dry_2016,
            pct_diff(dry_2016, clim_by_season$clim_mean[clim_by_season$season_grp == "dry"]),
            clim_by_season$clim_mean[clim_by_season$season_grp == "dry"],
            clim_by_season$clim_sd[clim_by_season$season_grp == "dry"]))
cat(sprintf("  September %d minimum: %.1f mm -> %.1f%% vs long-term Sept. mean (%.0f +/- %.0f)\n",
            dry_year, sep_2016, pct_diff(sep_2016, sep_clim$clim_mean),
            sep_clim$clim_mean, sep_clim$clim_sd))

# 4) 2016-2017 ENSO event / 2017 early rainy season anomaly -----------------
# "Early rainy season" (Sep-Dec 2017) - an ad-hoc window distinct from the
# general "rainy non-flooded" (Oct-Jan) grouping above.
early_rainy_yearly <- et_data %>%
  filter(month %in% 9:12) %>%
  group_by(year) %>%
  summarise(period_mean = mean(value, na.rm = TRUE), .groups = "drop")

clim_early_rainy <- early_rainy_yearly %>%
  summarise(clim_mean = mean(period_mean, na.rm = TRUE),
            clim_sd   = sd(period_mean, na.rm = TRUE),
            n_years   = sum(!is.na(period_mean)))

early_rainy_2017 <- early_rainy_yearly %>% filter(year == 2017) %>% pull(period_mean)
oct_2017 <- et_data %>% filter(year == 2017, month == 10) %>% pull(value)

cat("------------------------------------------------------------\n")
cat("4) 2016-2017 ENSO event - early rainy season (Sep-Dec) anomaly:\n")
cat(sprintf("  Long-term early-rainy-season mean: %.0f +/- %.0f mm (n=%d years)\n",
            clim_early_rainy$clim_mean, clim_early_rainy$clim_sd, clim_early_rainy$n_years))
cat(sprintf("  Sep-Dec 2017: %.1f mm -> %.1f%% vs long-term mean\n",
            early_rainy_2017, pct_diff(early_rainy_2017, clim_early_rainy$clim_mean)))
cat(sprintf("  October 2017 minimum: %.1f mm -> %.1f%% vs long-term Oct. mean (%.0f +/- %.0f)\n",
            oct_2017, pct_diff(oct_2017, oct_clim$clim_mean), oct_clim$clim_mean, oct_clim$clim_sd))

# 5) Supporting drivers: Jan->Feb rainfall/radiation, and Oct-2017 VPD -------
# (kept from the original paragraph; recomputed here for consistency with
# the same date range/pipeline as the ET numbers)
trmm_raw <- read_csv("data/TRMM_ban_19980101_20181001.csv")
trmm_raw$date <- parse_date_time(substr(trmm_raw$`system:index`, 6, 16), "ymd_h")
trmm_monthly <- trmm_raw %>%
  mutate(precip_acum = precipitation * 3) %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(tot_precip = sum(precip_acum, na.rm = TRUE), .groups = "drop")

rain_clim <- trmm_monthly %>%
  group_by(month) %>%
  summarise(clim_mean = mean(tot_precip, na.rm = TRUE), clim_sd = sd(tot_precip, na.rm = TRUE), .groups = "drop")

ssrd_monthly <- df %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(ssrd_mean = mean(ssrd, na.rm = TRUE), .groups = "drop")

ssrd_clim <- ssrd_monthly %>%
  group_by(month) %>%
  summarise(clim_mean = mean(ssrd_mean, na.rm = TRUE), clim_sd = sd(ssrd_mean, na.rm = TRUE), .groups = "drop")

vpd_monthly <- df %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(vpd_mean = mean(vpd, na.rm = TRUE), .groups = "drop")

vpd_oct_clim <- vpd_monthly %>% filter(month == 10) %>%
  summarise(clim_mean = mean(vpd_mean, na.rm = TRUE), clim_sd = sd(vpd_mean, na.rm = TRUE))
vpd_oct_2017 <- vpd_monthly %>% filter(year == 2017, month == 10) %>% pull(vpd_mean)

cat("------------------------------------------------------------\n")
cat("5) Supporting drivers:\n")
cat(sprintf("  Rainfall Jan -> Feb (climatology): %.1f -> %.1f mm (delta = %.1f mm)\n",
            rain_clim$clim_mean[rain_clim$month == 1], rain_clim$clim_mean[rain_clim$month == 2],
            rain_clim$clim_mean[rain_clim$month == 2] - rain_clim$clim_mean[rain_clim$month == 1]))
cat(sprintf("  SSRD Jan -> Feb (climatology): %.1f -> %.1f W/m2 (delta = %.1f W/m2)\n",
            ssrd_clim$clim_mean[ssrd_clim$month == 1], ssrd_clim$clim_mean[ssrd_clim$month == 2],
            ssrd_clim$clim_mean[ssrd_clim$month == 2] - ssrd_clim$clim_mean[ssrd_clim$month == 1]))
cat(sprintf("  VPD October 2017: %.2f kPa -> %.1f%% vs long-term Oct. mean (%.2f +/- %.2f kPa)\n",
            vpd_oct_2017, pct_diff(vpd_oct_2017, vpd_oct_clim$clim_mean),
            vpd_oct_clim$clim_mean, vpd_oct_clim$clim_sd))
cat("============================================================\n")


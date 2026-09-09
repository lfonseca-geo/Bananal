# Update the tower-vs-MODIS comparison paragraph:
#   "Mean annual ET was X mm/day (tower, reconstructed) and Y mm/day
#    (MODIS)... r2 = Z... interannual variability MODIS/modelled = A%/B%"
# using the RECONSTRUCTED tower series only (observed + gap-filled, from
# predict_ET_full_2004_2016.R) - kept simple on purpose, comparing only
# reconstructed vs MODIS, to avoid ambiguity in the paragraph about which
# tower series is being referenced.
#
# data/ET_MOD16.csv is MODIS MOD16, already pre-aggregated to monthly
# totals (mm/month), format DD/MM/YY - the DD is always "01", only
# month/year vary.
#
# Reuses `pred_df_full` from predict_ET_full_2004_2016.R - run that script
# first.

library(tidyverse)
library(lubridate)

if (!exists("pred_df_full")) {
  stop("Rode predict_ET_full_2004_2016.R primeiro - este script depende de ",
       "`pred_df_full` gerado por ele.")
}

# 1) Load MODIS MOD16 monthly ET ---------------------------------------------
modis_monthly <- read_csv("data/ET_MOD16.csv", col_types = cols()) %>%
  dplyr::filter(!is.na(ET_MOD16) & Data != "") %>%
  mutate(date = parse_date_time(Data, "dmy"),
         year = year(date), month = month(date)) %>%
  select(year, month, ET_modis = ET_MOD16)

cat("MODIS monthly range:", format(min(with(modis_monthly, parse_date_time(paste(year,month,15),"ymd")))),
    "to", format(max(with(modis_monthly, parse_date_time(paste(year,month,15),"ymd")))), "\n")
cat("MODIS months available:", nrow(modis_monthly), "\n")

# 2) Tower monthly series: RECONSTRUCTED (observed + gap-filled) only --------
tower_monthly <- pred_df_full %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(ET_modelled = sum(ET_gapfilled, na.rm = TRUE), .groups = "drop")

# 3) Join, restricted to the MODIS-covered window ----------------------------
compare_df <- modis_monthly %>%
  left_join(tower_monthly, by = c("year", "month")) %>%
  mutate(n_days = days_in_month(as.Date(paste(year, month, 1), "%Y %m %d")))

cat("------------------------------------------------------------\n")
cat("Months with both series present:", sum(complete.cases(compare_df[c("ET_modis","ET_modelled")])), "\n")

# 4) Mean daily ET (mm/day) over the common period --------------------------
# total ET summed over the period / total days in the period - avoids bias
# from simply averaging monthly rates of unequal-length months.
mean_daily <- function(monthly_values, n_days) sum(monthly_values, na.rm = TRUE) / sum(n_days[!is.na(monthly_values)])

cat("------------------------------------------------------------\n")
cat("4) Mean daily ET over the common period (mm/day):\n")
cat(sprintf("  Tower (reconstructed): %.2f mm/day\n", mean_daily(compare_df$ET_modelled, compare_df$n_days)))
cat(sprintf("  MODIS (MOD16): %.2f mm/day\n", mean_daily(compare_df$ET_modis, compare_df$n_days)))

# 5) Agreement (r2) between reconstructed tower ET and MODIS, monthly scale -
valid_pair <- compare_df %>% dplyr::filter(!is.na(ET_modelled) & !is.na(ET_modis))
r2_model_modis <- cor(valid_pair$ET_modelled, valid_pair$ET_modis, use = "complete.obs")^2

cat("------------------------------------------------------------\n")
cat("5) Agreement (r2), monthly scale:\n")
cat(sprintf("  Tower (reconstructed) vs MODIS: r2 = %.2f (n = %d months)\n", r2_model_modis, nrow(valid_pair)))

# 6) Interannual variability (CV%), using only years with 12/12 months ------
cv_pct <- function(x) 100 * sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)

annual_totals <- compare_df %>%
  group_by(year) %>%
  summarise(
    n_modis  = sum(!is.na(ET_modis)),
    n_model  = sum(!is.na(ET_modelled)),
    tot_modis    = if_else(n_modis == 12, sum(ET_modis, na.rm = TRUE), NA_real_),
    tot_modelled = if_else(n_model == 12, sum(ET_modelled, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

cat("------------------------------------------------------------\n")
cat("6) Annual totals per series (NA = that year had <12 complete months):\n")
print(annual_totals %>% select(year, tot_modis, tot_modelled), n = 20)

cat("------------------------------------------------------------\n")
cat("Interannual variability (CV%, using only years with 12/12 months for that series):\n")
cat(sprintf("  MODIS: %.2f%% (n = %d complete years)\n",
            cv_pct(annual_totals$tot_modis), sum(!is.na(annual_totals$tot_modis))))
cat(sprintf("  Tower (reconstructed): %.2f%% (n = %d complete years)\n",
            cv_pct(annual_totals$tot_modelled), sum(!is.na(annual_totals$tot_modelled))))


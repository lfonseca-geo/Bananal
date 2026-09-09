# Exploratory check: are 2011-2012 tower predictors + ET good enough to
# extend tET_6 gap-filling past the current training window (which stops at
# 2010-09-30 in GammaDist_GAMs.R / predict_tET_6.R / predict_ET_GAM_LOYO.R)?
#
# Three views:
#   1) Hourly time series of each tET_6 predictor + ET for 2011-2012, with a
#      dashed line marking where the current training window ends.
#   2) Monthly % of hours with ALL tET_6 predictors simultaneously present
#      (over ALL hours, regardless of whether ET is already observed).
#   3) The number that actually matters for gap-filling: restricted to just
#      the hours where ET is MISSING, what % of those gaps have all
#      predictors available (i.e., could actually be filled)?

library(tidyverse)
library(lubridate)

source('R/utils/metutils.R')
source('R/Pre_proc_data.R') # builds `df` (hourly ERA5 + tower + TRMM + GRACE)

vars_tET6 <- c("Ki", "Tar", "q", "vpd_tow", "UmidSol_20cm", "UmidSol_293cm", "ET")

# current training window used elsewhere in the project stops here
cutoff_treino <- as.POSIXct("2010-09-30", tz = "UTC")

explore_df <- df %>%
  dplyr::filter(date >= as.POSIXct("2011-01-01", tz = "UTC"),
                date <  as.POSIXct("2013-01-01", tz = "UTC")) %>%
  select(date, all_of(vars_tET6))

# 1) Time series panels ------------------------------------------------------
explore_long <- explore_df %>%
  pivot_longer(-date, names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = vars_tET6),
         role = if_else(variable == "ET", "ET (target)", "Predictor"))

p_series <- ggplot(explore_long, aes(date, value, color = role)) +
  geom_line(na.rm = TRUE, linewidth = 0.3) +
  geom_vline(xintercept = cutoff_treino, linetype = "dashed", color = "black") +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("ET (target)" = "firebrick", "Predictor" = "steelblue"),
                      guide = "none") +
  labs(title = "tET_6 predictors + ET - 2011/2012 exploratory check",
       subtitle = "Dashed line = end of the current training window (2010-09-30). Gaps in the line = missing data.",
       x = "Date", y = NULL) +
  theme_bw()

print(p_series)

# 2) Monthly completeness of the full predictor set --------------------------
monthly_completeness <- explore_df %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    n_hours = n(),
    n_complete = sum(!is.na(Ki) & !is.na(Tar) & !is.na(q) &
                        !is.na(UmidSol_20cm) & !is.na(UmidSol_293cm)),
    pct_complete = 100 * n_complete / n_hours,
    .groups = "drop"
  )

p_completeness <- ggplot(monthly_completeness, aes(month, pct_complete)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey40") +
  scale_x_datetime(date_breaks = "2 months", date_labels = "%Y-%m") +
  labs(title = "Monthly % of hours with all tET_6 predictors present - 2011/2012",
       x = NULL, y = "% complete hours") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_completeness)

print(monthly_completeness, n = 24)

# 3) Predictor completeness restricted to the actual ET gaps -----------------
# This is the number that matters for gap-filling: of the hours where ET is
# missing, how many have every predictor available to make a prediction?
month_seq <- tibble(month = seq(as.POSIXct("2011-01-01", tz = "UTC"),
                                 as.POSIXct("2012-12-01", tz = "UTC"),
                                 by = "1 month"))

gapfill_monthly <- explore_df %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    n_missing_ET = sum(is.na(ET)),
    n_gap_fillable = sum(is.na(ET) & !is.na(Ki) & !is.na(Tar) & !is.na(q) &
                            !is.na(UmidSol_20cm) & !is.na(UmidSol_293cm)),
    .groups = "drop"
  ) %>%
  right_join(month_seq, by = "month") %>%
  mutate(n_missing_ET = replace_na(n_missing_ET, 0),
         n_gap_fillable = replace_na(n_gap_fillable, 0),
         pct_fillable = if_else(n_missing_ET > 0,
                                 100 * n_gap_fillable / n_missing_ET,
                                 NA_real_)) %>%
  arrange(month)

p_gapfillable <- ggplot(gapfill_monthly, aes(month, pct_fillable)) +
  geom_col(fill = "darkorange", na.rm = TRUE) +
  geom_text(aes(label = paste0(n_missing_ET, " gaps")),
            vjust = -0.4, size = 3, na.rm = TRUE) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
  labs(title = "% of ET gaps that could be filled (all predictors present) - 2011/2012",
       subtitle = "Denominator = hours with ET missing that month. Label = how many hours that is.",
       x = NULL, y = "% of gaps fillable") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_gapfillable)

print(gapfill_monthly, n = 24)

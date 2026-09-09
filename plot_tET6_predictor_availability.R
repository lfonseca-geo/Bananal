# Monthly availability of tET_6's predictors (Ki, Tar, q, UmidSol_20cm,
# UmidSol_293cm) since 2004, split by whether ET is already measured or
# missing (i.e. would need to be filled by the tET_6 gap-filling model).
#
# Each bar = % of that month's hours with ALL tET_6 predictors present,
# stacked into two colors:
#   - ET measured        -> predictors present AND ET already observed
#   - Gap (fill w/ tET_6) -> predictors present AND ET missing

library(tidyverse)
library(lubridate)

source('R/utils/metutils.R')
source('R/Pre_proc_data.R') # builds `df` (hourly ERA5 + tower + TRMM + GRACE)

vars_tET6_predictors <- c("Ki", "Tar", "q", "UmidSol_20cm", "UmidSol_293cm")

monthly_avail <- df %>%
  mutate(month = floor_date(date, "month"),
         predictors_ok = !is.na(Ki) & !is.na(Tar) & !is.na(q) &
                           !is.na(UmidSol_20cm) & !is.na(UmidSol_293cm)) %>%
  group_by(month) %>%
  summarise(
    n_hours  = n(),
    n_obs_ok = sum(predictors_ok & !is.na(ET)),
    n_gap_ok = sum(predictors_ok & is.na(ET)),
    .groups = "drop"
  ) %>%
  mutate(`ET measured`              = 100 * n_obs_ok / n_hours,
         `Gap - fillable w/ tET_6` = 100 * n_gap_ok / n_hours) %>%
  select(month, `ET measured`, `Gap - fillable w/ tET_6`) %>%
  pivot_longer(-month, names_to = "status", values_to = "pct") %>%
  mutate(status = factor(status, levels = c("ET measured", "Gap - fillable w/ tET_6")))

ggplot(monthly_avail, aes(month, pct, fill = status)) +
  geom_col() +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey40") +
  scale_fill_manual(name = NULL, values = c(
    "ET measured" = "steelblue",
    "Gap - fillable w/ tET_6" = "darkorange"
  )) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Monthly % of hours with all tET_6 predictors present - since 2004",
       subtitle = "Blue = ET already measured | Orange = ET missing, but fillable with tET_6",
       x = NULL, y = "% of hours with all predictors present") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

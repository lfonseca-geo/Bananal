# Fit the production tET_6 (tower) and mET_6 (ERA5) models, then predict ET
# across the FULL data frame (2004-2016) - not just the "good density"
# training window. mgcv's predict.gam() defaults to na.action = na.pass, so
# a row with any missing predictor simply gets NA back (same length as
# newdata, safe to use inside mutate()) - this is exactly what "quando
# houver valores das covariates" means in practice: no manual filtering
# needed, predict() already returns NA where a model can't be evaluated.
#
# Mirrors the pattern used in R/FitModels/GammaDist_GAMs.R:
#   pred_df_mET_6 <- df %>%
#     mutate(hour = hour(date), month = month(date)) %>%
#     mutate(et_pred = predict(mET_6, newdata = ., type = 'response'))
# applied to both models, over the entire record instead of a filtered range.

library(mgcv)
library(tidyverse)
library(lubridate)

source('R/utils/metutils.R')
source('R/Pre_proc_data.R') # builds `df` (hourly ERA5 + tower + TRMM + GRACE), through 2016

# Training window ------------------------------------------------------------
# Same "good data density" period filter used everywhere else in the project
# (R/FitModels/GammaDist_GAMs.R lines 62-86, R/predict_ET/predict_tET_6.R
# lines 14-27, predict_ET_GAM_LOYO.R). Named uniquely (not `tmp_df`) so it
# can never collide with the same-named scratch variable in those scripts if
# they're run in the same R session (see predict_ET_GAM_LOYO.R for context).
model_fit_window <- df %>%
  dplyr::filter((date >= as.POSIXct('2004-01-01', tz='UTC') &
                   date <= as.POSIXct('2006-06-30', tz='UTC')) |
                  (date >= as.POSIXct('2006-09-30', tz='UTC') &
                     date <= as.POSIXct('2007-01-31', tz='UTC')) |
                  (date >= as.POSIXct('2007-09-01', tz='UTC') &
                     date <= as.POSIXct('2008-05-31', tz='UTC')) |
                  (date >= as.POSIXct('2008-12-01', tz='UTC') &
                     date <= as.POSIXct('2009-03-31', tz='UTC')) |
                  (date >= as.POSIXct('2009-06-01', tz='UTC') &
                     date <= as.POSIXct('2010-09-30', tz='UTC'))) %>%
  dplyr::filter(is.na(ET) == FALSE & ET > 0) %>%
  dplyr::filter(hour >= 7 & hour <= 17)

# NOTE: unlike GammaDist_GAMs.R / predict_tET_6.R, this fits on the FULL
# filtered window rather than a sample_n(10000) subsample - since this is
# meant to be the production model used for gap-filling (not an exploratory
# comparison across many candidate formulas), using all available good rows
# gives a more stable fit and removes the run-to-run variability that comes
# from sampling without a set.seed().

# mET_6 (ERA5) variables -------------------------------------------------
data_mET6_fit <- model_fit_window %>%
  dplyr::filter(
    !is.na(vpd), !is.na(ssrd), !is.na(hour), !is.na(swvl2), !is.na(swvl4)
  )
cat("mET_6 training rows:", nrow(data_mET6_fit), "\n")

mET_6 <- bam(
  ET ~ te(vpd, ssrd, hour) + te(ssrd, swvl2, swvl4),
  data = data_mET6_fit,
  method = 'REML',
  select = TRUE,
  family = Gamma(link = 'log')
)
summary(mET_6)

# tET_6 (Tower) variables, incl. the Ki/Net quality filter from
# predict_tET_6.R lines 32-34 --------------------------------------------
data_tET6_fit <- model_fit_window %>%
  dplyr::filter(Ki >= 50 & Net >= 0) %>%
  dplyr::filter(
    !is.na(vpd_tow), !is.na(Ki), !is.na(hour),
    !is.na(UmidSol_20cm), !is.na(UmidSol_293cm)
  )
cat("tET_6 training rows:", nrow(data_tET6_fit), "\n")

tET_6 <- bam(
  ET ~ te(vpd_tow, Ki, hour) + te(Ki, UmidSol_20cm, UmidSol_293cm),
  data = data_tET6_fit,
  method = 'REML',
  select = TRUE,
  family = Gamma(link = 'log')
)
summary(tET_6)

# Prediction uncertainty ------------------------------------------------------
# Reviewer requirement: quantify prediction uncertainty and propagate it into
# every downstream (seasonal/interannual/ENSO) analysis, instead of reporting
# a bare point estimate.
#
# For a log-linked Gamma GAM, two distinct sources of uncertainty combine at
# each hour:
#   1) Uncertainty in the fitted MEAN (parameter/smoothing uncertainty - this
#      is where ERA5-predictor noise and model-fitting uncertainty enter):
#      predict(..., type='link', se.fit=TRUE) gives se on the log scale;
#      mapped to the response scale via the delta method:
#        Var_mean(mu) ~= mu^2 * se_link^2
#   2) Residual (observation-level) variance around that mean, from the
#      Gamma family itself: Var_resid(mu) = phi * mu^2, where phi is the
#      dispersion (scale) parameter the model estimates.
# Combined (treating the two sources as independent - a standard, but
# approximate, simplification): Var_total(mu) = mu^2 * (se_link^2 + phi).
# mu +/- 1.96*sqrt(Var_total) is therefore a genuine PREDICTION interval (how
# far a single new hourly value could plausibly fall), not just a confidence
# interval for the mean - the distinction the reviewer is asking for.
phi_mET6 <- summary(mET_6)$dispersion
phi_tET6 <- summary(tET_6)$dispersion
cat("mET_6 dispersion (phi):", phi_mET6, "\n")
cat("tET_6 dispersion (phi):", phi_tET6, "\n")

pred_df_full <- df %>%
  mutate(hour = hour(date), month = month(date))

# as.numeric() strips any dim/class attribute predict.bam() may attach (it
# can come back as a 1-column matrix rather than a plain vector) - without
# it, is.na()/case_when() below choke with "must be a logical vector, not a
# logical 1D array".
mET6_link <- predict(mET_6, newdata = pred_df_full, type = 'link', se.fit = TRUE)
tET6_link <- predict(tET_6, newdata = pred_df_full, type = 'link', se.fit = TRUE)

pred_df_full <- pred_df_full %>%
  mutate(
    et_pred_mET6         = as.numeric(exp(mET6_link$fit)),
    et_pred_mET6_se_link = as.numeric(mET6_link$se.fit),
    et_pred_mET6_var     = et_pred_mET6^2 * (et_pred_mET6_se_link^2 + phi_mET6),

    et_pred_tET6         = as.numeric(exp(tET6_link$fit)),
    et_pred_tET6_se_link = as.numeric(tET6_link$se.fit),
    et_pred_tET6_var     = et_pred_tET6^2 * (et_pred_tET6_se_link^2 + phi_tET6)
  )

# Sanity check: is tET_6's uncertainty genuinely smaller than mET_6's, or did
# something silently go NA (which sum(..., na.rm=TRUE) would zero out later,
# making the monthly ribbon vanish for tET_6-filled months without erroring)?
cat("---------------------------------------------------\n")
cat("Uncertainty diagnostics (non-NA rows only):\n")
cat("phi_mET6 =", phi_mET6, "| phi_tET6 =", phi_tET6, "\n")
cat("et_pred_mET6_se_link summary:\n"); print(summary(pred_df_full$et_pred_mET6_se_link))
cat("et_pred_tET6_se_link summary:\n"); print(summary(pred_df_full$et_pred_tET6_se_link))
cat("et_pred_mET6_var summary:\n"); print(summary(pred_df_full$et_pred_mET6_var))
cat("et_pred_tET6_var summary:\n"); print(summary(pred_df_full$et_pred_tET6_var))
cat("NA count - et_pred_tET6 (point):", sum(is.na(pred_df_full$et_pred_tET6)),
    "| et_pred_tET6_var:", sum(is.na(pred_df_full$et_pred_tET6_var)), "\n")

# Gap-filled ET: keep the tower measurement where it exists; where it's
# missing, prefer the tower-driven model (closer to the actual sensor) and
# fall back to the ERA5-driven model only where tET_6 itself has no
# prediction (i.e. a tower covariate is also missing).
pred_df_full <- pred_df_full %>%
  mutate(
    ET_source = case_when(
      !is.na(ET) ~ "observed",
      !is.na(et_pred_tET6) ~ "tET_6 (tower model)",
      !is.na(et_pred_mET6) ~ "mET_6 (ERA5 model)",
      TRUE ~ NA_character_
    ),
    ET_gapfilled = coalesce(ET, et_pred_tET6, et_pred_mET6)
  )

cat("---------------------------------------------------\n")
cat("Total hours:", nrow(pred_df_full), "\n")
print(table(pred_df_full$ET_source, useNA = "ifany"))

# write.csv(pred_df_full, "data/pred_df_tET6_mET6_full_2004_2016.csv", row.names = FALSE)

# Monthly composition of ET_source: observed vs tower model vs GAM model -----
source_levels <- c("observed", "tET_6 (tower model)", "mET_6 (ERA5 model)",
                    "not fillable")

monthly_source <- pred_df_full %>%
  mutate(month = floor_date(date, "month"),
         ET_source_label = if_else(is.na(ET_source), "not fillable", ET_source),
         ET_source_label = factor(ET_source_label, levels = source_levels)) %>%
  count(month, ET_source_label, .drop = FALSE) %>%
  group_by(month) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()

ggplot(monthly_source, aes(month, pct, fill = ET_source_label)) +
  geom_col() +
  scale_fill_manual(name = NULL, values = c(
    "observed"             = "steelblue",
    "tET_6 (tower model)"  = "darkorange",
    "mET_6 (ERA5 model)"   = "forestgreen",
    "not fillable"         = "grey80"
  )) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(title = "ET data source by month - 2004-2016",
       subtitle = "Observed vs gap-filled with tET_6 (tower) vs gap-filled with mET_6 (ERA5) vs not fillable by either",
       x = NULL, y = "% of hours") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Monthly ET totals: observed vs tET_6 vs mET_6 -------------------------------
# Same convention used elsewhere in the project (e.g. R/FitModels/
# GammaDist_GAMs.R): sum to monthly totals, plot at the 15th of each month.
# CAVEAT: each series is summed only over the hours where IT has a value, and
# the three series don't have the same hourly coverage (mET_6 ~ full ERA5
# coverage, tET_6 much sparser, ET observed sparser still in gappy months) -
# so a low "ET observed" bar in a gappy month partly reflects missing hours,
# not necessarily lower ET. Useful to compare shapes/magnitude, not to treat
# any one series as a true monthly total without checking coverage first.
monthly_values <- pred_df_full %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(
    ET      = sum(ET, na.rm = TRUE),
    ET_tET6 = sum(et_pred_tET6, na.rm = TRUE),
    ET_mET6 = sum(et_pred_mET6, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(date = parse_date_time(paste(year, month, 15), "ymd")) %>%
  pivot_longer(cols = c(ET, ET_tET6, ET_mET6), names_to = "series", values_to = "value")

ggplot(monthly_values, aes(date, value, color = series)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(
    name = NULL,
    breaks = c("ET", "ET_tET6", "ET_mET6"),
    labels = c("ET observed", "tET_6 (tower)", "mET_6 (ERA5)"),
    values = c("ET" = "black", "ET_tET6" = "darkorange", "ET_mET6" = "forestgreen")
  ) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Monthly ET - observed vs tET_6 vs mET_6 (2004-2016)",
       x = NULL, y = expression(paste("ET (mm ", month^-1, ")"))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Monthly ET: gap-filled series vs observed-only ------------------------------
# ET_gapfilled = coalesce(ET, et_pred_tET6, et_pred_mET6), so this line has
# a value in every hour where AT LEAST ONE of the three sources works - the
# gap between the two lines below is exactly what gap-filling adds back.
monthly_gapfilled <- pred_df_full %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(
    ET_observed  = sum(ET, na.rm = TRUE),
    ET_gapfilled = sum(ET_gapfilled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(date = parse_date_time(paste(year, month, 15), "ymd")) %>%
  pivot_longer(cols = c(ET_observed, ET_gapfilled), names_to = "series", values_to = "value")

ggplot(monthly_gapfilled, aes(date, value, color = series)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(
    name = NULL,
    breaks = c("ET_gapfilled", "ET_observed"),
    labels = c("ET gap-filled (final)", "ET observed only"),
    values = c("ET_gapfilled" = "purple", "ET_observed" = "black")
  ) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Monthly ET - gap-filled vs observed-only (2004-2016)",
       subtitle = "Gap-filled = observed ET, else tET_6 prediction, else mET_6 prediction",
       x = NULL, y = expression(paste("ET (mm ", month^-1, ")"))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Single continuous ET line, colored by source, hour-by-hour:
#   1) Observed (Tower)              - ET actually measured that hour
#   2) Predicted (tET_6 (Tower))     - ET missing, but tET_6's tower
#      covariates are available AND we're still inside the window where the
#      tower record is usable (< tET6_usable_until (Tower) predictors like
#      Tar/q basically vanish from 2013 on, see the per-year coverage check)
#   3) Predicted (mET_6 - GAM-ERA5)  - fallback: ET missing and tET_6 can't
#      predict either (missing tower covariate, or past tET6_usable_until)
# Now that Pre_proc_data.R's sum_na_aware() fix keeps real ET gaps as NA
# (instead of a false 0), is.na(ET) here is trustworthy again.
# Restricted to >= 2004 (start of the tower record) - `df` also carries
# ERA5-only rows back to 2000 (ERA5 spans 2000-2017, tower spans 2004-2016),
# which would otherwise show up as a spurious ERA5-only stretch before the
# observational period even starts.
tET6_usable_until <- as.POSIXct("2013-01-01", tz = "UTC")

hourly_filled <- pred_df_full %>%
  dplyr::filter(date >= as.POSIXct("2004-01-01", tz = "UTC")) %>%
  mutate(
    tET6_in_window = date < tET6_usable_until,
    ET_value = case_when(
      !is.na(ET) ~ ET,
      tET6_in_window & !is.na(et_pred_tET6) ~ et_pred_tET6,
      TRUE ~ et_pred_mET6
    ),
    # Observed hours are treated as known (no propagated model uncertainty -
    # tower instrument error is a separate, unmodeled source not addressed
    # here). Filled hours carry the prediction variance of whichever model
    # produced them.
    ET_value_var = case_when(
      !is.na(ET) ~ 0,
      tET6_in_window & !is.na(et_pred_tET6) ~ et_pred_tET6_var,
      TRUE ~ et_pred_mET6_var
    ),
    ET_hour_source = case_when(
      !is.na(ET) ~ "Observed (Tower)",
      tET6_in_window & !is.na(et_pred_tET6) ~ "Predicted (Tower)",
      TRUE ~ "Predicted (ERA5)"
    )
  )

# CAVEAT (important for the reviewer response): summing hourly variances to
# get a monthly variance assumes the hours within a month are independent.
# ET has strong diurnal/serial autocorrelation, so the true effective sample
# size per month is smaller than the raw hour count - this naive sum is
# therefore a LOWER BOUND on the true monthly uncertainty, not an exact
# value. A fully rigorous treatment would use a block bootstrap or an
# autocorrelation-adjusted effective-N; flag this explicitly if reporting
# these intervals in the manuscript.
monthly_final <- hourly_filled %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(
    value    = sum(ET_value, na.rm = TRUE),
    # sum_na_aware() (defined in R/Pre_proc_data.R, already sourced above)
    # returns NA instead of a false 0 when every contributing hour's
    # variance is NA - otherwise a month filled entirely by one model would
    # silently render with a zero-width ribbon if that model's se.fit ever
    # came back NA, instead of visibly flagging the missing uncertainty.
    variance = sum_na_aware(ET_value_var),
    # classify the month by whichever source contributed the most hours
    # (a month that's mostly observed with one or two filled hours still
    # reads as "Observed")
    tipo = names(sort(table(ET_hour_source), decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  mutate(
    se_month   = sqrt(variance),
    ci_lower   = value - 1.96 * se_month,
    ci_upper   = value + 1.96 * se_month,
    month_date = parse_date_time(paste(year, month, 15), "ymd")
  ) %>%
  arrange(month_date)

# build segments so the line stays visually continuous across color changes
# (each segment is colored by its starting month's `tipo`)
segments_final <- monthly_final %>%
  transmute(month_date, value, tipo,
            month_date_next = lead(month_date),
            value_next      = lead(value)) %>%
  filter(!is.na(month_date_next))

ggplot() +
  # 95% prediction interval, propagated from the hourly model uncertainty
  # (0-width where a month is fully observed, since ET_value_var = 0 there)
  geom_ribbon(data = monthly_final,
              aes(x = month_date, ymin = ci_lower, ymax = ci_upper),
              inherit.aes = FALSE, fill = "grey50", alpha = 0.3) +
  geom_segment(data = segments_final,
               aes(x = month_date, xend = month_date_next,
                   y = value, yend = value_next, color = tipo),
               linewidth = 0.7) +
  scale_color_manual(name = "Legend:", values = c(
    "Observed (Tower)"  = "black",
    "Predicted (Tower)" = "darkorange",
    "Predicted (ERA5)"  = "red"
  )) +
  scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Reconstructed monthly ET time series (2004–2017)",
       subtitle = "Shaded band = 95% prediction interval (residual uncertainty)",
       x = "Ano", y = expression(paste("ET (mm ", month^-1, ")"))) +
  theme_bw() +
  theme(legend.position = "bottom")
# 5. Salvar a figura em alta resolução (altura aumentada para 8.5 in devido ao número de variáveis)


ggsave(
  filename = "fig/ET_reconstructed_2004_2017.png",
  plot = last_plot(),
  width = 12,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Seasonal/interannual anomalies WITH propagated uncertainty ------------------
# Reviewer requirement: "percentage reductions, anomalies, and multiyear
# persistence are interpreted without showing whether they exceed the
# uncertainty..." - this section computes each month's anomaly against its
# calendar-month climatology and flags whether it is statistically
# distinguishable from zero once BOTH sources of uncertainty are combined:
#   - `variance`: this month's own propagated prediction uncertainty (0 for
#     fully-observed months, from `monthly_final` above)
#   - `clim_var`: sampling uncertainty of the climatological mean itself
#     (variance of that calendar month's values across years, divided by the
#     number of years contributing to it)
clim <- monthly_final %>%
  group_by(month) %>%
  summarise(
    clim_mean = mean(value, na.rm = TRUE),
    clim_var  = var(value, na.rm = TRUE) / sum(!is.na(value)),
    .groups = "drop"
  )

anomalies <- monthly_final %>%
  left_join(clim, by = "month") %>%
  mutate(
    anomaly       = value - clim_mean,
    pct_anomaly   = 100 * anomaly / clim_mean,
    se_anomaly    = sqrt(variance + clim_var),
    significant   = abs(anomaly) > 1.96 * se_anomaly
  )

cat("---------------------------------------------------\n")
cat("Monthly ET anomalies vs. calendar-month climatology, with propagated\n")
cat("95% uncertainty (this month's model variance + climatology sampling\n")
cat("variance). 'significant' = TRUE means the anomaly exceeds that combined\n")
cat("uncertainty and is not attributable to prediction noise alone:\n")
print(anomalies %>%
        select(month_date, value, anomaly, pct_anomaly, se_anomaly, significant),
      n = 200)

# write.csv(anomalies, "data/ET_monthly_anomalies_with_uncertainty.csv", row.names = FALSE)

ggplot(anomalies, aes(month_date, anomaly, fill = significant)) +
  geom_col() +
  geom_errorbar(aes(ymin = anomaly - 1.96 * se_anomaly, ymax = anomaly + 1.96 * se_anomaly),
                width = 0, alpha = 0.4, na.rm = TRUE) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_fill_manual(name = "Significant (95%)",
                     values = c(`TRUE` = "firebrick", `FALSE` = "grey70")) +
  scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Monthly ET anomalies with propagated uncertainty",
       subtitle = "Red = anomaly exceeds combined model + climatology uncertainty (95%)",
       x = NULL, y = expression(paste("ET anomaly (mm ", month^-1, ")"))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



monthly_final %>%
  dplyr::filter(year <= 2014) %>%
  summarise(mean_daily = sum(value, na.rm = TRUE) / sum(days_in_month(month_date)))

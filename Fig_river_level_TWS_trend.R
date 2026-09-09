# Figure 12 (manuscript): two-panel figure.
#   a) Long-term Javaes River water level (ANA, Barreira da Cruz station)
#      since 1980, with a linear trend line quantifying the "long-term
#      declining trend... over the past four decades" claim currently
#      stated in the Discussion without a reported slope/p-value.
#   b) Javaes level, flood height at the BAN tower, and standardized GRACE
#      TWS anomaly, restricted to the GRACE-covered window (2002+).
#
# Adapted from R/grace_storage.R (lines 32-67, the `riv` plot) for panel b.
# Panel a uses the FULL ANA record: data/jav_level.xlsx sheet 2 actually
# starts in 1969 (R/grace_storage.R's `year>=2002` filter, kept for panel b
# to align with GRACE's coverage, was hiding this longer history).

library(tidyverse)
library(lubridate)
library(readxl)
library(cowplot)

source('R/utils/metutils.R')
source('R/Pre_proc_data.R') # builds `df` (needed for lwe_thickness_csr / GRACE)

# Full ANA record (Javaes, Barreira da Cruz station), no year filter -------
raw_full <- read_xlsx("data/jav_level.xlsx", sheet = 2) %>%
  mutate(year = year(Data), month = month(Data)) %>%
  mutate(date1 = parse_date_time(paste(year, month, 1), "ymd"))

# a) Long-term trend since 1980 -----------------------------------------------
riv_1980 <- raw_full %>%
  dplyr::filter(year >= 1980, !is.na(media_calculada))

trend_model <- lm(media_calculada / 100 ~ as.numeric(date1), data = riv_1980)
trend_summary <- summary(trend_model)
slope_per_year <- coef(trend_model)[2] * 60 * 60 * 24 * 365.25 # m per year (date1 is POSIXct -> seconds)
n_years_1980 <- as.numeric(diff(range(riv_1980$date1))) / (60 * 60 * 24 * 365.25)

cat("------------------------------------------------------------\n")
cat("ANA (Javaes river level) linear trend, 1980-present:\n")
cat(sprintf("  Slope: %.4f m/year (p = %.4g, R2 = %.3f, n = %d)\n",
            slope_per_year, trend_summary$coefficients[2, 4],
            trend_summary$r.squared, nobs(trend_model)))
cat(sprintf("  Over %.0f years, implied change: %.2f m\n",
            n_years_1980, slope_per_year * n_years_1980))

panel_a <- riv_1980 %>%
  ggplot(aes(date1, media_calculada / 100)) +
  geom_line(color = "blue4") +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "grey60") +
  scale_x_datetime(breaks = "2 years", date_labels = "%Y") +
  ylab("Javaes river level (m)") + xlab(NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 16))

# b) Javaes level + Flood height (BAN tower) + GRACE TWS anomaly, 2002+ ------
raw1 <- raw_full %>% dplyr::filter(year >= 2002)

grc_month <- df %>%
  mutate(year = year(date), month = month(date)) %>%
  dplyr::filter(year >= 2002) %>%
  group_by(year, month) %>%
  summarise(lwe_thickness_csr = mean(lwe_thickness_csr, na.rm = TRUE), .groups = "drop") %>%
  mutate(date1 = parse_date_time(paste(year, month, 1), "ymd")) %>%
  arrange(date1)

riv <- left_join(raw1, grc_month, by = "date1")

panel_b <- riv %>%
  ggplot(aes(date1, media_calculada / 100, color = 'Javaes level')) +
  geom_line() +
  geom_line(aes(date1, lwe_thickness_csr / 10, color = 'TWS anomaly')) +
  geom_line(aes(date1, torre, color = 'Flood level')) +
  scale_color_manual(name = ' ', values = c(
    'TWS anomaly'   = "black",
    'Javaes level'  = 'blue4',
    'Flood level'   = 'red'
  )) +
  theme_bw() +
  theme(legend.justification = c(1, 0.2),
        legend.position = c(0.2, 0.78),
        legend.background = element_blank(),
        axis.text = element_text(size = 16),
        text = element_text(size = 16),
        legend.key = element_blank()) +
  theme(legend.key.width = unit(2, 'lines'),
        legend.spacing.y = unit(0.0000000005, "cm")) +
  theme(panel.grid.major = element_blank()) +
  scale_x_datetime(breaks = '1 year', date_labels = "%Y") +
  scale_y_continuous(sec.axis = sec_axis(~. * 10, name = "Total water storage (TWS) anomaly (cm)")) +
  ylab('Javaes/flood height (m)') +
  xlab(' ')

# Combine, labeled a/b top to bottom ------------------------------------------
fig_river <- plot_grid(panel_a, panel_b, ncol = 1, align = "v",
                        labels = c("a", "b"), label_size = 18, label_fontface = "bold")
fig_river

ggsave(
  filename = "fig/Fig_river_level_TWS_trend.png",
  plot = fig_river,
  width = 14, height = 12, units = "in",
  dpi = 300, bg = "white"
)

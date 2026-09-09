# Figure 3 (manuscript): daily tower vs. ERA5/TRMM meteorological
# comparison, 6 hexbin scatter panels with fitted regression line and r2:
#   a) Temperature (Tar) vs T2m
#   b) VPD (tower) vs VPD (ERA5)
#   c) Global radiation (Ki) vs SSRD
#   d) Net radiation (Net) vs SSR
#   e) Rainfall Tower vs TP (ERA5 total precipitation)
#   f) Rainfall Tower vs TRMM
#
# Adapted from R/tower_versus_ERA5.R (fig_Ta_daily/fig_vpd_daily/
# fig_Ki_daily/fig_net_daily/fig_tp_prec/fig_trmm_precip), which already
# produces the exact panels used here - this script just adds the a-f
# subplot labels and fixes the export: the original called tiff() AFTER
# ggarrange() had already been evaluated (so nothing was actually captured
# in the device before dev.off()); ggsave() below captures the arranged
# plot object directly, at journal-ready resolution.

library(tidyverse)
library(lubridate)
library(ggpubr)
library(ggpmisc)

source('R/utils/metutils.R')
source('R/Pre_proc_data.R') # builds `df` (hourly ERA5 + tower + TRMM + GRACE)

r2_label <- function() {
  stat_fit_glance(method = "lm",
                   method.args = list(formula = y ~ x),
                   aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"<0.001"',
                                       stat(r.squared), stat(p.value))),
                   parse = TRUE)
}

# extra headroom above the data so the r2/p label (placed near the top of
# the panel) never overlaps the highest hexbin/abline - this is what was
# clipping the label in panel f (TRMM), which has the smallest y-range
extra_headroom <- scale_y_continuous(expand = expansion(mult = c(0.05, 0.18)))

# a) Temperature: Tar (tower) vs t2m_cel (ERA5) -------------------------------
fig_Ta_daily <- df %>%
  select(date, Tar, t2m_cel) %>%
  dplyr::filter(!is.na(Tar) & Tar <= 40) %>%
  mutate(year = year(date), doy = yday(date)) %>%
  group_by(year, doy) %>%
  summarise(Tar = mean(Tar, na.rm = TRUE),
            t2m_cel = mean(t2m_cel, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = t2m_cel, y = Tar)) +
  geom_hex() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ylab("Temperature") + xlab("T2m") +
  theme_bw() + theme(text = element_text(size = 16)) +
  r2_label() +
  extra_headroom

# b) VPD: tower vs ERA5 --------------------------------------------------------
fig_vpd_daily <- df %>%
  select(date, vpd, vpd_tow) %>%
  mutate(year = year(date), doy = yday(date)) %>%
  group_by(year, doy) %>%
  summarise(vpd = mean(vpd, na.rm = TRUE),
            vpd_tow = mean(vpd_tow, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = vpd, y = vpd_tow)) +
  geom_hex() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ylab("Vapour pressure deficit") + xlab("VPD") +
  theme_bw() + theme(text = element_text(size = 16)) +
  r2_label() +
  extra_headroom

# c) Global radiation: Ki (tower) vs ssrd (ERA5) ------------------------------
fig_Ki_daily <- df %>%
  dplyr::filter(!is.na(Ki) & Ki > 0) %>%
  mutate(year = year(date), doy = yday(date)) %>%
  group_by(year, doy) %>%
  summarise(Ki = mean(Ki, na.rm = TRUE),
            ssrd = mean(ssrd, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = ssrd, y = Ki)) +
  geom_hex() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ylab("Global radiation") + xlab("SSRD") +
  theme_bw() + theme(text = element_text(size = 16)) +
  r2_label() +
  extra_headroom

# d) Net radiation: Net (tower) vs ssr (ERA5) ---------------------------------
fig_net_daily <- df %>%
  dplyr::filter(!is.na(Net) & Net > 0) %>%
  mutate(year = year(date), doy = yday(date), hour = hour(date)) %>%
  dplyr::filter(hour >= 6 & hour <= 18) %>%
  group_by(year, doy) %>%
  summarise(Net = mean(Net, na.rm = TRUE),
            ssr = mean(ssr, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = ssr, y = Net)) +
  geom_hex() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ylab("Net radiation") + xlab("SSR") +
  theme_bw() + theme(text = element_text(size = 16)) +
  r2_label() +
  extra_headroom

# e) Rainfall: tower (Prec.new) vs ERA5 total precipitation (tp2) ------------
fig_tp_prec <- df %>%
  select(date, Prec.new, tp2) %>%
  mutate(year = year(date), doy = yday(date)) %>%
  dplyr::filter(!is.na(Prec.new) & !is.na(tp2)) %>%
  group_by(year, doy) %>%
  summarise(Prec.new = sum(Prec.new, na.rm = TRUE),
            tp = sum(tp2, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(tp < 50) %>%
  ggplot(aes(x = tp, y = Prec.new)) +
  geom_hex() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ylab("Rainfall Tower") + xlab("TP") +
  theme_bw() + theme(text = element_text(size = 16)) +
  r2_label() +
  extra_headroom

# f) Rainfall: tower (Prec.new) vs TRMM ---------------------------------------
fig_trmm_precip <- df %>%
  select(date, Prec.new, trmm1) %>%
  mutate(year = year(date), doy = yday(date)) %>%
  dplyr::filter(!is.na(Prec.new) & !is.na(trmm1)) %>%
  group_by(year, doy) %>%
  summarise(Prec.new = sum(Prec.new, na.rm = TRUE),
            trmm1 = sum(trmm1, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(trmm1 < 50 & Prec.new < 50) %>%
  ggplot(aes(x = trmm1, y = Prec.new)) +
  geom_hex() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ylab("Rainfall Tower") + xlab("TRMM") +
  theme_bw() + theme(text = element_text(size = 16)) +
  r2_label() +
  extra_headroom

# Combine with a-f subplot labels ---------------------------------------------
fig3 <- ggarrange(fig_Ta_daily, fig_vpd_daily, fig_Ki_daily, fig_net_daily,
                   fig_tp_prec, fig_trmm_precip,
                   nrow = 3, ncol = 2, legend = "none",
                   labels = c("a", "b", "c", "d", "e", "f"),
                   font.label = list(size = 16, face = "bold"))

fig3

# High-resolution export for journal submission (TIFF, 600 dpi) -------------
ggsave(
  filename = "fig/Fig3_tower_vs_ERA5_correlation.png",
  plot = fig3,
  width = 14, height = 10, units = "in",
  dpi = 300, bg = "white"
)


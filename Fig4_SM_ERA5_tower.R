# Figure (manuscript): interannual soil moisture time series, ERA5 layers
# vs. tower FDR sensors at matching depths, 3 panels 2000-2019:
#   a) SM ERA5 layer2 vs SM tower 20cm
#   b) SM ERA5 layer3 vs SM tower 80cm
#   c) SM ERA5 layer4 vs SM tower 220cm/293cm
#
# Adapted from R/tower_versus_ERA5.R (swvl2_20cm/swvl3_80cm/swvl4_293cm,
# lines 9-114), which already produces these exact panels - this script
# just adds the a-c subplot labels and fixes the export: the original
# called tiff() AFTER ggarrange() had already been evaluated (so nothing
# was actually captured in the device before dev.off()); ggsave() below
# captures the arranged plot object directly, at journal-ready resolution.

library(tidyverse)
library(lubridate)
library(ggpubr)

source('R/utils/metutils.R')
source('R/Pre_proc_data.R') # builds `df` (hourly ERA5 + tower + TRMM + GRACE)

# Same quality screen as the original script: implausible tower FDR values
# outside the sensor's realistic calibrated range are set to NA before
# plotting, not filtered out of `df` itself.
df_sm <- df %>%
  mutate(UmidSol_20cm  = if_else(UmidSol_20cm <= 0.22 | UmidSol_20cm >= 0.6, NA_real_, UmidSol_20cm),
         UmidSol_80cm  = if_else(UmidSol_80cm <= 0.3  | UmidSol_80cm >= 0.58, NA_real_, UmidSol_80cm),
         UmidSol_293cm = if_else(UmidSol_293cm >= 0.45, NA_real_, UmidSol_293cm))

common_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.direction = "horizontal",
        axis.text = element_text(size = 12))

ylab_sm <- expression(paste("Soil moisture (SM) (", ' ', m^3 * m^-3, ")"))

# a) SM ERA5 layer2 (0-20 cm) vs SM tower 20cm --------------------------------
fig_sm_20cm <- df_sm %>%
  select(date, swvl2, UmidSol_20cm) %>%
  ggplot(aes(date, swvl2, color = 'SM ERA5 layer2')) +
  geom_line() +
  geom_line(aes(date, UmidSol_20cm, color = 'SM tower 20cm')) +
  scale_color_manual(name = ' ', values = c('SM ERA5 layer2' = "black", 'SM tower 20cm' = 'red')) +
  scale_x_datetime(breaks = '1 year', date_labels = "%Y") +
  ylab(ylab_sm) + xlab(' ') +
  common_theme +
  theme(legend.justification = c(1, 0.2), legend.position = c(0.29, 0.9),
        axis.text.x = element_blank())

# b) SM ERA5 layer3 (20-80 cm) vs SM tower 80cm -------------------------------
fig_sm_80cm <- df_sm %>%
  select(date, swvl3, UmidSol_80cm) %>%
  ggplot(aes(date, swvl3, color = 'SM ERA5 layer3')) +
  geom_line() +
  geom_line(aes(date, UmidSol_80cm, color = 'SM tower 80cm')) +
  scale_color_manual(name = ' ', values = c('SM ERA5 layer3' = "black", 'SM tower 80cm' = 'blue')) +
  scale_x_datetime(breaks = '1 year', date_labels = "%Y") +
  ylab(ylab_sm) + xlab(' ') +
  common_theme +
  theme(legend.justification = c(1, 0.2), legend.position = c(0.29, 0.9),
        axis.text.x = element_blank())

# c) SM ERA5 layer4 (80-189 cm) vs SM tower 220cm/293cm -----------------------
fig_sm_293cm <- df_sm %>%
  select(date, swvl4, UmidSol_293cm, UmidSol_220cm) %>%
  ggplot(aes(date, swvl4, color = 'SM ERA5 layer 4')) +
  geom_line() +
  geom_line(aes(date, UmidSol_293cm, color = 'SM tower 293 cm')) +
  geom_line(aes(date, UmidSol_220cm, color = 'SM tower 220 cm')) +
  scale_color_manual(name = ' ', values = c(
    'SM ERA5 layer 4'  = "black",
    'SM tower 293 cm'  = 'darkgoldenrod',
    'SM tower 220 cm'  = 'purple'
  )) +
  scale_x_datetime(breaks = '1 year', date_labels = "%Y") +
  ylab(ylab_sm) + xlab(' ') +
  common_theme +
  theme(legend.justification = c(1, 0.2), legend.position = c(0.2, 0.3),
        legend.direction = "vertical")

# Combine with a-c subplot labels ---------------------------------------------
fig_sm <- ggarrange(fig_sm_20cm, fig_sm_80cm, fig_sm_293cm,
                     nrow = 3,
                     labels = c("a", "b", "c"),
                     font.label = list(size = 16, face = "bold"))

fig_sm

# High-resolution export for journal submission (PNG, 300 dpi) -------------
ggsave(
  filename = "fig/Fig4_SM_ERA5_tower.png",
  plot = fig_sm,
  width = 11, height = 9, units = "in",
  dpi = 300, bg = "white"
)

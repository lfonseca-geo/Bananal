# Multi-panel time series: reconstructed ET + its climate/hydrology drivers.
# Reuses `monthly_final` and `df` created by predict_ET_full_2004_2016.R -
# run that script first (this one does NOT re-fit the GAMs, to keep it fast
# to iterate on styling).
#
# Panels (top to bottom): ET (reconstructed), Rainfall + Flood height,
# VPD (ERA5 + tower), SSRD (ERA5) + Ki (tower), Soil Moisture (ERA5 layers
# 2 & 4 + tower 20cm/293cm), T2m (ERA5) + Tar (tower).
# CWD panel intentionally omitted (per request).
# Grey/orange bands mark approximate 2015-16 El Nino / 2017 post-El Nino
# drought - adjust `band1`/`band2` below if you have exact dates.
#
# x_range is enforced TWICE for every panel: once by filtering the source
# data itself to [x_range[1], x_range[2]] before aggregating to monthly
# values, and once visually via coord_cartesian()/scale_x_datetime(limits=).
# The data-level filter matters because some sources extend well past
# Dec/2017 (TRMM's own file goes to Oct/2018) - relying on coord_cartesian
# alone only hides the extra points, it doesn't stop a partial trailing
# month from being computed and potentially leaking in at the edge.

library(tidyverse)
library(lubridate)
library(readxl)
library(cowplot)

if (!exists("monthly_final") || !exists("df")) {
  stop("Rode predict_ET_full_2004_2016.R primeiro - este script depende de ",
       "`monthly_final` e `df` gerados por ele.")
}

# Common x-axis range and highlight bands, shared by every panel -----------
x_range <- c(as.POSIXct("2004-01-01", tz = "UTC"), as.POSIXct("2017-12-31", tz = "UTC"))

in_range <- function(d) d >= x_range[1] & d <= x_range[2]

band1 <- c(as.POSIXct("2015-06-01", tz = "UTC"), as.POSIXct("2016-12-31", tz = "UTC")) # El Nino 2015-16

# Yellow dry-season band (May-Sep) repeated every year - reviewer request:
# the annual dry season should be visible in every year, not just implied
# by an ENSO-specific band. Drawn first (bottom layer) so the hatched ENSO
# overlay below remains visually distinct where the two overlap.
dry_season_years <- 2004:2017
dry_season_bands <- lapply(dry_season_years, function(yr) {
  annotate("rect",
           xmin = as.POSIXct(sprintf("%d-05-01", yr), tz = "UTC"),
           xmax = as.POSIXct(sprintf("%d-09-30", yr), tz = "UTC"),
           ymin = -Inf, ymax = Inf, fill = "gold", alpha = 0.18)
})

# 2015-2016 ENSO period drawn as a HATCHED overlay (not a solid fill) per
# reviewer request, so it stays visually distinct from the solid yellow
# dry-season bands instead of blending/being confused with them. Requires
# the `ggpattern` package (installed on demand if missing).
if (!requireNamespace("ggpattern", quietly = TRUE)) install.packages("ggpattern")
library(ggpattern)

enso_period_df <- tibble(xmin = band1[1], xmax = band1[2], ymin = -Inf, ymax = Inf)

enso_hatch <- geom_rect_pattern(
  data = enso_period_df,
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  inherit.aes = FALSE, fill = NA, color = NA,
  pattern = "stripe", pattern_fill = "grey60", pattern_colour = "grey60",
  pattern_angle = 45, pattern_density = 0.10, pattern_spacing = 0.04,
  pattern_size = 0.3, pattern_alpha = 0.4
)

enso_bands <- c(dry_season_bands, list(enso_hatch))

no_x_axis <- theme(axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank())

base_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 12))

legend_inset <- function(x, y) theme(
  legend.position = c(x, y),
  legend.title = element_blank(),
  legend.background = element_blank(),
  legend.key = element_blank()
)

# 1) ET (reconstructed) ------------------------------------------------------
et_monthly <- monthly_final %>%
  dplyr::filter(in_range(month_date)) %>%
  arrange(month_date)

p_et <- ggplot(et_monthly, aes(month_date, value)) +
  enso_bands +
  geom_line(color = "#141477", linewidth = 0.4) +
  geom_point(color = "#141477", size = 1) +
  coord_cartesian(xlim = x_range) +
  ylab("ET (mm)") +
  base_theme + no_x_axis

# 2) Rainfall (TRMM) + Flood height (Jav river level) ------------------------
trmm_raw <- read_csv("data/TRMM_ban_19980101_20181001.csv")
trmm_raw$date <- parse_date_time(substr(trmm_raw$`system:index`, 6, 16), "ymd_h")

trmm_monthly <- trmm_raw %>%
  dplyr::filter(in_range(date)) %>%
  mutate(precip_acum = precipitation * 3) %>% # 3-hourly steps -> accumulation
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(tot_precip = sum(precip_acum, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_date = parse_date_time(paste(year, month, 15), "ymd")) %>%
  dplyr::filter(in_range(month_date))

flood_raw <- read_xlsx("data/jav_level.xlsx", sheet = 1)
flood_monthly <- flood_raw %>%
  dplyr::filter(in_range(Data)) %>%
  mutate(year = year(Data), month = month(Data)) %>%
  group_by(year, month) %>%
  summarise(flood_m = mean(media_calculada, na.rm = TRUE) / 100, .groups = "drop") %>%
  mutate(month_date = parse_date_time(paste(year, month, 15), "ymd")) %>%
  dplyr::filter(in_range(month_date))

# left_join FROM trmm_monthly on purpose: TRMM is the complete/primary
# series (no gaps within x_range - confirmed against the raw CSV), flood
# height is the shorter auxiliary series (jav_level.xlsx starts later,
# ~2006). A full_join here risks dropping/shifting TRMM rows if the flood
# side's month_date keys don't line up perfectly; left_join guarantees
# every TRMM month survives regardless of what the flood series does.
rain_flood <- trmm_monthly %>%
  select(month_date, tot_precip) %>%
  left_join(flood_monthly %>% select(month_date, flood_m), by = "month_date") %>%
  arrange(month_date)

# scale flood height onto the rainfall axis so both fit in one panel,
# de-scaled back for the secondary axis label (same trick as fig1_second_paper.R)
flood_scale <- max(rain_flood$tot_precip, na.rm = TRUE) / max(rain_flood$flood_m, na.rm = TRUE)

p_rain <- ggplot(rain_flood, aes(month_date, tot_precip)) +
  enso_bands +
  # width is in the units of the x scale - `month_date` is POSIXct, so width
  # must be given in SECONDS (25 days), not the "25" that would work on a
  # plain Date axis. Without this the bars render at ~25 seconds wide and
  # are invisible next to points spaced ~30 days apart.
  geom_col(fill = "steelblue", width = 25 * 24 * 3600, na.rm = TRUE) +
  # Flood height, made more prominent per reviewer request: thicker line,
  # bold contrasting color, and an explicit legend entry (rather than a
  # bare unlabeled black line easy to miss against the rainfall bars).
  geom_line(aes(y = flood_m * flood_scale, color = "Flood height"),
            linewidth = 1.1, na.rm = TRUE) +
  scale_color_manual(name = NULL, values = c("Flood height" = "black")) +
  scale_y_continuous(name = "Rainfall (mm)",
                      sec.axis = sec_axis(~ . / flood_scale, name = "Flood height (m)")) +
  coord_cartesian(xlim = x_range) +
  base_theme + no_x_axis +
  theme(legend.position = c(0.92, 0.85), legend.background = element_blank(),
        legend.key = element_blank(), legend.title = element_blank())

# 3) VPD (ERA5) ---------------------------------------------------------------
vpd_monthly <- df %>%
  dplyr::filter(in_range(date)) %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(vpd_mean = mean(vpd, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_date = parse_date_time(paste(year, month, 15), "ymd")) %>%
  dplyr::filter(in_range(month_date))

p_vpd <- ggplot(vpd_monthly, aes(month_date, vpd_mean)) +
  enso_bands +
  geom_line(color = "red", linewidth = 0.4) +
  geom_point(color = "red", size = 1) +
  coord_cartesian(xlim = x_range) +
  ylab("VPD (KPa)") +
  base_theme + no_x_axis

# 4) SSRD (ERA5) --------------------------------------------------------------
ssrd_monthly <- df %>%
  dplyr::filter(in_range(date)) %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(ssrd_mean = mean(ssrd, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_date = parse_date_time(paste(year, month, 15), "ymd")) %>%
  dplyr::filter(in_range(month_date))

p_ssrd <- ggplot(ssrd_monthly, aes(month_date, ssrd_mean)) +
  enso_bands +
  geom_line(color = "#8B5A2B", linewidth = 0.4) +
  geom_point(color = "#8B5A2B", size = 1) +
  coord_cartesian(xlim = x_range) +
  ylab(expression(paste("SSRD (W ", m^-2, ")"))) +
  base_theme + no_x_axis

# 5) Soil moisture (ERA5 layers 2 & 4) ----------------------------------------
sm_monthly <- df %>%
  dplyr::filter(in_range(date)) %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(sm2 = mean(swvl2, na.rm = TRUE),
            sm4 = mean(swvl4, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_date = parse_date_time(paste(year, month, 15), "ymd")) %>%
  dplyr::filter(in_range(month_date))

p_sm <- ggplot(sm_monthly, aes(month_date)) +
  enso_bands +
  geom_line(aes(y = sm2, color = "SM ERA5 layer 2"), linewidth = 0.4) +
  geom_point(aes(y = sm2, color = "SM ERA5 layer 2"), size = 1) +
  geom_line(aes(y = sm4, color = "SM ERA5 layer4"), linewidth = 0.4) +
  scale_color_manual(values = c(
    "SM ERA5 layer 2" = "#1b9e77",
    "SM ERA5 layer4"  = "#377eb8"
  )) +
  coord_cartesian(xlim = x_range) +
  ylab(expression(paste("SM (", m^3 * m^-3, ")"))) +
  base_theme + no_x_axis + legend_inset(0.15, 0.15)

# 6) T2m (ERA5, bottom panel keeps the x-axis) --------------------------------
t2m_monthly <- df %>%
  dplyr::filter(in_range(date)) %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(t2m_mean = mean(t2m_cel, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_date = parse_date_time(paste(year, month, 15), "ymd")) %>%
  dplyr::filter(in_range(month_date))

p_t2m <- ggplot(t2m_monthly, aes(month_date, t2m_mean)) +
  enso_bands +
  geom_line(color = "firebrick", linewidth = 0.4) +
  geom_point(color = "firebrick", size = 1) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y", limits = x_range) +
  xlab(NULL) + ylab("T2m (°C)") +
  base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine all panels, labeled a-f top to bottom -------------------------------
# label_x/label_y nudge the letters inside the panel's plotting area itself
# (past the y-axis title column) - cowplot's default label_x=0 is relative
# to the whole aligned grob including the axis-title margin, which collided
# with wider titles like "SSRD (W m^-2)".
plot_grid(p_et, p_rain, p_vpd, p_ssrd, p_sm, p_t2m,
          ncol = 1, align = "v",
          rel_heights = c(1, 1, 1, 1, 1, 1.3),
          labels = c("a", "b", "c", "d", "e", "f"),
          label_size = 16, label_fontface = "bold",
          label_x = 0.06, label_y = 0.98, hjust = 0, vjust = 1)
# 5. Salvar a figura em alta resolução (altura aumentada para 8.5 in devido ao número de variáveis)
ggsave(
  filename = "fig/multipanel_ET_drivers.png",
  plot = last_plot(),
  width = 10,           
  height = 8.5,           
  units = "in",
  dpi = 300,            
  bg = "white"          
)


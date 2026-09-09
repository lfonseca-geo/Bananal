# Seasonal ET cycle by year: every year overlaid on the same Jan-Dec axis,
# using the RECONSTRUCTED monthly ET series (`monthly_final`, from
# predict_ET_full_2004_2016.R). Same visual pattern as
# R/Figures_scripts/interannual_plot2_changeLegend.R (t2m_inter/vpd_inter),
# adapted for ET with the reconstructed data.
#
# Highlighted years: 2006 & 2009 in blue, 2016 & 2017 in red (the ENSO
# years discussed in the Results text), 2005 & 2010 in orange, all other
# years in grey, plus the calendar-month mean (black) with a +/-1 SD ribbon.

library(tidyverse)

if (!exists("monthly_final")) {
  stop("Rode predict_ET_full_2004_2016.R primeiro - este script depende de ",
       "`monthly_final` gerado por ele.")
}

# 2018 excluded (per request - likely just a stray partial month at the
# edge of the reconstructed record) - removed from both the individual year
# lines and the climatology mean/SD below, not just hidden visually.
monthly_final_f <- monthly_final %>% dplyr::filter(year != 2018)

# Climatological mean +/- SD per calendar month, across all years ------------
clim <- monthly_final_f %>%
  group_by(month) %>%
  summarise(clim_mean = mean(value, na.rm = TRUE),
            clim_sd   = sd(value, na.rm = TRUE),
            .groups = "drop")

plot_df <- monthly_final_f %>%
  mutate(year_f = as.character(year)) %>%
  select(year_f, month, value) %>%
  arrange(year_f, month)

# Color mapping: grey for every year by default, then override the
# highlighted ones. Every year still gets its own legend entry (matches the
# reference figure, which lists 2004-2017 individually).
# Same-hue light/dark pairs (tried before) washed out too close to the grey
# background and to each other. Switched to 6 fully distinct, high-contrast
# colors (a Set1-style qualitative palette) - grouping is now conveyed by
# the legend labels/text discussed in the paper, not by color similarity.
all_years <- sort(unique(plot_df$year_f))
year_colors <- setNames(rep("grey80", length(all_years)), all_years)
year_colors["2006"] <- "#377EB8" # wettest years - blue
year_colors["2009"] <- "#4DAF4A" # wettest years - green
year_colors["2016"] <- "#E41A1C" # ENSO - red
year_colors["2017"] <- "#FF7F00" # ENSO - orange
year_colors["2005"] <- "#984EA3" # ENSO - purple
year_colors["2010"] <- "#A65628" # ENSO - brown
year_colors["mean"] <- "black"

highlighted <- c("2005", "2006", "2009", "2010", "2016", "2017")

ggplot() +
  geom_line(data = plot_df, aes(month, value, group = year_f, color = year_f),
            linewidth = 0.35) +
  geom_point(data = plot_df %>% dplyr::filter(year_f %in% highlighted),
             aes(month, value, color = year_f), size = 1.6) +
  geom_ribbon(data = clim, aes(x = month, ymin = clim_mean - clim_sd, ymax = clim_mean + clim_sd),
              inherit.aes = FALSE, fill = "grey50", alpha = 0.3) +
  geom_line(data = clim, aes(month, clim_mean, color = "mean"), linewidth = 0.9) +
  geom_point(data = clim, aes(month, clim_mean, color = "mean"), size = 1.6) +
  scale_color_manual(name = "years", values = year_colors) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  xlab(NULL) + ylab(expression(paste("ET (mm ", month^-1, ")"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.key.size = unit(1.2, 'lines'))

ggsave(
  filename = "fig/ET_seasonal_yeares.png",
  plot = last_plot(),
  width = 14,          
  height = 8.5,           
  units = "in",
  dpi = 300,            
  bg = "white"          
)


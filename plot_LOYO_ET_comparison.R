# Compare tET_6 and mET_6 LOYO predictions against tower-observed ET, for a
# given year - reuses objects created by predict_ET_GAM_LOYO.R, so run that
# script first (with run_tET6/run_mET6 set as needed) before this one.

library(tidyverse)
library(lubridate)

if (!exists("dados_preditos_LOYO")) {
  stop("Rode predict_ET_GAM_LOYO.R primeiro - este script depende de ",
       "`dados_preditos_LOYO` gerado por ele.")
}

# Both observed ET and predictions come straight from `dados_preditos_LOYO`
# (not from `tmp_df`) on purpose: `tmp_df` is a generic name reused by
# several scripts in this project (GammaDist_GAMs.R, predict_tET_6.R,
# predict_ET_GAM_LOYO.R itself), so if any of those ran later in the same R
# session, a stale/overwritten `tmp_df` would silently leak other years into
# this plot. Filtering is done on an explicit [start_date, end_date) range
# instead of `year(date) == ano`, to avoid any ambiguity with the `year`
# column that already exists in these data frames.
#
# Two stacked panels (facets), one per model - each shows that model's own
# prediction against the SAME observed tower series, so tET_6 and mET_6 can
# be compared on the same y-scale without one obscuring the other.
facet_labels <- c(tET_6 = "GAM model tower", mET_6 = "GAM model ERA5")

# `fim` lets you cut the plot short of Dec 31 - useful here because the
# "good data density" window used to build the LOYO datasets in
# predict_ET_GAM_LOYO.R only goes up to 2010-09-30, so anything plotted past
# that is just empty white space.
plot_loyo_year_lanes <- function(ano, fim = NULL) {
  start_date <- as.Date(paste0(ano, "-01-01"))
  end_date   <- if (is.null(fim)) as.Date(paste0(ano, "-12-31")) else as.Date(fim)
  
  dados_ano <- dados_preditos_LOYO %>%
    mutate(date = as.Date(date)) %>%
    dplyr::filter(date >= start_date & date <= end_date)
  
  if (nrow(dados_ano) == 0) {
    stop("Nao ha previsoes LOYO para o periodo pedido em `dados_preditos_LOYO`.")
  }
  
  modelos_presentes <- unique(dados_ano$Model)
  to_lane <- function(m) recode(as.character(m), !!!facet_labels)
  
  # Observed ET is the same tower reading regardless of model; replicate it
  # once per model so each facet gets its own copy of the observed line.
  obs_daily <- dados_ano %>%
    distinct(date, ET) %>%
    group_by(date) %>%
    summarise(ET_obs = sum(ET, na.rm = TRUE), .groups = "drop") %>%
    tidyr::crossing(Model = modelos_presentes)
  
  # 95% CI band from the LOYO prediction's standard error (se.fit), summed
  # to the daily scale along with the point prediction itself.
  pred_daily <- dados_ano %>%
    group_by(Model, date) %>%
    summarise(
      ET_pred  = sum(ET_pred_LOYO, na.rm = TRUE),
      CI_lower = sum(ET_pred_LOYO - 1.96 * ET_se_LOYO, na.rm = TRUE),
      CI_upper = sum(ET_pred_LOYO + 1.96 * ET_se_LOYO, na.rm = TRUE),
      .groups = "drop"
    )
  
  plot_df <- bind_rows(
    obs_daily  %>% transmute(Model, date, value = ET_obs,  serie = "ET observed"),
    pred_daily %>% transmute(Model, date, value = ET_pred, serie = "ET predicted")
  ) %>%
    mutate(Model = factor(Model, levels = c("tET_6", "mET_6")),
           lane = to_lane(Model))
  
  ribbon_df <- pred_daily %>%
    mutate(Model = factor(Model, levels = c("tET_6", "mET_6")),
           lane = to_lane(Model))
  
  ggplot(plot_df, aes(date, value, color = serie)) +
    geom_ribbon(data = ribbon_df, aes(x = date, ymin = CI_lower, ymax = CI_upper),
                inherit.aes = FALSE, fill = "#d95f02", alpha = 0.2) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~lane, ncol = 1) +
    scale_color_manual(name = NULL, values = c(
      "ET observed" = "black",
      "ET predicted" = "#d95f02"
    )) +
    # belt-and-suspenders: clamp the visible range even if something
    # upstream ever leaks an extra date in
    coord_cartesian(xlim = c(start_date, end_date)) +
    labs(title = paste("Model Estimation -", ano),
         subtitle = "Shaded band = 95% confidence interval of the prediction (Leave-one-year-out)",
         x = "Date", y = expression(paste("ET (mm ", day^-1, ")"))) +
    theme_bw() + theme(legend.position = "bottom")
}

plot_loyo_year_lanes(2010, fim = as.Date("2010-10-31"))
ggsave(
  filename = "fig/compare_model_2010_enso.png",
  plot = last_plot(),
  width = 10,           
  height = 8.5,           
  units = "in",
  dpi = 300,            
  bg = "white"          
)

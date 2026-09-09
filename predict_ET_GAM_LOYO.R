# Leave-One-Year-Out (LOYO) cross-validation, comparing the two GAM
# approaches used in this project:
#   - tET_6: driven by TOWER-measured met. variables (vpd_tow, Ki, hour,
#     UmidSol_20cm, UmidSol_293cm), as defined in R/predict_ET/predict_tET_6.R
#   - mET_6: driven by ERA5 reanalysis variables (vpd, ssrd, hour, swvl2,
#     swvl4), as defined in R/FitModels/GammaDist_GAMs.R (lines 303-310)
#
# Each fold refits the model's exact formula leaving one year out, predicts
# on that year, and reports RMSE/MAE/R2 + prediction uncertainty (se.fit).
# Both models share the same "good data density" period filter; tET_6 adds
# the Ki>=50 & Net>=0 quality filter used in its original fitting script.
#
# Set the flags below to FALSE to skip a model entirely - e.g. run only
# mET_6 without needing tET_6 to fit successfully first.
run_tET6 <- TRUE
run_mET6 <- FALSE

# This script has no random sampling (train/test are split by year, not by
# sample_n()), so results should be 100% reproducible across runs *as long
# as it is sourced fully, in a clean session*. set.seed() here is just a
# defensive no-op in case any randomness is ever added later.
set.seed(123)

library(mgcv)
library(tidyverse)
library(lubridate)
if (!requireNamespace("Metrics", quietly = TRUE)) install.packages("Metrics")
library(Metrics) # rmse()/mae()

# Run data pipeline --------------------------------------------------------
source('R/utils/metutils.R')
source('R/Pre_proc_data.R') # builds `df` (hourly ERA5 + tower + TRMM + GRACE)

# Same "good data density" period + quality filters used to build tmp_train
# for both models (R/FitModels/GammaDist_GAMs.R lines 62-86 and
# R/predict_ET/predict_tET_6.R lines 14-27).
# NOTE: deliberately NOT called `tmp_df` - that generic name is reused as a
# scratch variable in GammaDist_GAMs.R and predict_tET_6.R. If either of
# those is run earlier in the same R session, a leftover/shadowed `tmp_df`
# would silently change which rows survive filtering here (this is exactly
# what caused the "wrong years show up" bug in the comparison plot before).
# Using a name unique to this script removes that whole failure mode.
loyo_base_df <- df %>%
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

# Generic Leave-One-Year-Out routine -----------------------------------------
# NOTE: the formula argument is deliberately NOT named `formula` - naming a
# local variable/parameter `formula` and handing it to bam()/gam() is a known
# trap: mgcv resolves free variables via environment(formula), which is this
# function's own frame: if that frame also binds a symbol literally called
# `formula`, you can get "promise already under evaluation: recursive
# default argument reference" instead of a normal fit.
run_loyo <- function(data, model_formula, label, min_test_n = 100) {

  anos_disponiveis <- sort(unique(data$year))
  cat("====================================================\n")
  cat("Modelo:", label, "| Anos disponiveis:", paste(anos_disponiveis, collapse = ", "), "\n")

  lista_resultados <- list()
  lista_previsoes <- list()

  for (ano_teste in anos_disponiveis) {

    cat("--------------------------------------------------\n")
    cat("[", label, "] Treinando sem o ano:", ano_teste, "\n")

    treino <- data %>% dplyr::filter(year != ano_teste)
    teste  <- data %>% dplyr::filter(year == ano_teste)

    if (nrow(teste) < min_test_n) {
      cat("Poucos dados para o ano", ano_teste, "- Pulando...\n")
      next
    }

    mod_loyo <- tryCatch(
      bam(
        model_formula,
        data = treino,
        method = 'REML',
        select = TRUE,
        family = Gamma(link = 'log')
      ),
      error = function(e) {
        cat("Falha ao ajustar o modelo [", label, "] para o ano", ano_teste, ":",
            conditionMessage(e), "\n")
        NULL
      }
    )
    if (is.null(mod_loyo)) next

    previsoes <- predict(mod_loyo, newdata = teste, type = 'response', se.fit = TRUE)
    teste$ET_pred_LOYO <- previsoes$fit
    teste$ET_se_LOYO   <- previsoes$se.fit

    rmse_val <- rmse(teste$ET, teste$ET_pred_LOYO)
    mae_val  <- mae(teste$ET, teste$ET_pred_LOYO)
    r2_val   <- cor(teste$ET, teste$ET_pred_LOYO, use = "complete.obs")^2

    lista_resultados[[as.character(ano_teste)]] <- data.frame(
      Model = label,
      Year = ano_teste,
      N_Train = nrow(treino),
      N_Test = nrow(teste),
      R2 = round(r2_val, 3),
      RMSE = round(rmse_val, 3),
      MAE = round(mae_val, 3)
    )

    lista_previsoes[[as.character(ano_teste)]] <- teste %>%
      mutate(Model = label) %>%
      select(Model, date, year, ET, ET_pred_LOYO, ET_se_LOYO)

    cat("Concluido! R2:", round(r2_val, 3), "| RMSE:", round(rmse_val, 3), "\n")
  }

  list(metrics = bind_rows(lista_resultados),
       predictions = bind_rows(lista_previsoes))
}

# tET_6 - tower variables ----------------------------------------------------
if (run_tET6) {
  data_tET6 <- loyo_base_df %>%
    dplyr::filter(Ki >= 50 & Net >= 0) %>%
    dplyr::filter(
      !is.na(ET) & ET > 0,
      !is.na(vpd_tow),
      !is.na(Ki),
      !is.na(hour),
      !is.na(UmidSol_20cm),
      !is.na(UmidSol_293cm)
    )
  cat("data_tET6:", nrow(data_tET6), "linhas horarias apos filtros\n")

  loyo_tET6 <- run_loyo(
    data_tET6,
    ET ~ te(vpd_tow, Ki, hour) + te(Ki, UmidSol_20cm, UmidSol_293cm),
    "tET_6"
  )
}

# mET_6 - ERA5 variables ------------------------------------------------------
if (run_mET6) {
  data_mET6 <- loyo_base_df %>%
    dplyr::filter(
      !is.na(ET) & ET > 0,
      !is.na(vpd),
      !is.na(ssrd),
      !is.na(hour),
      !is.na(swvl2),
      !is.na(swvl4)
    )
  cat("data_mET6:", nrow(data_mET6), "linhas horarias apos filtros\n")

  loyo_mET6 <- run_loyo(
    data_mET6,
    ET ~ te(vpd, ssrd, hour) + te(ssrd, swvl2, swvl4),
    "mET_6"
  )
}

# Consolidate results (only for the model(s) actually run) -------------------
loyo_runs <- Filter(Negate(is.null), list(
  if (exists("loyo_tET6")) loyo_tET6 else NULL,
  if (exists("loyo_mET6")) loyo_mET6 else NULL
))

tabela_LOYO <- bind_rows(lapply(loyo_runs, `[[`, "metrics"))
dados_preditos_LOYO <- bind_rows(lapply(loyo_runs, `[[`, "predictions"))

print(tabela_LOYO)

# write.csv(tabela_LOYO, "data/LOYO_metrics_tET6_mET6.csv", row.names = FALSE)

# Metrics per held-out year, one or both models side by side -----------------
tabela_LOYO %>%
  pivot_longer(cols = c(R2, RMSE, MAE), names_to = "metric", values_to = "value") %>%
  ggplot(aes(factor(Year), value, fill = Model)) +
  geom_col(position = "dodge") +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  theme_bw() +
  xlab("Held-out year") + ylab(NULL) +
  labs(title = "Leave-One-Year-Out performance")

# Uncertainty plot for a given held-out year, one model at a time -------------
plot_loyo_year <- function(ano, modelo) {
  dados_preditos_LOYO %>%
    filter(year == ano & Model == modelo) %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>%
    summarise(
      ET_obs = sum(ET, na.rm = TRUE),
      ET_pred = sum(ET_pred_LOYO, na.rm = TRUE),
      CI_upper = sum(ET_pred_LOYO + (1.96 * ET_se_LOYO), na.rm = TRUE),
      CI_lower = sum(ET_pred_LOYO - (1.96 * ET_se_LOYO), na.rm = TRUE)
    ) %>%
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), fill = "red", alpha = 0.2) +
    geom_line(aes(y = ET_pred, color = "Predicted (LOYO)"), linewidth = 1) +
    geom_line(aes(y = ET_obs, color = "Observed"), linewidth = 0.8) +
    scale_color_manual(values = c("Observed" = "black", "Predicted (LOYO)" = "red")) +
    labs(
      title = paste0(modelo, " LOYO Validation - ", ano),
      subtitle = "Red shaded area represents the 95% Confidence Interval (Uncertainty)",
      y = expression(paste("ET (mm ", day^-1, ")")), x = "Date"
    ) +
    theme_bw() + theme(legend.position = "bottom")
}

# example: first and last held-out year available for each model that ran
if (exists("loyo_tET6")) {
  plot_loyo_year(min(loyo_tET6$metrics$Year), "tET_6")
  plot_loyo_year(max(loyo_tET6$metrics$Year), "tET_6")
}
if (exists("loyo_mET6")) {
  plot_loyo_year(min(loyo_mET6$metrics$Year), "mET_6")
  plot_loyo_year(max(loyo_mET6$metrics$Year), "mET_6")
}

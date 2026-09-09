# GAM description required by reviewer: basis dimensions, fitting/penalization
# criteria, effective degrees of freedom, residual diagnostics, concurvity,
# and the criterion used to select the final predictor set.
# Reuses mET_6/tET_6 already fit in predict_ET_full_2004_2016.R - run that
# script first.

library(mgcv)
library(tidyverse)

if (!exists("mET_6") || !exists("tET_6")) {
  stop("Rode predict_ET_full_2004_2016.R primeiro - este script depende de ",
       "mET_6 e tET_6 ja ajustados.")
}

report_gam <- function(model, label) {
  s <- summary(model)

  cat("\n============================================================\n")
  cat("Model:", label, "\n")
  cat("Formula: ", deparse(formula(model)), "\n", sep = "")
  cat("Family: ", model$family$family, " | Link: ", model$family$link, "\n", sep = "")

  # Fitting and penalization criteria --------------------------------------
  cat("Fitting method: ", model$method,
      " (smoothing parameters chosen by restricted maximum likelihood)\n", sep = "")
  cat("Penalization / predictor-selection criterion: select = TRUE ",
      "(double penalty per Marra & Wood 2011) - each smooth gets an extra ",
      "penalty on its null space, so a term with no real signal is shrunk ",
      "toward an effective degrees of freedom (edf) of ~0 and is effectively ",
      "dropped by the fitting procedure itself, rather than by a separate ",
      "stepwise/AIC predictor-selection step.\n", sep = "")

  # Basis dimension (k) and edf per smooth term ----------------------------
  # NOTE: a simple s() smooth stores its basis dimension directly in
  # sm$bs.dim, but a tensor-product te() smooth (used here) does NOT - its
  # marginal bases (one per variable in the te()) each have their own
  # bs.dim, nested under sm$margin. get_k() below returns sm$bs.dim when
  # present, otherwise the product of the marginal bs.dim's (the raw,
  # pre-constraint tensor-product basis dimension - the conventional way
  # "k" is reported for te() terms).
  get_k <- function(sm) {
    if (!is.null(sm$bs.dim) && length(sm$bs.dim) == 1) return(sm$bs.dim)
    if (!is.null(sm$margin)) {
      margin_dims <- vapply(sm$margin, function(m) m$bs.dim, numeric(1))
      return(prod(margin_dims))
    }
    NA_real_
  }

  cat("------------------------------------------------------------\n")
  cat("Basis dimension (k) and effective degrees of freedom (edf) per term:\n")
  k_per_term   <- vapply(model$smooth, get_k, numeric(1))
  label_per_term <- vapply(model$smooth, function(sm) sm$label, character(1))
  edf_table <- tibble(term = label_per_term, k = k_per_term) %>%
    left_join(
      tibble(term = rownames(s$s.table), edf = s$s.table[, "edf"],
             ref.df = s$s.table[, "Ref.df"], p.value = s$s.table[, "p-value"]),
      by = "term"
    )
  print(edf_table)
  cat("Total model edf (incl. parametric terms):", sum(s$edf), "\n")

  # Residual diagnostics + basis dimension adequacy (k-index) -------------
  cat("------------------------------------------------------------\n")
  cat("gam.check() - k-index / p-value flags whether k is large enough per term,\n")
  cat("plus the standard 4-panel residual diagnostic plots (QQ, residuals vs\n")
  cat("linear predictor, histogram, response vs fitted):\n")
  gam.check(model)

  # Concurvity --------------------------------------------------------------
  cat("------------------------------------------------------------\n")
  cat("Concurvity, full=TRUE (PRIMARY measure to report): each term's overall\n")
  cat("concurvity with the rest of the model (worst/observed/estimate rows).\n")
  cat("This is the summary recommended by Wood (2017) for reporting:\n")
  print(concurvity(model, full = TRUE))
  cat("------------------------------------------------------------\n")
  cat("Concurvity, full=FALSE (supplementary): pairwise 'worst' between every\n")
  cat("pair of terms. NOTE: two te() smooths that share one covariate (as here)\n")
  cat("can saturate at exactly 1.0 by construction of this pairwise measure -\n")
  cat("this is a documented mgcv artifact, not necessarily a real\n")
  cat("identifiability problem; interpret alongside the full=TRUE numbers above:\n")
  print(concurvity(model, full = FALSE)$worst)

  # Dispersion / variance explained ------------------------------------------
  cat("------------------------------------------------------------\n")
  cat("Dispersion (scale) parameter (Gamma):", s$dispersion, "\n")
  cat("Deviance explained:", round(s$dev.expl * 100, 1), "%\n")
  cat("Adjusted R-sq (on the link scale):", round(s$r.sq, 3), "\n")
  cat("============================================================\n")
}

report_gam(mET_6, "mET_6 (ERA5 predictors)")
report_gam(tET_6, "tET_6 (tower predictors)")

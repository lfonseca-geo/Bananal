# Bananal ET reconstruction — analysis scripts

Code accompanying: *"Estimation of long-term evapotranspiration over a tropical
floodplain forest at the Amazon–Cerrado boundary"* (manuscript SAMES-D-26-00460_R1).

**Raw Bananal tower data are not included in this repository** (confidential/restricted
access). Scripts expect the pre-processed inputs under `data/` (not published here) —
contact the corresponding author for data access.

## Pipeline order

1. **Preprocessing**
   - `R/Pre_proc_data.R` — aggregates 30-min tower data to hourly and merges with
     ERA5, TRMM, and GRACE.
   - `R/utils/metutils.R` — shared meteorological utility functions.
   - `R/grace_storage.R` — GRACE total water storage (TWS) extraction/processing.
   - `R/tower_versus_ERA5.R` — tower-vs-ERA5 daily comparison (source of Figures 3–4).

2. **Model definitions**
   - `R/FitModels/GammaDist_GAMs.R` — `mET_6` GAM (ERA5-driven).
   - `R/FitModels/fit_tower_models.R` — tower-driven GAM model.
   - `R/predict_ET/predict_tET_6.R` — `tET_6` GAM (tower-driven) prediction.

3. **Validation**
   - `predict_ET_GAM_LOYO.R` — Leave-One-Year-Out cross-validation (Table 3).
   - `explore_predictors_2011_2012.R` — predictor coverage analysis informing the
     gap-filling extension window.
   - `plot_LOYO_ET_comparison.R` — LOYO comparison plot.
   - `plot_tET6_predictor_availability.R` — predictor availability plot.
   - `gam_diagnostics.R` — k, edf, concurvity, dispersion, deviance explained (Table S1).

4. **Full reconstruction**
   - `predict_ET_full_2004_2016.R` — fits the production `tET_6`/`mET_6` models,
     predicts across 2004–2017, classifies `ET_source`, builds `ET_gapfilled` with
     propagated uncertainty (Figure 6).

5. **Post-reconstruction analysis**
   - `seasonal_ENSO_stats_reconstructed_ET.R` — seasonal/ENSO-year statistics
     reported in Results.
   - `compare_ET_MODIS_tower.R` — reconstructed tower ET vs. MODIS MOD16 (Figure 9).

6. **Manuscript figures**
   - `Data_Inventory_chart_tower_covariates.R` — Figure 2 (data inventory).
   - `Fig3_tower_vs_ERA5_correlation.R` — Figure 3.
   - `Fig4_SM_ERA5_tower.R` — Figure 4.
   - `plot_multipanel_ET_drivers.R` — Figure 10.
   - `plot_ET_seasonal_cycle_by_year.R` — Figure 11.
   - `Fig_river_level_TWS_trend.R` — Figure 12.

## Not included

**Pending confirmation** (possible role in this pipeline, not yet verified — ask
before adding):
- `Fig5_ETOBS_ETGAM.R` — possibly Figure 5 (may overlap with `plot_LOYO_ET_comparison.R`)
- `R/ET_hourly_density_measures.R` — possibly the source of the "12,444 hourly
  measurements / 69.91%" daytime-window statistic (Section 2.3)
- `R/predict_ET/predict_tET_6v2.R`, `R/FitModels/GammaDist_GAMs_hourly.R`,
  `R/FitModels/GAM_scaleERA5toEddyFlux.R` — related GAM variants, unclear whether
  superseded

**Excluded — redundant drafts:**
- `teste_loyo_dois_modelos.R`, `teste_predict_gams_28082026.R` — earlier drafts of
  `predict_ET_GAM_LOYO.R` (same header/content)

**Excluded — belong to other projects in the source repo (by naming/content):**
- Anything with `_tese` in the name (thesis chapters, not this paper)
- `R/Figures_scripts/*_second_paper.R` (a different manuscript)
- GPP/EVI/SIF/LAI/WUE scripts, `Prophet_model/`, `GradientBoostingModels/`,
  `R/local/`, `R/scratch/`

**Never included:** `chatgpt.R` was found in the source repo containing a
hardcoded OpenAI API key in plain text — flagged to the author for key rotation,
not copied here under any circumstance.

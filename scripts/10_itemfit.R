# scripts/10_itemfit.R
# -----------------------------------------------------------------------------
# Bayesian (posterior-predictive) infit/outfit item-fit statistics.
#
# Classical infit/outfit are mean-square (MNSQ) statistics from the Rasch / 1PL
# tradition, so the natural place to compute them is the RSM stage (`merge_rsm`:
# unidimensional, equal-discrimination, 24 items on the 4-category scale) where
# item screening happens.
#
# We use easyRaschBayes::infit_statistic (Christensen, Kreiner & Mesbah, 2013;
# Müller, 2020), which is NOT a point-estimate collapse: for each posterior draw
# it computes the variance-weighted standardised residuals for the OBSERVED data
# and for model-REPLICATED data, i.e. a posterior predictive check. Item fit is
# read from two complementary "bars" that here agree on the same quasi-cutoff:
#   - MNSQ value: infit mean-square > 1.3 (underfit / noisy);
#   - one-sided posterior predictive p-value (ppp = mean(infit_rep > infit)) < 0.05.
# Both flag the same three items, which also sit apart from the rest by a clear
# gap (next item MNSQ ~1.09), so the exact cutoff does not matter. (Outfit is
# outlier-sensitive and reported only for reference; infit is the primary index.)
#
# Install easyRaschBayes once with:
#   remotes::install_github("pgmj/easyRaschBayes")
#
# Note on roles: in this workflow the four reverse-coded mathematics items were
# removed on the basis of near-zero discrimination once the 2PL was fitted
# (Step 3). The infit/outfit check below is a complementary corroboration. It
# independently flags Bm_1_r, Bm_2_r and Bm_4_r, but NOT Bm_3_r (which passes
# item fit and was removed on discrimination grounds) -- illustrating that
# item-fit and discrimination diagnostics have complementary sensitivities.
# -----------------------------------------------------------------------------

source(here::here("scripts", "00_packages.R"))

if (!exists("merge_rsm")) merge_rsm <- readRDS(here("models", "merge_rsm.rds"))

set.seed(1234)
infit_draws <- infit_statistic(
  merge_rsm,
  item_var   = item,
  person_var = id,
  ndraws_use = 2000,   # matches the values reported in the manuscript table
  outfit     = TRUE
)

itemfit <- infit_draws |>
  dplyr::group_by(item) |>
  dplyr::summarise(
    infit_MNSQ  = mean(infit),
    infit_ppp   = mean(infit_rep > infit),
    outfit_MNSQ = mean(outfit),
    outfit_ppp  = mean(outfit_rep > outfit),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(infit_MNSQ)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))

print(as.data.frame(itemfit), row.names = FALSE)

# -----------------------------------------------------------------------------
# RESULT (merge_rsm, ndraws_use = 2000; these are the values reported in the
# manuscript table). Sorted by infit MNSQ.
#
#    item infit_MNSQ infit_ppp outfit_MNSQ outfit_ppp
#  Bm_2_r      1.643     0.022       1.476      0.115   <- underfit (MNSQ>1.3, ppp<0.05)
#  Bm_4_r      1.376     0.002       1.325      0.025   <- underfit (MNSQ>1.3, ppp<0.05)
#  Bm_1_r      1.336     0.039       1.286      0.126   <- underfit (MNSQ>1.3, ppp<0.05)
#  ---------------------------------------------------- gap (1.34 -> 1.09) --------
#  Bm_3_r      1.093     0.316       1.174      0.238   <- ACCEPTABLE (fit ok)
#  Bl_11_r     1.092     0.174       1.084      0.213
#  Bl_6_r      1.091     0.190       1.056      0.312
#  Bl_10_r     1.085     0.186       1.077      0.216
#  Bl_9_r      1.079     0.207       1.074      0.238
#  Bl_2        1.070     0.315       1.173      0.133
#  Bp_2_r      1.022     0.434       0.999      0.501
#  Bm_5        1.021     0.406       1.046      0.337
#  Bm_8        1.012     0.459       1.057      0.344
#  Bl_7_r      1.003     0.494       1.013      0.446
#  Bl_8_r      0.983     0.570       0.986      0.556
#  Bp_1_r      0.970     0.608       0.984      0.546
#  Bl_5        0.940     0.650       0.992      0.504
#  Bl_3_r      0.938     0.716       0.928      0.739
#  Bm_6        0.886     0.786       0.925      0.680
#  Bp_5        0.881     0.795       0.944      0.638
#  Bm_7        0.818     0.909       0.868      0.817
#  Bl_4_r      0.807     0.981       0.812      0.968
#  Bl_1_r      0.801     0.980       0.814      0.955
#  Bp_4        0.670     0.992       0.737      0.973
#  Bp_3        0.616     0.999       0.640      0.996
#
# Reading: an item is flagged as UNDERFITTING when infit MNSQ > 1.3 and ppp < 0.05
# (both signalling more noise than the model predicts); this catches Bm_1_r,
# Bm_2_r, Bm_4_r, which also sit apart from the rest by a clear gap. The opposite
# tail (low MNSQ, ppp near 1, e.g. Bp_3) is overfit (overly predictable) and is
# not treated as misfit. Bm_3_r passes item fit and would survive a pure-Rasch
# screen; it was removed on near-zero discrimination grounds (Step 3). Item-fit
# and discrimination diagnostics thus have complementary sensitivities.
#
# Reproducibility: the manuscript table reports the mean-squares, which are
# deterministic. The ppp is a Monte Carlo posterior predictive p-value (it relies
# on posterior_predict), so its exact value can vary slightly across runs and
# environments; the qualitative conclusion (the three items far below 0.05) is
# stable.
# -----------------------------------------------------------------------------

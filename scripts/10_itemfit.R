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
  ndraws_use = 500,    # subset of draws for speed; use more for stable ppp
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
# RESULT (merge_rsm, ndraws_use = 500; MNSQ values are stable, ppp carries some
# Monte Carlo noise at this draw count). Sorted by infit MNSQ.
#
#    item infit_MNSQ infit_ppp outfit_MNSQ outfit_ppp
#  Bm_2_r      1.651     0.016       1.484      0.110   <- misfit (MNSQ>1.3, ppp<0.05)
#  Bm_4_r      1.374     0.004       1.323      0.022   <- misfit (MNSQ>1.3, ppp<0.05)
#  Bm_1_r      1.328     0.048       1.279      0.152   <- misfit (MNSQ>1.3, ppp<0.05)
#  ---------------------------------------------------- gap (1.33 -> 1.09) --------
#  Bl_6_r      1.093     0.174       1.057      0.286
#  Bm_3_r      1.092     0.364       1.174      0.280   <- ACCEPTABLE (fit ok)
#  Bl_11_r     1.090     0.176       1.083      0.190
#  Bl_10_r     1.082     0.176       1.075      0.202
#  Bl_9_r      1.077     0.214       1.073      0.242
#  Bl_2        1.070     0.266       1.174      0.100
#  Bm_5        1.026     0.380       1.050      0.322
#  Bp_2_r      1.022     0.404       0.999      0.486
#  Bm_8        1.013     0.500       1.059      0.352
#  Bl_7_r      1.003     0.476       1.012      0.450
#  Bl_8_r      0.982     0.538       0.984      0.522
#  Bp_1_r      0.970     0.624       0.984      0.558
#  Bl_5        0.940     0.710       0.992      0.550
#  Bl_3_r      0.939     0.698       0.929      0.708
#  Bm_6        0.886     0.820       0.925      0.734
#  Bp_5        0.883     0.784       0.949      0.626
#  Bm_7        0.820     0.912       0.870      0.814
#  Bl_4_r      0.808     0.986       0.813      0.982
#  Bl_1_r      0.804     0.978       0.817      0.964
#  Bp_4        0.672     0.994       0.741      0.976
#  Bp_3        0.619     0.998       0.645      0.996
#
# Reading: infit independently flags Bm_1_r, Bm_2_r, Bm_4_r (MNSQ > 1.3 and
# ppp < 0.05 give the same three; they also sit apart from the rest by a clear
# gap). Bm_3_r is NOT flagged by item fit (infit 1.09, ppp 0.36) -- it was
# removed on near-zero discrimination grounds (Step 3). Item-fit and
# discrimination diagnostics thus have complementary sensitivities, which is why
# the multi-step workflow catches more than any single statistic would.
# -----------------------------------------------------------------------------

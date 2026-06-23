# scripts/09_reliability.R
# -----------------------------------------------------------------------------
# Reliability for the final bi-factor model `bf_grm_20`.
#
# For a multidimensional (bi-factor) instrument, the dimensions are designed to
# act jointly toward a single measurement purpose, so the reliability of any one
# latent dimension on its own understates the instrument. The headline quantity
# is therefore the model-based composite reliability (omega family); the
# per-dimension person reliabilities are reported as a complement and as a
# conservative lower bound.
#
# (A) Model-based omega (Rodriguez, Reise & Haviland, 2016; McDonald, 1999),
#     computed per posterior draw to obtain a credible interval:
#       - omega_total : reliability of the unit-weighted total score, pooling the
#                       reliable variance of the general AND specific factors.
#       - omega_H     : proportion of total-score variance due to the general
#                       factor alone (general-factor saturation).
#       - ECV         : explained common variance attributable to the general
#                       factor.
#     Standardised loadings are obtained from the model: an item's slope on a
#     factor is disc_i * sigma_factor, with a logistic residual variance of
#     pi^2/3.
#
# (B) RMU (Relative Measurement Uncertainty) reliability, computed directly on
#     the person posterior draws with easyRaschBayes::RMUreliability
#     (Bignardi, Kievit & Bürkner, 2025). The reviewer-suggested, fully-Bayesian
#     per-dimension person reliability, reported with a 95% HDCI; a conservative
#     lower bound under shrinkage at small N.
#     Install once with:
#       remotes::install_github("pgmj/easyRaschBayes")
# -----------------------------------------------------------------------------

source(here::here("scripts", "00_packages.R"))
library(posterior)

if (!exists("bf_grm_20")) bf_grm_20 <- readRDS(here("models", "bf_grm_20.rds"))
data_3 <- readRDS(here("data", "processed", "data_3.rds"))

draws <- as_draws_df(bf_grm_20)

# --- item -> specific-factor membership (each item is 1 on exactly one dim_*) --
items <- sort(unique(as.character(data_3$item)))
spec_dims <- c("dim_m", "dim_l", "dim_p")
membership <- sapply(items, function(it) {
  row <- data_3[as.character(data_3$item) == it, spec_dims][1, ]
  spec_dims[which(as.numeric(row) == 1)]
})

# ---------------------------------------------------------------------------
# (A) Model-based omega coefficients, with full posterior propagation
# ---------------------------------------------------------------------------
b_disc_col <- "b_disc_Intercept"
disc_cols  <- paste0("r_item__disc[", items, ",Intercept]")   # log-scale offsets
sd_cols    <- c(general = "sd_id__Intercept",
                dim_m   = "sd_id__dim_m",
                dim_l   = "sd_id__dim_l",
                dim_p   = "sd_id__dim_p")

resid_var <- pi^2 / 3
n_draws   <- nrow(draws)
omega_total <- omega_H <- ecv_g <- numeric(n_draws)

for (s in seq_len(n_draws)) {
  disc_i <- exp(draws[[b_disc_col]][s] + vapply(disc_cols, function(c) draws[[c]][s], numeric(1)))
  sg     <- draws[[sd_cols["general"]]][s]
  ssp    <- c(dim_m = draws[[sd_cols["dim_m"]]][s],
              dim_l = draws[[sd_cols["dim_l"]]][s],
              dim_p = draws[[sd_cols["dim_p"]]][s])

  ag <- disc_i * sg                                  # general-factor slopes
  as <- disc_i * ssp[membership]                     # specific-factor slopes
  denom  <- sqrt(ag^2 + as^2 + resid_var)
  lam_g  <- ag / denom                               # standardised loadings
  lam_s  <- as / denom

  num_g <- sum(lam_g)^2
  num_s <- sum(vapply(spec_dims, function(g) sum(lam_s[membership == g])^2, numeric(1)))
  uniq  <- sum(1 - lam_g^2 - lam_s^2)

  omega_total[s] <- (num_g + num_s) / (num_g + num_s + uniq)
  omega_H[s]     <- num_g / (num_g + num_s + uniq)
  ecv_g[s]       <- num_g / (num_g + num_s)
}

qsum <- function(x) c(median = median(x), L95 = quantile(x, .025), U95 = quantile(x, .975))
omega_table <- round(rbind(
  omega_total = qsum(omega_total),
  omega_H     = qsum(omega_H),
  ECV_general = qsum(ecv_g)
), 3)
colnames(omega_table) <- c("median", "L95", "U95")

cat("=== (A) Model-based omega (posterior median [95% CrI]) ===\n")
print(omega_table)

# ---------------------------------------------------------------------------
# (B) RMU reliability (Bignardi, Kievit & Bürkner, 2025) via easyRaschBayes.
#     Per-dimension person reliability computed directly on the posterior draws
#     (persons x draws); a conservative lower bound under shrinkage.
# ---------------------------------------------------------------------------
rmu_for <- function(term, label) {
  cols <- grep(paste0("^r_id\\[.*,", term, "\\]$"), variables(draws), value = TRUE)
  M    <- t(as.matrix(draws[, cols]))            # persons x draws
  h    <- RMUreliability(M)                       # mean + 95% HDCI
  data.frame(factor = label,
             rmu = round(h$rmu_estimate, 3),
             L95 = round(h$hdci_lowerbound, 3),
             U95 = round(h$hdci_upperbound, 3))
}
set.seed(1234)  # RMUreliability splits the draws at random; fix for reproducibility
rmu_table <- rbind(
  rmu_for("Intercept", "General (belief)"),
  rmu_for("dim_m",     "Specific: mathematics"),
  rmu_for("dim_l",     "Specific: learners"),
  rmu_for("dim_p",     "Specific: pedagogy")
)
cat("\n=== (B) RMU reliability [95% HDCI] (easyRaschBayes) ===\n")
print(rmu_table, row.names = FALSE)

# -----------------------------------------------------------------------------
# RESULT (bf_grm_20)
#
# (A) Model-based omega (posterior median [95% CrI])
#            median   L95   U95
# omega_total  0.898 0.869 0.921   <- total-score reliability (HEADLINE): high
# omega_H      0.613 0.483 0.717   <- general-factor saturation
# ECV_general  0.683 0.548 0.787   <- general-factor share of common variance
#
# (B) RMU reliability [95% HDCI] (easyRaschBayes; Bignardi/Burkner 2025)
#                 factor   rmu   L95   U95
#       General (belief) 0.610 0.500 0.705
#  Specific: mathematics 0.621 0.530 0.711
#     Specific: learners 0.551 0.434 0.656
#     Specific: pedagogy 0.317 0.146 0.499
#
# Reading: the composite total score is highly reliable (omega_total = 0.90),
# carried by a dominant general factor (omega_H 0.61, ECV 0.68). The single-
# dimension person reliabilities (RMU ~0.61 for the general factor, weak for
# specifics) are conservative lower bounds under shrinkage at N = 151 and show
# that NO single latent dimension suffices on its own, which is exactly the point
# of a multidimensional instrument and an argument against scoring the subscales
# separately.
#
# Note: this is the standard IRT/bi-factor omega (from the model's standardised
# loadings), computed over the full posterior -- i.e. a Bayesian omega.
# -----------------------------------------------------------------------------

# scripts/00_packages.R

library(here)        # project-root-relative paths (cross-platform: Windows/macOS/Linux)
library(readxl)
library(tidyverse)
library(brms)
library(easyRaschBayes)  # GitHub only: remotes::install_github("pgmj/easyRaschBayes")
library(bayesplot)
library(patchwork)
library(ggthemes)
library(ggrepel)
#ggplot theme setting...
theme_set(bayesplot::theme_default())

# rstan preset...
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = 4)
# loo(moment_match = TRUE, reloo = TRUE) exports the full fitted model (~750 MiB)
# to parallel workers; raise the future limit so it is not rejected (default 500 MiB).
options(future.globals.maxSize = 2 * 1024^3)  # 2 GiB

# Create directory for saving models
if(!dir.exists(here("models"))) dir.create(here("models"))

# scripts/00_packages.R

library(readxl)
library(tidyverse)
library(brms)
library(bayesplot)
library(patchwork)
library(ggthemes)
library(ggrepel)
#ggplot theme setting...
theme_set(bayesplot::theme_default())

# rstan preset...
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = 4)

# Create directory for saving models
if(!dir.exists("models")) dir.create("models")

# ---------------------------------------------------------------------------- #
# Run Analyses - Reduced Models (Retaining cnDoSS Random Effect)
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/yn3z6/).

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# TODO





# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./GitHub Repo/yn3z6/syntax/00_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages with groundhog

groundhog.library(mitml, groundhog_day)
groundhog.library(nlme, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

# TODO





# ---------------------------------------------------------------------------- #
# Unstack imputed datasets ----
# ---------------------------------------------------------------------------- #

# TODO





impList_contemp <- as.mitml.list(split(imps2$contemp_imps, 
                                       imps2$contemp_imps$imp_num))
impList_lagged <- as.mitml.list(split(imps2$lagged_imps,
                                      imps2$lagged_imps$imp_num))

# ---------------------------------------------------------------------------- #
# Run analyses for intent-to-treat sample ----
# ---------------------------------------------------------------------------- #

# TODO





# Note: extra.pars of testEstimates reports the variance for each random effect, 
# covariances among random effects, residual variance, and ICC. By contrast,
# lme (and lmer) itself reports the standard deviation for each random effect,
# correlations among random effects, and residual standard deviation.

# Confirmed that random effects variances are not 0 (default when they cannot be 
# estimated) and that random effects correlations are not +/- 1 (default when they 
# cannot be estimated)--see Brown (2021). Checked variance estimates by looking at 
# extra.pars output after pooling and computed random effects correlations after
# pooling using a custom function defined above.

# Define df as though data were complete, one set for models of the total effect
# and one set for models of the unique effect

# TODO





df_tot_eff_model <- c(rep(130, 3), 42)
df_unq_eff_model <- c(rep(128, 5), rep(40, 3))

# Contemporaneous models using average item scores

# TODO





# Contemporaneous models using POMP scores

# TODO






# Lagged models using average item scores

# TODO





# Lagged models using POMP scores

# TODO






# Combine all results into a list

# TODO





result_itt_max <- list("contemp_meanDSS_wth" = contemp_meanDSS_wth, 
                       "contemp_cnDoSS_wth" = contemp_cnDoSS_wth, 
                       "contemp_KMTOT_wth" = contemp_KMTOT_wth, 
                       "contemp_all" = contemp_all, 
                       "contemp_meanDSS_wth_pomp" = contemp_meanDSS_wth_pomp, 
                       "contemp_cnDoSS_wth_pomp" = contemp_cnDoSS_wth_pomp, 
                       "contemp_KMTOT_wth_pomp" = contemp_KMTOT_wth_pomp, 
                       "contemp_all_pomp" = contemp_all_pomp, 
                       "lagged_meanDSS_wth" = lagged_meanDSS_wth, 
                       "lagged_cnDoSS_wth" = lagged_cnDoSS_wth, 
                       "lagged_KMTOT_wth" = lagged_KMTOT_wth, 
                       "lagged_all" = lagged_all, 
                       "lagged_meanDSS_wth_pomp" = lagged_meanDSS_wth_pomp, 
                       "lagged_cnDoSS_wth_pomp" = lagged_cnDoSS_wth_pomp, 
                       "lagged_KMTOT_wth_pomp" = lagged_KMTOT_wth_pomp, 
                       "lagged_all_pomp" = lagged_all_pomp)

# Save results as R object

# TODO





save(result_itt_max, file = "./results/result_itt_max.RData")

# ---------------------------------------------------------------------------- #
# Run analyses for completer sample ----
# ---------------------------------------------------------------------------- #

# Contemporaneous models using average item scores

# TODO





# Contemporaneous models using POMP scores

# TODO





# Lagged models using average item scores

# TODO





# Lagged models using POMP scores

# TODO





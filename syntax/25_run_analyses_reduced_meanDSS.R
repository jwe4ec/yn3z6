# ---------------------------------------------------------------------------- #
# Run Analyses - Reduced Models (Retaining meanDSS Random Effect)
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

load("./data/final/imps2.RData")

# ---------------------------------------------------------------------------- #
# Unstack relevant imputed datasets ----
# ---------------------------------------------------------------------------- #

impList_contemp <- as.mitml.list(split(imps2$contemp_imps$red_meanDSS_contemp_imps, 
                                       imps2$contemp_imps$red_meanDSS_contemp_imps$imp_num))
impList_lagged <- as.mitml.list(split(imps2$lagged_imps$red_meanDSS_lagged_imps,
                                      imps2$lagged_imps$red_meanDSS_lagged_imps$imp_num))

# ---------------------------------------------------------------------------- #
# Run analyses for intent-to-treat sample ----
# ---------------------------------------------------------------------------- #

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

df_tot_eff_model <- c(rep(130, 3), 42)
df_unq_eff_model <- c(rep(128, 5), rep(40, 3))

# Contemporaneous models using average item scores

# Error: optim problem, convergence error code = 1. Resolved with msMaxIter.
# modelList <-
#   with(impList_contemp,
#        lme(fixed = drtotl_m_imp ~ time0 + meanDSS_wth + meanDSS_btw_cent,
#            random = ~ 1 + time0 + meanDSS_wth | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <-
  with(impList_contemp,
       lme(fixed = drtotl_m_imp ~ time0 + meanDSS_wth + meanDSS_btw_cent,
           random = ~ 1 + time0 + meanDSS_wth | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
contemp_meanDSS_wth <- create_results_list(modelList, pooled, 3)

# Error: optim problem, convergence error code = 1. Resolved with msMaxIter.
# modelList <-
#   with(impList_contemp,
#        lme(fixed = drtotl_m_imp ~ time0 + cnDoSS_wth + cnDoSS_btw_cent,
#            random = ~ 1 + time0 | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <-
  with(impList_contemp,
       lme(fixed = drtotl_m_imp ~ time0 + cnDoSS_wth + cnDoSS_btw_cent,
           random = ~ 1 + time0 | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
contemp_cnDoSS_wth <- create_results_list(modelList, pooled, 2)

# Error: optim problem, convergence error code = 1. Resolved with msMaxIter.
# modelList <-
#   with(impList_contemp,
#        lme(fixed = drtotl_m_imp ~ time0 + KMTOT_wth + KMTOT_btw_cent,
#            random = ~ 1 + time0 | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <-
  with(impList_contemp,
       lme(fixed = drtotl_m_imp ~ time0 + KMTOT_wth + KMTOT_btw_cent,
           random = ~ 1 + time0 | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
contemp_KMTOT_wth <- create_results_list(modelList, pooled, 2)

# Error: optim problem, convergence error code = 1. Resolved with msMaxIter.
# modelList <-
#   with(impList_contemp,
#        lme(fixed = drtotl_m_imp ~ time0 + meanDSS_wth + cnDoSS_wth + KMTOT_wth +
#              meanDSS_btw_cent + cnDoSS_btw_cent + KMTOT_btw_cent,
#            random = ~ 1 + time0 + meanDSS_wth | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <-
  with(impList_contemp,
       lme(fixed = drtotl_m_imp ~ time0 + meanDSS_wth + cnDoSS_wth + KMTOT_wth +
             meanDSS_btw_cent + cnDoSS_btw_cent + KMTOT_btw_cent,
           random = ~ 1 + time0 + meanDSS_wth | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_unq_eff_model)
contemp_all <- create_results_list(modelList, pooled, 3)

# Contemporaneous models using POMP scores

# Error: optim problem, convergence error code = 1. Resolved with msMaxIter.
# modelList <-
#   with(impList_contemp,
#        lme(fixed = drtotl_m_imp_pomp ~ time0 + meanDSS_wth_pomp + meanDSS_btw_cent_pomp,
#            random = ~ 1 + time0 + meanDSS_wth_pomp | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <-
  with(impList_contemp,
       lme(fixed = drtotl_m_imp_pomp ~ time0 + meanDSS_wth_pomp + meanDSS_btw_cent_pomp,
           random = ~ 1 + time0 + meanDSS_wth_pomp | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
contemp_meanDSS_wth_pomp <- create_results_list(modelList, pooled, 3)

modelList <- 
  with(impList_contemp,
       lme(fixed = drtotl_m_imp_pomp ~ time0 + cnDoSS_wth_pomp + cnDoSS_btw_cent_pomp,
           random = ~ 1 + time0 | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim"),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
contemp_cnDoSS_wth_pomp <- create_results_list(modelList, pooled, 2)

modelList <-
  with(impList_contemp,
       lme(fixed = drtotl_m_imp_pomp ~ time0 + KMTOT_wth_pomp + KMTOT_btw_cent_pomp,
           random = ~ 1 + time0 | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim"),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
contemp_KMTOT_wth_pomp <- create_results_list(modelList, pooled, 2)

# Error: optim problem, convergence error code = 1. Resolved with msMaxIter.
# modelList <-
#   with(impList_contemp,
#        lme(fixed = drtotl_m_imp_pomp ~ time0 + meanDSS_wth_pomp + cnDoSS_wth_pomp +
#              KMTOT_wth_pomp + meanDSS_btw_cent_pomp + cnDoSS_btw_cent_pomp +
#              KMTOT_btw_cent_pomp,
#            random = ~ 1 + time0 + meanDSS_wth_pomp | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <-
  with(impList_contemp,
       lme(fixed = drtotl_m_imp_pomp ~ time0 + meanDSS_wth_pomp + cnDoSS_wth_pomp +
             KMTOT_wth_pomp + meanDSS_btw_cent_pomp + cnDoSS_btw_cent_pomp +
             KMTOT_btw_cent_pomp,
           random = ~ 1 + time0 + meanDSS_wth_pomp | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_unq_eff_model)
contemp_all_pomp <- create_results_list(modelList, pooled, 3)

# Lagged models using average item scores

# Error: optim problem, convergence error code = 1. Resolved with msMaxIter.
# modelList <-
#   with(impList_lagged,
#        lme(fixed = drtotl_m_imp ~ time0 + lmeanDSS_wth + lmeanDSS_btw_cent,
#            random = ~ 1 + time0 + lmeanDSS_wth | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <-
  with(impList_lagged,
       lme(fixed = drtotl_m_imp ~ time0 + lmeanDSS_wth + lmeanDSS_btw_cent,
           random = ~ 1 + time0 + lmeanDSS_wth | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
lagged_meanDSS_wth <- create_results_list(modelList, pooled, 3)

# Error: optim problem, convergence error code = 1. Resolved with msMaxIter.
# modelList <-
#   with(impList_lagged,
#        lme(fixed = drtotl_m_imp ~ time0 + lcnDoSS_wth + lcnDoSS_btw_cent,
#            random = ~ 1 + time0 | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <-
  with(impList_lagged,
       lme(fixed = drtotl_m_imp ~ time0 + lcnDoSS_wth + lcnDoSS_btw_cent,
           random = ~ 1 + time0 | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
lagged_cnDoSS_wth <- create_results_list(modelList, pooled, 2)

# Error: optim problem, convergence error code = 1. Resolved with msMaxIter.
# modelList <-
#   with(impList_lagged,
#        lme(fixed = drtotl_m_imp ~ time0 + lKMTOT_wth + lKMTOT_btw_cent,
#            random = ~ 1 + time0 | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <-
  with(impList_lagged,
       lme(fixed = drtotl_m_imp ~ time0 + lKMTOT_wth + lKMTOT_btw_cent,
           random = ~ 1 + time0 | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
lagged_KMTOT_wth <- create_results_list(modelList, pooled, 2)

# Error: optim problem, convergence error code = 1. Resolved with msMaxIter.
# modelList <-
#   with(impList_lagged,
#        lme(fixed = drtotl_m_imp ~ time0 + lmeanDSS_wth + lcnDoSS_wth + lKMTOT_wth +
#              lmeanDSS_btw_cent + lcnDoSS_btw_cent + lKMTOT_btw_cent,
#            random = ~ 1 + time0 + lmeanDSS_wth | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <-
  with(impList_lagged,
       lme(fixed = drtotl_m_imp ~ time0 + lmeanDSS_wth + lcnDoSS_wth + lKMTOT_wth +
             lmeanDSS_btw_cent + lcnDoSS_btw_cent + lKMTOT_btw_cent,
           random = ~ 1 + time0 + lmeanDSS_wth | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_unq_eff_model)
lagged_all <- create_results_list(modelList, pooled, 3)

# Lagged models using POMP scores

# Error: optim problem, convergence error code = 1. Resolved with msMaxIter.
# modelList <-
#   with(impList_lagged,
#        lme(fixed = drtotl_m_imp_pomp ~ time0 + lmeanDSS_wth_pomp + lmeanDSS_btw_cent_pomp,
#            random = ~ 1 + time0 + lmeanDSS_wth_pomp | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <-
  with(impList_lagged,
       lme(fixed = drtotl_m_imp_pomp ~ time0 + lmeanDSS_wth_pomp + lmeanDSS_btw_cent_pomp,
           random = ~ 1 + time0 + lmeanDSS_wth_pomp | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
lagged_meanDSS_wth_pomp <- create_results_list(modelList, pooled, 3)

modelList <-
  with(impList_lagged,
       lme(fixed = drtotl_m_imp_pomp ~ time0 + lcnDoSS_wth_pomp + lcnDoSS_btw_cent_pomp,
           random = ~ 1 + time0 | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim"),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
lagged_cnDoSS_wth_pomp <- create_results_list(modelList, pooled, 2)

modelList <-
  with(impList_lagged,
       lme(fixed = drtotl_m_imp_pomp ~ time0 + lKMTOT_wth_pomp + lKMTOT_btw_cent_pomp,
           random = ~ 1 + time0 | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim"),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
lagged_KMTOT_wth_pomp <- create_results_list(modelList, pooled, 2)

# Error: optim problem, convergence error code = 1. Resolved with msMaxIter.
# modelList <-
#   with(impList_lagged,
#        lme(fixed = drtotl_m_imp_pomp ~ time0 + lmeanDSS_wth_pomp + lcnDoSS_wth_pomp +
#              lKMTOT_wth_pomp + lmeanDSS_btw_cent_pomp + lcnDoSS_btw_cent_pomp +
#              lKMTOT_btw_cent_pomp,
#            random = ~ 1 + time0 + lmeanDSS_wth_pomp | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <-
  with(impList_lagged,
       lme(fixed = drtotl_m_imp_pomp ~ time0 + lmeanDSS_wth_pomp + lcnDoSS_wth_pomp +
             lKMTOT_wth_pomp + lmeanDSS_btw_cent_pomp + lcnDoSS_btw_cent_pomp +
             lKMTOT_btw_cent_pomp,
           random = ~ 1 + time0 + lmeanDSS_wth_pomp | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_unq_eff_model)
lagged_all_pomp <- create_results_list(modelList, pooled, 3)

# Combine all results into a list

result_itt_red_meanDSS <- list("contemp_meanDSS_wth" = contemp_meanDSS_wth, 
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

save(result_itt_red_meanDSS, file = "./results/result_itt_red_meanDSS.RData")

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





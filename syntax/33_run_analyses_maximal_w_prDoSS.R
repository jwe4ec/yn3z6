# ---------------------------------------------------------------------------- #
# Run Analyses - Maximal Models with prDoSS (Retaining All Random Effects)
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

load("./data/final/max_contemp_w_prDoSS_imps.RData")
load("./data/final/max_lagged_w_prDoSS_imps.RData")

# ---------------------------------------------------------------------------- #
# Unstack relevant imputed datasets ----
# ---------------------------------------------------------------------------- #

impList_contemp <- as.mitml.list(split(max_contemp_w_prDoSS_imps, 
                                       max_contemp_w_prDoSS_imps$imp_num))
impList_lagged <- as.mitml.list(split(max_lagged_w_prDoSS_imps,
                                      max_lagged_w_prDoSS_imps$imp_num))

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
#            random = ~ 1 + time0 + cnDoSS_wth | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <- 
  with(impList_contemp,
       lme(fixed = drtotl_m_imp ~ time0 + cnDoSS_wth + cnDoSS_btw_cent,
           random = ~ 1 + time0 + cnDoSS_wth | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
contemp_cnDoSS_wth <- create_results_list(modelList, pooled, 3)

# Error: optim problem, convergence error code = 1. Resolved with msMaxIter.
# modelList <-
#   with(impList_contemp,
#        lme(fixed = drtotl_m_imp ~ time0 + KMTOT_wth + KMTOT_btw_cent,
#            random = ~ 1 + time0 + KMTOT_wth | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <-
  with(impList_contemp,
       lme(fixed = drtotl_m_imp ~ time0 + KMTOT_wth + KMTOT_btw_cent,
           random = ~ 1 + time0 + KMTOT_wth | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
contemp_KMTOT_wth <- create_results_list(modelList, pooled, 3)

# "Error: Fewer observations than random effects in all level 1 groups." After
# overriding this with allow.n.lt.q and resolving convergence error with msMaxIter,
# obtained "Error in chol.default((value + t(value))/2): the leading minor of order 
# 4 is not positive definite." Unclear how to resolve this.
# modelList <-
#   with(impList_contemp,
#        lme(fixed = drtotl_m_imp ~ time0 + meanDSS_wth + cnDoSS_wth + KMTOT_wth +
#              meanDSS_btw_cent + cnDoSS_btw_cent + KMTOT_btw_cent,
#            random = ~ 1 + time0 + meanDSS_wth + cnDoSS_wth + KMTOT_wth | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
# modelList <-
#   with(impList_contemp,
#        lme(fixed = drtotl_m_imp ~ time0 + meanDSS_wth + cnDoSS_wth + KMTOT_wth +
#              meanDSS_btw_cent + cnDoSS_btw_cent + KMTOT_btw_cent,
#            random = ~ 1 + time0 + meanDSS_wth + cnDoSS_wth + KMTOT_wth | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim", allow.n.lt.q = TRUE),
#            method = "REML"))
# 
# modelList <- 
#   with(impList_contemp,
#        lme(fixed = drtotl_m_imp ~ time0 + meanDSS_wth + cnDoSS_wth + KMTOT_wth +
#              meanDSS_btw_cent + cnDoSS_btw_cent + KMTOT_btw_cent,
#            random = ~ 1 + time0 + meanDSS_wth + cnDoSS_wth + KMTOT_wth | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim", 
#                                 allow.n.lt.q = TRUE, 
#                                 msMaxIter = 1e9),
#            method = "REML"))
nonconvergence_msg <- "Model did not converge"
warning(paste0("contemp_all: ", nonconvergence_msg))
modelList <- nonconvergence_msg
pooled <- NA
contemp_all <- create_results_list(modelList, pooled, 5)

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
#            random = ~ 1 + time0 + lcnDoSS_wth | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <- 
  with(impList_lagged,
       lme(fixed = drtotl_m_imp ~ time0 + lcnDoSS_wth + lcnDoSS_btw_cent,
           random = ~ 1 + time0 + lcnDoSS_wth | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
lagged_cnDoSS_wth <- create_results_list(modelList, pooled, 3)

# Error: optim problem, convergence error code = 1. Resolved with msMaxIter.
# modelList <-
#   with(impList_lagged,
#        lme(fixed = drtotl_m_imp ~ time0 + lKMTOT_wth + lKMTOT_btw_cent,
#            random = ~ 1 + time0 + lKMTOT_wth | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
modelList <- 
  with(impList_lagged,
       lme(fixed = drtotl_m_imp ~ time0 + lKMTOT_wth + lKMTOT_btw_cent,
           random = ~ 1 + time0 + lKMTOT_wth | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_tot_eff_model)
lagged_KMTOT_wth <- create_results_list(modelList, pooled, 3)

# "Error: Fewer observations than random effects in all level 1 groups." Can
# override with allow.n.lt.q and then resolve nonconvergence with msMaxIter.
# modelList <-
#   with(impList_lagged,
#        lme(fixed = drtotl_m_imp ~ time0 + lmeanDSS_wth + lcnDoSS_wth + lKMTOT_wth +
#              lmeanDSS_btw_cent + lcnDoSS_btw_cent + lKMTOT_btw_cent,
#            random = ~ 1 + time0 + lmeanDSS_wth + lcnDoSS_wth +
#              lKMTOT_wth | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim"),
#            method = "REML"))
# modelList <-
#   with(impList_lagged,
#        lme(fixed = drtotl_m_imp ~ time0 + lmeanDSS_wth + lcnDoSS_wth + lKMTOT_wth +
#              lmeanDSS_btw_cent + lcnDoSS_btw_cent + lKMTOT_btw_cent,
#            random = ~ 1 + time0 + lmeanDSS_wth + lcnDoSS_wth +
#              lKMTOT_wth | ResearchID,
#            correlation = corAR1(form = ~ time0 | ResearchID),
#            control = lmeControl(opt = "optim", allow.n.lt.q = TRUE),
#            method = "REML"))
modelList <- 
  with(impList_lagged,
       lme(fixed = drtotl_m_imp ~ time0 + lmeanDSS_wth + lcnDoSS_wth + lKMTOT_wth + 
             lmeanDSS_btw_cent + lcnDoSS_btw_cent + lKMTOT_btw_cent,
           random = ~ 1 + time0 + lmeanDSS_wth + lcnDoSS_wth + 
             lKMTOT_wth | ResearchID,
           correlation = corAR1(form = ~ time0 | ResearchID),
           control = lmeControl(opt = "optim", 
                                allow.n.lt.q = TRUE,
                                msMaxIter = 1e9),
           method = "REML"))
pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df_unq_eff_model)
lagged_all <- create_results_list(modelList, pooled, 5)

# Combine all results into a list

result_itt_max_w_prDoSS <- list("contemp_meanDSS_wth" = contemp_meanDSS_wth, 
                                "contemp_cnDoSS_wth" = contemp_cnDoSS_wth, 
                                "contemp_KMTOT_wth" = contemp_KMTOT_wth, 
                                "contemp_all" = contemp_all, 
                                "lagged_meanDSS_wth" = lagged_meanDSS_wth, 
                                "lagged_cnDoSS_wth" = lagged_cnDoSS_wth, 
                                "lagged_KMTOT_wth" = lagged_KMTOT_wth, 
                                "lagged_all" = lagged_all)

# Save results as R object

save(result_itt_max_w_prDoSS, file = "./results/result_itt_max_w_prDoSS.RData")
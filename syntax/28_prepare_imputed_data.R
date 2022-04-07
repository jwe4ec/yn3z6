# ---------------------------------------------------------------------------- #
# Prepare Imputed Data
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

# No packages loaded

# ---------------------------------------------------------------------------- #
# Define functions used throughout script ----
# ---------------------------------------------------------------------------- #

# Define function to compute variables for between-person effects (i.e., grand-
# mean-centered person means, where grand mean is the mean of person means) and 
# within-person effects (i.e., person-mean-centered scores) from imputed scores. 
# Even though the latent group (i.e., person) means estimated by the imputation 
# model were saved with the imputed data, per consultation with Craig Enders
# (6/15/2021), arithmetic means computed from the imputed scores can be used to 
# compute the between-person effects and within-person effects prior to analysis.

disaggregate_post_imp <- function(imps_df, score_colname) {
  if (score_colname %in% names(imps_df)) {
    # Compute between-person effects
    
    score_btw_colname <- paste0(score_colname, "_btw")
    score_btw_mean_colname <- paste0(score_colname, "_btw_mean")
    score_btw_cent_colname <- paste0(score_colname, "_btw_cent")
    
    person_means <- aggregate(imps_df[, score_colname],
                              list(imps_df$ResearchID, imps_df$imp_num),
                              mean, na.rm = FALSE)
    names(person_means)[1] <- "ResearchID"
    names(person_means)[2] <- "imp_num"
    names(person_means)[3] <- score_btw_colname
    
    mean_person_means <- aggregate(person_means[, score_btw_colname],
                                   list(person_means$imp_num),
                                   mean, na.rm = FALSE)
    names(mean_person_means)[1] <- "imp_num"
    names(mean_person_means)[2] <- score_btw_mean_colname
    
    output <- merge(imps_df, 
                    person_means, 
                    by = c("imp_num", "ResearchID"), 
                    all.x = TRUE)
    output <- merge(output,
                    mean_person_means,
                    by = "imp_num",
                    all.x = TRUE)
    
    output[, score_btw_cent_colname] <-
      output[, score_btw_colname] - output[, score_btw_mean_colname]
    
    # Compute within-person effects
    
    score_wth_colname <- paste0(score_colname, "_wth")
    
    output[, score_wth_colname] <- 
      output[, score_colname] - output[, score_btw_colname]
    
    return(output)
  } else {
    warning(paste0(score_colname, " not in imps_df"))
  }
}

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/intermediate/scale_defs.Rdata")

max_contemp_imps <- read.csv("./data/imputed/maximal/contemp/actual/imps.csv",
                             header = FALSE)
max_lagged_imps <- read.csv("./data/imputed/maximal/lagged/actual/imps.csv",
                            header = FALSE)
red_meanDSS_contemp_imps <- read.csv("./data/imputed/reduced_meanDSS/contemp/actual/imps.csv",
                                     header = FALSE)
red_meanDSS_lagged_imps <- read.csv("./data/imputed/reduced_meanDSS/lagged/actual/imps.csv",
                                    header = FALSE)
red_cnDoSS_contemp_imps <- read.csv("./data/imputed/reduced_cnDoSS/contemp/actual/imps.csv",
                                     header = FALSE)
red_cnDoSS_lagged_imps <- read.csv("./data/imputed/reduced_cnDoSS/lagged/actual/imps.csv",
                                    header = FALSE)
red_KMTOT_contemp_imps <- read.csv("./data/imputed/reduced_KMTOT/contemp/actual/imps.csv",
                                    header = FALSE)
red_KMTOT_lagged_imps <- read.csv("./data/imputed/reduced_KMTOT/lagged/actual/imps.csv",
                                   header = FALSE)

max_contemp_w_prDoSS_imps <- read.csv("./data/imputed/maximal_w_prDoSS/contemp/actual/imps.csv",
                                      header = FALSE)
max_lagged_w_prDoSS_imps <- read.csv("./data/imputed/maximal_w_prDoSS/lagged/actual/imps.csv",
                                     header = FALSE)

contemp_imps <- list("max_contemp_imps" = max_contemp_imps,
                     "red_meanDSS_contemp_imps" = red_meanDSS_contemp_imps,
                     "red_cnDoSS_contemp_imps" = red_cnDoSS_contemp_imps,
                     "red_KMTOT_contemp_imps" = red_KMTOT_contemp_imps,
                     "max_contemp_w_prDoSS_imps" = max_contemp_w_prDoSS_imps)
lagged_imps <- list("max_lagged_imps" = max_lagged_imps,
                    "red_meanDSS_lagged_imps" = red_meanDSS_lagged_imps,
                    "red_cnDoSS_lagged_imps" = red_cnDoSS_lagged_imps,
                    "red_KMTOT_lagged_imps" = red_KMTOT_lagged_imps,
                    "max_lagged_w_prDoSS_imps" = max_lagged_w_prDoSS_imps)
imps <- list("contemp_imps" = contemp_imps, "lagged_imps" = lagged_imps)

# ---------------------------------------------------------------------------- #
# Label with Blimp-outputted column names ----
# ---------------------------------------------------------------------------- #

# Note: Although in the imputation model the Level 1 predictors "meanDSS",
# "cnDoSS", and "KMTOT" were group mean centered (i.e., person mean centered)
# based on estimated latent group means, Blimp outputs imputed values for these
# predictors in terms of their original metrics. Blimp also outputs estimates
# for the random intercept, random slopes, and latent group means (i.e., person 
# means) from the imputation model. Although in the imputation model the person
# means were grand mean centered, the outputted person means are not. Prior to
# running the analysis model, the person means should be used to compute person-
# mean-centered Level 1 predictors and the person means should then be grand-
# mean-centered before being entered into the analysis model at Level 2.

max_contemp_imps_blimp_out_names <- c("imp#", "ResearchI", "time0", "Condition",
                                      "AIN", "Period",
                                      "meanDSS", "drtotl_m_", "cnDoSS", "KMTOT",
                                      "DDS14_fac",
                                      "DDS17a2_f", "prDoSS", "DDS17a2_1", "DDS17a2_2",
                                      "cond0rev",
                                      "drtotl_m_[ResearchI]",
                                      "drtotl_m_$time0[ResearchI]",
                                      "drtotl_m_$meanDSS[ResearchI]",
                                      "drtotl_m_$KMTOT[ResearchI]",
                                      "drtotl_m_$cnDoSS[ResearchI]",
                                      "prDoSS[ResearchI]",
                                      "prDoSS$time0[ResearchI]",
                                      "prDoSS$meanDSS[ResearchI]",
                                      "prDoSS$KMTOT[ResearchI]",
                                      "prDoSS$cnDoSS[ResearchI]",
                                      "meanDSS.mean[ResearchI]",
                                      "cnDoSS.mean[ResearchI]",
                                      "KMTOT.mean[ResearchI]")
max_lagged_imps_blimp_out_names <- c("imp#", "ResearchI", "time0", "Condition", 
                                     "AIN", "Period", 
                                     "meanDSS", "drtotl_m_", "cnDoSS", "KMTOT", 
                                     "DDS14_fac",
                                     "DDS17a2_f", "prDoSS", "DDS17a2_1", "DDS17a2_2", 
                                     "cond0rev", 
                                     "lmeanDSS", "lKMTOT", "lcnDoSS", 
                                     "drtotl_m_[ResearchI]",
                                     "drtotl_m_$time0[ResearchI]", 
                                     "drtotl_m_$lmeanDSS[ResearchI]", 
                                     "drtotl_m_$lKMTOT[ResearchI]",
                                     "drtotl_m_$lcnDoSS[ResearchI]", 
                                     "prDoSS[ResearchI]", 
                                     "prDoSS$time0[ResearchI]", 
                                     "prDoSS$lmeanDSS[ResearchI]",
                                     "prDoSS$lKMTOT[ResearchI]", 
                                     "prDoSS$lcnDoSS[ResearchI]", 
                                     "lmeanDSS.mean[ResearchI]",
                                     "lKMTOT.mean[ResearchI]", 
                                     "lcnDoSS.mean[ResearchI]")

red_meanDSS_contemp_imps_blimp_out_names <-
  max_contemp_imps_blimp_out_names[!max_contemp_imps_blimp_out_names %in% 
                                     c("drtotl_m_$KMTOT[ResearchI]",
                                       "drtotl_m_$cnDoSS[ResearchI]")]
red_meanDSS_lagged_imps_blimp_out_names <-
  max_lagged_imps_blimp_out_names[!max_lagged_imps_blimp_out_names %in% 
                                     c("drtotl_m_$lKMTOT[ResearchI]",
                                       "drtotl_m_$lcnDoSS[ResearchI]")]

red_cnDoSS_contemp_imps_blimp_out_names <-
  max_contemp_imps_blimp_out_names[!max_contemp_imps_blimp_out_names %in% 
                                     c("drtotl_m_$meanDSS[ResearchI]",
                                       "drtotl_m_$KMTOT[ResearchI]")]
red_cnDoSS_lagged_imps_blimp_out_names <-
  max_lagged_imps_blimp_out_names[!max_lagged_imps_blimp_out_names %in% 
                                    c("drtotl_m_$lmeanDSS[ResearchI]",
                                      "drtotl_m_$lKMTOT[ResearchI]")]

red_KMTOT_contemp_imps_blimp_out_names <-
  max_contemp_imps_blimp_out_names[!max_contemp_imps_blimp_out_names %in% 
                                     c("drtotl_m_$meanDSS[ResearchI]",
                                       "drtotl_m_$cnDoSS[ResearchI]")]
red_KMTOT_lagged_imps_blimp_out_names <-
  max_lagged_imps_blimp_out_names[!max_lagged_imps_blimp_out_names %in% 
                                    c("drtotl_m_$lmeanDSS[ResearchI]",
                                      "drtotl_m_$lcnDoSS[ResearchI]")]

names(imps$contemp_imps$max_contemp_imps) <- max_contemp_imps_blimp_out_names
names(imps$lagged_imps$max_lagged_imps) <- max_lagged_imps_blimp_out_names
names(imps$contemp_imps$red_meanDSS_contemp_imps) <- red_meanDSS_contemp_imps_blimp_out_names
names(imps$lagged_imps$red_meanDSS_lagged_imps) <- red_meanDSS_lagged_imps_blimp_out_names
names(imps$contemp_imps$red_cnDoSS_contemp_imps) <- red_cnDoSS_contemp_imps_blimp_out_names
names(imps$lagged_imps$red_cnDoSS_lagged_imps) <- red_cnDoSS_lagged_imps_blimp_out_names
names(imps$contemp_imps$red_KMTOT_contemp_imps) <- red_KMTOT_contemp_imps_blimp_out_names
names(imps$lagged_imps$red_KMTOT_lagged_imps) <- red_KMTOT_lagged_imps_blimp_out_names

names(imps$contemp_imps$max_contemp_w_prDoSS_imps) <- max_contemp_imps_blimp_out_names
names(imps$lagged_imps$max_lagged_w_prDoSS_imps) <- max_lagged_imps_blimp_out_names

# ---------------------------------------------------------------------------- #
# Rename columns ----
# ---------------------------------------------------------------------------- #

for (i in 1:length(imps)) {
  for (j in 1:length(imps[[i]])) {
    names(imps[[i]][[j]]) <- 
      gsub("ResearchI", "ResearchID", names(imps[[i]][[j]]), fixed = TRUE)
    
    names(imps[[i]][[j]])[names(imps[[i]][[j]]) == "imp#"] <- "imp_num"
    names(imps[[i]][[j]])[names(imps[[i]][[j]]) == "drtotl_m_"] <- "drtotl_m_imp"
    names(imps[[i]][[j]])[names(imps[[i]][[j]]) == "DDS14_fac"] <- "DDS14_factor"
    names(imps[[i]][[j]])[names(imps[[i]][[j]]) == "DDS17a2_f"] <- "DDS17a2_factor"
    names(imps[[i]][[j]])[names(imps[[i]][[j]]) == "DDS17a2_1"] <- "DDS17a2_factor_collapsed"
    names(imps[[i]][[j]])[names(imps[[i]][[j]]) == "DDS17a2_2"] <- "DDS17a2_factor_collapsed2"
  }
}

# ---------------------------------------------------------------------------- #
# Recode and drop variables ----
# ---------------------------------------------------------------------------- #

# Recode 999 as NA in lagged data (due to lagged variables "lmeanDSS", "lKMTOT", 
# and "lcnDoSS" being imputed instead of "meanDSS", "KMTOT", and "cnDoSS")

for (i in 1:length(imps$lagged_imps)) {
  imps$lagged_imps[[i]][imps$lagged_imps[[i]] == 999] <- NA
}

# Drop auxiliary variables, estimated latent group means, and estimated random 
# effects from all tables and drop non-lagged variables from lagged tables

drop_contemp <- c("DDS14_factor",
                  "DDS17a2_factor", "prDoSS", "DDS17a2_factor_collapsed",
                  "DDS17a2_factor_collapsed2",
                  "drtotl_m_[ResearchID]",
                  "drtotl_m_$time0[ResearchID]", "drtotl_m_$meanDSS[ResearchID]",
                  "drtotl_m_$KMTOT[ResearchID]", "drtotl_m_$cnDoSS[ResearchID]",
                  "prDoSS[ResearchID]", "prDoSS$time0[ResearchID]", 
                  "prDoSS$meanDSS[ResearchID]", "prDoSS$KMTOT[ResearchID]", 
                  "prDoSS$cnDoSS[ResearchID]",
                  "meanDSS.mean[ResearchID]", "cnDoSS.mean[ResearchID]",
                  "KMTOT.mean[ResearchID]")

drop_lagged <- c("meanDSS", "cnDoSS", "KMTOT",
                 "DDS14_factor",
                 "DDS17a2_factor", "prDoSS", "DDS17a2_factor_collapsed",
                 "DDS17a2_factor_collapsed2",
                 "drtotl_m_[ResearchID]",
                 "drtotl_m_$time0[ResearchID]", "drtotl_m_$lmeanDSS[ResearchID]",
                 "drtotl_m_$lKMTOT[ResearchID]", "drtotl_m_$lcnDoSS[ResearchID]",
                 "prDoSS[ResearchID]", "prDoSS$time0[ResearchID]", 
                 "prDoSS$lmeanDSS[ResearchID]", "prDoSS$lKMTOT[ResearchID]", 
                 "prDoSS$lcnDoSS[ResearchID]",
                 "lmeanDSS.mean[ResearchID]", "lcnDoSS.mean[ResearchID]",
                 "lKMTOT.mean[ResearchID]")

for (i in 1:length(imps$contemp_imps)) {
  imps$contemp_imps[[i]][, drop_contemp] <- NULL
}
for (i in 1:length(imps$lagged_imps)) {
  imps$lagged_imps[[i]][, drop_lagged] <- NULL
}

# ---------------------------------------------------------------------------- #
# Compute variables for between- and within-person effects ----
# ---------------------------------------------------------------------------- #

imps2 <- imps

for (i in 1:length(imps2$contemp_imps)) {
  imps2$contemp_imps[[i]] <- disaggregate_post_imp(imps2$contemp_imps[[i]], "meanDSS")
  imps2$contemp_imps[[i]] <- disaggregate_post_imp(imps2$contemp_imps[[i]], "cnDoSS")
  imps2$contemp_imps[[i]] <- disaggregate_post_imp(imps2$contemp_imps[[i]], "KMTOT")
}

for (i in 1:length(imps2$lagged_imps)) {
  imps2$lagged_imps[[i]] <- disaggregate_post_imp(imps2$lagged_imps[[i]], "lmeanDSS")
  imps2$lagged_imps[[i]] <- disaggregate_post_imp(imps2$lagged_imps[[i]], "lcnDoSS")
  imps2$lagged_imps[[i]] <- disaggregate_post_imp(imps2$lagged_imps[[i]], "lKMTOT")
}

# ---------------------------------------------------------------------------- #
# Compute POMP scores ----
# ---------------------------------------------------------------------------- #

# Compute percent-of-maximum-possible (POMP) scores (Cohen et al., 1999, p. 323)

drtotl_vars <- "drtotl_m_imp"
meanDSS_vars <- c("meanDSS", "meanDSS_btw", "meanDSS_btw_mean", 
                  "meanDSS_btw_cent", "meanDSS_wth")
cnDoSS_vars <- c("cnDoSS", "cnDoSS_btw", "cnDoSS_btw_mean", 
                 "cnDoSS_btw_cent", "cnDoSS_wth")
KMTOT_vars <- c("KMTOT", "KMTOT_btw", "KMTOT_btw_mean", 
                "KMTOT_btw_cent", "KMTOT_wth")
lmeanDSS_vars <- c("lmeanDSS", "lmeanDSS_btw", "lmeanDSS_btw_mean", 
                   "lmeanDSS_btw_cent", "lmeanDSS_wth")
lcnDoSS_vars <- c("lcnDoSS", "lcnDoSS_btw", "lcnDoSS_btw_mean", 
                  "lcnDoSS_btw_cent", "lcnDoSS_wth")
lKMTOT_vars <- c("lKMTOT", "lKMTOT_btw", "lKMTOT_btw_mean", 
                 "lKMTOT_btw_cent", "lKMTOT_wth")

for (i in 1:length(imps2$contemp_imps)) {
  imps2$contemp_imps[[i]] <- 
    compute_pomp(imps2$contemp_imps[[i]], drtotl_vars,
                 scale_defs$drtotl_n_items, scale_defs$drtotl_min, scale_defs$drtotl_max)
  imps2$contemp_imps[[i]] <- 
    compute_pomp(imps2$contemp_imps[[i]], meanDSS_vars, 
                 scale_defs$meanDSS_n_items, scale_defs$meanDSS_min, scale_defs$meanDSS_max)
  imps2$contemp_imps[[i]] <- 
    compute_pomp(imps2$contemp_imps[[i]], cnDoSS_vars, 
                 scale_defs$cnDoSS_n_items, scale_defs$cnDoSS_min, scale_defs$cnDoSS_max)
  imps2$contemp_imps[[i]] <- 
    compute_pomp(imps2$contemp_imps[[i]], KMTOT_vars, 
                 scale_defs$KMTOT_n_items, scale_defs$KMTOT_min, scale_defs$KMTOT_max)
}

for (i in 1:length(imps2$lagged_imps)) {
  imps2$lagged_imps[[i]] <- 
    compute_pomp(imps2$lagged_imps[[i]], drtotl_vars, 
                 scale_defs$drtotl_n_items, scale_defs$drtotl_min, scale_defs$drtotl_max)
  imps2$lagged_imps[[i]] <- 
    compute_pomp(imps2$lagged_imps[[i]], lmeanDSS_vars, 
                 scale_defs$meanDSS_n_items, scale_defs$meanDSS_min, scale_defs$meanDSS_max)
  imps2$lagged_imps[[i]] <- 
    compute_pomp(imps2$lagged_imps[[i]], lcnDoSS_vars, 
                 scale_defs$cnDoSS_n_items, scale_defs$cnDoSS_min, scale_defs$cnDoSS_max)
  imps2$lagged_imps[[i]] <- 
    compute_pomp(imps2$lagged_imps[[i]], lKMTOT_vars, 
                 scale_defs$KMTOT_n_items, scale_defs$KMTOT_min, scale_defs$KMTOT_max)
}

# ---------------------------------------------------------------------------- #
# Export final data ----
# ---------------------------------------------------------------------------- #

# TODO: Do not overwrite "imps2" until imputation models are rerun with "prDoSS"
# and "cond0rev" now added to "contemp_aux.csv". Instead, save imputed data based
# on this new "contemp_aux.csv" separately for now.





dir.create("./data/final")
# save(imps2, file = "./data/final/imps2.RData")

max_contemp_w_prDoSS_imps <- imps2$contemp_imps$max_contemp_w_prDoSS_imps
max_lagged_w_prDoSS_imps <-  imps2$lagged_imps$max_lagged_w_prDoSS_imps

save(max_contemp_w_prDoSS_imps, file = "./data/final/max_contemp_w_prDoSS_imps.RData")
save(max_lagged_w_prDoSS_imps, file = "./data/final/max_lagged_w_prDoSS_imps.RData")
# ---------------------------------------------------------------------------- #
# Search for auxiliary variables
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

groundhog.library(rcompanion, groundhog_day)
groundhog.library(DescTools, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/intermediate/data4.Rdata")
load("./data/intermediate/scale_defs.Rdata")

data5 <- data4

# ---------------------------------------------------------------------------- #
# Prepare data ----
# ---------------------------------------------------------------------------- #

# Convert to wide format

ignore_vars <- c("ResearchID", "Condition", "AIN",
                 "SH132_factor", "DDS04", "DDS06b_factor", "DDS10_factor",
                 "DDS14_factor", "DDS15a_factor", "DDS16a_factor",
                 "DDS17a2_factor", "DDS25_factor", "DDS26_factor", "race",
                 "PPVT", "SCPN120", "evanxdx", "evdep", "eveatdx", "evsubab",
                 "nowanxdx", "nowdep", "noweatdx", "nowsomat", "nowsubab",
                 "primaryDX", "bpdcrit", "researchNoncompliance")
                 
varying_vars <- setdiff(names(data5$contemp_aux), ignore_vars)

contemp_aux_wide <- reshape(data5$contemp_aux,
                            v.names = varying_vars,
                            timevar = "time0",
                            idvar = "ResearchID",
                            direction = "wide")

# Subset column names

factor_x_vars <- character()
ordered_x_vars <- character()
int_num_x_vars <- character()

time0_vars <- names(contemp_aux_wide)[grep("time0", names(contemp_aux_wide))]
Period_vars <- names(contemp_aux_wide)[grep("Period", names(contemp_aux_wide))]
ind_y_vars <- names(contemp_aux_wide)[grep("_ind", names(contemp_aux_wide))]

for (i in 1:length(contemp_aux_wide)) {
  if (is.factor(contemp_aux_wide[, i]) & !is.ordered(contemp_aux_wide[, i])) {
    factor_x_vars <- c(factor_x_vars, names(contemp_aux_wide[i]))
  } else if (is.ordered(contemp_aux_wide[, i])) {
    ordered_x_vars <- c(ordered_x_vars, names(contemp_aux_wide[i]))
  } else if ((is.integer(contemp_aux_wide[, i]) | is.numeric(contemp_aux_wide[, i])) &
             !(names(contemp_aux_wide[i]) %in% 
               c("ResearchID", "AIN", time0_vars, Period_vars, ind_y_vars))) {
    int_num_x_vars <- c(int_num_x_vars, names(contemp_aux_wide[i]))
  }
}

# ---------------------------------------------------------------------------- #
# Identify continuous auxiliary variables ----
# ---------------------------------------------------------------------------- #

# Compute point-biserial correlation coefficient (cor.test function of stats 
# package) between continuous variables and binary missing data indicators

x_var <- rep(int_num_x_vars, each = length(ind_y_vars))
x_var_i <- rep(1:length(int_num_x_vars), each = length(ind_y_vars))
ind_y_var <- rep(ind_y_vars, length(int_num_x_vars))
ind_y_var_j <- rep(1:length(ind_y_vars), length(int_num_x_vars))
ind_y_cor <- vector("double", length(int_num_x_vars)*length(ind_y_vars))
ind_y_note <- rep(NA, length(int_num_x_vars)*length(ind_y_vars))

k <- 1

for (i in 1:length(int_num_x_vars)) {
  for (j in 1:length(ind_y_vars)) {
    if (unlist(strsplit(int_num_x_vars[i], ".", fixed = TRUE))[1] == 
          unlist(strsplit(ind_y_vars[j], "_ind", fixed = TRUE))[1]) {
      ind_y_cor[k] <- NA
      ind_y_note[k] <- "ind_y_var is the missing data indicator of x_var"
    } else if (unlist(strsplit(int_num_x_vars[i], ".", fixed = TRUE))[1] ==
                 unlist(strsplit(ind_y_vars[j], "_btw", fixed = TRUE))[1] |
               unlist(strsplit(int_num_x_vars[i], ".", fixed = TRUE))[1] ==
                 unlist(strsplit(ind_y_vars[j], "_wth", fixed = TRUE))[1] |
               
               unlist(strsplit(int_num_x_vars[i], "_btw", fixed = TRUE))[1] ==
                 unlist(strsplit(ind_y_vars[j], "_ind", fixed = TRUE))[1] |
               unlist(strsplit(int_num_x_vars[i], "_btw", fixed = TRUE))[1] ==
                 unlist(strsplit(ind_y_vars[j], "_wth", fixed = TRUE))[1] |
               
               unlist(strsplit(int_num_x_vars[i], "_wth", fixed = TRUE))[1] ==
                 unlist(strsplit(ind_y_vars[j], "_ind", fixed = TRUE))[1] |
               unlist(strsplit(int_num_x_vars[i], "_wth", fixed = TRUE))[1] ==
                 unlist(strsplit(ind_y_vars[j], "_btw", fixed = TRUE))[1]) {
      ind_y_cor[k] <- NA
      ind_y_note[k] <- "x_var and ind_y_var are computed from the same variable"
    } else {
      if (sum(contemp_aux_wide[, ind_y_vars[j]]) > 0 &
          all(which(contemp_aux_wide[, ind_y_vars[j]] == 1) %in% 
            which(is.na(contemp_aux_wide[, int_num_x_vars[i]])))) {
        ind_y_cor[k] <- NA
        ind_y_note[k] <- "Rows with missing ind_y_var are also missing x_var"
      } else if (sd(contemp_aux_wide[, int_num_x_vars[i]], na.rm = TRUE) == 0 |
                   sd(contemp_aux_wide[, ind_y_vars[j]]) == 0) {
        ind_y_cor[k] <- NA
        ind_y_note[k] <- "Standard deviation of x_var or ind_y_var is 0"
      } else {
        ind_y_cor[k] <- cor.test(contemp_aux_wide[, int_num_x_vars[i]],
                                    as.numeric(contemp_aux_wide[, ind_y_vars[j]]),
                                    method = "pearson")$estimate
      }
    }
    k <- k + 1
  }
}

result <- data.frame(x_var, x_var_i, ind_y_var, ind_y_var_j, ind_y_cor, ind_y_note)

result_trim <- 
  result[!(result$ind_y_var %in% unique(result$ind_y_var[grepl("_btw", result$ind_y_var)])) &
           !(result$ind_y_var %in% unique(result$ind_y_var[grepl("_wth", result$ind_y_var)])), ]

# Restrict to correlations of at least |.20| with missing data indicators

int_num_miss <- result_trim[!is.na(result_trim$ind_y_cor) & 
                              result_trim$ind_y_cor >= abs(.2), ]

# Compute Pearson product-moment correlation coefficient with analysis variables

int_num_miss_anal <- int_num_miss
int_num_miss_anal$x_var_i <- NULL
int_num_miss_anal$ind_y_var_j <- NULL

int_num_miss_anal$y_var <- sub("_ind", "", fixed = TRUE, int_num_miss_anal$ind_y_var)
int_num_miss_anal$y_cor <- vector("double", nrow(int_num_miss_anal))
int_num_miss_anal$y_note <- rep(NA, nrow(int_num_miss_anal))

for (i in 1:nrow(int_num_miss_anal)) {
  int_num_miss_anal$y_cor[i] <- cor.test(contemp_aux_wide[, int_num_miss_anal$x_var[i]],
                                         contemp_aux_wide[, int_num_miss_anal$y_var[i]],
                                         method = "pearson")$estimate
}

# Restrict to correlations of at least |.40| with analysis variables

int_num_miss_anal_thres <- int_num_miss_anal[int_num_miss_anal$y_cor >= abs(.4), ]

# prDoSS.0 is neither an analysis variable nor a subscale of an analysis variable
# already in the imputation model. Therefore, it could be included as an auxiliary
# variable. However, this was not realized until after analyses were complete.

# ---------------------------------------------------------------------------- #
# Identify ordinal auxiliary variables ----
# ---------------------------------------------------------------------------- #

# Compute rank-biserial correlation coefficient (wilcoxonRG function of 
# rcompanion package) between ordinal variables and binary missing data indicators

x_var <- rep(ordered_x_vars, each = length(ind_y_vars))
x_var_i <- rep(1:length(ordered_x_vars), each = length(ind_y_vars))
ind_y_var <- rep(ind_y_vars, length(ordered_x_vars))
ind_y_var_j <- rep(1:length(ind_y_vars), length(ordered_x_vars))
ind_y_rg <- vector("double", length(ordered_x_vars)*length(ind_y_vars))
ind_y_note <- rep(NA, length(ordered_x_vars)*length(ind_y_vars))

k <- 1

for (i in 1:length(ordered_x_vars)) {
  for (j in 1:length(ind_y_vars)) {
    if (sum(contemp_aux_wide[, ind_y_vars[j]]) > 0 &
        all(which(contemp_aux_wide[, ind_y_vars[j]] == 1) %in% 
            which(is.na(contemp_aux_wide[, ordered_x_vars[i]])))) {
      ind_y_rg[k] <- NA
      ind_y_note[k] <- "Rows with missing ind_y_var are also missing x_var"
    } else if (sd(contemp_aux_wide[, ind_y_vars[j]]) == 0) {
      ind_y_rg[k] <- NA
      ind_y_note[k] <- "Standard deviation of ind_y_var is 0"
    } else {
      df_complete_x_var <- 
        contemp_aux_wide[!is.na(contemp_aux_wide[, ordered_x_vars[i]]), ]
      ind_y_rg[k] <- wilcoxonRG(df_complete_x_var[, ordered_x_vars[i]],
                                as.numeric(df_complete_x_var[, ind_y_vars[j]]),
                                verbose = TRUE)
    }
    k <- k + 1
  }
}

result <- data.frame(x_var, x_var_i, ind_y_var, ind_y_var_j, ind_y_rg, ind_y_note)

result_trim <- 
  result[!(result$ind_y_var %in% unique(result$ind_y_var[grepl("_btw", result$ind_y_var)])) &
           !(result$ind_y_var %in% unique(result$ind_y_var[grepl("_wth", result$ind_y_var)])), ]

# Restrict to correlations of at least |.20| with missing data indicators

ordered_miss <- result_trim[!is.na(result_trim$ind_y_rg) & 
                              result_trim$ind_y_rg >= abs(.2), ]

# Compute Kendall's coefficient of rank correlation with analysis variables

ordered_miss_anal <- ordered_miss
ordered_miss_anal$x_var_i <- NULL
ordered_miss_anal$ind_y_var_j <- NULL

ordered_miss_anal$y_var <- sub("_ind", "", fixed = TRUE, ordered_miss_anal$ind_y_var)
ordered_miss_anal$y_tau <- vector("double", nrow(ordered_miss_anal))
ordered_miss_anal$y_note <- rep(NA, nrow(ordered_miss_anal))

for (i in 1:nrow(ordered_miss_anal)) {
  ordered_miss_anal$y_tau[i] <- 
    cor.test(as.numeric(contemp_aux_wide[, ordered_miss_anal$x_var[i]]),
             contemp_aux_wide[, ordered_miss_anal$y_var[i]],
             method = "kendall",
             exact = FALSE)$estimate
}

# Restrict to correlations of at least |.40| with analysis variables

ordered_miss_anal_thres <- ordered_miss_anal[ordered_miss_anal$y_tau >= abs(.4), ]

# No auxiliary variables identified

# ---------------------------------------------------------------------------- #
# Identify categorical auxiliary variables ----
# ---------------------------------------------------------------------------- #

# Compute Cramer's V (CramerV function of DescTools package) for correlations 
# between categorical variables and missing data indicators

x_var <- rep(factor_x_vars, each = length(ind_y_vars))
x_var_i <- rep(1:length(factor_x_vars), each = length(ind_y_vars))
ind_y_var <- rep(ind_y_vars, length(factor_x_vars))
ind_y_var_j <- rep(1:length(ind_y_vars), length(factor_x_vars))
ind_y_v <- vector("double", length(factor_x_vars)*length(ind_y_vars))
ind_y_note <- rep(NA, length(factor_x_vars)*length(ind_y_vars))

k <- 1

for (i in 1:length(factor_x_vars)) {
  for (j in 1:length(ind_y_vars)) {
    if (sum(contemp_aux_wide[, ind_y_vars[j]]) > 0 &
        all(which(contemp_aux_wide[, ind_y_vars[j]] == 1) %in% 
            which(is.na(contemp_aux_wide[, factor_x_vars[i]])))) {
      ind_y_v[k] <- NA
      ind_y_note[k] <- "Rows with missing ind_y_var are also missing x_var"
    } else if (sd(contemp_aux_wide[, ind_y_vars[j]]) == 0) {
      ind_y_v[k] <- NA
      ind_y_note[k] <- "Standard deviation of ind_y_var is 0"
    } else {
      ind_y_v[k] <- CramerV(as.numeric(contemp_aux_wide[, factor_x_vars[i]]),
                            as.numeric(contemp_aux_wide[, ind_y_vars[j]]))
    }
    k <- k + 1
  }
}

result <- data.frame(x_var, x_var_i, ind_y_var, ind_y_var_j, ind_y_v, ind_y_note)

result_trim <- 
  result[!(result$ind_y_var %in% unique(result$ind_y_var[grepl("_btw", result$ind_y_var)])) &
           !(result$ind_y_var %in% unique(result$ind_y_var[grepl("_wth", result$ind_y_var)])), ]

# Restrict to correlations of at least |.20| with missing data indicators

factor_miss <- result_trim[!is.na(result_trim$ind_y_v) & 
                              result_trim$ind_y_v >= abs(.2), ]

# Compute R-squared from one-way-ANOVA with analysis variables

factor_miss_anal <- factor_miss
factor_miss_anal$x_var_i <- NULL
factor_miss_anal$ind_y_var_j <- NULL

factor_miss_anal$y_var <- sub("_ind", "", fixed = TRUE, factor_miss_anal$ind_y_var)
factor_miss_anal$y_r2 <- vector("double", nrow(factor_miss_anal))
factor_miss_anal$y_note <- rep(NA, nrow(factor_miss_anal))

for (i in 1:nrow(factor_miss_anal)) {
  factor_miss_anal$y_r2[i] <- 
    summary(lm(contemp_aux_wide[, factor_miss_anal$y_var[i]] ~ 
                 contemp_aux_wide[, factor_miss_anal$x_var[i]]))$r.squared
}

# Restrict to R-squared of at least .16 (r >= .4) with analysis variables

factor_miss_anal_thres <- factor_miss_anal[factor_miss_anal$y_r2 >= .16, ]

# Use DDS14_factor and DDS17a2_factor as auxiliary variables

# ---------------------------------------------------------------------------- #
# Save results ----
# ---------------------------------------------------------------------------- #

dir.create("./results/aux_vars")

save(int_num_miss_anal_thres, file = "./results/aux_vars/int_num_miss_anal_thres.RData")
save(ordered_miss_anal_thres, file = "./results/aux_vars/ordered_miss_anal_thres.RData")
save(factor_miss_anal_thres, file = "./results/aux_vars/factor_miss_anal_thres.RData")

# ---------------------------------------------------------------------------- #
# Collapse sparse factor levels of auxiliary variables ----
# ---------------------------------------------------------------------------- #

# Restrict columns

data5$contemp_aux <- 
  data5$contemp_aux[, c("ResearchID", "time0", "Condition", "AIN", "Period", 
                        "meanDSS", "meanDSS_ind", "meanDSS_btw", "meanDSS_wth", 
                        "drtotl_m_imp", "drtotl_m_imp_ind", "drtotl_m_imp_btw", "drtotl_m_imp_wth",
                        "cnDoSS", "cnDoSS_ind", "cnDoSS_btw", "cnDoSS_wth", 
                        "KMTOT", "KMTOT_ind", "KMTOT_btw", "KMTOT_wth", 
                        "DDS14_factor", "DDS17a2_factor", "prDoSS")]

# Collapse sparse levels of DDS17a2_factor, as imputation model does not
# converge after burn-in period with 100,000 iterations

data5$contemp_aux$DDS17a2_factor_collapsed <- data5$contemp_aux$DDS17a2_factor
data5$contemp_aux$DDS17a2_factor_collapsed2 <- data5$contemp_aux$DDS17a2_factor

other_employed <- c("Sales, e.g., insurance, real estate, auto",
                    "Clerical, e.g., secretary, retail clerk, typist",
                    "Skilled worker, craftsperson, foreman (non-farm)",
                    "Transport or equipment operator",
                    "Unskilled worker, laborer (non-farm)",
                    "Farm workers, e.g., farmer, farm laborer, farm manager or...",
                    "Service worker, e.g., custodian, waitress, guard, barber",
                    "Private household worker")

levels(data5$contemp_aux$DDS17a2_factor_collapsed)[levels(data5$contemp_aux$DDS17a2_factor_collapsed) %in% 
                                           other_employed] <- "Other Employed"

paid <- c("Professional, technical, e.g., clergy, engineer, teacher,...",
          "Owner, manager, administrator or executive of business (n...",
          other_employed)

unpaid <- c("Full-time homemaker",
              "Full-time student")

nonworking <- c("Unemployed",
                "Retired")

levels(data5$contemp_aux$DDS17a2_factor_collapsed2)[levels(data5$contemp_aux$DDS17a2_factor_collapsed2) %in%
                                                      paid] <- "Paid"
levels(data5$contemp_aux$DDS17a2_factor_collapsed2)[levels(data5$contemp_aux$DDS17a2_factor_collapsed2) %in%
                                                      unpaid] <- "Unpaid"
levels(data5$contemp_aux$DDS17a2_factor_collapsed2)[levels(data5$contemp_aux$DDS17a2_factor_collapsed2) %in%
                                                      nonworking] <- "Nonworking"

# Create contingency table for the factors. Sparse cells can contribute to slow
# convergence in the imputation model, but once the factors were added to the
# FIXED line in Blimp (because they are complete), the model converged without
# needing to resolve the sparse cells.

table(data5$contemp_aux$DDS14_factor,
      data5$contemp_aux$DDS17a2_factor_collapsed2,
      dnn = c("DDS14_factor", "DDS17a2_factor_collapsed2"))

# ---------------------------------------------------------------------------- #
# Report missingness per level of marital status and occupation ----
# ---------------------------------------------------------------------------- #

# Define function to compute proportion of missing assessments across time points
# based on a given measure's binary missing data indicator

compute_prop_miss_assess <- function(df, scale_ind) {
  tmp_ag <- aggregate(formula(paste0(scale_ind, " ~ ResearchID")), df, FUN = sum)
  
  scale_ind_sum_name <- paste0(scale_ind, "_sum")
  scale_ind_prop_name <- paste0(scale_ind, "_prop")
  
  names(tmp_ag)[names(tmp_ag) == scale_ind] <- scale_ind_sum_name

  tmp_ag[, scale_ind_prop_name] <- tmp_ag[, scale_ind_sum_name] / 4
  
  df <- merge(df, tmp_ag, by = "ResearchID", all.x = TRUE)
}

# Run function for each analysis variable

data5$contemp_aux <- compute_prop_miss_assess(data5$contemp_aux, "meanDSS_ind")
data5$contemp_aux <- compute_prop_miss_assess(data5$contemp_aux, "drtotl_m_imp_ind")
data5$contemp_aux <- compute_prop_miss_assess(data5$contemp_aux, "cnDoSS_ind")
data5$contemp_aux <- compute_prop_miss_assess(data5$contemp_aux, "KMTOT_ind")

# Restrict data to selected time-invariant variables

contemp_aux_iv <- data5$contemp_aux[, c("ResearchID", "Condition", "AIN",
                                        "DDS14_factor", "DDS17a2_factor",
                                        "DDS17a2_factor_collapsed", 
                                        "DDS17a2_factor_collapsed2",
                                        "meanDSS_ind_sum", "meanDSS_ind_prop",
                                        "drtotl_m_imp_ind_sum", "drtotl_m_imp_ind_prop",
                                        "cnDoSS_ind_sum", "cnDoSS_ind_prop",
                                        "KMTOT_ind_sum", "KMTOT_ind_prop")]

contemp_aux_iv <- unique(contemp_aux_iv)

# Define function to compute mean proportion of missing assessments for a given
# measure across time points within each level of auxiliary variables

compute_desc_by_level_for_scale <- function(df, scale_ind) {
  scale_ind_prop_name <- paste0(scale_ind, "_prop")
  
  aux_vars <- c("DDS14_factor", 
                "DDS17a2_factor", "DDS17a2_factor_collapsed", 
                "DDS17a2_factor_collapsed2")
  aux_var_labels <- c("Marital Status", 
                      "Occupation", "Occupation (Collapsed)", 
                      "Occupation (Collapsed into Employment Status)")
  
  res <- data.frame()
  
  for (i in 1:length(aux_vars)) {
    # Compute count, mean, and standard deviation
    
    tbl <-     table(df[, aux_vars[i]])
    ag_mean <- aggregate(formula(paste0(scale_ind_prop_name, " ~ ", aux_vars[i])), 
                         df, FUN = mean, drop = FALSE)
    ag_sd <-   aggregate(formula(paste0(scale_ind_prop_name, " ~ ", aux_vars[i])), 
                         df, FUN = sd, drop = FALSE)
    
    var_res <- rbind(data.frame(label = aux_var_labels[i],
                                n     = NA,
                                M_SD  = NA),
                     data.frame(label = names(tbl),
                                n     = as.numeric(tbl),
                                M_SD  = paste0(round(ag_mean[, scale_ind_prop_name], 2), " (",
                                               round(ag_sd[, scale_ind_prop_name], 2), ")")))
    
    res <- rbind(res, var_res)
  }
  
  return(res)
}

# Run function for each analysis variable, combine results into table, and export table

res_meanDSS      <- compute_desc_by_level_for_scale(contemp_aux_iv, "meanDSS_ind")
res_drtotl_m_imp <- compute_desc_by_level_for_scale(contemp_aux_iv, "drtotl_m_imp_ind")
res_cnDoSS       <- compute_desc_by_level_for_scale(contemp_aux_iv, "cnDoSS_ind")
res_KMTOT        <- compute_desc_by_level_for_scale(contemp_aux_iv, "KMTOT_ind")

all_res <- cbind(res_drtotl_m_imp, 
                 res_meanDSS[, names(res_meanDSS) != "label"], 
                 res_cnDoSS[, names(res_cnDoSS) != "label"],
                 res_KMTOT[, names(res_KMTOT) != "label"])

write.csv(all_res,
          file = "./results/table_s5.csv",
          row.names = FALSE)

# ---------------------------------------------------------------------------- #
# Prepare and export data for Blimp ----
# ---------------------------------------------------------------------------- #

# Note: "prDoSS" is not completely observed so cannot go on FIXED line in Blimp

sum(is.na(data5$contemp_aux$prDoSS))

# Define "cond0rev" so DBT-ST is coded 1 and ASG is coded 0. Note: This coding was
# used in simple mediation model of Neacsiu et al. (2018; https://doi.org/gdkhfn) 
# because PROCESS macro is based on regression. By contrast, Neacsiu et al. (2014; 
# http://doi.org/f6cntg) used "cond0" (where DBT-ST = 0 and ASG = 1) in conflated
# 2-1-1 contemporaneous multilevel mediation model because "cond0" was treated as
# categorical (entered after "BY" argument of "MIXED" command) and SPSS treats the 
# level with the highest value (ASG) as the reference group.

data5$contemp_aux$cond0rev[data5$contemp_aux$Condition == 23] <- 1
data5$contemp_aux$cond0rev[data5$contemp_aux$Condition == 24] <- 0

# Recode factors as numeric, as Blimp only reads numeric values

data5$contemp_aux$DDS14_factor <- as.numeric(data5$contemp_aux$DDS14_factor)
data5$contemp_aux$DDS17a2_factor <- as.numeric(data5$contemp_aux$DDS17a2_factor)
data5$contemp_aux$DDS17a2_factor_collapsed <- 
  as.numeric(data5$contemp_aux$DDS17a2_factor_collapsed)
data5$contemp_aux$DDS17a2_factor_collapsed2 <- 
  as.numeric(data5$contemp_aux$DDS17a2_factor_collapsed2)

# Remove columns for between-person and within-person effects. Although we
# initially included these in the imputation model instead of the raw scores,
# the raw scores should be entered into the imputation model and the between-
# person and within-person effects should be computed after imputation, per
# consultation with Craig Enders (6/8/2021).

exclude_cols <- c("meanDSS_btw", "meanDSS_wth", 
                  "drtotl_m_imp_btw", "drtotl_m_imp_wth",
                  "cnDoSS_btw", "cnDoSS_wth", 
                  "KMTOT_btw", "KMTOT_wth")

data5$contemp_aux[, exclude_cols] <- NULL

# Compute POMP scores for mediation dataset only (given that Bayesian results will
# be interpreted for mediation model instead of reanalyzing imputed data). POMP scores 
# for models of within-person relations are computed after multiple imputation.

data5$contemp_aux_med <- data5$contemp_aux

data5$contemp_aux_med <- 
  compute_pomp(data5$contemp_aux_med, "drtotl_m_imp",
               scale_defs$drtotl_n_items, scale_defs$drtotl_min, scale_defs$drtotl_max)
data5$contemp_aux_med <- 
  compute_pomp(data5$contemp_aux_med, "meanDSS",
               scale_defs$meanDSS_n_items, scale_defs$meanDSS_min, scale_defs$meanDSS_max)
data5$contemp_aux_med <- 
  compute_pomp(data5$contemp_aux_med, "cnDoSS",
               scale_defs$cnDoSS_n_items, scale_defs$cnDoSS_min, scale_defs$cnDoSS_max)
data5$contemp_aux_med <- 
  compute_pomp(data5$contemp_aux_med, "KMTOT",
               scale_defs$KMTOT_n_items, scale_defs$KMTOT_min, scale_defs$KMTOT_max)

# Code NA as 999, which will be specified as the missing code for Blimp

data5$contemp_aux[is.na(data5$contemp_aux)] <- 999
data5$contemp_aux_med[is.na(data5$contemp_aux_med)] <- 999

# Export intermediate data for Blimp, which does not allow column names

save(data5, file = "./data/intermediate/data5.RData")

write.table(data5$contemp_aux, 
            "./data/intermediate/contemp_aux.csv",
            row.names = FALSE,
            col.names = FALSE,
            sep = ",")

write.table(data5$contemp_aux_med, 
            "./data/intermediate/contemp_aux_med.csv",
            row.names = FALSE,
            col.names = FALSE,
            sep = ",")

# ---------------------------------------------------------------------------- #
# Create directories for Blimp ----
# ---------------------------------------------------------------------------- #

# Create directories for Blimp sensitivity analyses

sen_anyls <- c("maximal", "reduced_cnDoSS", "reduced_KMTOT", "reduced_meanDSS",
               "maximal_w_prDoSS")

for (i in 1:length(sen_anyls)) {
  dir.create(paste0(wd_dir, "./data/imputed/", sen_anyls[i], "/contemp/diagnostic"), recursive = TRUE)
  dir.create(paste0(wd_dir, "./data/imputed/", sen_anyls[i], "/contemp/actual"), recursive = TRUE)
  dir.create(paste0(wd_dir, "./data/imputed/", sen_anyls[i], "/lagged/diagnostic"), recursive = TRUE)
  dir.create(paste0(wd_dir, "./data/imputed/", sen_anyls[i], "/lagged/actual"), recursive = TRUE)
}

# Create directories for Blimp mediation analysis

sen_anyls_med <- c("maximal/lagged/avg_item", "maximal/lagged/pomp", 
                   "maximal_w_prDoSS/lagged/avg_item")

for (i in 1:length(sen_anyls_med)) {
  dir.create(paste0(wd_dir, "./data/imputed/mediation/", sen_anyls_med[i], "/diagnostic"),
             recursive = TRUE)
  dir.create(paste0(wd_dir, "./data/imputed/mediation/", sen_anyls_med[i], "/actual"), 
             recursive = TRUE)
}
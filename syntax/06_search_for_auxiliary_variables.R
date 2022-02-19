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
# Prepare and export data and create directories for Blimp ----
# ---------------------------------------------------------------------------- #

# Restrict columns

data5$contemp_aux <- 
  data4$contemp_aux[, c("ResearchID", "time0", "Condition", "AIN", "Period", 
                        "meanDSS", "meanDSS_btw", "meanDSS_wth", 
                        "drtotl_m_imp", "drtotl_m_imp_btw", "drtotl_m_imp_wth",
                        "cnDoSS", "cnDoSS_btw", "cnDoSS_wth", 
                        "KMTOT", "KMTOT_btw", "KMTOT_wth", 
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

# Define "cond0rev" so DBT-ST is coded 1 and ASG is coded 0. Note: This coding was
# used in Neacsiu et al. (2018; https://doi.org/gdkhfn), whereas in Neacsiu et al.
# (2014; http://doi.org/f6cntg) "cond0" was used (where DBT-ST = 0 and ASG = 1).

data5$contemp_aux$cond0rev[data5$contemp_aux$Condition == 23] <- 1
data5$contemp_aux$cond0rev[data5$contemp_aux$Condition == 24] <- 0

# Recode factors as numeric, as Blimp only reads numeric values

data5$contemp_aux$DDS14_factor <- as.numeric(data5$contemp_aux$DDS14_factor)
data5$contemp_aux$DDS17a2_factor <- as.numeric(data5$contemp_aux$DDS17a2_factor)
data5$contemp_aux$DDS17a2_factor_collapsed <- 
  as.numeric(data5$contemp_aux$DDS17a2_factor_collapsed)
data5$contemp_aux$DDS17a2_factor_collapsed2 <- 
  as.numeric(data5$contemp_aux$DDS17a2_factor_collapsed2)

# Create contingency table for the factors. Sparse cells can contribute to slow
# convergence in the imputation model, but once the factors were added to the
# FIXED line in Blimp (because they are complete), the model converged without
# needing to resolve the sparse cells.

table(data5$contemp_aux$DDS14_factor,
      data5$contemp_aux$DDS17a2_factor_collapsed2,
      dnn = c("DDS14_factor", "DDS17a2_factor_collapsed2"))

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

# Code NA as 999, which will be specified as the missing code for Blimp

data5$contemp_aux[is.na(data5$contemp_aux)] <- 999

# Export intermediate data for Blimp, which does not allow column names

save(data5, file = "./data/intermediate/data5.RData")

write.table(data5$contemp_aux, 
            "./data/intermediate/contemp_aux.csv",
            row.names = FALSE,
            col.names = FALSE,
            sep = ",")

# Create directories for Blimp sensitivity analyses

sen_anyls <- c("maximal", "reduced_cnDoSS", "reduced_KMTOT", "reduced_meanDSS")

for (i in 1:length(sen_anyls)) {
  dir.create(paste0(wd_dir, "./data/imputed/", sen_anyls[i], "/contemp/diagnostic"), recursive = TRUE)
  dir.create(paste0(wd_dir, "./data/imputed/", sen_anyls[i], "/contemp/actual"), recursive = TRUE)
  dir.create(paste0(wd_dir, "./data/imputed/", sen_anyls[i], "/lagged/diagnostic"), recursive = TRUE)
  dir.create(paste0(wd_dir, "./data/imputed/", sen_anyls[i], "/lagged/actual"), recursive = TRUE)
}

# Create directories for Blimp mediation analysis

dir.create(paste0(wd_dir, "./data/imputed/mediation/maximal/lagged/diagnostic"), recursive = TRUE)
dir.create(paste0(wd_dir, "./data/imputed/mediation/maximal/lagged/actual"), recursive = TRUE)
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

source("./GitHub Repo/yn3z6/syntax/0_define_functions.R")

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
# Identify correlates with missing data indicators ----
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

# TODO: Use point-biserial correlation coefficient for continuous variables
# (cor.test function of stats package), rank-biserial correlation coefficient
# for ordinal variables (wilcoxonRG function of rcompanion package), and
# Cramer's V for categorical variables (CramerV function of DescTools package)





# ---------------------------------------------------------------------------- #
# Identify correlates with analysis variables ----
# ---------------------------------------------------------------------------- #

# TODO





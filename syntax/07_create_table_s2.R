# ---------------------------------------------------------------------------- #
# Create Table S2
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

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import results ----
# ---------------------------------------------------------------------------- #

load("./results/aux_vars/int_num_miss_anal_thres.RData")
load("./results/aux_vars/ordered_miss_anal_thres.RData")
load("./results/aux_vars/factor_miss_anal_thres.RData")

# ---------------------------------------------------------------------------- #
# Format table ----
# ---------------------------------------------------------------------------- #

# Note: No potential ordinal auxiliary variables were found

ordered_miss_anal_thres

# Remove unneeded columns

int_num_miss_anal_thres$ind_y_note <- NULL
int_num_miss_anal_thres$ind_y_var <- NULL
int_num_miss_anal_thres$y_note <- NULL

factor_miss_anal_thres$ind_y_note <- NULL
factor_miss_anal_thres$ind_y_var <- NULL
factor_miss_anal_thres$y_note <- NULL

# Rearrange columns

int_num_miss_anal_thres <- int_num_miss_anal_thres[, c("x_var",
                                                       "y_var",
                                                       "ind_y_cor",
                                                       "y_cor")]

factor_miss_anal_thres <- factor_miss_anal_thres[, c("x_var",
                                                     "y_var",
                                                     "ind_y_v",
                                                     "y_r2")]

# Reorder rows

row.names(int_num_miss_anal_thres) <- 1:nrow(int_num_miss_anal_thres)
row.names(factor_miss_anal_thres) <- 1:nrow(factor_miss_anal_thres)

int_num_miss_anal_thres <- int_num_miss_anal_thres[c(8, 3, 1,
                                                     9, 10, 6, 7, 4, 5, 11,
                                                     2), ]
factor_miss_anal_thres <- factor_miss_anal_thres[c(1:2,
                                                   4, 8, 11,
                                                   3, 7, 10,
                                                   5, 12,
                                                   6, 9, 13), ]

# Round to two decimal places

int_num_miss_anal_thres[, c("ind_y_cor", "y_cor")] <- 
  round(int_num_miss_anal_thres[, c("ind_y_cor", "y_cor")], 2)
factor_miss_anal_thres[, c("ind_y_v", "y_r2")] <- 
  round(factor_miss_anal_thres[, c("ind_y_v", "y_r2")], 2)

# Merge objects. First create NA columns to ensure the objects have the same
# column names. Then combine the rows.

int_num_miss_anal_thres$ind_y_v <- NA
int_num_miss_anal_thres$y_r2 <- NA
factor_miss_anal_thres$ind_y_cor <- NA
factor_miss_anal_thres$y_cor <- NA

table_s2 <- rbind(factor_miss_anal_thres, int_num_miss_anal_thres)

# Rearrange columns

table_s2 <- table_s2[, c("x_var", "y_var", "ind_y_cor", "ind_y_v", "y_cor", "y_r2")]

# Write to CSV

write.csv(table_s2, "./results/table_s2.csv", row.names = FALSE)
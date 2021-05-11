# ---------------------------------------------------------------------------- #
# Restructure Data
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

# TODO





# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/intermediate/data3.Rdata")

# ---------------------------------------------------------------------------- #
# Create tables for analysis ----
# ---------------------------------------------------------------------------- #

# TODO: Add potential auxiliary variables below





# Identify key analysis variables

meanDSS_vars <- c("meanDSS", "meanDSS_btw", "meanDSS_wth",
                  "meanDSS_pomp", "meanDSS_pomp_btw", "meanDSS_pomp_wth")
drtotl_m_imp_vars <- c("drtotl_m_imp", "drtotl_m_imp_btw", "drtotl_m_imp_wth",
                       "drtotl_m_imp_pomp", "drtotl_m_imp_pomp_btw", 
                       "drtotl_m_imp_pomp_wth")
cnDoSS_vars <- c("cnDoSS", "cnDoSS_btw", "cnDoSS_wth",
                 "cnDoSS_pomp", "cnDoSS_pomp_btw", "cnDoSS_pomp_wth")
KMTOT_vars <- c("KMTOT", "KMTOT_btw", "KMTOT_wth",
                "KMTOT_pomp", "KMTOT_pomp_btw", "KMTOT_pomp_wth")

# Create table for contemporaneous model

contemp <- data3$dbt_wccl[, c("ResearchID", "Condition", "AIN", "Period", 
                              "time0", meanDSS_vars)]
contemp <- merge(contemp, 
                 data3$ders[, c("ResearchID", "time0", drtotl_m_imp_vars)],
                 by = c("ResearchID", "time0"),
                 all.x = TRUE)
contemp <- merge(contemp, 
                 data3$doss[, c("ResearchID", "time0", cnDoSS_vars)],
                 by = c("ResearchID", "time0"),
                 all.x = TRUE)
contemp <- merge(contemp,
                 data3$kims[, c("ResearchID", "time0", KMTOT_vars)],
                 by = c("ResearchID", "time0"),
                 all.x = TRUE)

# Create table for lagged model

lagged <- data3$dbt_wccl[, c("ResearchID", "Condition", "AIN", "Period",
                             "time0_lag", meanDSS_vars)]
lagged <- merge(lagged,
                data3$ders[, c("ResearchID", "time0_lag", drtotl_m_imp_vars)],
                by = c("ResearchID", "time0_lag"),
                all.x = TRUE)
lagged <- merge(lagged,
                data3$doss[, c("ResearchID", "time0_lag", cnDoSS_vars)],
                by = c("ResearchID", "time0_lag"),
                all.x = TRUE)
lagged <- merge(lagged,
                data3$kims[, c("ResearchID", "time0_lag", KMTOT_vars)],
                by = c("ResearchID", "time0_lag"),
                all.x = TRUE)
lagged <- lagged[!is.na(lagged$time0_lag), ]

# ---------------------------------------------------------------------------- #
# Save intermediate data ----
# ---------------------------------------------------------------------------- #

# Save data as R object

# TODO





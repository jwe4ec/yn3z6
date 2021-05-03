# ---------------------------------------------------------------------------- #
# Run Analyses
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

load("./data/intermediate/data2.Rdata")

# ---------------------------------------------------------------------------- #
# Prepare data for analysis ----
# ---------------------------------------------------------------------------- #

# Create table for contemporaneous model

contemp <- data2$dbt_wccl[, c("ResearchID", "Condition", "AIN", "Period",
                              "time0", "meanDSS")]
contemp <- merge(contemp,
                 data2$ders[, c("ResearchID", "time0", "drtotl")],
                 by = c("ResearchID", "time0"),
                 all.x = TRUE)
contemp <- merge(contemp,
                 data2$doss[, c("ResearchID", "time0", "cnDoSS")],
                 by = c("ResearchID", "time0"),
                 all.x = TRUE)
contemp <- merge(contemp,
                 data2$kims[, c("ResearchID", "time0", "KMTOT")],
                 by = c("ResearchID", "time0"),
                 all.x = TRUE)

# Create table for lagged model

# TODO





# ---------------------------------------------------------------------------- #
# Run analyses ----
# ---------------------------------------------------------------------------- #

# TODO






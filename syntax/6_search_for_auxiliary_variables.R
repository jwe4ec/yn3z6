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

# TODO: Change data to wide format





# TODO: Use point-biserial correlation coefficient for continuous variables
# (cor.test function of stats package), rank-biserial correlation coefficient
# for ordinal variables (wilcoxonRG function of rcompanion package), and
# Cramer's V for categorical variables (CramerV function of DescTools package)





# ---------------------------------------------------------------------------- #
# Identify correlates with analysis variables ----
# ---------------------------------------------------------------------------- #

# TODO





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
# Recode potential demographic auxiliary variables ----
# ---------------------------------------------------------------------------- #

# TODO: Recode nominal vars as characters (note: "race" is already character)





class(data5$contemp_aux$race)

table(data5$contemp_aux$SH132, useNA = "always")
table(data5$contemp_aux$DDS06b, useNA = "always")
table(data5$contemp_aux$DDS10, useNA = "always")
table(data5$contemp_aux$DDS14, useNA = "always")
table(data5$contemp_aux$DDS17a2, useNA = "always")
table(data5$contemp_aux$DDS25, useNA = "always")
table(data5$contemp_aux$DDS26, useNA = "always")

# TODO: Recode ordered vars as ordered factors





# Education

table(data5$contemp_aux$DDS15a, useNA = "always")

# Gross annual income

table(data5$contemp_aux$DDS16a, useNA = "always")

# TODO: Keep age as integer

# Age (keep as integer)

table(data5$contemp_aux$DDS04, useNA = "always")

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





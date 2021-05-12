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

groundhog.library(car, groundhog_day)
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

# TODO: Tasks below and consider doing earlier in pipeline





# Sexual orientation (change to factor)

table(data5$contemp_aux$SH132, useNA = "always")

# Race (recode then change to factor)

table(data5$contemp_aux$DDS06, useNA = "always")
table(data5$contemp_aux$DDS06c, useNA = "always")

# test <- recode(data5$contemp_aux$DDS06,
#                "1 = 'White/Caucasian';
#                 2 = 'Native American, American Indian or Alaska Native';
#                 3 = 'Black or African American';
#                 4 = 'Chinese or Chinese American';
#                 5 = 'Japanese or Japanese American';
#                 6 = 'Korean or Korean American';
#                 7 = 'Other Asian or Asian American (India, Malaysia, Pakistan, ...';
#                 11 = 'East Indian (counted as Asian)';
#                 12 = 'Middle Eastern/Arab';
#                 13 = 'Other';
#                 14 = 'Native Hawaiian or Pacific Islander'")
# 
# test2 <- recode(data5$contemp_aux$DDS06c,
#                 "1 = 'White/Caucasian';
#                 2 = 'Native American, American Indian or Alaska Native';
#                 3 = 'Black or African American';
#                 4 = 'Chinese or Chinese American';
#                 5 = 'Japanese or Japanese American';
#                 6 = 'Korean or Korean American';
#                 7 = 'Other Asian or Asian American (India, Malaysia, Pakistan, ...';
#                 11 = 'East Indian';
#                 12 = 'Middle Eastern/Arab';
#                 13 = 'Other';
#                 14 = 'Native Hawaiian or Pacific Islander';
#                 15 = 'More than one other racial group'")

# Ethnicity (change to factor)

table(data5$contemp_aux$DDS06b, useNA = "always")

# Adopted (change to factor)

table(data5$contemp_aux$DDS10, useNA = "always")

# Marital status (change to factor)

table(data5$contemp_aux$DDS14, useNA = "always")

# Occupation (change to factor)

table(data5$contemp_aux$DDS17a2, useNA = "always")

# Learning disability (change to factor)

table(data5$contemp_aux$DDS25, useNA = "always")

# Physical disability (change to factor)

table(data5$contemp_aux$DDS26, useNA = "always")

# ORDINAL

# Education (make ordered factor)

table(data5$contemp_aux$DDS15a, useNA = "always")

# Gross annual income (make ordered factor)

table(data5$contemp_aux$DDS16a, useNA = "always")

# CONTINUOUS

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





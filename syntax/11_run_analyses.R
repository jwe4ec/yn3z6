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

source("./GitHub Repo/yn3z6/syntax/00_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages with groundhog

groundhog.library(nlme, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

# TODO





# ---------------------------------------------------------------------------- #
# Run analyses for intent-to-treat sample ----
# ---------------------------------------------------------------------------- #

# Contemporaneous models using POMP scores

# TODO





# Contemporaneous models using average item scores

# TODO





# Lagged models using POMP scores

# TODO





# Lagged models using average item scores

# TODO





# ---------------------------------------------------------------------------- #
# Run analyses for completer sample ----
# ---------------------------------------------------------------------------- #

# Contemporaneous models using POMP scores

# TODO





# Contemporaneous models using average item scores

# TODO





# Lagged models using POMP scores

# TODO





# Lagged models using average item scores

# TODO





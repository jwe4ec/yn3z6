# ---------------------------------------------------------------------------- #
# Check MCAR
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
# Import further cleaned CSV data files ----
# ---------------------------------------------------------------------------- #

# Obtain file names of CSV data files

further_clean_data_dir <- paste0(wd_dir, "/data/clean_further")
filenames <- list.files(paste0(further_clean_data_dir),
                        pattern = "*.csv",
                        full.names = FALSE)

# Import data files and store them in list

data <- lapply(paste0(further_clean_data_dir, "/", filenames),
               read.csv)

# Name each data file in list

split_char <- ".csv"
names(data) <- unlist(lapply(filenames,
                             function(f) {
                               unlist(strsplit(f,
                                               split = split_char,
                                               fixed = FALSE))[1]
                             }))

# ---------------------------------------------------------------------------- #
# Check MCAR ----
# ---------------------------------------------------------------------------- #

# TODO





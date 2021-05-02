# ---------------------------------------------------------------------------- #
# Compute Missing Data Rates
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
# Store working directory, install correct R version, load packages ----
# ---------------------------------------------------------------------------- #

wd_dir <- getwd()

script_R_version <- "R version 4.0.3 (2020-10-10)"
current_R_version <- R.Version()$version.string

if(current_R_version != script_R_version) {
  warning(paste0("This script is based on ", script_R_version,
                 ". You are running ", current_R_version, "."))
}

library(groundhog)
groundhog_day <- "2021-04-01"

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
# Compute item-level missingness ----
# ---------------------------------------------------------------------------- #

# TODO





# ---------------------------------------------------------------------------- #
# Compute scale-level missingness ----
# ---------------------------------------------------------------------------- #

# TODO





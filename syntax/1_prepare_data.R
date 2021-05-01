# ---------------------------------------------------------------------------- #
# Prepare Data
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

# Store working directory

wd_dir <- getwd()

# Run the code below to ensure you are running the same version of R used at the
# time the script was written. If you need to install a previous version, go to
# https://cran.r-project.org/bin/windows/base/old/

script_R_version <- "R version 4.0.3 (2020-10-10)"
current_R_version <- R.Version()$version.string

if(current_R_version != script_R_version) {
  warning(paste0("This script is based on ", script_R_version,
                 ". You are running ", current_R_version, "."))
}

# Load packages using "groundhog", which installs and loads the most recent
# versions of packages available on the specified date ("groundhog_day"). This 
# is important for reproducibility so that everyone running the script is using
# the same versions of packages used at the time the script was written.

# Note that packages may take longer to load the first time you load them with
# "groundhog.library". This is because you may not have the correct versions of 
# the packages installed based on the "groundhog_day". After "groundhog.library"
# automatically installs the correct versions alongside other versions you may 
# have installed, it will load the packages more quickly.

# If in the process of loading packages with "groundhog.library" for the first 
# time the console states that you first need to install "Rtools", follow steps 
# here (https://cran.r-project.org/bin/windows/Rtools/) for installing "Rtools" 
# and putting "Rtools" on the PATH. Then try loading the packages again.

library(groundhog)
groundhog_day <- "2021-04-01"

groundhog.library(haven, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import clean SAV data files ----
# ---------------------------------------------------------------------------- #

# Obtain file names of SAV data files

clean_data_dir <- paste0(wd_dir, "/data/clean")
sav_filenames <- list.files(paste0(clean_data_dir, "/sav"), 
                        pattern = "*.sav", 
                        full.names = FALSE)

# Import data files and store them in list, keeping user-defined missing values

spss_data <- lapply(paste0(clean_data_dir, "/sav/", sav_filenames),
               read_sav, user_na = TRUE)

# Name each data file in list

split_char <- ".sav"
names(spss_data) <- unlist(lapply(sav_filenames,
                                  function(f) {
                                    unlist(strsplit(f,
                                                    split = split_char,
                                                    fixed = FALSE))[1]
                                  }))

# Report the names of the imported tables

cat("Imported tables: ")
names(spss_data)

# ---------------------------------------------------------------------------- #
# Convert SAV data files to CSV data files ----
# ---------------------------------------------------------------------------- #

# Write CSV data files retaining user-defined missing values

dir.create(paste0(clean_data_dir, "/csv/with_user-defined_missing_values"),
           recursive = TRUE)

csv_filenames <- paste0(names(spss_data), "_user-defined_missings.csv")

for (i in 1:length(spss_data)) {
  readr::write_csv(
    spss_data[[i]],
    file = paste0(clean_data_dir, 
                  "/csv/with_user-defined_missing_values/",
                  csv_filenames[i]))
}

# Write CSV data files with user-defined missing values converted to NA

data <- lapply(spss_data, zap_missing)

dir.create(paste0(clean_data_dir, "/csv/without_user-defined_missing_values"))

csv_filenames <- paste0(names(data), "_missings_as_NA.csv")

for (i in 1:length(data)) {
  readr::write_csv(data[[i]],
                   file = paste0(clean_data_dir,
                                 "/csv/without_user-defined_missing_values/",
                                 csv_filenames[i]))
}

# ---------------------------------------------------------------------------- #
# Confirm CSV data files do not contain other user-defined missing values ----
# ---------------------------------------------------------------------------- #

data <- lapply(data, zap_label)
data <- lapply(data, zap_labels)
data <- lapply(data, zap_formats)
data <- lapply(data, zap_widths)

# TODO










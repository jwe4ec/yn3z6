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

# The present script reads selected clean SPSS (SAV) data files obtained for the 
# Emotion Regulation Treatment Study. Given that SAV files are proprietary, this
# script converts them to two sets of CSV files: one set retaining user-defined 
# missing values from SPSS, and another set in which these missing values have
# been converted to NA (for analysis in R). These two sets of CSV files, written
# to the "clean" folder, simply represent CSV versions of the original SAV files.

# After converting the user-defined missing values to NA, this script conducts
# additional data cleaning to generate a third set of CSV files, written to the
# "clean_further" folder, that are analyzed in the present manuscript.

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

# ---------------------------------------------------------------------------- #
# Create code book for SAV data files ----
# ---------------------------------------------------------------------------- #

# TODO (consider this: https://libguides.library.kent.edu/spss/codebooks)





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
# Further prepare data ----
# ---------------------------------------------------------------------------- #

# TODO: Confirm data are deidentified





# Strip data of other attributes from SPSS

data <- lapply(data, zap_label)
data <- lapply(data, zap_labels)
data <- lapply(data, zap_formats)
data <- lapply(data, zap_widths)

# Convert empty character values to NA

for (i in 1:length(data)) {
  for (j in 1:length(data[[i]])) {
    if (is.character(data[[i]][[j]])) {
      data[[i]][[j]] <- zap_empty(data[[i]][[j]])
    }
  }
}

# Remove irrelevant time and filter variables

data$`dbt-wccl data file`$time1 <- NULL
data$`dbt-wccl data file`$time2 <- NULL
data$`dbt-wccl data file`$time3 <- NULL
data$`dbt-wccl data file`$time4 <- NULL
data$`dbt-wccl data file`$time5 <- NULL
data$`dbt-wccl data file`$`filter_$` <- NULL

# Confirm numeric columns do not contain other user-defined missing values. The
# "whymiss" columns can be ignored; they represent reasons for missingness. We
# will not check character or POSIXct columns at this time.

for (i in 1:length(data)) {
  for (j in 1:length(data[[i]])) {
    table_i_name <- names(data[i])
    column_j_name <- names(data[[i]][j])
    table_i_column_j_name <- paste0(table_i_name, "$", column_j_name)
    
    if (is.numeric(data[[i]][[j]])) {
      if (any(data[[i]][j] < 0, na.rm = TRUE)) {
        print(table_i_column_j_name)
      }
    }
  }
}

# Shorten table names

names(data) <- strsplit(names(data), split = " data file", fixed = FALSE)
names(data)[names(data) == "demographic"] <- "demog"
names(data)[names(data) == "dimensions of stress"] <- "dss"
names(data)[names(data) == "scid i brief"] <- "scid1"

# Write further cleaned CSV data files

further_clean_data_dir <- paste0(wd_dir, "/data/clean_further")
dir.create(further_clean_data_dir)

csv_filenames <- paste0(names(data), ".csv")

for (i in 1:length(data)) {
  readr::write_csv(data[[i]],
                   file = paste0(further_clean_data_dir, 
                                 "/",
                                 csv_filenames[i]))
}
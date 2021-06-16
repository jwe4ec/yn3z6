# ---------------------------------------------------------------------------- #
# Compute Analysis Variables
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

# No packages loaded

# ---------------------------------------------------------------------------- #
# Define functions used throughout script ----
# ---------------------------------------------------------------------------- #

# Define function to compute variables for between-person effects (i.e., person
# means) and within-person effects (i.e., person-mean centered scores) for
# average item scores. Note that here the between-person effects are not grand
# mean centered, whereas they are grand mean centered in the imputation model.

disaggregate <- function(df, avg_item_colname) {
  
  # Compute between-person effects
  
  avg_item_btw_colname <- paste0(avg_item_colname, "_btw")

  person_means <- aggregate(df[, avg_item_colname],
                            list(df$ResearchID),
                            mean, na.rm = TRUE)
  names(person_means)[1] <- "ResearchID"
  names(person_means)[2] <- avg_item_btw_colname

  output <- merge(df, person_means, by = "ResearchID", all.x = TRUE)
  output[is.na(output[, avg_item_colname]), avg_item_btw_colname] <- NA
  
  # Compute within-person effects
  
  avg_item_wth_colname <- paste0(avg_item_colname, "_wth")

  output[, avg_item_wth_colname] <- 
    output[, avg_item_colname] - output[, avg_item_btw_colname]

  return(output)
}

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/intermediate/data2.Rdata")

data3 <- data2

# ---------------------------------------------------------------------------- #
# Compute variables for between- and within-person effects ----
# ---------------------------------------------------------------------------- #

data3$ders <- disaggregate(data2$ders, "drtotl_m_imp")
data3$dbt_wccl <- disaggregate(data2$dbt_wccl, "meanDSS")
data3$doss <- disaggregate(data2$doss, "cnDoSS")
data3$kims <- disaggregate(data2$kims, "KMTOT")

# ---------------------------------------------------------------------------- #
# Save intermediate data ----
# ---------------------------------------------------------------------------- #

# Save data as R object

save(data3, file = "./data/intermediate/data3.Rdata")
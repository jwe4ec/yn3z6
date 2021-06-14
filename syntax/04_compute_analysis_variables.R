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
# means) and within-person effects (i.e., person-mean centered scores) for both
# average item scores and POMP scores

disaggregate <- function(df, avg_item_colname, pomp_colname) {
  
  # Compute between-person effects
  
  avg_item_btw_colname <- paste0(avg_item_colname, "_btw")
  pomp_btw_colname <- paste0(pomp_colname, "_btw")
  
  person_means <- aggregate(df[, c(avg_item_colname, pomp_colname)],
                            list(df$ResearchID),
                            mean, na.rm = TRUE)
  names(person_means)[1] <- "ResearchID"
  names(person_means)[2] <- avg_item_btw_colname
  names(person_means)[3] <- pomp_btw_colname
  
  output <- merge(df, person_means, by = "ResearchID", all.x = TRUE)
  output[is.na(output[, avg_item_colname]), 
         c(avg_item_btw_colname, pomp_btw_colname)] <- NA
  
  # Compute within-person effects
  
  avg_item_wth_colname <- paste0(avg_item_colname, "_wth")
  pomp_wth_colname <- paste0(pomp_colname, "_wth")
  
  output[, avg_item_wth_colname] <- 
    output[, avg_item_colname] - output[, avg_item_btw_colname]
  output[, pomp_wth_colname] <- 
    output[, pomp_colname] - output[, pomp_btw_colname]
  
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

data3$ders <- disaggregate(data2$ders, "drtotl_m_imp", "drtotl_m_imp_pomp")
data3$dbt_wccl <- disaggregate(data2$dbt_wccl, "meanDSS", "meanDSS_pomp")
data3$doss <- disaggregate(data2$doss, "cnDoSS", "cnDoSS_pomp")
data3$kims <- disaggregate(data2$kims, "KMTOT", "KMTOT_pomp")

# ---------------------------------------------------------------------------- #
# Save intermediate data ----
# ---------------------------------------------------------------------------- #

# Save data as R object

save(data3, file = "./data/intermediate/data3.Rdata")
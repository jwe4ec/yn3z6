# ---------------------------------------------------------------------------- #
# Write Results
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

# No packages loaded

# ---------------------------------------------------------------------------- #
# Define functions used throughout script ----
# ---------------------------------------------------------------------------- #

# Define function to write results to TXT file

write_results <- function(result, path, sample) {
  sink(file = path)
  
  print(paste("Sample:", deparse(substitute(sample))))
  cat("\n")
  
  print("-------------------------------------------------------------------")
  cat("\n")
  
  for (i in 1:length(result)) {
    if (result[[i]]$modelList[1] == "Model did not converge") {
      print(paste0("Model: ", names(result[i])))
      cat("\n")
      
      print(result[[i]]$modelList)
      cat("\n")
      
      print("-------------------------------------------------------------------")
      cat("\n")
    } else {
      print(paste0("Model: ", names(result[i])))
      cat("\n")
      
      print("modelList[[1]]$call")
      print(result[[i]]$modelList[[1]]$call)
      cat("\n")
      
      print(result[[i]]$pooled)
      cat("\n")
      
      print("Random Effects Correlations:")
      print(result[[i]]$re_cor)
      cat("\n")
      
      print("Fixed Effects 95% CI:")
      print(result[[i]]$ci)
      cat("\n")
      
      print("-------------------------------------------------------------------")
      cat("\n")
    }
  }
  sink()
}

# ---------------------------------------------------------------------------- #
# Import results ----
# ---------------------------------------------------------------------------- #

load("./results/result_itt_max.RData")
load("./results/result_itt_red_meanDSS.RData")
load("./results/result_itt_red_cnDoSS.RData")
load("./results/result_itt_red_KMTOT.RData")

# ---------------------------------------------------------------------------- #
# Write results ----
# ---------------------------------------------------------------------------- #

write_results(result_itt_max, "./results/itt_max.txt", ITT)
write_results(result_itt_red_meanDSS, "./results/itt_red_meanDSS.txt", ITT)
write_results(result_itt_red_cnDoSS, "./results/itt_red_cnDoSS.txt", ITT)
write_results(result_itt_red_KMTOT, "./results/itt_red_KMTOT.txt", ITT)
# ---------------------------------------------------------------------------- #
# Create Tables
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

# Define function to format results for table 1

format_table1 <- function(result_list) {
  for (i in 1:length(result_list)) {
    if (result_list[[i]]$modelList[1] == "Model did not converge") {
      result_list[[i]]$table1 <- list("estimate_ci" = NA,
                                      "var_resid" = NA,
                                      "re_cor_lower_tri" = NA)
    } else {
      estimate <- as.data.frame(result_list[[i]]$pooled$estimate)
      ci <- as.data.frame(result_list[[i]]$ci)
      re_var_cov <- result_list[[i]]$re_var_cov
      re_cor <- result_list[[i]]$re_cor
      extra.pars <- result_list[[i]]$pooled$extra.pars
      
      estimate <- estimate[, !names(estimate) %in% c("RIV", "FMI")]
      
      estimate[, !names(estimate) %in% "P(>|t|)"] <-
        round(estimate[, !names(estimate) %in% "P(>|t|)"], 2)
      estimate[, "P(>|t|)"] <- round(estimate[, "P(>|t|)"], 3)
      
      estimate$`B (SE)` <- paste0(estimate$Estimate, " (", estimate$Std.Error, ")")
      estimate[, c("Estimate", "Std.Error")] <- NULL
      
      names(estimate)[names(estimate) == "t.value"] <- "t"
      
      estimate$p <- as.character(estimate[, "P(>|t|)"])
      for (j in 1:nrow(estimate)) {
        if (estimate[j, "P(>|t|)"] < .001) {
          estimate$p[j] <- "< .001***"
        } else if (estimate[j, "P(>|t|)"] < .01) {
          estimate$p[j] <- paste0(estimate[j, "P(>|t|)"], "**")
        } else if (estimate[j, "P(>|t|)"] < .05) {
          estimate$p[j] <- paste0(estimate[j, "P(>|t|)"], "*")
        }
      }
      estimate$p <- sub("^0+", "", estimate$p)
      
      estimate <- estimate[, c("B (SE)", "t", "df", "p")]
      
      ci <- round(ci, 2)
      ci$`95% CI` <- paste0("[", ci$`2.5 %`, ", ", ci$`97.5 %`, "]")
      ci[, c("2.5 %", "97.5 %")] <- NULL
      
      estimate_ci <- cbind(estimate, ci)
      
      re_var_cov <- round(re_var_cov, 2)
      var <- diag(re_var_cov)
      var <- as.data.frame(var)
      
      resid <- extra.pars["Residual~~Residual", ]
      resid <- round(resid, 2)
      resid <- as.data.frame(resid)
      rownames(resid) <- "Residual"
      colnames(resid) <- "var"
      
      var_resid <- rbind(var, resid)
      
      re_cor <- round(re_cor, 2)
      re_cor_lower_tri <- matrix(as.character(re_cor), 
                                 nrow = nrow(re_cor), 
                                 ncol = ncol(re_cor))
      rownames(re_cor_lower_tri) <- rownames(re_cor)
      colnames(re_cor_lower_tri) <- colnames(re_cor)
      diag(re_cor_lower_tri) <- "\u2014"
      re_cor_lower_tri[upper.tri(re_cor_lower_tri)] <- ""
      
      result_list[[i]]$table1 <- list("estimate_ci" = estimate_ci,
                                      "var_resid" = var_resid,
                                      "re_cor_lower_tri" = re_cor_lower_tri)
    }
  }
  return(result_list)
}

# Define function to write table 1 to CSV

write_table1 <- function(result, path, sample) {
  sink(file = path)
  
  print(paste("Sample:", deparse(substitute(sample))))
  cat("\n")
  
  print("-------------------------------------------------------------------")
  cat("\n")
  
  for (i in 1:length(result)) {
    print(paste0("Model: ", names(result[i])))
    write.csv(result[[i]]$table1$estimate_ci)
  }
  cat("\n")
  
  print("-------------------------------------------------------------------")
  cat("\n")
  
  for (i in 1:length(result)) {
    print(paste0("Model: ", names(result[i])))
    write.csv(result[[i]]$table1$var_resid)
  }
  cat("\n")
  
  print("-------------------------------------------------------------------")
  cat("\n")
  
  for (i in 1:length(result)) {
    print(paste0("Model: ", names(result[i])))
    write.csv(result[[i]]$table1$re_cor_lower_tri)
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
load("./results/result_itt_max_w_prDoSS.RData")

# ---------------------------------------------------------------------------- #
# Format tables ----
# ---------------------------------------------------------------------------- #

# Table 1

result_itt_max <- format_table1(result_itt_max)
write_table1(result_itt_max, "./results/table1_max.csv", ITT)

result_itt_red_meanDSS <- format_table1(result_itt_red_meanDSS)
write_table1(result_itt_red_meanDSS, "./results/table1_itt_red_meanDSS.csv", ITT)

result_itt_red_cnDoSS <- format_table1(result_itt_red_cnDoSS)
write_table1(result_itt_red_cnDoSS, "./results/table1_itt_red_cnDoSS.csv", ITT)

result_itt_red_KMTOT <- format_table1(result_itt_red_KMTOT)
write_table1(result_itt_red_KMTOT, "./results/table1_itt_red_KMTOT.csv", ITT)

result_itt_max_w_prDoSS <- format_table1(result_itt_max_w_prDoSS)
write_table1(result_itt_max_w_prDoSS, "./results/table1_max_w_prDoSS.csv", ITT)
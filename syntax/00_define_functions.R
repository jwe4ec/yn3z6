# ---------------------------------------------------------------------------- #
# Define Functions
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Define version_control() ----
# ---------------------------------------------------------------------------- #

# Define function to check correct R version and load groundhog package

version_control <- function() {
  # Ensure you are using the same version of R used at the time the script was 
  # written. To install a previous version, go to 
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
  meta.groundhog("2021-07-01")
  groundhog_day <- "2021-04-01"
  return(groundhog_day)
}

# ---------------------------------------------------------------------------- #
# Define compute_pomp() ----
# ---------------------------------------------------------------------------- #

# Define function to compute POMP scores starting from scale-related variables
# that are on the metric of the scale's average item scores. POMP scores that
# result are on a metric of 0-100.

compute_pomp <- function(df, scale_vars, n_items, scale_min, scale_max) {
  output <- df
  
  if (all(scale_vars %in% names(df))) {
    for (i in 1:length(scale_vars)) {
      scale_var_pomp_colname <- paste0(scale_vars[i], "_pomp")
      
      avg_item_score <- df[, scale_vars[i]]
      item_sum_score <- avg_item_score*n_items
      pomp_score <- ((item_sum_score - scale_min)/(scale_max - scale_min))*100
      output[, scale_var_pomp_colname] <- pomp_score
    }
    
    return(output)
  } else {
    warning("At least one scale_vars not in df")
  }
}

# ---------------------------------------------------------------------------- #
# Define create_re_var_cov() ----
# ---------------------------------------------------------------------------- #

# Define function to create random effects variance-covariance matrix from 
# extra.pars output of testEstimates function

create_re_var_cov <- function(pooled, n_random_effects) {
  var_first <- 1
  var_last <- n_random_effects
  var <- pooled$extra.pars[1:var_last]
  cov_first <- n_random_effects + 1
  cov_last <- length(pooled$extra.pars) - 2
  cov <- pooled$extra.pars[cov_first:cov_last]
  
  names <- unlist(lapply(strsplit(labels(pooled$extra.pars)[[1]][1:var_last], 
                                  split = "~~", 
                                  fixed = TRUE),
                         function(x) x[1]))
  
  var_cov <- matrix(NA, ncol = length(var), nrow = length(var))
  rownames(var_cov) <- names
  colnames(var_cov) <- names
  diag(var_cov) <- var
  var_cov[upper.tri(var_cov)] <- cov
  var_cov[lower.tri(var_cov)] <- t(var_cov)[lower.tri(t(var_cov))]
  
  return(var_cov)
}

# ---------------------------------------------------------------------------- #
# Define create_results_list() ----
# ---------------------------------------------------------------------------- #

# Define function to create list of results for a model, including random
# effects correlation matrix and confidence intervals for fixed effects

create_results_list <- function(modelList, pooled, n_random_effects) {
  if (modelList[1] == "Model did not converge" & is.na(pooled[1])) {
    re_var_cov <- NA
    re_cor <- NA
    ci <- NA
  } else {
    re_var_cov <- create_re_var_cov(pooled, n_random_effects)
    re_cor <- cov2cor(re_var_cov)
    ci <- confint(pooled)
  }
  results_list <- list("modelList" = modelList,
                       "pooled" = pooled,
                       "re_var_cov" = re_var_cov,
                       "re_cor" = re_cor,
                       "ci" = ci)
  
  return(results_list)
}
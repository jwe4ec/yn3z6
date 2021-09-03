# ---------------------------------------------------------------------------- #
# Compute Internal Consistency and Reliability of Person Mean
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

groundhog.library(lavaan, groundhog_day)
groundhog.library(semTools, groundhog_day)
groundhog.library(MBESS, groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Define functions used throughout script ----
# ---------------------------------------------------------------------------- #

# Define function to compute reliability of person mean using Spearmean-Brown
# prophecy formula based on omega at a given time point and k number of time 
# points (per Wang & Maxwell, 2015, p. 73). Note that this formula assumes that 
# omega is the same at all time points.

compute_person_mean_reliability <- function(omega, k) {
   person_mean_reliability <- k*omega/((k - 1)*omega + 1)
   return(person_mean_reliability)
}

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/intermediate/data2.Rdata")
load("./data/intermediate/scale_defs.Rdata")

# ---------------------------------------------------------------------------- #
# Compute internal consistency for drtotl score of DERS ----
# ---------------------------------------------------------------------------- #

# Define subscales. Note: DERS-36 items in this study were numbered differently 
# from those in Gratz & Roemer (2004).

drclar_items <- c("rdr01", "DR04", "DR05", "rdr07", "DR09")
drgoal_items <- c("DR13", "DR18", "rdr20", "DR26", "DR33")
drimp_items <- c("DR03", "DR14", "DR19", "rdr24", "DR27", "DR32")
drnona_items <- c("DR11", "DR12", "DR21", "DR23", "DR25", "DR29")
drstra_items <- c("DR15", "DR16", "rdr22", "DR28", "DR30", "DR31", "DR35", "DR36")
drawar_items <- c("rdr02", "rdr06", "rdr08", "rdr10", "rdr17", "rdr34")

# Prepare lavaan syntax for latent factors

drtotl_gen <- paste(c(drclar_items, drgoal_items, drimp_items, drnona_items, 
                      drstra_items, drawar_items), collapse = " + ")
drclar_s1 <- paste(drclar_items, collapse = " + ")
drgoal_s2 <- paste(drgoal_items, collapse = " + ")
drimp_s3 <- paste(drimp_items, collapse = " + ")
drnona_s4 <- paste(drnona_items, collapse = " + ")
drstra_s5 <- paste(drstra_items, collapse = " + ")
drawar_s6 <- paste(drawar_items, collapse = " + ")

# Compute omega-hierarchical for drtotl general factor using complete data at 
# baseline based on bifactor model following Flora (2020)

mod_drtotl_bf_36 <- 
  'gen =~ rdr01 + DR04 + DR05 + rdr07 + DR09 + 
          DR13 + DR18 + rdr20 + DR26 + DR33 + 
          DR03 + DR14 + DR19 + rdr24 + DR27 + DR32 + 
          DR11 + DR12 + DR21 + DR23 + DR25 + DR29 + 
          DR15 + DR16 + rdr22 + DR28 + DR30 + DR31 + DR35 + DR36 + 
          rdr02 + rdr06 + rdr08 + rdr10 + rdr17 + rdr34
   s1 =~ rdr01 + DR04 + DR05 + rdr07 + DR09
   s2 =~ DR13 + DR18 + rdr20 + DR26 + DR33
   s3 =~ DR03 + DR14 + DR19 + rdr24 + DR27 + DR32
   s4 =~ DR11 + DR12 + DR21 + DR23 + DR25 + DR29
   s5 =~ DR15 + DR16 + rdr22 + DR28 + DR30 + DR31 + DR35 + DR36
   s6 =~ rdr02 + rdr06 + rdr08 + rdr10 + rdr17 + rdr34'

# Note: CFA step yields this warning message:
   
# In lav_model_vcov(lavmodel = lavmodel, lavsamplestats = lavsamplestats,  :
#   lavaan WARNING:
#     The variance-covariance matrix of the estimated parameters (vcov)
#     does not appear to be positive definite! The smallest eigenvalue
#     (= -7.818241e-16) is smaller than zero. This may be a symptom that
#     the model is not identified.

fit_drtotl_bf_36 <- cfa(mod_drtotl_bf_36, 
                        data = data2$ders[data2$ders$time0 == 0, ],
                        std.lv = TRUE,
                        estimator = "MLR",
                        orthogonal = TRUE)

# Model fit is poor (robust CFI = .456, which is < .9, robust TLI = 0.386, which
# is < .9, robust RMSEA = .163, which is > .08). Thus, omega-hierarchical values 
# (omega2 based on model-implied variance of the total score and omega3 based on 
# observed sample variance; see Flora, 2020, p. 496) based on this factor structure 
# may be inappropriate.

summary(fit_drtotl_bf_36, fit.measures = TRUE)
residuals(fit_drtotl_bf_36, type = 'cor')

reliability(fit_drtotl_bf_36)

# Note: ci.reliability function of MBESS package cannot be used to estimate a CI
# for omega-hierarchical based on a bifactor model (see Flora, 2020, Endnote 11)

# Compute omega for drtotl using complete data at baseline based on single-factor 
# model following Flora (2020)

mod_drtotl_1f_36 <- 
  'f1 =~ rdr01 + DR04 + DR05 + rdr07 + DR09 + DR13 + DR18 + rdr20 + DR26 + DR33 + 
         DR03 + DR14 + DR19 + rdr24 + DR27 + DR32 + 
         DR11 + DR12 + DR21 + DR23 + DR25 + DR29 + 
         DR15 + DR16 + rdr22 + DR28 + DR30 + DR31 + DR35 + DR36 + 
         rdr02 + rdr06 + rdr08 + rdr10 + rdr17 + rdr34'

# Note: CFA step yields this warning message:

# In lav_model_vcov(lavmodel = lavmodel, lavsamplestats = lavsamplestats,  :
#   lavaan WARNING:
#     The variance-covariance matrix of the estimated parameters (vcov)
#     does not appear to be positive definite! The smallest eigenvalue
#     (= -2.434578e-16) is smaller than zero. This may be a symptom that
#     the model is not identified.

fit_drtotl_1f_36 <- cfa(mod_drtotl_1f_36, 
                        data = data2$ders[data2$ders$time0 == 0, ],
                        std.lv = TRUE,
                        estimator = "MLR")

# Model fit is poor (robust CFI = .151, robust TLI = 0.099, robust RMSEA = .198),
# with large residual interitem correlations, suggesting potential 
# multidimensionality (see Flora, 2020, p. 488). Thus, omega values (omega and 
# omega2 based on model-implied variance of the total score and omega3 based on 
# observed sample variance; see Flora, 2020, p. 490) based on this factor 
# structure may be inappropriate. The factor loadings are also highly variable, 
# indicating a violation of tau equivalence that would be assumed by Cronbach's 
# alpha, which would thus be even less appropriate in this case.

summary(fit_drtotl_1f_36, fit.measures = TRUE)
residuals(fit_drtotl_1f_36, type = 'cor')

reliability(fit_drtotl_1f_36)

# Computing 95% CI for omega (either using percentile bootstrap per Flora, 2020, 
# or using bias-corrected and accelerated bootstrap per Dunn et al., 2014) based 
# on single-factor model yields this error message:

# Error in if (const(t, min(1e-08, mean(t, na.rm = TRUE)/1e+06))) { : 
#   missing value where TRUE/FALSE needed

rawData <- data2$ders[data2$ders$time0 == 0, scale_defs$drtotl_items]

sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega",
               conf.level = 0.95,
               interval.type = "perc",
               B = 1000)
ci.reliability(data = completeData,
               type = "omega",
               conf.level = 0.95,
               interval.type = "bca",
               B = 1000)

# ---------------------------------------------------------------------------- #
# Compute internal consistency for meanDSS score of DBT-WCCL ----
# ---------------------------------------------------------------------------- #

# Prepare lavaan syntax for latent factor

meanDSS_f1 <- paste(scale_defs$meanDSS_items, collapse = " + ")

# Compute omega for meanDSS using complete data at baseline based on single-factor 
# model following Flora (2020, supp.)

mod_meanDSS_1f <- 
  'f1 =~ RWC01 + RWC02 + RWC04 + RWC06 + RWC11 + RWC12 + RWC13 + RWC15 + 
         RWC18 + RWC21 + RWC22 + RWC24 + RWC25 + RWC27 + RWC34 + RWC35 + 
         RWC38 + RWC43 + RWC45 + RWC46 + RWC47 + RWC48 + RWC50 + RWC51 + 
         RWC52 + RWC54 + RWC56 + RWC57 + RWC60 + RWC63 + RWC65 + RWC66 + 
         RWC68 + RWC69 + RWC71 + RWC73 + RWC74 + RWC75'

# Note: CFA step yields this warning message:

# In lav_model_vcov(lavmodel = lavmodel, lavsamplestats = lavsamplestats,  :
#   lavaan WARNING:
#     The variance-covariance matrix of the estimated parameters (vcov)
#     does not appear to be positive definite! The smallest eigenvalue
#     (= -8.381213e-17) is smaller than zero. This may be a symptom that
#     the model is not identified.

fit_meanDSS_1f <- cfa(mod_meanDSS_1f, 
                      data = data2$dbt_wccl[data2$dbt_wccl$time0 == 0, ],
                      std.lv = TRUE,
                      estimator = "MLR")

# Model fit is poor (robust CFI = .184, robust TLI = 0.138, robust RMSEA = .161),
# with large residual interitem correlations, suggesting potential 
# multidimensionality (see Flora, 2020, p. 488). Thus, omega values (omega and 
# omega2 based on model-implied variance of the total score and omega3 based on 
# observed sample variance; see Flora, 2020, p. 490) based on this factor 
# structure may be inappropriate. The factor loadings are also highly variable, 
# indicating a violation of tau equivalence that would be assumed by Cronbach's 
# alpha, which would thus be even less appropriate in this case.

summary(fit_meanDSS_1f, fit.measures = TRUE)
residuals(fit_meanDSS_1f, type = 'cor')

reliability(fit_meanDSS_1f)

# Computing 95% CI for omega (using either percentile bootstrap per Flora, 2020, 
# or using bias-corrected and accelerated bootstrap per Dunn et al., 2014) based 
# on single-factor model yields this error and warnings:

# Error in if (const(t, min(1e-08, mean(t, na.rm = TRUE)/1e+06))) { : 
#   missing value where TRUE/FALSE needed
# In addition: Warning messages:
# 1: In lavaan::lavaan(model = model, data = data, missing = missing,  :
#   lavaan WARNING:
#     the optimizer warns that a solution has NOT been found!
# 2: In lavaan::lavaan(model = model, data = data, missing = missing,  :
#   lavaan WARNING: estimation of the baseline model failed.

rawData <- data2$dbt_wccl[data2$dbt_wccl$time0 == 0, scale_defs$meanDSS_items]

sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega",
               conf.level = 0.95,
               interval.type = "perc",
               B = 5000)
ci.reliability(data = completeData,
               type = "omega",
               conf.level = 0.95,
               interval.type = "bca",
               B = 5000)

# ---------------------------------------------------------------------------- #
# Compute internal consistency for cnDoSS score of DoSS ----
# ---------------------------------------------------------------------------- #

# Prepare lavaan syntax for latent factor

cnDoSS_f1 <- paste(scale_defs$cnDoSS_items, collapse = " + ")

# Compute omega for cnDoSS using complete data at baseline based on single-factor 
# model following Flora (2020, supp.)

mod_cnDoSS_1f <- 'f1 =~ DoSS01 + rDoSS12 + DoSS16 + rDoSS23'

fit_cnDoSS_1f <- cfa(mod_cnDoSS_1f, 
                     data = data2$doss[data2$doss$time0 == 0, ],
                     std.lv = TRUE,
                     estimator = "MLR")

# Model fit is poor (robust CFI = .655, robust TLI = -0.036, robust RMSEA = .277),
# with small-to-moderate residual interitem correlations, suggesting potential 
# multidimensionality (see Flora, 2020, p. 488). Thus, omega values (omega and 
# omega2 based on model-implied variance of the total score and omega3 based on 
# observed sample variance; see Flora, 2020, p. 490) based on this factor 
# structure may be inappropriate. The factor loadings are also highly variable, 
# indicating a violation of tau equivalence that would be assumed by Cronbach's 
# alpha, which would thus be even less appropriate in this case.

summary(fit_cnDoSS_1f, fit.measures = TRUE)
residuals(fit_cnDoSS_1f, type = 'cor')

reliability(fit_cnDoSS_1f)

# Computing 95% CI for omega (using either percentile bootstrap per Flora, 2020, 
# or using bias-corrected and accelerated bootstrap per Dunn et al., 2014) based 
# on single-factor model yields these warning messages:

# In lav_object_post_check(object) :
#   lavaan WARNING: some estimated ov variances are negative
# In lavaan::lavaan(model = model, data = data, missing = missing,  ... :
#   lavaan WARNING:
#     the optimizer warns that a solution has NOT been found!
# In lavaan::lavaan(model = model, data = data, missing = missing,  ... :
#   lavaan WARNING:
#     the optimizer (NLMINB) claimed the model converged, but not all
#     elements of the gradient are (near) zero; the optimizer may not
#     have found a local solution use check.gradient = FALSE to skip
#     this check.

rawData <- data2$doss[data2$doss$time0 == 0, scale_defs$cnDoSS_items]

sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega",
               conf.level = 0.95,
               interval.type = "perc",
               B = 1000)
ci.reliability(data = completeData,
               type = "omega",
               conf.level = 0.95,
               interval.type = "bca",
               B = 1000)

# ---------------------------------------------------------------------------- #
# Compute internal consistency for KMTOT score of KIMS ----
# ---------------------------------------------------------------------------- #

# Define subscales

KMObserve_items <- c("KM01", "KM05", "KM09", "KM13", "KM17", "KM21", "KM25", 
                     "KM29", "KM30", "KM33", "KM37", "KM39")
KMDescribe_items <- c("KM02", "KM06", "KM10", "KM14r", "KM18r", "KM22r", "KM26", 
                      "KM34")
KMAware_items <- c("KM03r", "KM07", "KM11r", "KM15", "KM19", "KM23r", "KM27r", 
                   "KM31r", "KM35r", "KM38")
KMNonJudgmental_items <- c("KM04r", "KM08r", "KM12r", "KM16r", "KM20r", "KM24r",
                           "KM28r", "KM32r", "KM36r")

# Prepare lavaan syntax for latent factors

KMTOT_gen <- paste(c(KMObserve_items, KMDescribe_items, KMAware_items, 
                     KMNonJudgmental_items), collapse = " + ")
KMObserve_s1 <- paste(KMObserve_items, collapse = " + ")
KMDescribe_s2 <- paste(KMDescribe_items, collapse = " + ")
KMAware_s3 <- paste(KMAware_items, collapse = " + ")
KMNonJudgmental_s4 <- paste(KMNonJudgmental_items, collapse = " + ")

# Compute omega-hierarchical for KMTOT general factor using complete data at 
# baseline based on bifactor model following Flora (2020)

mod_KMTOT_bf <- 
  'gen =~ KM01 + KM05 + KM09 + KM13 + KM17 + KM21 + KM25 + KM29 + KM30 + KM33 + 
            KM37 + KM39 + 
          KM02 + KM06 + KM10 + KM14r + KM18r + KM22r + KM26 + KM34 + 
          KM03r + KM07 + KM11r + KM15 + KM19 + KM23r + KM27r + KM31r + KM35r + KM38 + 
          KM04r + KM08r + KM12r + KM16r + KM20r + KM24r + KM28r + KM32r + KM36r
   s1 =~ KM01 + KM05 + KM09 + KM13 + KM17 + KM21 + KM25 + KM29 + KM30 + KM33 + 
           KM37 + KM39
   s2 =~ KM02 + KM06 + KM10 + KM14r + KM18r + KM22r + KM26 + KM34
   s3 =~ KM03r + KM07 + KM11r + KM15 + KM19 + KM23r + KM27r + KM31r + KM35r + KM38
   s4 =~ KM04r + KM08r + KM12r + KM16r + KM20r + KM24r + KM28r + KM32r + KM36r'

# Note: CFA step yields this warning message:

# In lav_model_vcov(lavmodel = lavmodel, lavsamplestats = lavsamplestats,  :
#   lavaan WARNING:
#     The variance-covariance matrix of the estimated parameters (vcov)
#     does not appear to be positive definite! The smallest eigenvalue
#     (= -1.140614e-15) is smaller than zero. This may be a symptom that
#     the model is not identified.

fit_mod_KMTOT_bf <- cfa(mod_KMTOT_bf, 
                        data = data2$kims[data2$kims$time0 == 0, ],
                        std.lv = TRUE,
                        estimator = "MLR",
                        orthogonal = TRUE)

# Model fit is poor (robust CFI = .517, which is < .9, robust TLI = 0.460, which
# is < .9, robust RMSEA = .169, which is > .08). Thus, omega-hierarchical values 
# (omega2 based on model-implied variance of the total score and omega3 based on 
# observed sample variance; see Flora, 2020, p. 496) based on this factor structure 
# may be inappropriate.

summary(fit_mod_KMTOT_bf, fit.measures = TRUE)
residuals(fit_mod_KMTOT_bf, type = 'cor')

reliability(fit_mod_KMTOT_bf)

# Note: ci.reliability function of MBESS package cannot be used to estimate a CI
# for omega-hierarchical based on a bifactor model (see Flora, 2020, Endnote 11)

# Compute omega for KMTOT using complete data at baseline based on single-factor 
# model following Flora (2020)

mod_KMTOT_1f <- 
  'f1 =~ KM01 + KM05 + KM09 + KM13 + KM17 + KM21 + KM25 + KM29 + KM30 + KM33 + 
            KM37 + KM39 + 
         KM02 + KM06 + KM10 + KM14r + KM18r + KM22r + KM26 + KM34 + 
         KM03r + KM07 + KM11r + KM15 + KM19 + KM23r + KM27r + KM31r + KM35r + KM38 + 
         KM04r + KM08r + KM12r + KM16r + KM20r + KM24r + KM28r + KM32r + KM36r'

# Note: CFA step yields this warning message:

# In lav_model_vcov(lavmodel = lavmodel, lavsamplestats = lavsamplestats,  :
#   lavaan WARNING:
#     The variance-covariance matrix of the estimated parameters (vcov)
#     does not appear to be positive definite! The smallest eigenvalue
#     (= -5.065196e-16) is smaller than zero. This may be a symptom that
#     the model is not identified.

fit_KMTOT_1f <- cfa(mod_KMTOT_1f, 
                    data = data2$kims[data2$kims$time0 == 0, ],
                    std.lv = TRUE,
                    estimator = "MLR")

# Model fit is poor (robust CFI = .226, robust TLI = 0.183, robust RMSEA = .208),
# with large residual interitem correlations, suggesting potential 
# multidimensionality (see Flora, 2020, p. 488). Thus, omega values (omega and 
# omega2 based on model-implied variance of the total score and omega3 based on 
# observed sample variance; see Flora, 2020, p. 490) based on this factor 
# structure may be inappropriate. The factor loadings are also highly variable, 
# indicating a violation of tau equivalence that would be assumed by Cronbach's 
# alpha, which would thus be even less appropriate in this case.

summary(fit_KMTOT_1f, fit.measures = TRUE)
residuals(fit_KMTOT_1f, type = 'cor')

reliability(fit_KMTOT_1f)

# Computing 95% CI for omega (either using percentile bootstrap per Flora, 2020, 
# or using bias-corrected and accelerated bootstrap per Dunn et al., 2014) based 
# on single-factor model yields this error message:

# Error in if (const(t, min(1e-08, mean(t, na.rm = TRUE)/1e+06))) { : 
#   missing value where TRUE/FALSE needed

rawData <- data2$kims[data2$kims$time0 == 0, scale_defs$KMTOT_items]

sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega",
               conf.level = 0.95,
               interval.type = "perc",
               B = 1000)
ci.reliability(data = completeData,
               type = "omega",
               conf.level = 0.95,
               interval.type = "bca",
               B = 1000)

# ---------------------------------------------------------------------------- #
# Compute reliabilities of person means ----
# ---------------------------------------------------------------------------- #

# Compute reliabilities of person means for mechanistic variables based on omega 
# at Baseline (obtained above) and four time points

# Note: Use omega from bifactor model for KMTOT

p_mean_rel_meanDSS <- compute_person_mean_reliability(0.85, 4)
p_mean_rel_cnDoSS <- compute_person_mean_reliability(0.63, 4)
p_mean_rel_KMTOT <- compute_person_mean_reliability(0.03, 4)
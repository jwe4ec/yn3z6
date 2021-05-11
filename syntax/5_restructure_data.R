# ---------------------------------------------------------------------------- #
# Restructure Data
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
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/intermediate/data3.Rdata")

# ---------------------------------------------------------------------------- #
# Create tables for analysis ----
# ---------------------------------------------------------------------------- #

# Identify key analysis variables

meanDSS_vars <- c("meanDSS", "meanDSS_btw", "meanDSS_wth",
                  "meanDSS_pomp", "meanDSS_pomp_btw", "meanDSS_pomp_wth")
drtotl_m_imp_vars <- c("drtotl_m_imp", "drtotl_m_imp_btw", "drtotl_m_imp_wth",
                       "drtotl_m_imp_pomp", "drtotl_m_imp_pomp_btw", 
                       "drtotl_m_imp_pomp_wth")
cnDoSS_vars <- c("cnDoSS", "cnDoSS_btw", "cnDoSS_wth",
                 "cnDoSS_pomp", "cnDoSS_pomp_btw", "cnDoSS_pomp_wth")
KMTOT_vars <- c("KMTOT", "KMTOT_btw", "KMTOT_wth",
                "KMTOT_pomp", "KMTOT_pomp_btw", "KMTOT_pomp_wth")

# Identify potential auxiliary variables (other than "Condition", which already
# will be included in tables for analysis)

demog_aux <- c("SH132", "DDS04", "DDS06", "DDS06b", "DDS06c", "DDS10", 
               "DDS14", "DDS15a", "DDS16a", "DDS17a2", "DDS18", "DDS19", 
               "DDS25", "DDS26")
scid1_aux <- c("SCPN120", "evanxdx", "evdep", "eveatdx", "evsubab", "nowanxdx", 
               "nowdep", "noweatdx", "nowsomat", "nowsubab", "primaryDX")
dbt_wccl_aux <- c("meanDCS1", "meanDCS2", "meanDCS")
ders_aux <- c("dracce", "drawar", "drclar", "drcingr", "drgoal", "drimp", 
              "drstra")
doss_aux <- c("nvDoSS", "drDoSS", "csDoSS", "prDoSS", "slDoSS")
kims_aux <- c("KMAware", "KMDescribe", "KMNonJudgmental", "KMObserve")
oasis_aux <- c("OASISsum")
phq_aux <- c("phqsum")
ess_aux <- c("essbod", "essbs", "esscs", "esstot")
staxi_aux <- c("stxAXIN", "stxAXOUT", "stxAXCON", "stxAXEX")
oq_aux <- c("oq_sd", "oq_ir", "oq_sr", "oqsum")

# Create table for contemporaneous model (without potential auxiliary variables)

contemp <- data3$dbt_wccl[, c("ResearchID", "Condition", "AIN", "Period", 
                              "time0", meanDSS_vars)]
contemp <- merge(contemp, 
                 data3$ders[, c("ResearchID", "time0", drtotl_m_imp_vars)],
                 by = c("ResearchID", "time0"),
                 all.x = TRUE)
contemp <- merge(contemp, 
                 data3$doss[, c("ResearchID", "time0", cnDoSS_vars)],
                 by = c("ResearchID", "time0"),
                 all.x = TRUE)
contemp <- merge(contemp,
                 data3$kims[, c("ResearchID", "time0", KMTOT_vars)],
                 by = c("ResearchID", "time0"),
                 all.x = TRUE)

# Create table for contemporaneous model (with potential auxiliary variables)

contemp_aux <- merge(contemp, 
                     data3$demog[, c("ResearchID", demog_aux)],
                     by = c("ResearchID"),
                     all.x = TRUE)
contemp_aux <- merge(contemp_aux, 
                     data3$scid1[, c("ResearchID", scid1_aux)],
                     by = c("ResearchID"),
                     all.x = TRUE)
contemp_aux <- merge(contemp_aux, 
                     data3$dbt_wccl[, c("ResearchID", "time0", dbt_wccl_aux)],
                     by = c("ResearchID", "time0"),
                     all.x = TRUE)
contemp_aux <- merge(contemp_aux,
                     data3$ders[, c("ResearchID", "time0", ders_aux)],
                     by = c("ResearchID", "time0"),
                     all.x = TRUE)
contemp_aux <- merge(contemp_aux,
                     data3$doss[, c("ResearchID", "time0", doss_aux)],
                     by = c("ResearchID", "time0"),
                     all.x = TRUE)
contemp_aux <- merge(contemp_aux,
                     data3$kims[, c("ResearchID", "time0", kims_aux)],
                     by = c("ResearchID", "time0"),
                     all.x = TRUE)
contemp_aux <- merge(contemp_aux,
                     data3$oasis[, c("ResearchID", "time0", oasis_aux)],
                     by = c("ResearchID", "time0"),
                     all.x = TRUE)
contemp_aux <- merge(contemp_aux,
                     data3$phq[, c("ResearchID", "time0", phq_aux)],
                     by = c("ResearchID", "time0"),
                     all.x = TRUE)
contemp_aux <- merge(contemp_aux,
                     data3$ess[, c("ResearchID", "time0", ess_aux)],
                     by = c("ResearchID", "time0"),
                     all.x = TRUE)
contemp_aux <- merge(contemp_aux,
                     data3$staxi[, c("ResearchID", "time0", staxi_aux)],
                     by = c("ResearchID", "time0"),
                     all.x = TRUE)
contemp_aux <- merge(contemp_aux,
                     data3$oq[, c("ResearchID", "time0", oq_aux)],
                     by = c("ResearchID", "time0"),
                     all.x = TRUE)

# Create table for lagged model (without potential auxiliary variables)

lagged <- data3$dbt_wccl[, c("ResearchID", "Condition", "AIN", "Period",
                             "time0_lag", meanDSS_vars)]
lagged <- merge(lagged,
                data3$ders[, c("ResearchID", "time0_lag", drtotl_m_imp_vars)],
                by = c("ResearchID", "time0_lag"),
                all.x = TRUE)
lagged <- merge(lagged,
                data3$doss[, c("ResearchID", "time0_lag", cnDoSS_vars)],
                by = c("ResearchID", "time0_lag"),
                all.x = TRUE)
lagged <- merge(lagged,
                data3$kims[, c("ResearchID", "time0_lag", KMTOT_vars)],
                by = c("ResearchID", "time0_lag"),
                all.x = TRUE)
lagged <- lagged[!is.na(lagged$time0_lag), ]

# ---------------------------------------------------------------------------- #
# Save intermediate data ----
# ---------------------------------------------------------------------------- #

# Save data as R object

# TODO





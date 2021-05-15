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

data4 <- data3

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

analysis_vars <- c(meanDSS_vars, drtotl_m_imp_vars, cnDoSS_vars, KMTOT_vars)

# Identify potential auxiliary variables (other than "Condition", which already
# will be included in tables for analysis)

acs_aux <- c("ACSmean", "ACS_AG", "ACS_PA", "ACS_DM", "ACS_AX")
asi_aux <- "anyDrugs"
dbt_wccl_aux <- c("meanDCS1", "meanDCS2", "meanDCS")
demog_aux <- c("SH132", "DDS04", "DDS06b", "DDS10", "DDS14", "DDS15a", "DDS16a", 
               "DDS17a2", "DDS25", "DDS26", "race")
ders_aux <- c("dracce", "drawar", "drclar", "drcingr", "drgoal", "drimp", 
              "drstra")
doss_aux <- c("nvDoSS", "drDoSS", "csDoSS", "prDoSS", "slDoSS")
dpss_aux <- c("dpss_dp", "dpss_ds")
eis_aux <- c("totCred", "meanExpImp", "meanConf")
ess_aux <- c("esstot", "essbod", "essbs", "esscs")
kims_aux <- c("KMAware", "KMDescribe", "KMNonJudgmental", "KMObserve")
oasis_aux <- c("OASISsum")
oq_aux <- c("oqsum", "oq_sd", "oq_ir", "oq_sr")
phq_aux <- c("phqsum")
ppvt_aux <- "PPVT"
scid1_aux <- c("SCPN120", "evanxdx", "evdep", "eveatdx", "evsubab", "nowanxdx", 
               "nowdep", "noweatdx", "nowsomat", "nowsubab", "primaryDX")
scid2_bpd_aux <- "bpdcrit"
staxi_aux <- c("stxAXIN", "stxAXOUT", "stxAXCON", "stxAXEX")
tas_aux <- c("tas_sum", "tas_dif", "tas_ddf", "tas_eot")
thi_aux <- c("THP02_forCompliance", "tookMed", "medNoncomplianceMD",
             "researchNoncompliance")

# Create table for contemporaneous model (without potential auxiliary variables)

contemp <- data4$dbt_wccl[, c("ResearchID", "Condition", "AIN", "Period", 
                              "time0", meanDSS_vars)]
contemp <- merge(contemp, 
                 data4$ders[, c("ResearchID", "time0", drtotl_m_imp_vars)],
                 by = c("ResearchID", "time0"),
                 all.x = TRUE)
contemp <- merge(contemp, 
                 data4$doss[, c("ResearchID", "time0", cnDoSS_vars)],
                 by = c("ResearchID", "time0"),
                 all.x = TRUE)
contemp <- merge(contemp,
                 data4$kims[, c("ResearchID", "time0", KMTOT_vars)],
                 by = c("ResearchID", "time0"),
                 all.x = TRUE)

data4$contemp <- contemp

# Create table for contemporaneous model (with potential auxiliary variables)

contemp_aux <- contemp

time_invariant_aux_tables <- 
  unlist(strsplit(c("demog_aux", "ppvt_aux", "scid1_aux", "scid2_bpd_aux"), 
                  "_aux", 
                  fixed = TRUE))

time_varying_aux_tables <- 
  unlist(strsplit(c("acs_aux", "asi_aux", "dbt_wccl_aux", "ders_aux", "doss_aux", 
                    "dpss_aux", "eis_aux", "ess_aux", "kims_aux", "oasis_aux", 
                    "oq_aux", "phq_aux", "staxi_aux", "tas_aux", "thi_aux"),
                  "_aux",
                  fixed = TRUE))

for (i in 1:length(data4)) {
  if (names(data4[i]) %in% time_invariant_aux_tables) {
    aux_vars <- paste0(names(data4[i]), "_aux")
    
    contemp_aux <- merge(contemp_aux, 
                         data4[[i]][, c("ResearchID", get(aux_vars))],
                         by = "ResearchID",
                         all.x = TRUE)
  } else if (names(data4[i]) %in% time_varying_aux_tables) {
    aux_vars <- paste0(names(data4[i]), "_aux")
    
    contemp_aux <- merge(contemp_aux,
                         data4[[i]][, c("ResearchID", "time0", get(aux_vars))],
                         by = c("ResearchID", "time0"),
                         all.x = TRUE)
  }
}

data4$contemp_aux <- contemp_aux

# Create table for lagged model (without potential auxiliary variables)

lagged <- data4$dbt_wccl[, c("ResearchID", "Condition", "AIN", "Period",
                             "time0_lag", meanDSS_vars)]
lagged <- merge(lagged,
                data4$ders[, c("ResearchID", "time0_lag", drtotl_m_imp_vars)],
                by = c("ResearchID", "time0_lag"),
                all.x = TRUE)
lagged <- merge(lagged,
                data4$doss[, c("ResearchID", "time0_lag", cnDoSS_vars)],
                by = c("ResearchID", "time0_lag"),
                all.x = TRUE)
lagged <- merge(lagged,
                data4$kims[, c("ResearchID", "time0_lag", KMTOT_vars)],
                by = c("ResearchID", "time0_lag"),
                all.x = TRUE)
lagged <- lagged[!is.na(lagged$time0_lag), ]

data4$lagged <- lagged

# ---------------------------------------------------------------------------- #
# Create missing data indicators ----
# ---------------------------------------------------------------------------- #

# Create binary missing data indicators (0 = not missing, 1 = missing) for each
# analysis variable at each time point

for (i in 1:length(names(data4$contemp_aux))) {
  if (names(data4$contemp_aux)[i] %in% analysis_vars) {
    data4$contemp_aux[, paste0(names(data4$contemp_aux)[i], "_ind")] <- 
      is.na(data4$contemp_aux[, names(data4$contemp_aux)[i]])
  }
}

# ---------------------------------------------------------------------------- #
# Save intermediate data ----
# ---------------------------------------------------------------------------- #

# Save data as R object

save(data4, file = "./data/intermediate/data4.Rdata")
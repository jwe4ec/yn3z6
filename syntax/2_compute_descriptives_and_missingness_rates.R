# ---------------------------------------------------------------------------- #
# Compute Descriptives and Missingness Rates
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
# Define functions used throughout script ----
# ---------------------------------------------------------------------------- #

# Define function to report AIN and Period for each df in a list of dfs

report_AIN_Period <- function(dfs) {
  for (i in 1:length(dfs)) {
    cat(names(dfs[i]))
    cat("\n", "\n")
    
    cat("AIN:")
    if ("AIN" %in% names(dfs[[i]])) {
      print(table(dfs[[i]]["AIN"], useNA = "always"))
    } else {
      cat("No such column")
      cat("\n")
    }
    
    cat("Period:")
    if ("Period" %in% names(dfs[[i]])) {
      print(table(dfs[[i]]["Period"], useNA = "always"))
      cat("\n")
    } else {
      cat("No such column")
      cat("\n")
    }
  }
}

# Define function to report proportion of scale scores computed with at least
# one item missing

item_level <- function(df, scale, scale_items) {
  cat("Scale:", deparse(substitute(scale)), "\n")
  
  scale_obs <- df[!is.na(scale), ]
  
  num <- sum(apply(scale_obs[, scale_items],
                   MARGIN = 1,
                   FUN = function(x) { any(is.na(x)) }))
  cat("Computed scale scores with >= 1 item missing:", num, "\n")
  
  denom <- nrow(scale_obs)
  cat("Computed scale scores:", denom, "\n")
  
  prop <- num / denom
  cat("Proportion:", round(prop, 2), "\n")
  percent <- prop*100
  cat(paste0("Percent: ", round(percent, 2), "%"), "\n")
}

# Define function for computing n, M, and SD for each scale at each time point

describe_by_time <- function(df, scale) {
  fml <- as.formula(paste0(scale, "~ Period"))
  ag <- do.call(data.frame, aggregate(fml, 
                                      data = df,
                                      FUN = function(x) c(n = length(x), 
                                                          M = mean(x), 
                                                          SD = sd(x))))
  output <- cbind(scale, ag)
  names(output) <- c("Outcome", "Period", "n", "M", "SD")
  return(output)
}

# ---------------------------------------------------------------------------- #
# Import further cleaned CSV data files ----
# ---------------------------------------------------------------------------- #

# Obtain file names of CSV data files

further_clean_data_dir <- paste0(wd_dir, "/data/clean_further")
filenames <- list.files(paste0(further_clean_data_dir),
                        pattern = "*.csv",
                        full.names = FALSE)

# Import data files and store them in list

data <- lapply(paste0(further_clean_data_dir, "/", filenames),
               read.csv)

# Name each data file in list

split_char <- ".csv"
names(data) <- unlist(lapply(filenames,
                             function(f) {
                               unlist(strsplit(f,
                                               split = split_char,
                                               fixed = FALSE))[1]
                             }))

# ---------------------------------------------------------------------------- #
# Restrict to ITT sample and desired periods and define time0 ----
# ---------------------------------------------------------------------------- #

# Investigate AIN and Period for each table

report_AIN_Period(data)

# Restrict to ITT sample (AIN = 1)

data2 <- vector("list", length(data))

for (i in 1:length(data)) {
  data2[[i]] <- data[[i]]
  
  data2[[i]] <- data2[[i]][data2[[i]]$AIN == 1 & !is.na(data2[[i]]$AIN), ]
}

names(data2) <- names(data)

report_AIN_Period(data2)

# Confirm ResearchIDs are consistent across tables

all(setequal(unique(data2$dbt_wccl$ResearchID), unique(data2$demog$ResearchID)),
    setequal(unique(data2$dbt_wccl$ResearchID), unique(data2$ders$ResearchID)),
    setequal(unique(data2$dbt_wccl$ResearchID), unique(data2$doss$ResearchID)),
    setequal(unique(data2$dbt_wccl$ResearchID), unique(data2$kims$ResearchID)),
    setequal(unique(data2$dbt_wccl$ResearchID), unique(data2$scid1$ResearchID)))

# Exclude DERS pretreatment data (use DERS screening data instead)

table(data2$ders$Period)
data2$ders <- data2$ders[data2$ders$Period != 2, ]

report_AIN_Period(data2)

# Define time0 and time0_lag for each repeated-measures table (note that time0 
# is already defined in dbt_wccl table; the code below yields the same values)

for (i in 1:length(data2)) {
  if (names(data2[i]) %in% c("dbt_wccl", "doss", "kims")) {
    data2[[i]]$time0 <- NA
    data2[[i]]$time0[data2[[i]]$Period == 2] <- 0
    data2[[i]]$time0[data2[[i]]$Period == 3] <- 1
    data2[[i]]$time0[data2[[i]]$Period == 4] <- 2
    data2[[i]]$time0[data2[[i]]$Period == 5] <- 3
    
    data2[[i]]$time0_lag <- NA
    data2[[i]]$time0_lag[data2[[i]]$Period == 2] <- 0
    data2[[i]]$time0_lag[data2[[i]]$Period == 3] <- 1
    data2[[i]]$time0_lag[data2[[i]]$Period == 4] <- 2
    data2[[i]]$time0_lag[data2[[i]]$Period == 5] <- NA
  } else if (names(data2[i]) == "ders") {
    data2[[i]]$time0 <- NA
    data2[[i]]$time0[data2[[i]]$Period == 0] <- 0
    data2[[i]]$time0[data2[[i]]$Period == 3] <- 1
    data2[[i]]$time0[data2[[i]]$Period == 4] <- 2
    data2[[i]]$time0[data2[[i]]$Period == 5] <- 3
    
    data2[[i]]$time0_lag <- NA
    data2[[i]]$time0_lag[data2[[i]]$Period == 2] <- NA
    data2[[i]]$time0_lag[data2[[i]]$Period == 3] <- 0
    data2[[i]]$time0_lag[data2[[i]]$Period == 4] <- 1
    data2[[i]]$time0_lag[data2[[i]]$Period == 5] <- 2
  }
}

# Save data as R object

dir.create("./data/intermediate")
save(data2, file = "./data/intermediate/data2.Rdata")

# ---------------------------------------------------------------------------- #
# Define items that comprise each scale ----
# ---------------------------------------------------------------------------- #

# For Total Score of DERS, main outcomes paper analyzed the sum of available 
# items if at least 30 out of 36 items were not missing. Moreover, for each
# missing item, a value of 1 was added to the total score.

drtotl_items <- c("rdr01", "rdr02", "DR03", "DR04", "DR05", "rdr06", "rdr07", 
                  "rdr08", "DR09", "rdr10", "DR11", "DR12", "DR13", "DR14", 
                  "DR15", "DR16", "rdr17", "DR18", "DR19", "rdr20", "DR21", 
                  "rdr22", "DR23", "rdr24", "DR25", "DR26", "DR27", "DR28", 
                  "DR29", "DR30", "DR31", "DR32", "DR33", "rdr34", "DR35", 
                  "DR36")
length(drtotl_items)

# No drtotl scale scores were not computed on account of too few items

drtotl_items_missing <- apply(data2$ders[, drtotl_items],
                              MARGIN = 1, 
                              FUN = function(x) { sum(is.na(x)) })
sum((drtotl_items_missing > (36 - 30)) & (drtotl_items_missing != 36))

# Try to reproduce drtotl scale scores

drtotl_test <- rowSums(data2$ders[, drtotl_items], na.rm = TRUE)
drtotl_test[drtotl_items_missing == 36] <- NA

for (i in 1:length(drtotl_test)) {
  if (drtotl_items_missing[i] > 0 & drtotl_items_missing[i] <= (36 - 6)) {
    drtotl_test[i] <- drtotl_test[i] + drtotl_items_missing[i]
  }
}

all(is.na(data2$ders$drtotl) == is.na(drtotl_test))
all(data2$ders$drtotl == drtotl_test, na.rm = TRUE)

# View participants with item-level missingness for drtotl

View(data2$ders[!is.na(data2$ders$drtotl) &
                  rowSums(is.na(data2$ders[, drtotl_items])) > 0, ])

# Compute drtotl_mean_imp based on mean of available items rather than adding
# value of 1 to the total score for each missing item. View discrepant values.
# drtotl_mean_imp, not drtotl, will be used in analyses in present manuscript.

data2$ders$drtotl_m_imp <- rowMeans(data2$ders[, drtotl_items], na.rm = TRUE)*36
data2$ders$drtotl_m_imp[is.nan(data2$ders$drtotl_m_imp)] <- NA

all(is.na(data2$ders$drtotl_m_imp) == is.na(drtotl_test))
sum(data2$ders[!is.na(data2$ders$drtotl_m_imp), "drtotl_m_imp"] != 
      drtotl_test[!is.na(drtotl_test)])

View(data2$ders[(!is.na(data2$ders$drtotl_m_imp) & !is.na(data2$ders$drtotl)) &
                  (data2$ders$drtotl_m_imp != data2$ders$drtotl), 
                c("drtotl", "drtotl_m_imp")])

# For DBT Skills Subscale (DSS) of DBT-WCCL, main outcomes paper analyzed the 
# mean of available items if at least 30 out of 38 items were not missing

meanDSS_items <- c("RWC01", "RWC02", "RWC04", "RWC06", "RWC11", "RWC12", 
                   "RWC13", "RWC15", "RWC18", "RWC21", "RWC22", "RWC24", 
                   "RWC25", "RWC27", "RWC34", "RWC35", "RWC38", "RWC43", 
                   "RWC45", "RWC46", "RWC47", "RWC48", "RWC50", "RWC51", 
                   "RWC52", "RWC54", "RWC56", "RWC57", "RWC60", "RWC63", 
                   "RWC65", "RWC66", "RWC68", "RWC69", "RWC71", "RWC73", 
                   "RWC74", "RWC75")
length(meanDSS_items)

# No meanDSS scale scores were not computed on account of too few items

meanDSS_items_missing <- apply(data2$dbt_wccl[, meanDSS_items],
                               MARGIN = 1, 
                               FUN = function(x) { sum(is.na(x)) })
sum((meanDSS_items_missing > (38 - 30)) & (meanDSS_items_missing != 38))

# Try to reproduce meanDSS scale scores

meanDSS_test <- rowMeans(data2$dbt_wccl[, meanDSS_items], na.rm = TRUE)
meanDSS_test[is.nan(meanDSS_test)] <- NA

all(is.na(data2$dbt_wccl$meanDSS) == is.na(meanDSS_test))
all(data2$dbt_wccl$meanDSS == meanDSS_test, na.rm = TRUE)

# For Control Subscale of DoSS, previous analyses analyzed the mean of available
# items if at least 3 out of 4 items were not missing

cnDoSS_items <- c("DoSS01", "rDoSS12", "DoSS16", "rDoSS23")
length(cnDoSS_items)

# No cnDoSS scale scores were not computed on account of too few items

cnDoSS_items_missing <- apply(data2$doss[, cnDoSS_items],
                              MARGIN = 1, 
                              FUN = function(x) { sum(is.na(x)) })
sum((cnDoSS_items_missing > (4 - 3)) & (cnDoSS_items_missing != 4))

# Try to reproduce cnDoSS scale scores

cnDoSS_test <- rowMeans(data2$doss[, cnDoSS_items], na.rm = TRUE)
cnDoSS_test[is.nan(cnDoSS_test)] <- NA

all(is.na(data2$doss$cnDoSS) == is.na(cnDoSS_test))
all(data2$doss$cnDoSS == cnDoSS_test, na.rm = TRUE)

# For Total Score of KIMS, previous analyses analyzed the mean of available
# items if at least 35 of 39 items were not missing

KMTOT_items <- c("KM01", "KM02", "KM03r", "KM04r", "KM05", "KM06", "KM07", 
                 "KM08r", "KM09", "KM10", "KM11r", "KM12r", "KM13", "KM14r", 
                 "KM15", "KM16r", "KM17", "KM18r", "KM19", "KM20r", "KM21", 
                 "KM22r", "KM23r", "KM24r", "KM25", "KM26", "KM27r", "KM28r", 
                 "KM29", "KM30", "KM31r", "KM32r", "KM33", "KM34", "KM35r", 
                 "KM36r", "KM37", "KM38", "KM39")
length(KMTOT_items)

# No KMTOT scale scores were not computed on account of too few items

KMTOT_items_missing <- apply(data2$kims[, KMTOT_items],
                              MARGIN = 1, 
                              FUN = function(x) { sum(is.na(x)) })
sum((KMTOT_items_missing > (39 - 35)) & (KMTOT_items_missing != 39))

# Try to reproduce KMTOT scale scores

KMTOT_test <- rowMeans(data2$kims[, KMTOT_items], na.rm = TRUE)
KMTOT_test[is.nan(KMTOT_test)] <- NA

all(is.na(data2$kims$KMTOT) == is.na(KMTOT_test))
all(data2$kims$KMTOT == KMTOT_test, na.rm = TRUE)

# View participants with item-level missingness for KMTOT

View(data2$kims[!is.na(data2$kims$KMTOT) &
                  rowSums(is.na(data2$kims[, KMTOT_items])) > 0, ])

# ---------------------------------------------------------------------------- #
# Compute item-level missingness ----
# ---------------------------------------------------------------------------- #

item_level(data2$ders, data2$ders$drtotl_m_imp, drtotl_items)
item_level(data2$dbt_wccl, data2$dbt_wccl$meanDSS, meanDSS_items)
item_level(data2$doss, data2$doss$cnDoSS, cnDoSS_items)
item_level(data2$kims, data2$kims$KMTOT, KMTOT_items)

# ---------------------------------------------------------------------------- #
# Compute n, M, and SD for each scale at each time point ----
# ---------------------------------------------------------------------------- #

# Run function for each scale

ag.drtotl_m_imp <- describe_by_time(data2$ders, "drtotl_m_imp")
ag.meanDSS <- describe_by_time(data2$dbt_wccl, "meanDSS")
ag.cnDoSS <- describe_by_time(data2$doss, "cnDoSS")
ag.KMTOT <- describe_by_time(data2$kims, "KMTOT")

# Build table

ag.all <- rbind(ag.drtotl_m_imp, ag.meanDSS, ag.cnDoSS, ag.KMTOT)

# Round to two decimal places and recode and rename period

table <- ag.all

table$M <- round(table$M, 2)
table$SD <- round(table$SD, 2)

table$Period[table$Period == 0] <- "Screening"
table$Period[table$Period == 2] <- "Pretreatment"
table$Period[table$Period == 3] <- "2-Month"
table$Period[table$Period == 4] <- "4-Month"
table$Period[table$Period == 5] <- "Follow-Up"

names(table)[names(table) == "Period"] <- "Assessment"

write.csv(table, "./results/table_s1.csv", row.names = FALSE)

# ---------------------------------------------------------------------------- #
# Compute scale-level missingness ----
# ---------------------------------------------------------------------------- #

# Proportions

prop_drtotl_m_imp <- sum(is.na(data2$ders$drtotl_m_imp))/(44*4)
prop_meanDSS <- sum(is.na(data2$dbt_wccl$meanDSS))/(44*4)
prop_cnDoSS <- sum(is.na(data2$doss$cnDoSS))/(44*4)
prop_KMTOT <- sum(is.na(data2$kims$KMTOT))/(44*4)

# Percentages

round(prop_drtotl_m_imp*100, 2)
round(prop_meanDSS*100, 2)
round(prop_cnDoSS*100, 2)
round(prop_KMTOT*100, 2)
# ---------------------------------------------------------------------------- #
# Compute Scores, Descriptives, and Missingness Rates
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

groundhog.library(car, groundhog_day)

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
      cat("\n", "\n")
    }
    
    cat("Period:")
    if ("Period" %in% names(dfs[[i]])) {
      print(table(dfs[[i]]["Period"], useNA = "always"))
      cat("\n")
    } else {
      cat("No such column")
      cat("\n", "\n")
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
  
  if ("AIN" %in% names(data[[i]])) {
    data2[[i]] <- data2[[i]][data2[[i]]$AIN == 1 & !is.na(data2[[i]]$AIN), ]
  } else {
    print(paste0("AIN is not in ", names(data[i])))
  }
}

names(data2) <- names(data)

report_AIN_Period(data2)

# Confirm ResearchIDs are identical across tables

for (i in 1:length(data2)) {
  if ("ResearchID" %in% names(data2[[i]])) {
    if (!setequal(unique(data2[["dbt_wccl"]][, "ResearchID"]), 
                 unique(data2[[i]][, "ResearchID"]))) {
      warning(paste0("ResearchIDs in ", names(data2[i]), " are different"))
    }
  } else {
    print(paste0("ResearchID not in ", names(data2[i])))
  }
}

# Add AIN to "thi" table

data2$thi <- merge(data2$thi, 
                   data2$dbt_wccl[, c("ResearchID", "Period", "AIN")],
                   by = c("ResearchID", "Period"),
                   all.x = TRUE)

# Exclude DERS pretreatment data (use DERS screening data instead)

table(data2$ders$Period)
data2$ders <- data2$ders[data2$ders$Period != 2, ]

report_AIN_Period(data2)

# Define time0 and time0_lag for each repeated-measures table (note that time0 
# is already defined in dbt_wccl table; the code below yields the same values)

for (i in 1:length(data2)) {
  if (names(data2[i]) %in% c("acs", "asi", "dbt_wccl", "doss", "dpss", "eis", 
                             "ess", "kims", "oasis", "oq", "phq", "staxi", 
                             "tas", "thi")) {
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
    data2[[i]]$time0_lag[data2[[i]]$Period == 0] <- NA
    data2[[i]]$time0_lag[data2[[i]]$Period == 3] <- 0
    data2[[i]]$time0_lag[data2[[i]]$Period == 4] <- 1
    data2[[i]]$time0_lag[data2[[i]]$Period == 5] <- 2
  }
}

# ---------------------------------------------------------------------------- #
# Add "completer" to "doss" table ----
# ---------------------------------------------------------------------------- #

data2$doss <- merge(data2$doss, 
                    data2$dbt_wccl[, c("ResearchID", "Period", "completer")],
                    by = c("ResearchID", "Period"),
                    all.x = TRUE)

# ---------------------------------------------------------------------------- #
# Create factors for demographic auxiliary variables and compute race ----
# ---------------------------------------------------------------------------- #

# Create factors for nominal variables

data2$demog$DDS06_factor <-
  factor(data2$demog$DDS06,
         levels = as.character(c(1:7, 11:14)),
         labels = c("White/Caucasian", 
                    "Native American, American Indian or Alaska Native",
                    "Black or African American", 
                    "Chinese or Chinese American",
                    "Japanese or Japanese American", 
                    "Korean or Korean American",
                    "Other Asian or Asian American (India, Malaysia, Pakistan, ...",
                    "East Indian (counted as Asian)",
                    "Middle Eastern/Arab",
                    "Other",
                    "Native Hawaiian or Pacific Islander"))

data2$demog$DDS06c_factor <-
  factor(data2$demog$DDS06c,
         levels = as.character(c(1:7, 11:15)),
         labels = c("White/Caucasian", 
                    "Native American, American Indian or Alaska Native", 
                    "Black or African American", 
                    "Chinese or Chinese American", 
                    "Japanese or Japanese American", 
                    "Korean or Korean American", 
                    "Other Asian or Asian American (India, Malaysia, Pakistan, ...", 
                    "East Indian", 
                    "Middle Eastern/Arab", 
                    "Other", 
                    "Native Hawaiian or Pacific Islander", 
                    "More than one other racial group"))

data2$demog$SH132_factor <- 
  factor(data2$demog$SH132,
         levels = as.character(0:4),
         labels = c("Subject is uncertain", 
                    "Heterosexual", 
                    "Homosexual/Lesbian/Gay", 
                    "Bisexual", 
                    "Transgender"))

data2$demog$DDS06b_factor <- 
  factor(data2$demog$DDS06b,
         levels = as.character(0:1),
         labels = c("Not Hispanic or Latino",
                    "Yes"))

data2$demog$DDS10_factor <- 
  factor(data2$demog$DDS10,
         levels = as.character(0:1),
         labels = c("No",
                    "Yes"))

data2$demog$DDS14_factor <- 
  factor(data2$demog$DDS14,
         levels = as.character(1:5),
         labels = c("Single, never married", 
                    "Widowed", 
                    "Married", 
                    "Separated", 
                    "Divorced"))

data2$demog$DDS17a2_factor <- 
  factor(data2$demog$DDS17a2,
         levels = as.character(0:14),
         labels = c("Unemployed", 
                    "Professional, technical, e.g., clergy, engineer, teacher,...", 
                    "Owner, manager, administrator or executive of business (n...", 
                    "Sales, e.g., insurance, real estate, auto", 
                    "Clerical, e.g., secretary, retail clerk, typist", 
                    "Skilled worker, craftsperson, foreman (non-farm)", 
                    "Transport or equipment operator", 
                    "Unskilled worker, laborer (non-farm)", 
                    "Farm workers, e.g., farmer, farm laborer, farm manager or...", 
                    "Service worker, e.g., custodian, waitress, guard, barber", 
                    "Private household worker", 
                    "Full-time homemaker", 
                    "Full-time student", 
                    "Other", 
                    "Retired"))

data2$demog$DDS25_factor <- 
  factor(data2$demog$DDS25,
         levels = as.character(0:1),
         labels = c("No", 
                    "Yes"))

data2$demog$DDS26_factor <- 
  factor(data2$demog$DDS26,
         levels = as.character(0:1),
         labels = c("No", 
                    "Yes"))

# Compute single "race" column

data2$demog$race <- data2$demog$DDS06_factor
levels(data2$demog$race) <- c(levels(data2$demog$race), "More than one race")

for (i in 1:nrow(data2$demog)) {
  if (!is.na(data2$demog$DDS06c[i]) & 
      (as.character(data2$demog$DDS06_factor)[i] != 
       as.character(data2$demog$DDS06c_factor)[i])) {
    data2$demog$race[i] <- "More than one race"
  }
}

View(data2$demog[, c("ResearchID", "DDS06_factor", "DDS06c_factor", "race")])

# Create ordered factors for ordinal variables

data2$demog$DDS15a_factor <- 
  factor(data2$demog$DDS15a,
         levels = as.character(1:10),
         labels = c("eighth grade or less", 
                    "some high school", 
                    "GED", 
                    "high school graduate", 
                    "business or technical training beyond high school", 
                    "some college", 
                    "college graduate", 
                    "some graduate or professional school beyond college", 
                    "masters degree", 
                    "doctoral degree"),
         ordered = TRUE)

data2$demog$DDS16a_factor <- 
  factor(data2$demog$DDS16a,
         levels = as.character(1:8),
         labels = c("less than dollars5,000", 
                    "dollars5,000-9,999", 
                    "dollars10,000-14,999", 
                    "dollars15,000-19,999", 
                    "dollars20,000-24,999", 
                    "dollars25,000-29,999", 
                    "dollars30,000-49,999", 
                    "dollars50,000 or more"),
         ordered = TRUE)

# ---------------------------------------------------------------------------- #
# Investigate and compute average item scores for analysis variables ----
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

data2$ders$drtotl_m_imp <- rowMeans(data2$ders[, drtotl_items], na.rm = TRUE)
data2$ders$drtotl_m_imp[is.nan(data2$ders$drtotl_m_imp)] <- NA

data2$ders$drtotl_m_imp_sum <- data2$ders$drtotl_m_imp*36

all(is.na(data2$ders$drtotl_m_imp_sum) == is.na(drtotl_test))
sum(data2$ders[!is.na(data2$ders$drtotl_m_imp_sum), "drtotl_m_imp_sum"] != 
      drtotl_test[!is.na(drtotl_test)])

View(data2$ders[(!is.na(data2$ders$drtotl_m_imp_sum) & !is.na(data2$ders$drtotl)) &
                  (data2$ders$drtotl_m_imp_sum != data2$ders$drtotl), 
                c("drtotl", "drtotl_m_imp_sum")])

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
# Compute item sum scores ----
# ---------------------------------------------------------------------------- #

# Note: For DERS, drtotl_m_imp_sum already exists (see above)

data2$dbt_wccl$meanDSS_sum <- data2$dbt_wccl$meanDSS*length(meanDSS_items)
data2$doss$cnDoSS_sum <- data2$doss$cnDoSS*length(cnDoSS_items)
data2$kims$KMTOT_sum <- data2$kims$KMTOT*length(KMTOT_items)

# ---------------------------------------------------------------------------- #
# Save intermediate data ----
# ---------------------------------------------------------------------------- #

# Save data as R object

dir.create("./data/intermediate")
save(data2, file = "./data/intermediate/data2.Rdata")

# Save scale definitions as R object

scale_defs <- list(drtotl_items, meanDSS_items, cnDoSS_items, KMTOT_items)
names(scale_defs) <- c("drtotl_items", "meanDSS_items", "cnDoSS_items",
                       "KMTOT_items")

save(scale_defs, file = "./data/intermediate/scale_defs.Rdata")

# ---------------------------------------------------------------------------- #
# Compute item-level missingness ----
# ---------------------------------------------------------------------------- #

item_level(data2$ders, data2$ders$drtotl_m_imp, drtotl_items)
item_level(data2$dbt_wccl, data2$dbt_wccl$meanDSS, meanDSS_items)
item_level(data2$doss, data2$doss$cnDoSS, cnDoSS_items)
item_level(data2$kims, data2$kims$KMTOT, KMTOT_items)

# ---------------------------------------------------------------------------- #
# Compute n, M, and SD for each average item scores at each time point ----
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

write.csv(table, "./results/table_s2.csv", row.names = FALSE)

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
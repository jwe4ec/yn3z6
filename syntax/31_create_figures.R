# ---------------------------------------------------------------------------- #
# Create Figures
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

groundhog.library(ggplot2, groundhog_day)
groundhog.library(cowplot, groundhog_day)

# ---------------------------------------------------------------------------- #
# Define functions used throughout script ----
# ---------------------------------------------------------------------------- #

# Define function to plot raw scores over time

plot_raw <- function(var, title, y_title, scale_min, scale_max) {
  ggplot(data5$contemp_aux, 
         aes(x = time0, 
             y = .data[[var]],
             group = ResearchID,
             color = ResearchID)) +
    geom_line() +
    geom_point() +
    labs(title = title, 
         x = "Assessment",
         y = y_title) +
    scale_color_discrete(name = "ResearchID") +
    scale_y_continuous(breaks = scale_min:scale_max, 
                       limits = c(scale_min, scale_max)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "none")
}

# ---------------------------------------------------------------------------- #
# Import data and results ----
# ---------------------------------------------------------------------------- #

# Import unimputed data

load("./data/intermediate/data5.RData")

# Import results

load("./results/result_itt_max.RData")
load("./results/result_itt_red_meanDSS.RData")
load("./results/result_itt_red_cnDoSS.RData")
load("./results/result_itt_red_KMTOT.RData")

# ---------------------------------------------------------------------------- #
# Prepare data ----
# ---------------------------------------------------------------------------- #

# Code 999 back to NA

data5$contemp_aux[data5$contemp_aux == 999] <- NA

# Recode ResearchID as factor

data5$contemp_aux$ResearchID <- as.factor(data5$contemp_aux$ResearchID)

# ---------------------------------------------------------------------------- #
# Create figures ----
# ---------------------------------------------------------------------------- #

# Plot raw scores over time for the ITT sample

p_raw_drtotl_m_imp <- plot_raw("drtotl_m_imp", "Emotion Dysregulation",
                               "Average Item Score", 1, 5)
p_raw_meanDSS <- plot_raw("meanDSS", "Skills Use",
                          "Average Item Score", 0, 3)
p_raw_cnDoSS <- plot_raw("cnDoSS", "Perceived Control",
                         "Average Item Score", 0, 4)
p_raw_KMTOT <- plot_raw("KMTOT", "Mindfulness",
                        "Average Item Score", 1, 5)

p_raw_grid <- plot_grid(p_raw_drtotl_m_imp +
                          xlab(NULL), 
                        p_raw_meanDSS +
                          xlab(NULL) +
                          ylab(NULL), 
                        p_raw_cnDoSS,
                        p_raw_KMTOT +
                          ylab(NULL),
                        align = "hv",
                        ncol = 2)

ggsave2("./results/plot_itt_raw.png",
        plot = p_raw_grid,
        width = 10, height = 10)
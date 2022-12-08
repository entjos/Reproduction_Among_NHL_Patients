#//////////////////////////////////////////////////////////////////////////////
#
#//////////////////////////////////////////////////////////////////////////////

# clear memory
rm(list = ls())

# Load packages
library(stringr)
library(rstpm2)

source("./programs_and_log_files/user_defined_functions/fpm_plot_comparisions.R",
       echo = FALSE)

# Import data
fpms_nhl_sub_c5 <- readRDS("./data/analysis_data/fpms_nhl_sub_c5.RData")
fpms_nhl_sub_c7 <- readRDS("./data/analysis_data/fpms_nhl_sub_c7.RData")

load("./data/analysis_data/cox_models.R")

# 2. Create plots for clinical subtypes ---------------------------------------

fpm_plot_comparision(fpm_models = fpms_nhl_sub_c5,
                     cox_models = models_c5,
                     comparison = "hr",
                     text_xlab  = "Time since NHL diagnosis",
                     text_ylab  = "Hazard ratio",
                     text_title = c("Aggressive subtypes",
                                    "Indolent subtypes",
                                    "T-/NK-cell"),
                     ylim = c(0, 4),
                     xlim = c(0, 10),
                     file_path = "./graphs/fe_hr_c5.png")

# 3. Create plots for morphological subtypes ----------------------------------

fpm_plot_comparision(fpm_models = fpms_nhl_sub_c7,
                     cox_models = models_c7,
                     comparison = "hr",
                     text_xlab  = "Time since NHL diagnosis",
                     text_ylab  = "Hazard ratio",
                     text_title = c("DLBCL", "FL", "TNK", "Other B-cell"),
                     ylim = c(0, 4),
                     xlim = c(0, 10),
                     file_path = "./graphs/fe_hr_c7.png")

# /////////////////////////////////////////////////////////////////////////////
# END OF R-FILE

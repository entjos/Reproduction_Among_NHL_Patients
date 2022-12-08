#//////////////////////////////////////////////////////////////////////////////
#
#//////////////////////////////////////////////////////////////////////////////

# PREFIX ----------------------------------------------------------------------

# clear memory
rm(list = ls())

# Load packages
library(openxlsx)
library(haven)
library(dplyr)

source(paste0("./programs_and_log_files/user_defined_functions/",
              "save_to_excel_wb.R"))

mean_num_c7 <- readRDS(paste0("./data/analysis_data/",
                              "re_mean_num_nhl_sub_c7_risksets_delta.RData"))

mean_num_c5 <- readRDS(paste0("./data/analysis_data/",
                              "re_mean_num_nhl_sub_c5_risksets_delta.RData"))

# 1. Get CID for 2, 5, 10 years -----------------------------------------------

# Define function to get closesed value
closest <- function(x, value){
  
  x[which(abs(x - value) == min(abs(x - value)))]
  
}

#   1.1 For clinical subtypes =================================================

subtype_c5 <- c("agg_sub", "ind_sub")

for(i in 1:2){
  
  # Import data from Stata
  std_cif <- mean_num_c5[[subtype_c5[[i]]]]
  
  # Get estimates
  cif_selection <- std_cif %>% 
    filter(t %in% c(closest(std_cif$t, 2),
                    closest(std_cif$t, 5),
                    closest(std_cif$t, 10))) %>% 
    mutate(t = round(t, 1))
  
  save_to_excel_wb(cif_selection,
                   "./tables/re_mean_num.xlsx",
                   sheet_name = paste0("nhl_sub_c5_risksets_", i))
  
}

#   1.1 For morphological subtypes ============================================

subtype_c7 <- c("DLBCL", "FL", "TNK")

for(i in 1:3){
  
  # Import data from Stata
  std_cif <- mean_num_c7[[subtype_c7[[i]]]]
  
  # Get estimates
  cif_selection <- std_cif %>% 
    filter(t %in% c(closest(std_cif$t, 2),
                    closest(std_cif$t, 5),
                    closest(std_cif$t, 10))) %>% 
    mutate(t = round(t, 1))
  
  save_to_excel_wb(cif_selection,
                   "./tables/re_mean_num.xlsx",
                   sheet_name = paste0("nhl_sub_c7_risksets_", i))
  
}

# /////////////////////////////////////////////////////////////////////////////
# END OF R-SCRIPT
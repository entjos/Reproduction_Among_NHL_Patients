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

# 1. Get CID for 2, 5, 10 years -----------------------------------------------

# Define function to get closesed value
closest <- function(x, value){
  
  x[which(abs(x - value) == min(abs(x - value)))]
  
}

#   1.1 For clinical subtypes =================================================

for(i in 1:2){
  
  # Import data from Stata
  std_cif <- read_dta(paste0("./data/analysis_data/",
                             "fe_csCIF_nhl_sub_c5_risksets_", i, ".dta"))
  
  # Get estimates
  cif_selection <- std_cif %>% 
    filter(t %in% c(closest(std_cif$t, 2),
                    closest(std_cif$t, 5),
                    closest(std_cif$t, 10))) %>% 
    mutate(t = round(t, 1))
  
  save_to_excel_wb(cif_selection,
                   "./tables/fe_std_cif.xlsx",
                   sheet_name = paste0("nhl_sub_c5_risksets_", i))
  
}

#   1.1 For morphological subtypes ============================================

for(i in 1:3){
  
  # Import data from Stata
  std_cif <- read_dta(paste0("./data/analysis_data/",
                             "fe_csCIF_nhl_sub_c7_risksets_", i, ".dta"))
  
  # Get estimates
  cif_selection <- std_cif %>% 
    filter(t %in% c(closest(std_cif$t, 2),
                    closest(std_cif$t, 5),
                    closest(std_cif$t, 10))) %>% 
    mutate(t = round(t, 1))
  
  save_to_excel_wb(cif_selection,
                   "./tables/fe_std_cif.xlsx",
                   sheet_name = paste0("nhl_sub_c7_risksets_", i))
  
}

# /////////////////////////////////////////////////////////////////////////////
# END OF R-SCRIPT
#//////////////////////////////////////////////////////////////////////////////
#
#//////////////////////////////////////////////////////////////////////////////

# 1. Prefix -------------------------------------------------------------------

# clear memory
rm(list = ls())

# load packages
library(dplyr)
library(haven)
library(lubridate)
library(data.table)
library(openxlsx)

# Load exclusion table package
source("./programs_and_log_files/user_defined_functions/exclusion_table.R",
       echo = FALSE)

# Import merged data sets
st_data <- readRDS("./data/analysis_data/analysis_data.RData")

# 2. Calculate survival times -------------------------------------------------

st_data <- st_data %>% 
  mutate(ac_dt = case_when(country == "SE"  ~ ymd("2019-12-31"),
                           country == "NOR" ~ ymd("2017-12-31"),
                           country == "DK"  ~ ymd("2018-12-31")),
         
         # Restrict follow-up to 10 yrs
         fup_10_dt = entry_dt %m+% years(10),
         
         # Note 1
         exit_dt    = pmin(cb_1_dt, death_dt,
                           dx_dt_comp, fup_10_dt, ac_dt, 
                           na.rm = TRUE),
         # Define exit event
         exit_event = case_when(exit_dt == cb_1_dt    ~ "CB",
                                exit_dt == death_dt   ~ "death",
                                exit_dt == dx_dt_comp ~ "dx_dt comp",
                                exit_dt == ac_dt      ~ "END",
                                exit_dt == fup_10_dt  ~ "END"),
         
         # Define indicator for CB vs competing event
         exit_event_c2 = case_when(exit_event %in% c("CB")    ~ 1,
                                   exit_event %in% c("death") ~ 2,
                                   TRUE                       ~ 0),
         
         # Define indicator for exit event is CB
         exit_event_cb = if_else(exit_event == "CB", 1, 0),
         
         # Define survival time from time since dx
         st_yrs     = time_length(entry_dt %--% exit_dt,
                                  unit = "year"),
         
         # Define survival time from time since dx + 9 months
         st_yrs_9m  = time_length(entry_dt %m+% months(9) %--% exit_dt,
                                  unit = "year"),
         
         # Add one day of survival for individuals with survival time eq 0
         st_yrs_9m  = if_else(st_yrs_9m == 0, 1/365.24, st_yrs_9m))

# 3. Export data --------------------------------------------------------------

saveRDS(  st_data, "./data/analysis_data/fe_st_data.RData")
write_dta(st_data, "./data/analysis_data/fe_st_data.dta")

#'/////////////////////////////////////////////////////////////////////////////
#' NOTES:
#'
#'/////////////////////////////////////////////////////////////////////////////
#' END OF R-FILE
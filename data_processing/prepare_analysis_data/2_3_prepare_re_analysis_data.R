#//////////////////////////////////////////////////////////////////////////////
#
#//////////////////////////////////////////////////////////////////////////////

# 1. Prefix -------------------------------------------------------------------

# clear memory
rm(list = ls())

# load packages
library(dplyr)
library(tidyr)
library(haven)
library(lubridate)

# Load exclusion table package
source("./programs_and_log_files/user_defined_functions/exclusion_table.R",
       echo = FALSE)

# Import merged data sets
st_data <- readRDS("./data/analysis_data/analysis_data.RData")

# 2. Calculate survival times -------------------------------------------------

st_data <- st_data %>% 
  # Prepare data for recurrent event analysis
  pivot_longer(cols = matches("cb_\\d+_dt"),
               names_to = "no_cb_event",
               values_to = "cb_dt") %>% 
  mutate(cb_dt = if_else(!is.na(lag(cb_dt)) & lag(cb_dt) == cb_dt, 
                         cb_dt %m+% days(1), 
                         cb_dt)) %>% 
  arrange(lopnr) %>% 
  # Calculate survival time
  mutate(end_of_fup  = case_when(country == "SE"  ~ ymd("2019-12-31"),
                                 country == "NOR" ~ ymd("2017-12-31"),
                                 country == "DK"  ~ ymd("2018-12-31")),
         
         # Restrict follow-up to 10 yrs
         max_fup_10  = entry_dt %m+% years(10),
         
         # Note 1
         exit_dt     = pmin(cb_dt, death_dt,
                            dx_dt_comp, max_fup_10, end_of_fup, 
                            na.rm = TRUE),
         
         st_yrs_end  = time_length(entry_dt %--% exit_dt,
                                   unit = "year"),
         
         no_cb_event = gsub("(.+)(\\d)(.+)", "\\2", no_cb_event),
         
         no_cb_event = as.numeric(no_cb_event),
         
         # Set start time as the date of previouse event for individuals with
         # multiple events
         st_yrs_strt = if_else(no_cb_event == 1, 
                               0,
                               lag(st_yrs_end)),
         
         exit_event = case_when(exit_dt == cb_dt      ~ "CB",
                                exit_dt == death_dt   ~ "death",
                                exit_dt == dx_dt_comp ~ "dx_dt comp",
                                exit_dt == end_of_fup ~ "END",
                                exit_dt == max_fup_10 ~ "END"),
         
         # Define indicator for CB vs competing event
         exit_event_c2 = case_when(exit_event %in% c("CB")    ~ 1,
                                   exit_event %in% c("death") ~ 2,
                                   TRUE                       ~ 0))

# 3. Filter individuals cesored prior to second CB ----------------------------

# Exclude episodes for individuals that have been censored in a previous 
# episode
exclusions2 <- exclusion_table(st_data,
                               exclusion_criteria = "st_yrs_strt == st_yrs_end")

exclusions2$table_ex

st_data <- exclusions2$dataset

# 4. Export data --------------------------------------------------------------

saveRDS(st_data, "./data/analysis_data/re_st_data.RData")
write_dta(st_data, "./data/analysis_data/re_st_data.dta")

#'/////////////////////////////////////////////////////////////////////////////
#' NOTES:
#'
#'/////////////////////////////////////////////////////////////////////////////
#' END OF R-FILE
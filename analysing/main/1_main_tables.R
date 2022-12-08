#//////////////////////////////////////////////////////////////////////////////
#
#//////////////////////////////////////////////////////////////////////////////

# 1. Prefix -------------------------------------------------------------------

# clear memory
rm(list = ls())

# load packages
library(lubridate)
library(forcats)
library(openxlsx)
library(dplyr)
source("./programs_and_log_files/user_defined_functions/summary_table.R")
source(paste0("./programs_and_log_files/user_defined_functions/",
              "summary_table_to_workbook.R"))

# Import merged dataset
index_pop <- readRDS("./data/merged_data/index_pop.RData")

# Import survival datasets
fe_st_data <- readRDS("./data/analysis_data/fe_st_data.RData")
# re_st_data <- readRDS("./data/analysis_data/re_st_data.RData")

# 2. Prepare datasets ---------------------------------------------------------

#   2.1 Merged dataset ========================================================

fe_st_data$no_cb_c3 <- fct_lump_n(fe_st_data$no_cb_c5, 2, other_level = ">1")

#   2.2 Data for RC table =====================================================

# # Get no. subsequent childbirth and total time of follow-up
# re_st_data <- re_st_data %>% 
# group_by(lopnr) %>% 
#   summarise(no_exit_event_cb = sum(exit_event == "CB"),
#             st_yrs_re        = max(st_yrs_end))
# 
# # Add information for RE dataset
# fe_st_data <- fe_st_data %>% 
#   left_join(re_st_data, by = "lopnr")

# 3. Create tables  -----------------------------------------------------------

#   3.1 Table 1 ===============================================================

table1_country <- summary_table(fe_st_data,
                                vars = c("age_dx_c3", "year_dx_c5", "no_cb_c5", 
                                         "no_cb_c3", "nulliparous", "female",
                                         "nhl_sub_c5", "nhl_sub_c7"),
                                strata  = c("country", "case"))

table1_nhl_sub_c5 <- summary_table(fe_st_data,
                                vars = c("age_dx_c3", "year_dx_c5", "no_cb_c5", 
                                         "no_cb_c3", "nulliparous", "female",
                                         "nhl_sub_c5"),
                                strata  = c("nhl_sub_c5_risksets", "case"))

# Shorten names for Excel workbook
names(table1_nhl_sub_c5) <- gsub("_risksets", "", names(table1_nhl_sub_c5))

table1_overall <- summary_table(fe_st_data,
                                vars = c("age_dx_c3", "year_dx_c5", "no_cb_c5", 
                                         "no_cb_c3", "nulliparous", "female",
                                         "nhl_sub_c5", "nhl_sub_c7"),
                                strata  = c("case"))

summary_table_to_workbook("./tables/table1.xlsx",
                          c(table1_country, table1_overall))

summary_table_to_workbook("./tables/table1_nhl_sub.xlsx",
                          c(table1_nhl_sub_c5, table1_overall))

#   3.2 Table 2 ===============================================================

table2 <- summary_table(fe_st_data,
                        vars = c("age_dx_c3", "year_dx_c5", 
                                 "stage_c3", "perfwho_c3",
                                 "no_cb_c5", "country", "female",
                                 "nulliparous", "nhl_sub_c7"),
                        strata = c("nhl_sub_c7"),
                        overall = TRUE,
                        get_rates = TRUE,
                        event = "exit_event_cb",
                        st = "st_yrs")

summary_table_to_workbook("./tables/table2.xlsx",
                          table2)

#   3.3 Table 3 ===============================================================

table3 <- summary_table(fe_st_data,
                        vars = c("age_dx_c3", "year_dx_c5", 
                                 "stage_c3", "perfwho_c3",
                                 "no_cb_c5", "country", "female",
                                 "nulliparous", "nhl_sub_c5"),
                        strata = c("nhl_sub_c5"),
                        overall = TRUE,
                        get_rates = TRUE,
                        event = "exit_event_cb",
                        st = "st_yrs")

summary_table_to_workbook("./tables/table3.xlsx",
                          table3)

#   3.4 Create Table 3 for recurrent event analysis ===========================

# table3_re <- summary_table(fe_st_data,
#                         vars = c("age_dx_c3", "year_dx_c5", "year_dx_c4",
#                                  "stage_c3", "perfwho_c3",
#                                  "no_cb_c5", "country", "female",
#                                  "nulliparous", "ctreg_c4"),
#                         strata = c("ctreg_c4"),
#                         overall = TRUE,
#                         get_rates = TRUE,
#                         event = "no_exit_event_cb",
#                         st = "st_yrs_re")
# 
# summary_table_to_workbook("./tables/table3_re.xlsx",
#                           table3_re)

#'/////////////////////////////////////////////////////////////////////////////
#'NOTES
#'
#'/////////////////////////////////////////////////////////////////////////////
#'END OF R-FILE
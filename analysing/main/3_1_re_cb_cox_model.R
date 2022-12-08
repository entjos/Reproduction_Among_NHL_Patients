#//////////////////////////////////////////////////////////////////////////////
#
#//////////////////////////////////////////////////////////////////////////////

# 1. Prefix -------------------------------------------------------------------

# clear memory
rm(list = ls())

# load packages
library(dplyr)
library(tibble)
library(data.table)
library(purrr)
library(lubridate)
library(openxlsx)
library(survival)
library(Epi)
library(broom)

source(paste0("./programs_and_log_files/user_defined_functions/",
              "compare_model_estimates.R"))

source(paste0("./programs_and_log_files/user_defined_functions/",
              "save_to_excel_wb.R"))

source(paste0("./programs_and_log_files/user_defined_functions/",
              "summary_table.R"))

source(paste0("./programs_and_log_files/user_defined_functions/",
              "summary_table_to_workbook.R"))

# Import analysis data
re_st_data <- readRDS("./data/analysis_data/re_st_data.RData")

re_st_data_tsplit <- survSplit(Surv(re_st_data$st_yrs_strt, 
                                    re_st_data$st_yrs_end, 
                                    re_st_data$exit_event == "CB", 
                                    type = "counting") ~ female +
                                 case +
                                 nhl_sub_c5_risksets +
                                 nhl_sub_c7_risksets +
                                 lopnr,
                               data = re_st_data,
                               cut  = c(3, 10),
                               episode = "timeband")

re_st_data_tsplit$timeband <- factor(re_st_data_tsplit$timeband)

re_st_data_tsplit <- fastDummies::dummy_columns(re_st_data_tsplit,
                                                select_columns = "timeband")

# 1. Estimate Cox model -------------------------------------------------------

#   1.1 Estimate model for clinical subtypes ==================================

models_c5 <- 
  by(re_st_data_tsplit,
     re_st_data_tsplit$nhl_sub_c5_risksets,
     function(x) {coxph(Surv(tstart, tstop, event) ~ timeband * female +
                          female:timeband:case,
                        robust = TRUE,
                        cluster = lopnr,
                        data = x)})

# Test for interaction
models_c5_no_interaction <- 
  by(re_st_data_tsplit,
     re_st_data_tsplit$nhl_sub_c5_risksets,
     function(x) {coxph(Surv(tstart, tstop, event) ~ timeband + 
                          timeband:case,
                        robust = TRUE,
                        cluster = lopnr,
                        data = x)})

mapply(model1 = models_c5,
       model2 = models_c5_no_interaction,
       function(model1, model2) anova(model1, model2, test = "LRT"),
       SIMPLIFY = FALSE)

# Test for proportionality
lapply(models_c5, cox.zph)

save_to_excel_wb(compare_model_estimates(models_c5),
                 "./tables/re_cox_summary_combined.xlsx",
                 sheet_name = "r_output_cox_model_c5")

#   1.2 Estimate model for morphological subtypes =============================

models_c7 <- 
  by(re_st_data_tsplit,
     re_st_data_tsplit$nhl_sub_c7_risksets,
     function(x) {coxph(Surv(tstart, tstop, event) ~ timeband * female +
                          female:timeband:case,
                        robust = TRUE,
                        cluster = lopnr,
                        data = x)})

# Test for interaction
models_c7_no_interaction <- 
  by(re_st_data_tsplit,
     re_st_data_tsplit$nhl_sub_c7_risksets,
     function(x) {coxph(Surv(tstart, tstop, event) ~ timeband + 
                          timeband:case,
                        robust = TRUE,
                        cluster = lopnr,
                        data = x)})

mapply(model1 = models_c7,
       model2 = models_c7_no_interaction,
       function(model1, model2) anova(model1, model2, test = "LRT"),
       SIMPLIFY = FALSE)

# Test for proportionality
# lapply(models_c7, cox.zph) 
# error: Fejl i cox.zph(.) : exp overflow due to covariates

save_to_excel_wb(compare_model_estimates(models_c7),
                 "./tables/re_cox_summary_combined.xlsx",
                 sheet_name = "r_output_cox_model_c7")

# 2. Create tables ------------------------------------------------------------

re_st_data$female <- as.numeric(re_st_data$female) - 1

# Collapse data to obtain maximum time of follow-up for each individual
re_st_data_collapsed <- re_st_data %>% 
  group_by(lopnr, riskset)         %>% 
  mutate(st_yrs_max = max(st_yrs_end)) %>% 
  slice_head(n = 1)

#   2.1 for clinical subtypes =================================================

table_model_c5 <- summary_table(re_st_data_collapsed,
                                vars = c("case"),
                                strata = c("nhl_sub_c5_risksets", "female"),
                                overall = TRUE,
                                get_rates = TRUE,
                                event = "no_cb",
                                st = "st_yrs_max")

table_model_c5 <- setNames(table_model_c5, 
                           gsub("nhl_sub_", "", names(table_model_c5)))

summary_table_to_workbook("./tables/re_cox_summary_combined.xlsx", table_model_c5)

#   2.2 For morphological subtypes ============================================

table_model_c7 <- summary_table(re_st_data_collapsed,
                                vars = c("case"),
                                strata = c("nhl_sub_c7_risksets", "female"),
                                overall = TRUE,
                                get_rates = TRUE,
                                event = "no_cb",
                                st = "st_yrs_max")

table_model_c7 <- setNames(table_model_c7, 
                           gsub("nhl_sub_", "", names(table_model_c7)))

summary_table_to_workbook("./tables/re_cox_summary_combined.xlsx", table_model_c7)

# 3. Save models for further processing ---------------------------------------

save(models_c5, models_c7,
     file = "./data/analysis_data/re_cox_models.R")

# /////////////////////////////////////////////////////////////////////////////
# END OF R-SCRIPT
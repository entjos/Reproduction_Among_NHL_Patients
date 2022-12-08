#//////////////////////////////////////////////////////////////////////////////
#
#//////////////////////////////////////////////////////////////////////////////

# 1. Prefix -------------------------------------------------------------------

# clear memory
rm(list = ls())

# load packages
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(lubridate)
library(openxlsx)
library(survival)
library(Epi)
library(broom)
library(rateratio.test)
library(haven)

source(paste0("./programs_and_log_files/user_defined_functions/",
              "compare_model_estimates.R"))

source(paste0("./programs_and_log_files/user_defined_functions/",
              "save_to_excel_wb.R"))

source(paste0("./programs_and_log_files/user_defined_functions/",
              "summary_table.R"))

source(paste0("./programs_and_log_files/user_defined_functions/",
              "summary_table_to_workbook.R"))

# Import analysis data
fe_st_data <- readRDS("./data/analysis_data/fe_st_data.RData")

fe_st_data_tsplit <- survSplit(Surv(st_yrs, exit_event == "CB") ~ female +
                                 case +
                                 nhl_sub_c5_risksets +
                                 nhl_sub_c7_risksets +
                                 female_1 +
                                 case_1   +
                                 lopnr +
                                 riskset,
                               data = fe_st_data,
                               cut  = c(3, 10),
                               episode = "timeband")

fe_st_data_tsplit$timeband <- factor(fe_st_data_tsplit$timeband)

fe_st_data_tsplit <- fastDummies::dummy_columns(fe_st_data_tsplit,
                                                select_columns = "timeband")

# Export data to Stata for double coding
write_dta(fe_st_data_tsplit, 
          "./data/analysis_data/fe_st_data_tsplit.dta")

# 1. Estimate Cox model -------------------------------------------------------

#   1.1 Estimate model for clinical subtypes ==================================

models_c5 <- 
  by(fe_st_data_tsplit,
     fe_st_data_tsplit$nhl_sub_c5_risksets,
     function(x) {coxph(Surv(tstart, st_yrs, event) ~ timeband * female +
                          female:timeband:case, 
                        data = x)})

# Test for interaction
models_c5_no_interaction <- 
  by(fe_st_data_tsplit,
     fe_st_data_tsplit$nhl_sub_c5_risksets,
     function(x) {coxph(Surv(tstart, st_yrs, event) ~ timeband + 
                          timeband:case, 
                        data = x)})

mapply(model1 = models_c5,
       model2 = models_c5_no_interaction,
       function(model1, model2) anova(model1, model2, test = "LRT"),
       SIMPLIFY = FALSE)

# Test for proportionality
lapply(models_c5, cox.zph)

# Export model estimates to excel
save_to_excel_wb(compare_model_estimates(models_c5),
                 "./tables/cox_summary_c5.xlsx",
                 sheet_name = "r_output_cox_model")

save_to_excel_wb(compare_model_estimates(models_c5),
                 "./tables/cox_summary_combined.xlsx",
                 sheet_name = "r_output_cox_model_c5")

#   1.2 Estimate model for morphological subtypes =============================

models_c7 <- 
  by(fe_st_data_tsplit,
     fe_st_data_tsplit$nhl_sub_c7_risksets,
     function(x) {coxph(Surv(tstart, st_yrs, event) ~ timeband * female +
                          female:timeband:case,
                        data = x)})

# Test for interaction
models_c7_no_interaction <- 
  by(fe_st_data_tsplit,
     fe_st_data_tsplit$nhl_sub_c7_risksets,
     function(x) {coxph(Surv(tstart, st_yrs, event) ~ timeband + 
                          timeband:case, 
                        data = x)})

mapply(model1 = models_c7,
       model2 = models_c7_no_interaction,
       function(model1, model2) anova(model1, model2, test = "LRT"),
       SIMPLIFY = FALSE)

# Test for proportionality
lapply(models_c7, cox.zph)

# Export model estimates to excel
save_to_excel_wb(compare_model_estimates(models_c7),
                 "./tables/cox_summary_c7.xlsx",
                 sheet_name = "r_output_cox_model")

save_to_excel_wb(compare_model_estimates(models_c7),
                 "./tables/cox_summary_combined.xlsx",
                 sheet_name = "r_output_cox_model_c7")

# 2. Create tables ------------------------------------------------------------

fe_st_data$female <- as.numeric(fe_st_data$female) - 1

#   2.1 for clinical subtypes =================================================

table_c5 <- summary_table(fe_st_data,
                          vars = c("case"),
                          strata = c("nhl_sub_c5_risksets", "female"),
                          overall = TRUE,
                          get_rates = TRUE,
                          event = "exit_event_cb",
                          st = "st_yrs")

# Add test of difference in rates
table_c5_rrtest <- 
  lapply(seq_along(table_c5),
         function(i){
           
           temp <- rateratio.test(x = table_c5[[i]][["no_events"]],
                                  n = table_c5[[i]][["t_at_risk"]])
           
           
           cbind(table_c5[[i]],
                 data.frame(p_value = temp$p.value))
           
         })

table_c5_rrtest <- setNames(table_c5_rrtest, 
                            gsub("nhl_sub_", "", names(table_c5)))

summary_table_to_workbook("./tables/cox_summary_c5.xlsx", table_c5_rrtest)
summary_table_to_workbook("./tables/cox_summary_combined.xlsx", table_c5_rrtest)

#   2.2 For morphological subtypes ============================================

table_c7 <- summary_table(fe_st_data,
                          vars = c("case"),
                          strata = c("nhl_sub_c7_risksets", "female"),
                          overall = TRUE,
                          get_rates = TRUE,
                          event = "exit_event_cb",
                          st = "st_yrs")

# Add test of difference in rates
table_c7_rrtest <- 
  lapply(seq_along(table_c7),
         function(i){
           
           temp <- rateratio.test(x = table_c7[[i]][["no_events"]],
                                  n = table_c7[[i]][["t_at_risk"]])
           
           
           cbind(table_c7[[i]],
                 data.frame(p_value = temp$p.value))
           
         })

table_c7_rrtest <- setNames(table_c7_rrtest, 
                            gsub("nhl_sub_", "", names(table_c7)))

summary_table_to_workbook("./tables/cox_summary_c7.xlsx", table_c7_rrtest)
summary_table_to_workbook("./tables/cox_summary_combined.xlsx", table_c7_rrtest)

# 3. Save models for further processing ---------------------------------------

save(models_c5, models_c7,
     file = "./data/analysis_data/cox_models.R")

# /////////////////////////////////////////////////////////////////////////////
# END OF R-SCRIPT
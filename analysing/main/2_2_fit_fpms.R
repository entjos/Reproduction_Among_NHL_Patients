#//////////////////////////////////////////////////////////////////////////////
#
#//////////////////////////////////////////////////////////////////////////////

# 1. Prefix -------------------------------------------------------------------

# clear memory
rm(list = ls())

# load packages
library(dplyr)
library(data.table)
library(purrr)
library(lubridate)
library(openxlsx)
library(survival)
library(rstpm2)
library(Epi)
library(RColorBrewer)

source(paste0("./programs_and_log_files/user_defined_functions/",
              "fpm_test_dfs.R"))

# Define function for obtaining minimum AIC and BIC
min_aic_bic <- function(x){
  
  rbind(x[x[["aic"]] == min(x[["aic"]]), ],
        x[x[["bic"]] == min(x[["bic"]]), ])
  
}

# Import analysis data
st_data <- readRDS("./data/analysis_data/fe_st_data.RData")

# 1. Define formular ----------------------------------------------------------

# Childbirth
surv_formula_cb <- formula(Surv(st_yrs, 
                                exit_event == "CB") ~ 
                             female_1 + 
                             case_1   + 
                             female_1_case_1)

# Competing events
surv_formula_ce <- formula(Surv(st_yrs, 
                                exit_event == "death") ~ 
                             female_1 + 
                             case_1   + 
                             female_1_case_1)

# 1. Clinical subtypes --------------------------------------------------------

#   1.1 Childbirth ============================================================

#     1.1.1 Test BH ###########################################################
fpm_c5_bh_cb <- fpm_test_dfs(surv_formula_cb,
                             dfs_bh  = 1:10,
                             by_vars = "nhl_sub_c5_risksets",
                             data    = st_data)

lapply(fpm_c5_bh_cb, min_aic_bic)

#     1.1.2 Test TVC ##########################################################
fpm_c5_tvc_cb <- fpm_test_dfs(surv_formula_cb,
                              dfs_bh  = 3,
                              dfs_tvc = list(case_1          = 1:10,
                                             female_1        = 1:10,
                                             female_1_case_1 = 1:10),
                              same_dfs_tvc = TRUE,
                              by_vars = "nhl_sub_c5_risksets",
                              data    = st_data)

lapply(fpm_c5_tvc_cb, min_aic_bic)

#     1.1.3 Fit model #########################################################
fpms_nhl_sub_c5 <- by(st_data,
                      st_data$nhl_sub_c5_risksets,
                      function(x){
                        stpm2(formula = surv_formula_cb,
                              df = 3,
                              tvc = list(case_1          = 2,
                                         female_1        = 2,
                                         female_1_case_1 = 2),
                              data = x)
                      })

saveRDS(fpms_nhl_sub_c5, "./data/analysis_data/fpms_nhl_sub_c5.RData")

#   1.2 Competing events ======================================================

#     1.2.1 Test BH ###########################################################
fpm_c5_bh_ce <- fpm_test_dfs(surv_formula_ce,
                             dfs_bh  = 1:10,
                             by_vars = "nhl_sub_c5_risksets",
                             data    = st_data)

lapply(fpm_c5_bh_ce, min_aic_bic)

#     1.2.2 Test TVC ##########################################################
fpm_c5_tvc_ce <- fpm_test_dfs(surv_formula_ce,
                              dfs_bh  = 3,
                              dfs_tvc = list(case_1          = 1:10,
                                             female_1        = 1:10,
                                             female_1_case_1 = 1:10),
                              same_dfs_tvc = TRUE,
                              by_vars = "nhl_sub_c5_risksets",
                              data    = st_data)

lapply(fpm_c5_tvc_ce, min_aic_bic)

# 2. Morphological subtypes ---------------------------------------------------

#   2.1 Childbirth ============================================================

#     2.1.1 Test BH ###########################################################
fpm_c7_bh_cb <- fpm_test_dfs(surv_formula_cb,
                             dfs_bh  = 1:10,
                             by_vars = "nhl_sub_c7_risksets",
                             data    = st_data)

lapply(fpm_c7_bh_cb, min_aic_bic)

#     2.1.2 Test TVC ##########################################################
fpm_c7_tvc_cb <- fpm_test_dfs(surv_formula_cb,
                              dfs_bh  = 3,
                              dfs_tvc = list(case_1          = 1:10,
                                             female_1        = 1:10,
                                             female_1_case_1 = 1:10),
                              same_dfs_tvc = TRUE,
                              by_vars = "nhl_sub_c7_risksets",
                              data    = st_data)

lapply(fpm_c7_tvc_cb, min_aic_bic)

#     2.1.3 Fit model #########################################################
fpms_nhl_sub_c7 <- by(st_data,
                      st_data$nhl_sub_c7_risksets,
                      function(x){
                        stpm2(formula = surv_formula_cb,
                              df = 3,
                              tvc = list(case_1          = 2,
                                         female_1        = 2,
                                         female_1_case_1 = 2),
                              data = x)
                      })

saveRDS(fpms_nhl_sub_c7, "./data/analysis_data/fpms_nhl_sub_c7.RData")

#   2.2 Competing events ======================================================

#     2.2.1 Test BH ###########################################################
fpm_c7_bh_ce <- fpm_test_dfs(surv_formula_ce,
                             dfs_bh  = 1:10,
                             by_vars = "nhl_sub_c7_risksets",
                             data    = st_data)

lapply(fpm_c7_bh_ce, min_aic_bic)

#     2.2.2 Test TVC ##########################################################
fpm_c7_tvc_ce <- fpm_test_dfs(surv_formula_ce,
                              dfs_bh  = 2,
                              dfs_tvc = list(case_1          = 1:10,
                                             female_1        = 1:10,
                                             female_1_case_1 = 1:10),
                              same_dfs_tvc = TRUE,
                              by_vars = "nhl_sub_c7_risksets",
                              data    = st_data)

lapply(fpm_c7_tvc_ce, min_aic_bic)

# /////////////////////////////////////////////////////////////////////////////
# END OF RFILES
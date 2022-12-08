#//////////////////////////////////////////////////////////////////////////////
#
#//////////////////////////////////////////////////////////////////////////////

# 1. Prefix -------------------------------------------------------------------

# clear memory
rm(list = ls())

# load packages
library(dplyr)
library(rstpm2)
library(fastDummies)
library(lubridate)
library(tidyr)

source(paste0("./programs_and_log_files/user_defined_functions/",
              "fpm_test_dfs.R"))

# Import analysis data
st_re_data <- readRDS("./data/analysis_data/re_st_data.RData")
st_fe_data <- readRDS("./data/analysis_data/fe_st_data.RData")

# 2. Prepare datasets for analysis --------------------------------------------

# Prepare survival time for competing event model
st_fe_data <- st_fe_data %>% 
  mutate(exit_dt_death = pmin(death_dt, ac_dt, fup_10_dt, 
                              na.rm = TRUE),
         event         = if_else(exit_dt_death == death_dt & !is.na(death_dt), 
                                 1, 0),
         st_yrs_end    = time_length(entry_dt %--% exit_dt_death,
                                     unit = "year"),
         st_yrs_strt   = 0,
         state_cb = 0) %>% 
  select(lopnr, riskset, case_1, nhl_sub_c5_risksets, nhl_sub_c7_risksets, 
         female_1, female_1_case_1, state_cb, event, st_yrs_strt, st_yrs_end)

# Prepare data for recurrent event model
st_re_data <- st_re_data %>% 
  mutate(state_cb = 1,
         event    = if_else(exit_event == "CB", 1, 0)) %>% 
  select(lopnr, riskset, case_1, nhl_sub_c5_risksets, nhl_sub_c7_risksets, 
         female_1, female_1_case_1, state_cb, event, st_yrs_strt, st_yrs_end)

# Combine datasets
st_data <- bind_rows(st_re_data, st_fe_data) %>% 
  arrange(lopnr, riskset)

# 3. Define prediction functions ----------------------------------------------

#   3.1 Estimate the mean number of events ====================================
f_mean_num <- function(obj, 
                       newdata_ce, 
                       newdata_cb){
  
  surv_ce <- predict(obj, 
                     type = "surv", 
                     newdata_ce)
  
  haz_cb <- predict(obj, 
                    type = "haz", 
                    newdata_cb)
  
  cumsum(surv_ce * haz_cb * 0.03)
  
}

#   3.2 Estimate the difference in mean number of events ======================
f_mean_num_diff <- function(obj,
                            newdata_case_cb,
                            newdata_case_ce,
                            newdata_comp_cb,
                            newdata_comp_ce){
  
  mean_num_case <- f_mean_num(obj,
                              newdata_cb = newdata_case_cb,
                              newdata_ce = newdata_case_ce)
  
  mean_num_comp <- f_mean_num(obj,
                              newdata_cb = newdata_comp_cb,
                              newdata_ce = newdata_comp_ce)
  
  c(mean_num_case - mean_num_comp)
  
}


# 4. Define function for estimating mean number of events using FPMs ----------

average_no_cb <- function(x, strata){
  
  lapply(unique(x[[strata]]),
         function(i){
           
           st_data <- x[x[[strata]] == i, ]
           
           model_comb <- stpm2(Surv(time  = st_yrs_strt, 
                                    time2 = st_yrs_end, 
                                    event = event) ~
                                 case_1   * state_cb +
                                 female_1 * state_cb +
                                 female_1_case_1 * state_cb,
                               df = 3,
                               tvc = list(female_1 = 2,
                                          case_1   = 2,
                                          female_1_case_1 = 2,
                                          state_cb = 3),
                               robust = TRUE,
                               cluster = "lopnr",
                               data = st_data)
           
           out <- 
             lapply(0:1, function(I_female){
               
               # Define newdata used for predictions
               newdata_comp_ce <- cbind(data.frame(female_1        = I_female,
                                                   case_1          = 0,
                                                   female_1_case_1 = 0,
                                                   state_cb        = 0),
                                        st_yrs_end = seq(0.75, 10, 0.03))
               
               newdata_comp_cb <- transform(newdata_comp_ce,
                                            state_cb = 1)
               
               newdata_case_ce <- transform(newdata_comp_ce,
                                            case_1          = 1,
                                            female_1_case_1 = I_female)
               
               newdata_case_cb <- transform(newdata_case_ce,
                                            state_cb = 1)
               
               # Estimate the average number of events for cases
               mean_num_case <- 
                 predictnl(model_comb,
                           function(obj, newdata){
                             f_mean_num(obj = obj,
                                        newdata_cb = newdata_case_cb,
                                        newdata_ce = newdata_case_ce)
                           })
               
               # Estimate the average number of events for comparators
               mean_num_comp <- 
                 predictnl(model_comb,
                           function(obj, newdata){
                             f_mean_num(obj = obj,
                                        newdata_cb = newdata_comp_cb,
                                        newdata_ce = newdata_comp_ce)
                           })
               
               # Estimate the difference in the average number of events
               mean_num_diff <- 
                 predictnl(model_comb,
                           function(obj, newdata){
                             f_mean_num_diff(obj = obj,
                                             newdata_case_cb = newdata_case_cb,
                                             newdata_case_ce = newdata_case_ce,
                                             newdata_comp_cb = newdata_comp_cb,
                                             newdata_comp_ce = newdata_comp_ce)
                           })
               
               # Combine estimates in a data frame
               est <- 
                 data.frame(mean_num_comp[, "fit"], confint(mean_num_comp),
                            mean_num_case[, "fit"], confint(mean_num_case),
                            mean_num_diff[, "fit"], confint(mean_num_diff))
               
               # Alinge colnames of dataframe with stpm2cif output
               est <- setNames(est,
                               c(paste0("cif_case_", 
                                        rep(0:1, each = 3), 
                                        "_fem_", 
                                        I_female,
                                        "_c1",
                                        rep(c("", "_lci", "_uci"), 2)),
                                 paste0("cif_diff_fem_", 
                                        I_female,
                                        "_c1",
                                        c("", "_lci", "_uci"))))
               
               return(est)
               
             }) %>% bind_cols()
           
           # Add time variable
           out$t <- seq(0.75, 10, 0.03)
           
           return(out)
           
         }) %>% setNames(unique(x[[strata]]))
}

# 5. Apply function -----------------------------------------------------------

# Morphological subtypes
mean_num_c7 <- average_no_cb(st_data,
                            "nhl_sub_c7_risksets")
saveRDS(mean_num_c7,
        paste0("./data/analysis_data/",
               "re_mean_num_nhl_sub_c7_risksets_delta.RData"))

# Clinical subtypes
mean_num_c5 <- average_no_cb(st_data,
                            "nhl_sub_c5_risksets")
saveRDS(mean_num_c5,
        paste0("./data/analysis_data/",
               "re_mean_num_nhl_sub_c5_risksets_delta.RData"))

# ////////////////////////////////////////////////////////////////////////////
# END OF R-FILE
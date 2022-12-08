#' Compare Model Estimates
#' 
#' A function that combines esimtates from multiple regression models into
#' one data frame.
#' 
#' @param models
#' A named list of regression models. The names of the models will be used
#' as suffix for the model estimates in the combined data frame.
#' 
#' @return 
#' A wide `data.frame` in which each row represents one variable and each
#' column different estimates from each model.
#' 
#' @export compare_model_estimates

compare_model_estimates <- function(models, wide_format = FALSE){
  
  # Create unique column names
  out <- 
    purrr::map_df(seq_along(models), 
                  .f = function(i){
                    
                    tidy_model <- broom::tidy(models[[i]], 
                                              conf.int = TRUE) %>% 
                      dplyr::mutate(model = names(models)[[i]]) %>% 
                      dplyr::relocate(model, .before = term)
                    
                    return(tidy_model)
                    
                  })
  
  if(wide_format){
    
    out <- 
      tidyr::pivot_wider(tidy_models,
                         names_from = model,
                         values_from = estimate:conf.high,
                         names_glue = "{model}_{.value}") 
    
  }
  
  out <- dplyr::rename_with(out, ~ gsub("\\.", "_", .x))
  
  return(out)
  
}

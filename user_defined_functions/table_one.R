#' Table One
#'
#' @param var
#'    List of variables to be included in the table as character string
#'    
#' @param df
#'    Data frame that includes the variables.
#'    
#' @importFrom magritter %>%


table_one <- function(var, df){
  
  # Calculate numbers and proportion in each category for factor variables
  if(class(df[[var]]) == "factor"){
    
    lev <- levels(df[[var]])
    
    lapply(seq_along(lev), function(i){
      
      mean <- mean(as.numeric(df[[var]] == lev[i])) %>% 
        round(3)
      
      p25  <-  NA
      
      p75  <-  NA
      
      n_category <- sum(df[[var]] == lev[i])
      
      n_total    <- sum(!is.na(df[[var]]))
      
      varname <- paste(var, levels(df[[var]])[i]) %>% 
        as.character()
      
      data.frame(varname, mean, p25, p75, n_category, n_total)
      
    }) %>% bind_rows()
    
    # Calculate numbers and proportion in each category for dichotomiesed
    # integer variables
  } else if(class(df[[var]]) %in% c("integer", "numeric") & 
            max(df[[var]], na.rm = TRUE) == 1){
    
    mean <- mean(df[[var]])%>% 
      round(3)
    
    p25  <- NA
    
    p75  <- NA
    
    n_category <- sum(df[, var])
    
    n_total    <- sum(!is.na(df[[var]]))
    
    data.frame(var, mean, p25, p75, n_category, n_total)
    
    # Calculate median, q25, q75 for numeric variables    
  } else if(class(df[[var]]) == "numeric" & max(df[[var]], 
                                                na.rm = TRUE) != 1){
    
    mean <- quantile(df[[var]], 0.5)%>% 
      round(1)
    
    p25  <- quantile(df[[var]], 0.25)%>% 
      round(1)
    
    p75  <- quantile(df[[var]], 0.75)%>% 
      round(1)
    
    n_category <- NA
    
    n_total    <- sum(!is.na(df[[var]]))
    
    data.frame(var, mean, p25, p75, n_category, n_total)
    
  }
}
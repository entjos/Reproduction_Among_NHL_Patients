#' Plot Aalen Johansen Estimates
#' 
#' @param x
#'    Dataset that includes the data that should be used for the estimation of
#'    AJ estimate.
#'    
#' @param time
#'    Name of the survival time variable.
#'    
#' @param event
#'    Name of the event indicator variable. The event indicator needs to be 
#'    coded as 0 = censored, 1 = censored due to event, 
#'    2 = censored due to competing event.
#'    
#' @param strata
#'    A character vector specifying the name of the factor identifying the
#'    groups for which the AJ estimator shall be estimated.
#' @param xlab
#'    X-axis title.
#'    
#' @param ylab
#'    Y-axis title.
#'    
#' @export plot_aalen_johansen

plot_aalen_johansen <- function(x,
                                time,
                                event,
                                strata,
                                t_max,
                                xlab,
                                ylab,
                                type = "default"){
  
  # Initiate new plot
  plot.new()
  plot.window(xlim = c(0, t_max),
              ylim = c(0, 1))
  
  # Initiate colour indicator
  colour_ind <- 1
  
  for(i in levels(x[[strata]])){
    
    # Subset dataset
    selection <- x[[strata]] == i
    
    # Call prodlim
    prodlim_args <- list(formula = substitute(formula(Hist(time, 
                                                           event) ~ 1)),
                         type = "risk",
                         data = x[selection, ])
    
    aj_est <- do.call(prodlim,
                      args = prodlim_args)
    
    # Print AJ estimator
    lines(aj_est$time,
          aj_est$cuminc$`1`,
          type = "s",
          col  = colour_ind)
    
    if(type == "inverted"){
      
      lines(aj_est$time,
            1 - aj_est$cuminc$`2`,
            type = "s",
            col  = colour_ind,
            lty  = 2)  
      
    }
    
    # Update colour indicator
    colour_ind <- colour_ind + 1
    
  }
  
  # Add axes
  axis(1)
  axis(2)
  
  # Add titles
  title(xlab = xlab,
        ylab = ylab)
  
}
#' Plot comparing HR from FPMs and Cox models
#' 
#' @param fpm_models
#'    A list of `stpm2` model objects.
#'    
#' @param cox_modlels
#'    A list of `coxph` models corresponding to the models in `fpm_models`.
#'    Both lists need to be sorted in the same way.
#'    
#' @param comparison
#'    A character string indicating the comparison. This argument will be
#'    passed down to the `type` argument of the call to `rstpm2::lines()` and
#'    will usually be `"hr"`.
#'    
#' @param text_ylab
#'    The title for the y-axis of the plot.  
#'    
#' @param text_xlab
#'    The title for the x-axis of the plot.
#'    
#' @param text_title
#'    The plot title.
#'
#' @param ylim
#'    The range of the y-axis.
#'
#' @param xlim
#'    The range of the x-axis.
#'    
#' @param file_path
#'    The file path to the output device.

fpm_plot_comparision <- function(fpm_models,
                                 cox_models,
                                 comparison,
                                 text_ylab,
                                 text_xlab,
                                 text_title,
                                 ylim,
                                 xlim,
                                 file_path){
  
  # Define output devices
  png(file_path,
      width  = 4 * length(fpm_models),
      height = 4 * 2,
      units = "in",
      res = 600)
  
  
  # Set plot parameters
  par(mfrow = c(2, length(fpm_models)),
      oma = c(1, 1, 0, 0),
      cex = 0.7)
  
  # Create plots for each strata
  lapply(0:1,
         function(i){
           
           lapply(seq_along(fpm_models),
                  function(j){
                    
                    # Define plotting device
                    plot.new()
                    plot.window(xlim = xlim,
                                ylim = ylim)
                    
                    # Plot time varying HRs
                    lines(fpm_models[[j]],
                          type    = comparison,
                          newdata = data.frame(case_1   = 0,
                                               female_1 = i,
                                               female_1_case_1 = 0),
                          exposed = function(x) transform(x, case_1 = 1,
                                                          female_1_case_1 = i),
                          ci = TRUE)
                    
                    # Plot HRs from the Cox model
                    coef_name_t1 <- paste0("timeband1:female",
                                           ifelse(i == 1, "female", "male"),
                                           ":case1")
                    
                    coef_name_t2 <- paste0("timeband2:female",
                                           ifelse(i == 1, "female", "male"),
                                           ":case1")
                    
                    coef_1 <- 
                      exp(cox_models[[j]][["coefficients"]][[coef_name_t1]])
                    
                    coef_2 <- 
                      exp(cox_models[[j]][["coefficients"]][[coef_name_t2]])
                    
                    segments(x0  = 0.75,
                             x1  = 3,
                             y0  = coef_1,
                             y1  = coef_1,
                             lty = 2)
                    
                    segments(x0  = 3,
                             x1  = 10,
                             y0  = coef_2,
                             y1  = coef_2,
                             lty = 2)
                    
                    # Add axes
                    axis(1)
                    axis(2)
                    
                    # Add titles
                    title(xlab = text_xlab,
                          ylab = text_ylab)
                    
                    mtext(paste(ifelse(i == 1, "Females", "Males"),
                                text_title[[j]]),
                          adj = 0,
                          cex = 0.8)
                    
                    title(main = LETTERS[j + (i) * length(fpm_models)],
                          adj = 0)
                  })
           
         })
  
  dev.off()
  
}
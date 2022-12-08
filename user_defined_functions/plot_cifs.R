#' Function for plotting CIFs from a FPM fitted with Stata
#' 
#' @param cifs
#'    A dataframe inlcuding estimates of the CIF predicted using the stpm2cif
#'    command in Stata.
#'
#' @param no_cat_total
#'    The total number of strata that have been used when fitting the FPMs
#'    
#' @param no_cat
#'    The stratum number that is currenlty fitted
#'    
#' @param title
#'    A vector with graph titles for each stratum.

plot_cifs <- function(cifs,
                      no_cat_total,
                      no_cat,
                      ylim1,
                      ylim2,
                      axis_2_control = NULL,
                      axis_4_control = NULL,
                      title,
                      ylab,
                      ylab_control){
  
  #' BEGIN LOOP over sex ======================================================
  for(j in 0:1){
    
    plot.new()
    plot.window(xlim = c(0, 10),
                ylim = ylim1)
    
    #' BEGIN LOOP over case indicator #########################################
    for(k in c(1, 0)){
      
      t   <- cs_cifs[["t"]]
      est <- cs_cifs[[paste0("cif_case_", k, "_fem_", j, "_c1")]]
      lci <- cs_cifs[[paste0("cif_case_", k, "_fem_", j, "_c1_lci")]]
      uci <- cs_cifs[[paste0("cif_case_", k, "_fem_", j, "_c1_uci")]]
      
      if(k == 1){
        # Plot CIs
        polygon(c(t  , rev(t)),
                c(lci, rev(uci)),
                border = NA,
                col = "grey")
      }
      
      # Plot estimates
      lines(t,
            est,
            lty = k + 1)
      
    }
    #' END LOOP over case indicator ###########################################
    
    # Add axes
    axis(1)
    do.call(axis, args = append(list(side = 2),
                                axis_2_control))
    
    # Add titles
    title(xlab = "Time since NHL diagnosis (years)",
          cex.lab = 1)
    
    do.call(graphics::title, args = append(list(ylab = ylab),
                                           ylab_control))
    
    
    # Add main title with panel letters
    temp_title_1 <- LETTERS[no_cat + (j * no_cat_total)]
    temp_title_2 <- paste(ifelse(j == 0, "Males", "Females"), title)
    
    mtext(substitute(bold(temp_title_1), 
                     list(temp_title_1 = temp_title_1)),
          side = 3,
          line = 0,
          adj = -0.27,
          cex = 1)
    
    mtext(temp_title_2,
          adj = 0,
          cex = 1)
    
    # Add legend
    if(i == 1 & j == 0){
      
      legend("topleft",
             legend = c("Comparators", "Cases"),
             lty = 1:2,
             bty = "n",
             cex = 0.9)
      
    }
    
    # Add the CIF difference on a second y axis
    par(new = TRUE)
    plot.window(xlim = c(0, 10),
                ylim = ylim2)
    
    # Obtain CIF difference estimates
    est_diff <- cs_cifs[[paste0("cif_diff_fem_", j, "_c1")]]
    lci_diff <- cs_cifs[[paste0("cif_diff_fem_", j,"_c1_lci")]]
    uci_diff <- cs_cifs[[paste0("cif_diff_fem_", j,"_c1_uci")]]
    
    # Plot CIs
    polygon(c(t  , rev(t)),
            c(lci_diff, rev(uci_diff)),
            border = NA,
            col = "grey")
    
    # Plot esitmates
    lines(t,
          est_diff)
    
    # Add reference line
    segments(x0  = 0,
             x1  = 10,
             y0  = 0,
             y1  = 0,
             lty = 3)
    
    # Create second y-axis
    do.call(axis, args = append(list(side = 4),
                                axis_4_control))
    
    # Add label for second y-axis
    mtext("Difference", 
          side = 4, 
          line = 3, 
          adj = 0.9, 
          cex = 1)
    
  }
  #' END LOOP over sex ========================================================
}

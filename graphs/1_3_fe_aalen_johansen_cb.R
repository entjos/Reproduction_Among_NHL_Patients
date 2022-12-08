#//////////////////////////////////////////////////////////////////////////////
#
#//////////////////////////////////////////////////////////////////////////////

# 1. Prefix -------------------------------------------------------------------

# clear memory
rm(list = ls())

# load packages
library(stringr)
library(prodlim)

source(paste0("./programs_and_log_files/user_defined_functions/",
              "plot_aalen_johansen.R"))

# Import analysis data
fe_st_data <- readRDS("./data/analysis_data/fe_st_data.RData")

# 2. Clinical subtypes --------------------------------------------------------

# Define output devices
dev_path <- paste0("./graphs/fe_aalen_johansen_cb_c5.png")

png(dev_path,
    width  = 8,
    height = 8,
    units = "in",
    res = 600)

# Set plot parameters
par(mfrow = c(2, 2),
    oma = c(1, 1, 0, 0),
    cex = 0.9)

# Prepare titles
title_txt <- paste(rep(c("Males", "Females"), each = 2), 
                   rep(c("Aggressive Subtypes", 
                         "Indolent Subtypes"), 
                       2))
# Initiate counter
i <- 1

# Plot AF estimates by sex and NHL subtype
by(fe_st_data,
   paste(fe_st_data$female_1,
         fe_st_data$nhl_sub_c5_risksets),
   function(x){
     
     # Plot AJ estimates
     plot_aalen_johansen(x,
                         time   = st_yrs,
                         event  = exit_event_c2,
                         strata = "case",
                         t_max  = 10,
                         xlab   = "Time since NHL diagnosis (years)",
                         ylab   = "Cause-specific CIF of childbirth")
     
     # Add legend to first plot
     if(i == 1){
       
       legend("topleft",
              legend = c("Comparators", "Cases"),
              fill   = 1:2,
              bty    = "n")
     }
     
     # Add titles
     mtext(title_txt[[i]], 
           cex = 0.9,
           adj = 0)
     
     title(main = LETTERS[[i]],
           adj = 0)
     
     # Increase counter by 1
     i <<- i + 1
     
   })

# Close graphic device
dev.off()

# 2. Morphological subtypes ---------------------------------------------------

# Define output devices
dev_path <- paste0("./graphs/fe_aalen_johansen_cb_c7.png")

png(dev_path,
    width  = 12,
    height = 8,
    units = "in",
    res = 600)

# Set plot parameters
par(mfrow = c(2, 3),
    oma = c(1, 1, 0, 0),
    cex = 0.9)

# Prepare titles
title_txt <- paste(rep(c("Males", "Females"), each = 3), 
                   rep(c("DLBCL", "FL",
                         "T-/NK-cell"), 
                       2))
# Initiate counter
i <- 1

# Plot AF estimates by sex and NHL subtype (excluding Other B-cell)
fe_st_data_no_b_cell <- subset(fe_st_data, nhl_sub_c7_risksets != "Other_B")

by(fe_st_data_no_b_cell,
   paste(fe_st_data_no_b_cell$female_1,
         as.numeric(fe_st_data_no_b_cell$nhl_sub_c7_risksets)),
   function(x){
       
       # Plot AJ estimates
       plot_aalen_johansen(x,
                           time   = st_yrs,
                           event  = exit_event_c2,
                           strata = "case",
                           t_max  = 10,
                           xlab   = "Time since NHL diagnosis (years)",
                           ylab   = "Cause-specific CIF of childbirth")
       
       # Add legend to first plot
       if(i == 1){
           
           legend("topleft",
                  legend = c("Comparators", "Cases"),
                  fill   = 1:2,
                  bty    = "n")
       }
       
       # Add titles
       mtext(title_txt[[i]], 
             cex = 0.9,
             adj = 0)
       
       title(main = LETTERS[[i]],
             adj = 0)
       
       # Increase counter by 1
       i <<- i + 1
       
   })

# Close graphic device
dev.off()

#'/////////////////////////////////////////////////////////////////////////////
#' END OF R-FILE

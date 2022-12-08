#//////////////////////////////////////////////////////////////////////////////
#
#//////////////////////////////////////////////////////////////////////////////

# 1. Prefix -------------------------------------------------------------------

# clear memory
rm(list = ls())

# load packages
library(survival)
library(viridis)

# Import analysis data
re_st_data <- readRDS("./data/analysis_data/re_st_data.RData")

# 1. Define function ----------------------------------------------------------

plot_aalen_johansen_re <- function(x, ...){
  
  # Create riskset tables
  risksets_cb <- survfit(Surv(time  = st_yrs_strt,
                              time2 = st_yrs_end,
                              event = exit_event_c2 == 1,
                              type = "counting") ~ 1,
                         data = x)
  
  risksets_ce <- survfit(Surv(time  = st_yrs_strt,
                              time2 = st_yrs_end,
                              event = exit_event_c2 == 2,
                              type = "counting") ~ 1,
                         data = x)
  
  # Define time as time since HL diagnosis
  t         <- risksets_cb$time
  
  # Get leap survival function for all causes
  surv_ce   <- c(1, risksets_ce$surv)[-length(risksets_ce$surv) + 1]
  
  # Estimate Nelson-Aalen and Aalen-Johansen estimator for childbirth
  na_est_cb <- risksets_cb$n.event / risksets_cb$n.risk
  aj_est_cb <- cumsum(surv_ce * na_est_cb)
  
  # Plot lines
  lines(t,
        aj_est_cb,
        type = "s",
        ...)
}

# 2. Clinical subtypes --------------------------------------------------------

# Define output devices
dev_path <- paste0("./graphs/re_aalen_johansen_cb_c5.png")

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

# Plot AJ estimators
by(re_st_data,
   paste(re_st_data$female_1, 
         re_st_data$nhl_sub_c5_risksets),
   function(x){
     
     plot.new()
     plot.window(xlim = c(0, 10),
                 ylim = c(0, 1))
     
     plot_aalen_johansen_re(subset(x, case == 0),
                            col = 1)
     
     plot_aalen_johansen_re(subset(x, case == 1),
                            col = 2)
     
     axis(1)
     axis(2)
     
     # Add legend to first plot
     if(i == 1){
       
       legend("topleft",
              legend = c("Comparators", "Cases"),
              fill   = 1:2,
              bty    = "n")
     }
     
     title(xlab = "Time since NHL diagnosis (years)",
           ylab = "Mean number of childbirths")
     
     # Add titles
     mtext(title_txt[[i]], 
           cex = 0.9,
           adj = 0)
     
     title(main = LETTERS[[i]],
           adj = 0)
     
     i <<- i + 1
     
   })

dev.off()

# 3. Morphological subtypes ---------------------------------------------------

# Define output devices
dev_path <- paste0("./graphs/re_aalen_johansen_cb_c7.png")

png(dev_path,
    width  = 12,
    height = 8,
    units = "in",
    res = 600)

# Prepare titles
title_txt <- paste(rep(c("Males", "Females"), each = 4), 
                   rep(c("DLBCL", "FL",
                         "T-/NK-cell", "Other B-cell"), 
                       2))
# Initiate counter
i <- 1

# Set plot parameters
par(mfrow = c(2, 3),
    oma = c(1, 1, 0, 0),
    cex = 0.9)

# Plot AF estimates by sex and NHL subtype (excluding Other B-cell)
re_st_data_no_b_cell <- subset(re_st_data, nhl_sub_c7_risksets != "Other_B")

by(re_st_data_no_b_cell,
   paste(re_st_data_no_b_cell$female_1,
         as.numeric(re_st_data_no_b_cell$nhl_sub_c7_risksets)),
   function(x){
     
     plot.new()
     plot.window(xlim = c(0, 10),
                 ylim = c(0, 1))
     
     plot_aalen_johansen_re(subset(x, case == 0),
                            col = 1)
     
     plot_aalen_johansen_re(subset(x, case == 1),
                            col = 2)
     
     axis(1)
     axis(2)
     
     # Add legend to first plot
     if(i == 1){
       
       legend("topleft",
              legend = c("Comparators", "Cases"),
              fill   = 1:2,
              bty    = "n")
     }
     
     title(xlab = "Time since NHL diagnosis (years)",
           ylab = "Mean number of childbirths")
     
          # Add titles
     mtext(title_txt[[i]], 
           cex = 0.9,
           adj = 0)
     
     title(main = LETTERS[[i]],
           adj = 0)
     
     i <<- i + 1
     
   })

dev.off()

#'/////////////////////////////////////////////////////////////////////////////
#' END OF R-FILE

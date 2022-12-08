#//////////////////////////////////////////////////////////////////////////////
#
#//////////////////////////////////////////////////////////////////////////////

# 1. Prefix -------------------------------------------------------------------

# Clean memory
rm(list = ls())

library(haven)

source(paste0("./programs_and_log_files/user_defined_functions/",
              "plot_cifs.R"))

# 2. Plot CIFs for clinical subtypes ------------------------------------------

# Define output devices
dev_path <- paste0("./graphs/fe_csCIF_stata_c5.png")

png(dev_path,
    width  = 8,
    height = 8,
    units = "in",
    res = 600)

# Set plot parameters
par(mfcol = c(2, 2),
    oma = c(1, 1, 0, 0),
    mai = c(0.8, 0.8, 0.8, 0.8),
    cex = 0.9)
# Plot CIFs
titles <- c("Aggressive Subtypes", "Indolent Subtypes")

for(i in 1:2){
  
  cs_cifs <- read_dta(paste0("./data/analysis_data/",
                             "fe_csCIF_nhl_sub_c5_risksets_", i, ".dta"))
  
  plot_cifs(cs_cifs,
            no_cat_total = 2,
            no_cat = i,
            ylim1 = c(0, 1.6),
            ylim2 = c(-1.3, 0.3),
            axis_2_control = list(at     = seq(0, 1, 0.2),
                                  labels = format(seq(0, 1, 0.2),
                                                  digits = 2),
                                  las    = 1),
            axis_4_control = list(at = c(-0.3, 0, 0.3),
                                  las = 1),
            title = titles[[i]],
            ylab = "CIF of childbirth",
            ylab_control = list(adj = 0.21))
  
}

# Close device
dev.off()

# 3. Plot CIFs for morphological subtypes -------------------------------------

# Define output devices
dev_path <- paste0("./graphs/fe_csCIF_stata_c7.png")

png(dev_path,
    width  = 12,
    height = 8,
    units = "in",
    res = 600)

# Set plot parameters
par(mfcol = c(2, 3),
    oma = c(1, 1, 0, 0),
    mai = c(0.8, 0.8, 0.8, 0.8),
    cex = 0.9)

# Plot CIFs
titles <- c("DLBCL", "FL", "T-/NK-cell")

for(i in 1:3){
  
  cs_cifs <- read_dta(paste0("./data/analysis_data/",
                             "fe_csCIF_nhl_sub_c7_risksets_", i, ".dta"))
  
  plot_cifs(cs_cifs,
            no_cat_total = 3,
            no_cat = i,
            ylim1 = c(0, 1.6),
            ylim2 = c(-1.3, 0.3),
            axis_2_control = list(at     = seq(0, 1, 0.2),
                                  labels = format(seq(0, 1, 0.2),
                                                  digits = 2),
                                  las    = 1),
            axis_4_control = list(at = c(-0.3, 0, 0.3),
                                  las = 1),
            title = titles[[i]],
            ylab = "CIF of childbirth",
            ylab_control = list(adj = 0.21))
  
}

# Close device
dev.off()

# /////////////////////////////////////////////////////////////////////////////
# END OF RFILES

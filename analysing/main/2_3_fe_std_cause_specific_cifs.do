/*#############################################################################
#
#############################################################################*/

* PREFIX ----------------------------------------------------------------------
cd "<path to working directory>"

* 1. Model for clinical subtypes ----------------------------------------------

forvalues i = 1/2{
    
   * Load data
   use "./data/analysis_data/fe_st_data.dta", clear

   * Subset data
   keep if nhl_sub_c5_risksets == `i'
   
   * Shorten the name for interaction term
   gen fem_case = female_1_case_1

   * Set up survival time
   stset st_yrs, failure(exit_event_c2 == 1, 2)

   * Fit models for childbirths and competing evetns
   stpm2cr [cb: case_1 female_1 fem_case,                                   ///
                scale(hazard) df(3) dftvc(2) tvc(case_1 female_1 fem_case)] ///
	       [ce: case_1 female_1 fem_case,                                   ///
                scale(hazard) df(5) dftvc(1) tvc(case_1 female_1 fem_case)] ///
		   , events(exit_event_c2) cause(1 2) cens(0)	 
		   
   * Predict cause-specific CIFs
   range t 0.75 10 300
   
   predict cif_case_1_fem_1, cif at(case_1 1 female_1 1 fem_case 1) ci timevar(t)
   predict cif_case_0_fem_1, cif at(case_1 0 female_1 1 fem_case 0) ci timevar(t)
   predict cif_case_1_fem_0, cif at(case_1 1 female_1 0 fem_case 0) ci timevar(t)
   predict cif_case_0_fem_0, cif at(case_1 0 female_1 0 fem_case 0) ci timevar(t)

   * Predict CIF differences
   predict cif_diff_fem_1  , cifdiff1(case_1 1 female_1 1 fem_case 1) ///
                             cifdiff2(case_1 0 female_1 1 fem_case 0) ///
							 ci timevar(t)
							 
   predict cif_diff_fem_0  , cifdiff1(case_1 1 female_1 0 fem_case 0) ///
                             cifdiff2(case_1 0 female_1 0 fem_case 0) ///
							 ci timevar(t)
   
   * Keep time varaible and predicted CIF functions
   keep t cif*
   drop if cif_case_1_fem_1_c1_lci == .

   * Export datasets
   save "./data/analysis_data/fe_csCIF_nhl_sub_c5_risksets_`i'.dta", replace
	
}

* 1. Model for morphological subtypes -----------------------------------------

forvalues i = 1/3{
    
   * Load data
   use "./data/analysis_data/fe_st_data.dta", clear

   * Subset data
   keep if nhl_sub_c7_risksets == `i'
   
   * Shorten the name for interaction term
   gen fem_case = female_1_case_1

   * Set up survival time
   stset st_yrs, failure(exit_event_c2 == 1, 2)

   * Fit models for childbirths and competing evetns
   stpm2cr [cb: case_1 female_1 fem_case,                                   ///
                scale(hazard) df(3) dftvc(2) tvc(case_1 female_1 fem_case)] ///
	       [ce: case_1 female_1 fem_case,                                   ///
                scale(hazard) df(1) dftvc(1) tvc(case_1 female_1 fem_case)] ///
		   , events(exit_event_c2) cause(1 2) cens(0)	 
		   
   * Predict cause-specific CIFs
   range t 0.75 10 300
   
   predict cif_case_1_fem_1, cif at(case_1 1 female_1 1 fem_case 1) ci timevar(t)
   predict cif_case_0_fem_1, cif at(case_1 0 female_1 1 fem_case 0) ci timevar(t)
   predict cif_case_1_fem_0, cif at(case_1 1 female_1 0 fem_case 0) ci timevar(t)
   predict cif_case_0_fem_0, cif at(case_1 0 female_1 0 fem_case 0) ci timevar(t)

   * Predict CIF differences
   predict cif_diff_fem_1  , cifdiff1(case_1 1 female_1 1 fem_case 1) ///
                             cifdiff2(case_1 0 female_1 1 fem_case 0) ///
							 ci timevar(t)
							 
   predict cif_diff_fem_0  , cifdiff1(case_1 1 female_1 0 fem_case 0) ///
                             cifdiff2(case_1 0 female_1 0 fem_case 0) ///
							 ci timevar(t)
   
   * Keep time varaible and predicted CIF functions
   keep t cif*
   drop if cif_case_1_fem_1_c1_lci == .

   * Export datasets
   save "./data/analysis_data/fe_csCIF_nhl_sub_c7_risksets_`i'.dta", replace
	
}



///////////////////////////////////////////////////////////////////////////////
// END OF STATA FILE
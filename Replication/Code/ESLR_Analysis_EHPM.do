*****************************************************
*** ESLR: EHPM Outcomes - RD Analysis - HH Survey ***
*****************************************************

capture log close
clear
set matsize 3000
set more off

** Set Workspace **
cd /Users/`c(username)'/Dropbox/Research_ElSalvador_LandReform/Replication/

*********************
*** Load the Data ***
*********************

use "./Data/ehpm_incomemodule_wreform.dta", clear 
	
capture drop ln_hh_inc_pc hh_inc_pc_real ln_hh_inc_pc_real 
gen ln_hh_inc_pc = log(hh_income_pc)
gen hh_inc_pc_real = (year==2000)*hh_income_pc*71.57/100 + ///
                 (year==2001)*hh_income_pc*74.25/100 + (year==2004)*hh_income_pc*80.68/100 + ///
                 (year==2005)*hh_income_pc*84.47/100 + (year==2006)*hh_income_pc*87.88/100 + ///
                 (year==2007)*hh_income_pc*91.90/100 + (year==2008)*hh_income_pc*98.06/100 + ///
                 (year==2009)*hh_income_pc*99.10/100 + (year==2011)*hh_income_pc*105.13/100 + ///
                 (year==2012)*hh_income_pc*106.95/100 + (year==2013)*hh_income_pc*107.79/100 
gen ln_hh_inc_pc_real = log(hh_inc_pc_real )

*********************
*** Set RD Params ***
*********************

** Baseline: Will use local linear rd with manual bandwidth (due to sample size)
** with ses clustered at propietor level. 
** Will use rdrobust package

local polynomial_level 1
local cluster_level "Expropretario_ISTA"


****************************************
*** OUTCOME 1a - HH INCOME PC LEVELS ***
****************************************

** Avg HH per capita income:

local bwidth = 300


local bwidth = 300
reg hh_inc_pc_real Above500 norm_dist c.norm_dist#Above500 i_year* sex  age age2 if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
	sum hh_income_pc if abs(norm_dist)<`bwidth'
	
	outreg2 using "./Output/Table_Earnings.tex", replace se tex noobs nocons nor2 keep(Above500) addstat(Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')

local bwidth = 150
reg hh_inc_pc_real Above500 norm_dist c.norm_dist#Above500 i_year*  sex  age age2 if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
	sum hh_income_pc if abs(norm_dist)<`bwidth'
	outreg2 using "./Output/Table_Earnings.tex", append se tex noobs nocons nor2 keep(Above500) addstat(Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')





 
*******************************************
*** OUTCOME 1b - WAGE INCOME COMPRESSION ***
*******************************************

** IQR: 
** Reg:

preserve

collapse (iqr) hh_income_pc hh_inc_pc_real (mean) norm_dist Above500, by(match_id Expropretario_ISTA i_year*) cw
local bwidth =300

reg hh_inc_pc_real Above500 norm_dist c.norm_dist#Above500 i_year* if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
	sum hh_income_pc if abs(norm_dist)<`bwidth'
	outreg2 using "./Output/Table_Earnings.tex", append se tex noobs nocons nor2 keep(Above500) addstat(Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')

local bwidth = 150

reg hh_inc_pc_real Above500 norm_dist c.norm_dist#Above500 i_year* if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
	sum hh_income_pc if abs(norm_dist)<`bwidth'
	outreg2 using "./Output/Table_Earnings.tex", append se tex noobs nocons nor2 keep(Above500) addstat(Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')

restore


****************************************
*** QUANTILE REGRESSION COEFFICIENT PLOT 
****************************************

gen Above500_QPlot = Above500
label var Above500_QPlot "Quantile Estimates for: Above 500 (ha)"

drop if num_members < 5
local bwidth =150
bsqreg ln_hh_inc_pc_real  Above500_QPlot norm_dist norm_dist_Above  i_year1-i_year8 i_year10-i_year11  if abs(norm_dist) < `bwidth' & hh_inc_pc >0,  q(.50) 
set scheme lean1
grqreg Above500_QPlot,  ci reps(40) qstep(.2) seed(821) 
	graph export "./Output/EHPM_QuantilePlot_ln_hh_inc_pc_real.pdf", replace

 

 







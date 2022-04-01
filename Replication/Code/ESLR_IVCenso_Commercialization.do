*****************************************************
*** ESLR: LR Ag Outcomes - RD Analysis - Censo IV ***
*****************************************************

capture log close
clear
set matsize 3000
set more off
set scheme s2color

** Set Workspace **
cd /Users/`c(username)'/Dropbox/Research_ElSalvador_LandReform/Replication

** ssc install rdrobust; winsor2; outreg2; lpoly; cmogram; dm88_1; grqreg; gr0002_3; pdslasso; lassopack; univar; ietoolkit

*********************
*** Load the Data ***
*********************

use "Data/censo_ag_wreform.dta", clear 

label var Above500 "Above 500 Ha"
label var norm_dist "Normalized Distance to Reform Threshold (has)"
label var own_amt "Cumulative Landholdings of Former Owner (has)"

*********************
*** Set RD Params ***
*********************

** Baseline: Will use local linear rd with MSE optimal bandwidth
** with ses clustered at propietor level. 
local polynomial_level 1
local bandwidth_choice "mserd" 
local kernel_choice "tri"  
local cluster_level "Expropretario_ISTA"


**************************
*** Commercialization ****
**************************


* S20A - Commercialization 
merge 1:1 agg_id using "./Data/censo_ag_commercialization.dta", gen(S20A_merge) // S20A Vars.

** Mayorista:
rdrobust MAYO norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table_Commercialization.tex", replace se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')
	
** Minorista:
rdrobust MINO norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table_Commercialization.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

** Exported:
rdrobust EXPO norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table_Commercialization.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

** Other:
rdrobust OTRO norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table_Commercialization.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')


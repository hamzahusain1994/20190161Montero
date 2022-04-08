*****************************************************
*** ESLR: LR Ag Outcomes - RD Analysis - Censo IV ***
*****************************************************

capture log close
clear
set matsize 3000
set more off
set scheme s2color

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
** Will use rdrobust package: net install rdrobust, from(https://sites.google.com/site/rdpackages/rdrobust/stata) replace

local polynomial_level 1
local bandwidth_choice "mserd" // "mserd", "msecomb2" "msetwo"
local kernel_choice "tri"  
local cluster_level "Expropretario_ISTA"


********************************************************************************
******************************** CREDIT ACCESS *********************************
********************************************************************************


*********************
*** CREDIT ACCESS ***
*********************

* S18A - Credit Indicator S18P01; Credit Approved S18P02; Oportuna Appoval S18P03
* S18B - Credit Source; Destino (type of production or capital)
merge 1:1 agg_id using "./Data/censo_ag_credit.dta", gen(S18A_merge) // S18A Vars.

** Credit:
gen Credit_Applied = S18P01
rdrobust Credit_Applied norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "./Output/Table_Credit_agg.tex", replace se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')


** Credit Timely:
gen Credit_Timely = S18P03
replace Credit_Timely = . if S18P03==-2
rdrobust Credit_Timely norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "./Output/Table_Credit_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

***************************
*** CREDIT SOURCE + USE ***
***************************

merge 1:1 agg_id using "./Data/censo_ag_credittype.dta", gen(S18B_merge) // S18B Vars.

** Credit From State Bank:
rdrobust S18BBANCOESTATAL norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "./Output/Table_Credit_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

** Credit From Private Bank:
rdrobust S18BBANCOPRIVADO norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "./Output/Table_Credit_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')
	
** Credit From Credit Coop:
rdrobust S18BCOOPERATIVA norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "./Output/Table_Credit_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

** Credit From ONG:
rdrobust S18BONG norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "./Output/Table_Credit_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')


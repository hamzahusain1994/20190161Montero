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
local bandwidth_choice "mserd" 
local kernel_choice "tri"  
local cluster_level "Expropretario_ISTA"

*********************************************
*** OUTCOME 1 - AGRICULTURAL PRODUCTIVITY ***
*********************************************


*Logs OF REVENUE:
rdrobust ln_agprod_pricew_crops norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level') 
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'	
	outreg2 using "Output/Table4_LogProductivity_agg.tex", replace se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Fuzzy RD, N)

rdrobust ln_agprod_pricew_crops norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level') fuzzy(reform sharpbw)
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table4_LogProductivity_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Fuzzy RD, Y)
	*  rdpower ln_agprod_pricew_crops  norm_dist, c(0) tau(1) vce(cluster Expropretario_ISTA ) plot
	
**** NET OF COSTS w/o Labor costs:	
rdrobust ln_agprod norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table4_LogProductivity_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Fuzzy RD, N)

rdrobust ln_agprod norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level') fuzzy(reform sharpbw)
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table4_LogProductivity_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Fuzzy RD, Y)	

**** TFP PRODUCTIVITY:	
rdrobust ln_tfp_geo norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table4_LogProductivity_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Fuzzy RD, N)

rdrobust ln_tfp_geo norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level') fuzzy(reform sharpbw)
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table4_LogProductivity_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Fuzzy RD, Y)	


 
******************************
*** OUTCOME 2 - CASH CROPS ***
******************************

rdrobust CashCrop_Indicator norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	*outreg2 using "Output/Table2_CashCrops_agg.tex", replace se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

rdrobust CashCrop_Share norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table2_CashCrops_agg.tex", replace se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')



** Sugar Cane:
rdrobust SugarCane_Indicator norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table2_CashCrops_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

rdrobust SugarCane_Share norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table2_CashCrops_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')
	
* Note: small sample means cannot compute optimal bw. Setting BW manually at level in previous regression:
rdrobust SugarCane_Yield norm_dist, c(0) p(`polynomial_level')  h(`e(h_r)') b(`e(b_r)') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table2_CashCrops_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

** Coffee:
rdrobust Coffee_Indicator norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	outreg2 using "Output/Table2_CashCrops_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

rdrobust Coffee_Share norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	outreg2 using "Output/Table2_CashCrops_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

rdrobust Coffee_Yield norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	outreg2 using "Output/Table2_CashCrops_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')


********************************
*** OUTCOME 3 - STAPLE CROPS ***
********************************

rdrobust ConsCrop_Indicator norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	*outreg2 using "Output/Table3_ConsCrops_agg.tex", replace se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')
rdrobust StapleCrop_Share norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table3_ConsCrops_agg.tex", replace se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

** Maize:

rdrobust Maize_Indicator norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	* Check this. Strange since rd plot is so strong.
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table3_ConsCrops_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

rdrobust Maize_Share norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table3_ConsCrops_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

rdrobust Maize_Yield norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table3_ConsCrops_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

** Beans:

rdrobust Beans_Indicator norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level') // fuzzy(reform)
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table3_ConsCrops_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

rdrobust Beans_Share norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level') // fuzzy(reform)
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table3_ConsCrops_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')


* Note: Following Cannot Compute Optimal BW Above: rdrobust Beans_Yield norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level')
* Setting BW manually at level in previous regression:
rdrobust Beans_Yield norm_dist, c(0) p(`polynomial_level')  b(`e(h_r)') h(`e(b_r)') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table3_ConsCrops_agg.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

	


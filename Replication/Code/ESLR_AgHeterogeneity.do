********************************************************************************
******************************** HETEROGENEITY *********************************
********************************************************************************
	
use "/Users/eduardomontero/Dropbox/Research_ElSalvador_LandReform/R/Output/ag_census_wSegmCens_all.dta", clear
destring  Expropretario_ISTA, replace
replace Expropretario_ISTA = agg_id if Expropretario_ISTA==.

gen mean_med = age_mean- age_median
keep age_iqr mean_med Above500 ln_agprod ln_agprod_pricew_crops CashCrop_Share StapleCrop_Share norm_dist own_amt Expropretario_ISTA reform

**********************
*** Label the Data ***
**********************

** Label Variables for the output:
label variable ln_agprod_pricew_crops "Agricultural Revenues (ln($/ha))"
label variable ln_agprod "Agriculural Profits (ln($/ha))"
label variable CashCrop_Share "Share of Property for Cash Crops"
label variable StapleCrop_Share "Share of Property for Staple Crops"
label variable norm_dist "Distance to Reform Threshold (ha)"
label variable own_amt "Former Owner's Cumulative Landholdings (ha)"

*********************
*** Set RD Params ***
*********************

** Baseline: Will use local linear rd with MSE optimal bandwidth
** with ses clustered at propietor level. 
** Will also use two-sided MSE optimal bandwidth since big diff in density on
** both sides.
** Will use rdrobust package

local polynomial_level 1
local bandwidth_choice "mserd" // "mserd", "msecomb2" "msetwo"
local kernel_choice "tri" // "tri"
local cluster_level "Expropretario_ISTA"
local bw 100

*********************************************
*** OUTCOME - SEGM CENSALES HETEROGENEITY ***
*********************************************

** AGE Heterogeneity:
local het_var age_iqr

sum `het_var', d
local mean_agesd = `r(p50)'
dis "`mean_agesd'"
replace mean_med =`het_var'

count if mean_med <`mean_agesd' | reform==0
count  if mean_med >=`mean_agesd' | reform==0

gen abovemed_het = 0
replace abovemed_het=1 if (mean_med >=`mean_agesd' & mean_med!= .)

local bw 150
gen dis_meas = age_iqr //age_sd age_iqr
replace dis_meas=0 if dis_meas==.
reg ln_agprod Above500 c.Above500#c.dis_meas norm_dist c.Above500#c.norm_dist if abs(norm_dist) < 150, cluster(Expropretario_ISTA)


rdrobust ln_agprod_pricew_crops norm_dist if (mean_med >=`mean_agesd' & mean_med!= .) | reform==0, c(0) p(`polynomial_level')  h(`bw' `bw') kernel(`kernel_choice')  vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table_SegmCens_AgeHet.tex", replace se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

rdrobust ln_agprod_pricew_crops norm_dist if mean_med <`mean_agesd' | reform==0, c(0) p(`polynomial_level') h(`bw' `bw') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table_SegmCens_AgeHet.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')
*/

rdrobust ln_agprod norm_dist if (mean_med >=`mean_agesd' & mean_med!= .) | reform==0, c(0) p(`polynomial_level')  h(`bw' `bw') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table_SegmCens_AgeHet.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

rdrobust ln_agprod norm_dist if mean_med <`mean_agesd' | reform==0 , c(0) p(`polynomial_level') h(`bw' `bw') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table_SegmCens_AgeHet.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')


	
	*** TESTING COEFFICIENTS:
	* 1 vs. 2:
	local z = (-.430 - (-0.192))/sqrt(0.326^2 + 0.376^2)
	dis 2*(1-normal(abs(`z'))) 
	
		local z = (-.696 - (-0.255))/sqrt(0.411^2 + 0.504^2)
	dis 2*(1-normal(abs(`z'))) 
	
	

	
	
	/*gen group1 = abovemed_het ==1 // | reform==0
	gen group1_Above500 = group1*Above50
	
	reg ln_agprod_pricew_crops Above500 group1_Above500 norm_dist c.norm_dist#Above500 i.group1#c.norm_dist i.group1#c.norm_dist#Above500   if  abs(norm_dist) < 150, vce(cluster Expropretario_ISTA)  
	lincom _b[Above500] - _b[group1_Above500]

	
	reg ln_agprod Above500 group1_Above500 norm_dist c.norm_dist#Above500  if  abs(norm_dist) < 150 , vce(cluster Expropretario_ISTA)  
	lincom _b[Above500] - _b[group1_Above500]*/

local bw 300
rdrobust CashCrop_Share norm_dist if mean_med >=`mean_agesd' | reform==0, c(0) p(`polynomial_level')  h(`bw' `bw') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table_SegmCens_AgeHet2.tex", replace se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

rdrobust CashCrop_Share norm_dist if mean_med <`mean_agesd' | reform==0 , c(0) p(`polynomial_level') h(`bw' `bw') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table_SegmCens_AgeHet2.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')


rdrobust StapleCrop_Share norm_dist if mean_med >=`mean_agesd' | reform==0, c(0) p(`polynomial_level')  h(`bw' `bw') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table_SegmCens_AgeHet2.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')

rdrobust StapleCrop_Share norm_dist if mean_med <`mean_agesd' | reform==0 , c(0) p(`polynomial_level') h(`bw' `bw') kernel(`kernel_choice') vce(cluster `cluster_level')
	* outreg results
	distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
	local n_clust = `r(ndistinct)'
	su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
	outreg2 using "Output/Table_SegmCens_AgeHet2.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)')
*/


	** Crop ones
	* 1 vs. 2:
	local z = (-0.493 - (-0.571))/sqrt(0.139^2 + 0.132^2)
	dis 2*(1-normal(abs(`z'))) 
	
		local z = (0.150 - (0.344))/sqrt(0.168^2 + 0.190^2)
	dis 2*(1-normal(abs(`z'))) 
	

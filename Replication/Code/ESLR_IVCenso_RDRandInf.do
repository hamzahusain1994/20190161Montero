*******************************************************
*** ESLR: LR Ag Outcomes - RD Rand. Inf. - Censo IV ***
*******************************************************

capture log close
clear
set matsize 3000
set more off

*ssc install rdlocrand

*********************
*** Load the Data ***
*********************

use "Data/censo_ag_wreform.dta", clear 

label var Above500 "Above 500 Ha"
label var norm_dist "Normalized Distance to Reform Threshold (has)"
label var own_amt "Cumulative Landholdings of Former Owner (has)"

******************
*** Set Params ***
******************

** Robustness: We will use the randomization methods for RDs - https://sites.google.com/site/rdpackages/rdlocrand
** with ses clustered at proprietor level. 
** Will also use two-sided MSE optimal bandwidth since big diff in density on
** both sides.
** Will use rdrandinf package

local polynomial_levels 0 
local bandwidth_choice `" "mserd" "' 
local kernel_choice `" "uniform" "triangular" "epan" "'
local kernel_choice_rdrob `" "uniform" "triangular" "epanechnikov" "'

local cluster_level "Expropretario_ISTA" // not allowed in rdlocrand: vce(cluster `cluster_level')

** Also do Local Randomization methods with rdlocrand

** Selecting Window:
global covariates  canton_land_suit


**********************************************
*** OUTCOME 1A - AGRICULTURAL PRODUCTIVITY ***
**********************************************

set more off

local dep_var ln_agprod_pricew_crops

foreach pols in `polynomial_levels' {
	local count = 0
	*foreach band in `bandwidth_choice' {

		foreach kern in `kernel_choice' {
			su `dep_var' // if norm_dist < `r(wr)' & norm_dist > `r(wl)' & `dep_var' !=. & norm_dist!=.
			local summ = `r(mean)'
	
			dis "rdrandinf ln_agprod_pricew_crops norm_dist, c(0) p(`pols') kernel(`kern') covariates($covariates) approximate"
			rdrandinf `dep_var' norm_dist, c(0) p(`pols') kernel(`kern') covariates($covariates) approximate seed(123)
				
				* outreg results
				*distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
				local n_obs = `r(N)'
				local inf_estimate = `r(obs_stat)'
				local pvalue=`r(asy_pval)'
				local rw = `r(wr)'
				local lw = `r(wl)'
				if ("`kern'"=="epan") {
					local kern "epanechnikov"
				}
				dis "rdrobust `dep_var' norm_dist, c(0) p(`pols') kernel(`kern') vce(cluster `cluster_level')"
				rdrobust `dep_var' norm_dist, c(0) p(`pols')  bwselect(`bandwidth_choice') kernel(`kern') vce(cluster `cluster_level')
				if `count'==0 { 
					dis "outreg2 `r(obs_stat)' `r(randpval)' using, replace se tex noobs addstat(Observations, `n_obs', Mean Dep. Var., `summ', Randomization P-Value, `pvalue', Right Window, `rw', Left Window, `lw') addtext(Polynomial, `pol', Kernel, uniform, Fuzzy RD, N)"

					outreg2 using "Output/RandInfTable1_LogProductivity`pol'.tex", replace se pvalue tex noobs  addtext(Polynomial, `pols', Kernel, "`kern'", Fuzzy RD, N) addstat(Estimate, `inf_estimate', Randomization P-Value, `pvalue', Observations, `n_obs', Mean Dep. Var., `summ', Right Window, `rw', Left Window, `lw')
				} 
				if `count'!=0 {
					outreg2 using "Output/RandInfTable1_LogProductivity`pol'.tex", append se pvalue tex noobs  addtext(Polynomial, `pols', Kernel, "`kern'", Fuzzy RD, N) addstat(Estimate, `inf_estimate',Randomization P-Value, `pvalue', Observations, `n_obs', Mean Dep. Var., `summ', Right Window, `rw', Left Window, `lw')
				}
				local count = 1
		}
		
	*}
	
} 

local dep_var ln_agprod

foreach pols in `polynomial_levels' {
	local count = 0
	*foreach band in `bandwidth_choice' {

		foreach kern in `kernel_choice' {
			su `dep_var' // if norm_dist < `r(wr)' & norm_dist > `r(wl)' & `dep_var' !=. & norm_dist!=.
			local summ = `r(mean)'
	
			dis "rdrandinf ln_agprod_pricew_crops norm_dist, c(0) p(`pols') kernel(`kern') covariates($covariates) approximate"
			rdrandinf `dep_var' norm_dist, c(0) p(`pols') kernel(`kern') covariates($covariates) approximate  seed(123)
				
				* outreg results
				*distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
				local n_obs = `r(N)'
				local inf_estimate = `r(obs_stat)'
				local pvalue=`r(asy_pval)'
				local rw = `r(wr)'
				local lw = `r(wl)'
				if ("`kern'"=="epan") {
					local kern "epanechnikov"
				}
				dis "rdrobust `dep_var' norm_dist, c(0) p(`pols') kernel(`kern') vce(cluster `cluster_level')"
				rdrobust `dep_var' norm_dist, c(0) p(`pols')  bwselect(`bandwidth_choice') kernel(`kern') vce(cluster `cluster_level')
				if `count'==0 { 
					dis "outreg2 `r(obs_stat)' `r(randpval)' using, replace se tex noobs addstat(Observations, `n_obs', Mean Dep. Var., `summ', Randomization P-Value, `pvalue', , Right Window, `rw', Left Window, `lw') addtext(Polynomial, `pol', Kernel, uniform, Fuzzy RD, N)"

					outreg2 using "Output/RandInfTable1_LogProductivity`pol'.tex", append se pvalue tex noobs  addtext(Polynomial, `pols', Kernel, "`kern'", Fuzzy RD, N) addstat(Estimate, `inf_estimate', Randomization P-Value, `pvalue', Observations, `n_obs', Mean Dep. Var., `summ', Right Window, `rw', Left Window, `lw')
				} 
				if `count'!=0{
					outreg2 using "Output/RandInfTable1_LogProductivity`pol'.tex", append se pvalue tex noobs  addtext(Polynomial, `pols', Kernel, "`kern'", Fuzzy RD, N) addstat(Estimate, `inf_estimate',Randomization P-Value, `pvalue', Observations, `n_obs', Mean Dep. Var., `summ', Right Window, `rw', Left Window, `lw')
				}
				local count = 1
		}
		
	*}
	
} 


local dep_var ln_tfp_geo

foreach pols in `polynomial_levels' {
	local count = 0
	*foreach band in `bandwidth_choice' {

		foreach kern in `kernel_choice' {
			su `dep_var' // if norm_dist < `r(wr)' & norm_dist > `r(wl)' & `dep_var' !=. & norm_dist!=.
			local summ = `r(mean)'
	
			dis "rdrandinf ln_agprod_pricew_crops norm_dist, c(0) p(`pols') kernel(`kern') covariates($covariates) approximate"
			rdrandinf `dep_var' norm_dist, c(0) p(`pols') kernel(`kern') covariates($covariates) approximate  seed(123)
				
				* outreg results
				*distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
				local n_obs = `r(N)'
				local inf_estimate = `r(obs_stat)'
				local pvalue=`r(asy_pval)'
				local rw = `r(wr)'
				local lw = `r(wl)'
				if ("`kern'"=="epan") {
					local kern "epanechnikov"
				}
				dis "rdrobust `dep_var' norm_dist, c(0) p(`pols') kernel(`kern') vce(cluster `cluster_level')"
				rdrobust `dep_var' norm_dist, c(0) p(`pols')  bwselect(`bandwidth_choice') kernel(`kern') vce(cluster `cluster_level')
				if `count'==0 { 
					dis "outreg2 `r(obs_stat)' `r(randpval)' using, replace se tex noobs addstat(Observations, `n_obs', Mean Dep. Var., `summ', Randomization P-Value, `pvalue', , Right Window, `rw', Left Window, `lw') addtext(Polynomial, `pol', Kernel, uniform, Fuzzy RD, N)"

					outreg2 using "Output/RandInfTable1_LogProductivity`pol'.tex", append se pvalue tex noobs  addtext(Polynomial, `pols', Kernel, "`kern'", Fuzzy RD, N) addstat(Estimate, `inf_estimate', Randomization P-Value, `pvalue', Observations, `n_obs', Mean Dep. Var., `summ', Right Window, `rw', Left Window, `lw')
				} 
				if `count'!=0{
					outreg2 using "Output/RandInfTable1_LogProductivity`pol'.tex", append se pvalue tex noobs  addtext(Polynomial, `pols', Kernel, "`kern'", Fuzzy RD, N) addstat(Estimate, `inf_estimate',Randomization P-Value, `pvalue', Observations, `n_obs', Mean Dep. Var., `summ', Right Window, `rw', Left Window, `lw')
				}
				local count = 1
		}
		
	*}
	
} 

******************************
*** OUTCOME 2 - CASH CROPS ***
******************************

** SHARE LAND IN CASH CROPS:
set more off
local dep_var CashCrop_Share

foreach pols in `polynomial_levels' {
	local count = 0
	*foreach band in `bandwidth_choice' {

		foreach kern in `kernel_choice' {
			su `dep_var' // if norm_dist < `r(wr)' & norm_dist > `r(wl)' & `dep_var' !=. & norm_dist!=.
			local summ = `r(mean)'
			
			rdrandinf `dep_var' norm_dist, c(0) p(`pols') kernel(`kern') covariates($covariates) approximate  seed(123)
				
				* outreg results
				*distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
				local n_obs = `r(N)'
				local inf_estimate = `r(obs_stat)'
				local pvalue=`r(asy_pval)'
				local rw = `r(wr)'
				local lw = `r(wl)'
				
				if ("`kern'"=="epan") {
					local kern "epanechnikov"
				}
				rdrobust `dep_var' norm_dist, c(0) p(`pols')  bwselect(`bandwidth_choice') kernel(`kern') vce(cluster `cluster_level')
				
				if `count'==0 { 
					outreg2 using "Output/RandInfTable2_CropShare`pol'.tex", replace se tex noobs pvalue addtext(Polynomial, `pols', Kernel, "`kern'", Fuzzy RD, N) addstat(Estimate, `inf_estimate',Randomization P-Value, `pvalue', Observations, `n_obs', Mean Dep. Var., `summ', Right Window, `rw', Left Window, `lw')
				} 
				if `count'!=0{
					outreg2 using "Output/RandInfTable2_CropShare`pol'.tex", append se tex noobs pvalue addtext(Polynomial, `pols', Kernel, "`kern'", Fuzzy RD, N) addstat(Estimate, `inf_estimate',Randomization P-Value, `pvalue', Observations, `n_obs', Mean Dep. Var., `summ', Right Window, `rw', Left Window, `lw')
				}
				local count = 1
		}
		
	*}
	
}


********************************
*** OUTCOME 3 - STAPLE CROPS ***
********************************

** SHARE LAND IN STAPLE CROPS:
set more off
local dep_var StapleCrop_Share

foreach pols in `polynomial_levels' {
	local count = 0
	*foreach band in `bandwidth_choice' {

		foreach kern in `kernel_choice' {
			su `dep_var' // if norm_dist < `r(wr)' & norm_dist > `r(wl)' & `dep_var' !=. & norm_dist!=.
			local summ = `r(mean)'
			
			rdrandinf `dep_var' norm_dist, c(0) p(`pols') kernel(`kern') covariates($covariates) approximate  seed(123)
				
				* outreg results
				*distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
				local n_obs = `r(N)'
				local inf_estimate = `r(obs_stat)'
				local pvalue=`r(asy_pval)'
				local rw = `r(wr)'
				local lw = `r(wl)'
				
				if ("`kern'"=="epan") {
					local kern "epanechnikov"
				}
				rdrobust `dep_var' norm_dist, c(0) p(`pols')  bwselect(`bandwidth_choice') kernel(`kern') vce(cluster `cluster_level')

				outreg2 using "Output/RandInfTable2_CropShare`pol'.tex", append se tex noobs pvalue addtext(Polynomial, `pols', Kernel, "`kern'", Fuzzy RD, N) addstat(Estimate, `inf_estimate',Randomization P-Value, `pvalue', Observations, `n_obs', Mean Dep. Var., `summ', Right Window, `rw', Left Window, `lw')
				
		}
		
	*}
	
}

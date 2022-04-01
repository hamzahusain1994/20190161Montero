*******************************************************
*** ESLR: LR Ag Outcomes - RD Robustness - Censo IV ***
*******************************************************

capture log close
clear
set matsize 3000
set more off

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

** Baseline: Will use local linear rd with MSE optimal bandwidth
** with ses clustered at propietor level. 
** Will also use two-sided MSE optimal bandwidth since big diff in density on
** both sides.
** Will use rdrobust package

local polynomial_levels 0 1 2
local bandwidth_choice `" "mserd" "msetwo" "cerrd" "certwo" "'
local kernel_choice `" "tri" "uni" "epanechnikov" "'
local cluster_level "Expropretario_ISTA"

** Also do Local Randomization methods with rdlocrand

**********************************************
*** OUTCOME 1A - AGRICULTURAL PRODUCTIVITY ***
**********************************************

set more off

foreach pol in `polynomial_levels' {
	local count = 0
	foreach band in `bandwidth_choice' {

		foreach kern in `kernel_choice' {
		
			
			capture {
			rdrobust ln_agprod_pricew_crops norm_dist, c(0) p(`pol') bwselect(`band') kernel(`kern') vce(cluster `cluster_level') 
			} 
			if _rc==1 {
			rdrobust ln_agprod_pricew_crops norm_dist, c(0) p(`pol') b(100) h(150) kernel(`kern') vce(cluster `cluster_level') 
			}
				* outreg results
				distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
				local n_clust = `r(ndistinct)'
				su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
				if `count'==0 { 
					outreg2 using "Output/TableRDRobustness1_LogProductivity`pol'.tex", replace se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Polynomial, `pol', Bandwidth Type, "`band'", Kernel, "`kern'", Fuzzy RD, N)
				} 
				if `count'!=0{
					outreg2 using "Output/TableRDRobustness1_LogProductivity`pol'.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Polynomial, `pol', Bandwidth Type, "`band'", Kernel, "`kern'",Fuzzy RD, N)
				}
				local count = 1
		}
		
	}
	
}

foreach pol in `polynomial_levels' {
	local count = 0
	foreach band in `bandwidth_choice' {

		foreach kern in `kernel_choice' {
		
			capture {
			rdrobust ln_agprod norm_dist, c(0) p(`pol') bwselect(`band') kernel(`kern') vce(cluster `cluster_level') 
			} 
			if _rc==1 {
			rdrobust ln_agprod norm_dist, c(0) p(`pol') b(100) h(150) kernel(`kern') vce(cluster `cluster_level') 
			}
				* outreg results
				distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
				local n_clust = `r(ndistinct)'
				su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
				if `count'==0 { 
					outreg2 using "Output/TableRDRobustness1_LogProfits`pol'.tex", replace se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Polynomial, `pol', Bandwidth Type, "`band'", Kernel, "`kern'", Fuzzy RD, N)
				} 
				if `count'!=0{
					outreg2 using "Output/TableRDRobustness1_LogProfits`pol'.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Polynomial, `pol', Bandwidth Type, "`band'", Kernel, "`kern'",Fuzzy RD, N)
				}
				local count = 1
		}
		
	}
	
}

foreach pol in `polynomial_levels' {
	local count = 0
	foreach band in `bandwidth_choice' {

		foreach kern in `kernel_choice' {
		
			capture {
			rdrobust ln_tfp_geo norm_dist, c(0) p(`pol') bwselect(`band') kernel(`kern') vce(cluster `cluster_level') 
			} 
			if _rc==1 {
			rdrobust ln_tfp_geo norm_dist, c(0) p(`pol') b(100) h(150) kernel(`kern') vce(cluster `cluster_level') 
			}
				* outreg results
				distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
				local n_clust = `r(ndistinct)'
				su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
				if `count'==0 { 
					outreg2 using "Output/TableRDRobustness1_TFP`pol'.tex", replace se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Polynomial, `pol', Bandwidth Type, "`band'", Kernel, "`kern'", Fuzzy RD, N)
				} 
				if `count'!=0{
					outreg2 using "Output/TableRDRobustness1_TFP`pol'.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Polynomial, `pol', Bandwidth Type, "`band'", Kernel, "`kern'",Fuzzy RD, N)
				}
				local count = 1
		}
		
	}
	
}

******************************
*** OUTCOME 2 - CASH CROPS ***
******************************

** SHARE LAND IN CASH CROPS:
set more off

foreach pol in `polynomial_levels' {
	local count = 0
	foreach band in `bandwidth_choice' {

		foreach kern in `kernel_choice' {
		
			rdrobust CashCrop_Share norm_dist, c(0) p(`pol') bwselect(`band') kernel(`kern') vce(cluster `cluster_level') 
				* outreg results
				distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
				local n_clust = `r(ndistinct)'
				su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
				if `count'==0 { 
					outreg2 using "Output/TableRDRobustness2_CashCropShare`pol'.tex", replace se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Polynomial, `pol', Bandwidth Type, "`band'", Kernel, "`kern'", Fuzzy RD, N)
				} 
				if `count'!=0{
					outreg2 using "Output/TableRDRobustness2_CashCropShare`pol'.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Polynomial, `pol', Bandwidth Type, "`band'", Kernel, "`kern'",Fuzzy RD, N)
				}
				local count = 1
		}
		
	}
	
}


********************************
*** OUTCOME 3 - STAPLE CROPS ***
********************************

** SHARE LAND IN STAPLE CROPS:
set more off

foreach pol in `polynomial_levels' {
	local count = 0
	foreach band in `bandwidth_choice' {

		foreach kern in `kernel_choice' {
		
			rdrobust StapleCrop_Share norm_dist, c(0) p(`pol') bwselect(`band') kernel(`kern') vce(cluster `cluster_level') 
				* outreg results
				distinct `cluster_level' if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)' & `e(outcomevar)'!=. & `e(runningvar)'!=.
				local n_clust = `r(ndistinct)'
				su `e(outcomevar)'  if norm_dist < `e(h_l)' & norm_dist > -1*`e(h_r)'
				if `count'==0 { 
					outreg2 using "Output/TableRDRobustness3_StapleCropShare`pol'.tex", replace se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Polynomial, `pol', Bandwidth Type, "`band'", Kernel, "`kern'", Fuzzy RD, N)
				} 
				if `count'!=0{
					outreg2 using "Output/TableRDRobustness3_StapleCropShare`pol'.tex", append se tex noobs addstat(Observations, `e(N_h_l)' + `e(N_h_r)', Clusters, `n_clust', Mean Dep. Var., `r(mean)', Bandwidth, `e(h_l)') addtext(Polynomial, `pol', Bandwidth Type, "`band'", Kernel, "`kern'",Fuzzy RD, N)
				}
				local count = 1
		}
		
	}
	
}


capture log close
clear
set matsize 3000
set more off

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

***************************************
*** SENSITIVITY LAND/ASSET EARNINGS ***
***************************************

local count = 1
		
local cluster_level Expropretario_ISTA
	
foreach rate of numlist 0 57.17 114.15 201.13 {

	capture drop asset_per_worker 
	gen asset_per_worker =0
	replace asset_per_worker = ((`rate'*AREA_HECTAREA)/coop_size)/12 if reform==1 
	replace asset_per_worker = 0 if asset_per_worker==.
	capture drop hh_income_pc_minus_asset
	gen hh_income_pc_minus_asset = hh_inc_pc_real  - asset_per_worker 

	local bwidth = 300

	reg hh_income_pc_minus_asset Above500 norm_dist c.norm_dist#Above500 i_year* if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
		sum hh_income_pc_minus_asset if abs(norm_dist)<`bwidth'
	if(`count'==1) {
		outreg2 using "./Output/Table_Earnings_Sensitivity.tex", replace se tex noobs nocons nor2 keep(Above500)addstat(Land Value,`rate', Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')
	} 
	else {
	outreg2 using "./Output/Table_Earnings_Sensitivity.tex", append se tex noobs nocons nor2 keep(Above500) addstat(Land Value,`rate', Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')
	}
	local bwidth = 150

	reg hh_income_pc_minus_asset Above500 norm_dist c.norm_dist#Above500 i_year* if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
		sum hh_income_pc_minus_asset if abs(norm_dist)<`bwidth'
		outreg2 using "./Output/Table_Earnings_Sensitivity.tex", append se tex noobs nocons nor2 keep(Above500) addstat(Land Value,`rate', Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')

	*restore
	
	local count = `count' + 1
	
}

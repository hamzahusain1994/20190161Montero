***************************************************************
******* ESLR:Ag-Census-Plot-Level Outcomes-Extensions **********
****************************************************************

capture log close
clear
set matsize 3000
set more off


*********************
*** Load the Data ***
*********************

use "Data/censo_ag_wreform.dta", clear 

**********************
*** Label the Data ***
**********************

** Label Variables for the output:
label variable ln_agprod_pricew_crops "Agricultural Productivity (ln($/ha))"
label variable CashCrop_Share "Share of Property for Cash Crops"
label variable StapleCrop_Share "Share of Property for Staple Crops"
label variable norm_dist "Distance to Reform Threshold (ha)"
label variable own_amt "Former Owner's Cumulative Landholdings (ha)"

*********************
*** Set RD Params ***
*********************

** Baseline: Will use local linear rd with MSE optimal bandwidth
** with ses clustered at propietor level. 

local polynomial_level 1
local bandwidth_choice "mserd" 
local kernel_choice "tri" 
local cluster_level "Expropretario_ISTA"


**********************
*** CAPITAL STOCKS ***
**********************

* S16A - MDSC - Type of Capital
capture drop S16*
merge 1:1 agg_id using "./Data/censo_ag_investments.dta", gen(cap_merge)

** To Store Results:
global tflist ""
global modseq=0
global modid = 1

foreach dep_var of varlist S16A* {
	dis "`dep_var'"
	clear matrix
	** Coef Plots of Capital Stocks
	global modseq=$modseq+1
	tempfile tf$modseq 
	** Run RD: (Indicator on Prob. of Having Particular Capital Unit:
	capture rdrobust `dep_var' norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level') 
	** Store Results:
	capture parmest, label idn($modseq)  idstr("`dep_var'") saving(`tf$modseq',replace) flist(tflist)
}

preserve
dsconcat $tflist
sort idnum
outsheet using "./Output/Temp/CapitalStocks.csv", replace comma 
restore


**********************
*** INPUT MEASURES ***
**********************

* S15B - MDSC - Type of Input
capture drop S15*
merge 1:1 agg_id using "./Data/censo_ag_inputs.dta", gen(S15B_merge)
** To Store Results:
global tflist ""
global modseq=0
global modid = 1


foreach dep_var of varlist S15B* {
	dis "`dep_var'"
	clear matrix
	** For Coef Plots:
	global modseq=$modseq+1
	tempfile tf$modseq 
	** Run RD: (Indicator on Prob. of Having Used Particular Input:
	capture rdrobust `dep_var' norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level') 
	** Store Results:
	capture parmest, label idn($modseq)  idstr("`dep_var'") saving(`tf$modseq',replace) flist(tflist)
}

preserve
dsconcat $tflist
sort idnum
outsheet using "./Output/Temp/InputUse.csv", replace comma 
restore
drop S15B*


********************************************************************************
******************************** OTHER PRODUCTS ********************************
********************************************************************************
	
* S5B - MDSC - Minor Crops - Vegetables:
merge 1:1 agg_id using "./Data/censo_ag_minorcrops.dta", gen(S5B_merge)

** INDICATORS:
** To Store Results:
global tflist ""
global modseq=0
global modid = 1

foreach dep_var of varlist S5B* {
	dis "`dep_var'"
	clear matrix
	** For Coef Plots:
	global modseq=$modseq+1
	tempfile tf$modseq 
	** Run RD: (Indicator on Prob. of Prod a Minor Crop:
	capture rdrobust `dep_var' norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level') 
	** Store Results:
	capture parmest, label idn($modseq)  idstr("`dep_var'") saving(`tf$modseq',replace) flist(tflist) 
}

preserve
dsconcat $tflist
sort idnum
outsheet using "./Output/Temp/MinorCropProduction.csv", replace comma 
restore

	
* S5B - MDSC - Minor Crops - Fruits:
merge 1:1 agg_id using "./Data/censo_ag_minorfruits.dta", gen(S8B_merge)

** INDICATORS:
** To Store Results:
global tflist ""
global modseq=0
global modid = 1

foreach dep_var of varlist S8B* {
	dis "`dep_var'"
	clear matrix
	** For Coef Plots:
	global modseq=$modseq+1
	tempfile tf$modseq 
	** Run RD: (Indicator on Prob. of Prod a Minor Crop:
	capture rdrobust `dep_var' norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level') 
	** Store Results:
	capture parmest, label idn($modseq)  idstr("`dep_var'") saving(`tf$modseq',replace) flist(tflist) 
}

preserve
dsconcat $tflist
sort idnum
outsheet using "./Output/Temp/MinorFruitProduction.csv", replace comma 
restore

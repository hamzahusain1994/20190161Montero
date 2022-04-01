***************************************************************
******* ESLR: RD Plot - Plot-Level Outcomes - Modern **********
***************************************************************

capture log close
clear
set matsize 3000
set more off
set scheme s2color // Default Scheme

*********************
*** Load the Data ***
*********************

use "Data/censo_ag_wreform.dta", clear 

**********************
*** Label the Data ***
**********************

** Label Variables for the output:
label variable ln_agprod_pricew_crops "Revenues per Hectare (ln($/ha))"
label variable ln_agprod "Profits per Hectare (ln($/ha))"
label variable ln_tfp_geo "Farm Productivity (ln(s))"

label variable CashCrop_Share "Share of Property for Cash Crops"
label variable StapleCrop_Share "Share of Property for Staple Crops"
label variable norm_dist "Distance to Reform Threshold (ha)"
label variable own_amt "Former Owner's Cumulative Landholdings (ha)"

******************
*** Set Params ***
******************

** Baseline: Will use local linear rd with MSE optimal bandwidth
** with ses clustered at propietor level. 
** Will also use two-sided MSE optimal bandwidth since big diff in density on
** both sides.
** Will use rdrobust package

local polynomial_level 1
local bandwidth_choice "mserd" 
local kernel_choice "uni" 
local kernel_choice_lpoly "rec" 

local cluster_level Expropretario_ISTA
local lpoly_degree 1

******************************
*** MAKE A SET OF RD PLOTS ***
******************************

** Define outcome variables for the plot:
local dep_vars ln_agprod_pricew_crops ln_agprod ln_tfp_geo CashCrop_Share StapleCrop_Share

** define any controls:

** bin width:
local bin_widths 25 10 

** Keep Variables of Interest:
keep `dep_vars' norm_dist Expropretario_ISTA own_amt
sort norm_dist

foreach y_var of varlist `dep_vars' {

	foreach bin_width in `bin_widths' {

	preserve
	
	* Display Current Variable:
	dis "`y_var'"
	
	* Label Variables for Output Later On:
	local ylabel : variable label `y_var'
	local xlabel : variable label own_amt
	
	** Find Optimal Bandwidth:
	rdbwselect `y_var' norm_dist, c(0) p(`polynomial_level') bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level') 
	local bw = `e(h_mserd)'
	local xmin = 500 -`e(h_mserd)'
	local xmax = 500 +`e(h_mserd)'
	
	* Find Max and Min Vars for later on:
	qui sum `y_var' if own_amt>= 500 - `e(h_mserd)' & own_amt <= 500 + `e(h_mserd)'
	local ymax = `r(max)'
	local ymin = `r(min)'
	local ytick_space = (`ymax' - `ymin')/5

	** Fit LPoly ** Using lpoly from Dell 2015: Distance to 500
		tempfile tempdata
		save `tempdata', replace

		keep if (own_amt>500.00 & own_amt< 500 + `e(h_mserd)')
		lpoly `y_var' own_amt, kernel(`kernel_choice_lpoly') bwidth(`bw') degree(`lpoly_degree')  generate(x s) se(se) nograph cluster(`cluster_level') pwidth(`bw') 
		keep x s se 
		drop if x==.
		save "Output/Temp/RD", replace

		use `tempdata', clear
		keep if (own_amt<500.00 & own_amt>  500 - `e(h_mserd)')
		dis "(own_amt<500.00 & own_amt>  500 - `e(h_mserd)')"
		lpoly `y_var' own_amt, kernel(`kernel_choice_lpoly') bwidth(`bw') degree(`lpoly_degree')  generate(x s) se(se) nograph cluster(`cluster_level') pwidth(`bw') 
		keep x s se 
		drop if x==.
		append using "Output/Temp/RD"
		
		g ciplus=s+1.96*se
		g ciminus=s-1.96*se
		keep if x> 500 - `e(h_mserd)' & x < 500 + `e(h_mserd)'
		save "Output/Temp/RD", replace
	
	** Use the lpoly estimates to find means within beans	
		use `tempdata', replace
		keep if abs(norm_dist)<`bw'
		
		gen bin5=.
		foreach X of num 0(`bin_width')`bw' {
			di "`X'"
			replace bin=-`X' if (own_amt - 500.00 >=-`X' & own_amt-500.00<(-`X'+`bin_width') & own_amt-500.00<0)
			replace bin=`X' if (own_amt -500.00>`X' & own_amt-500.00<=(`X'+`bin_width'))
		}
		tab bin5
		
		drop if bin5==.
		collapse `y_var' own_amt, by(bin5)
		
		append using "Output/Temp/RD"
		
	** Plot and Save Output:
	local xmin = round(`xmin'-5.1,10)
	local xmax = round(`xmax'+5.1,10)
	local ymin = round(`ymin')
	local ymax = round(`ymax')
	dis "`ymin'(`ytick_space')`ymax'"
		dis "YyMIN::: `ymin' AND YMAX: `ymax'; PLOT:  yscale(r(`ymin' `ymax')) "
		dis "xmin::: `xmin' AND YMAX: `ymax'; PLOT:  yscale(r(`ymin' `ymax')) "

	if("`y_var'" == "CashCrop_Share"  | "`y_var'" ==  "StapleCrop_Share") {
	
	twoway (connected s x if x>500.00, sort msymbol(none) clcolor(black) clpat(solid) clwidth(medthick)) /*
	*/(connected ciplus x if x>500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/(connected ciminus x if x>500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/(connected s x if x<500.00, sort msymbol(none) clcolor(black) clpat(solid) clwidth(medthick)) /*
	*/(connected ciplus x if x<500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/(connected ciminus x if x<500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/ (scatter `y_var' own_amt, sort msize(med)xline(500) mcolor(black)), /*
	*/ legend(off) graphregion(color(white)) yscale(r(`ymin' `ymax')) ylabel(`ymin'(`ytick_space')`ymax')  /*
	*/ ytitle("`ylabel'")  xtitle("`xlabel'")  /* xlabel(`xmin'(50)`xmax')  ysc(r(`ymin' `ymax'))  ylabel(`ymin'(`ytick_space')`ymax')  xsc(r(`xmin' `xmax'))
	*/xline(500.00, lpattern(shortdash) lc(black)) ylab(,nogrid) /*
	  */ saving("Output/RDPlot_`y_var'_`bin_width'.pdf",replace)
	 	graph export "Output/RDPlot_`y_var'_`bin_width'.pdf", replace

	 } 
	 else {
	 twoway (connected s x if x>500.00, sort msymbol(none) clcolor(black) clpat(solid) clwidth(medthick)) /*
	*/(connected ciplus x if x>500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/(connected ciminus x if x>500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/(connected s x if x<500.00, sort msymbol(none) clcolor(black) clpat(solid) clwidth(medthick)) /*
	*/(connected ciplus x if x<500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/(connected ciminus x if x<500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/ (scatter `y_var' own_amt, sort msize(med)xline(500) mcolor(black)), /*
	*/ legend(off) graphregion(color(white))  /*
	*/ ytitle("`ylabel'")  xtitle("`xlabel'") xlabel(`xmin'(50)`xmax')   xsc(range(`xmin'(50)`xmax')) /* xsc(r(`xmin' `xmax')) ysc(r(`ymin' `ymax'))  ylabel(`ymin'(`ytick_space')`ymax') 
	*/xline(500.00, lpattern(shortdash) lc(black)) ylab(,nogrid) /*
	  */ saving("Output/RDPlot_`y_var'_`bin_width'.pdf",replace)
	 	graph export "Output/RDPlot_`y_var'_`bin_width'.pdf", replace
	 }
	  
	
	restore
	
	}
}

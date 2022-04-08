******************************************************
******* ESLR: RD Plot - Plot-Level Outcomes **********
******************************************************

capture log close
clear
set matsize 3000
set more off

******************************************************

** Load the Data:
use "Data/prop_data.dta", clear 

gen Above500 = 0
replace Above500 = 1 if Total_Propretario >= 500.00
label var Above500 "Above 500 Ha"
gen norm_dist = Total_Propretario - 500.00
label var norm_dist "Normalized Distance to Reform Threshold (has)"
gen own_amt = Total_Propretario
label var own_amt "Cumulative Landholdings of Former Owner (has)"


******************************************************

** Using lpoly from Dell 2015: Distance to 500

keep if norm_dist<300.00 & norm_dist> -300.00
sort norm_dist

tempfile tempdata
save `tempdata', replace

	keep if (own_amt>500.00 & own_amt<800.00)
	lpoly reform own_amt if (own_amt>500.00 & own_amt<800.00), kernel(rectangle) bwidth(300) degree(2)  generate(x s) se(se) nograph
	keep x s se 
	drop if x==.
	save "Output/Temp/RD", replace

	use `tempdata', clear
	keep if (own_amt<500.00 & own_amt>200.00)
	lpoly reform own_amt if (own_amt<500.00 & own_amt>200.00), kernel(rectangle) bwidth(300) degree(2)  generate(x s) se(se) nograph
	keep x s se 
	drop if x==.
	append using "Output/Temp/RD"
	
	g ciplus=s+1.96*se
	g ciminus=s-1.96*se
	keep if x>200.00 & x<800.00
	save "Output/Temp/RD", replace

	
	*---generate bins for taking averages---*
	
	use `tempdata', replace
	keep if abs(norm_dist)<300.00
	
	gen bin5=.
	foreach X of num 0(25.00)300.00 {
		di "`X'"
		replace bin=-`X' if (own_amt - 500.00 >=-`X' & own_amt-500.00<(-`X'+25.00) & own_amt-500.00<0)
		replace bin=`X' if (own_amt -500.00>`X' & own_amt-500.00<=(`X'+25.00))
	}
	tab bin5
	
	drop if bin5==.
	collapse reform own_amt, by(bin5)
	
	append using "Output/Temp/RD"

	
	twoway (connected s x if x>500.00, sort msymbol(none) clcolor(black) clpat(solid) clwidth(medthick)) /*
	*/(connected ciplus x if x>500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/(connected ciminus x if x>500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/(connected s x if x<500.00, sort msymbol(none) clcolor(black) clpat(solid) clwidth(medthick)) /*
	*/(connected ciplus x if x<500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/(connected ciminus x if x<500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/ (scatter reform own_amt, sort msize(med)xline(500) mcolor(black)), /*
	*/ legend(off) graphregion(color(white)) /*
	*/ ytitle("Expropriated")  xtitle("Cumulative Landholdings (ha)") xlabel(200(100)800) xsc(r(200.00 800.00)) ylabel(0(.2)1) ysc(r(0 1)) /*
	*/xline(500.00, lpattern(shortdash) lc(black)) ylab(,nogrid) /*
	  */ saving("./Output/RDPlot_ReformFS_Dist_300_3.pdf",replace)
	graph export "./Output/RDPlot_ReformFS_Dist_300_3.pdf", replace
	

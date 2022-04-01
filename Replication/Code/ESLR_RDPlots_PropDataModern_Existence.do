***************************************************************
******* ESLR: RD Plot - Plot-Level Outcomes - Modern **********
***************************************************************

capture log close
clear
set matsize 3000
set more off
set scheme s2color // Default Scheme

***************************************************************

** Load the Data on Prop. in 1980 and whether they appear in the 2007 Census:
use "./Data/LR_reform_existence.dta", clear 

gen norm_dist = Total_Propretario2 - 500.00
gen own_amt = Total_Propretario2

label var norm_dist "Normalized Distance to Reform Threshold (has)"
label var own_amt "Cumulative Landholdings of Former Owner (has)"
label var Exists "Exists in 2007"

***************************************************************

** Using lpoly from Dell 2015: Distance to 500

keep Exists own_amt norm_dist
keep if norm_dist<300.00 & norm_dist> -300.00
sort norm_dist

tempfile tempdata
save `tempdata', replace

** Dep Var: Exists; Bandwidth=300; Degree=1
	use `tempdata', clear
	keep if (own_amt>500.00 & own_amt<800.00)
	lpoly Exists own_amt if (own_amt>500.00 & own_amt<800.00), kernel(rectangle) bwidth(300) degree(1)  generate(x s) se(se) nograph pwidth(150)
	keep x s se 
	drop if x==.
	save "Output/Temp/RD", replace

	use `tempdata', clear
	keep if (own_amt<500.00 & own_amt>200.00)
	lpoly Exists own_amt if (own_amt<500.00 & own_amt>200.00), kernel(rectangle) bwidth(300) degree(1)  generate(x s) se(se) nograph pwidth(150)
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
	collapse Exists own_amt, by(bin5)
	
	append using "Output/Temp/RD"
	
	twoway (connected s x if x>500.00, sort msymbol(none) clcolor(black) clpat(solid) clwidth(medthick)) /*
	*/(connected ciplus x if x>500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/(connected ciminus x if x>500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/(connected s x if x<500.00, sort msymbol(none) clcolor(black) clpat(solid) clwidth(medthick)) /*
	*/(connected ciplus x if x<500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/(connected ciminus x if x<500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/ (scatter Exists own_amt, sort msize(med)xline(500) mcolor(black)), /*
	*/ legend(off) graphregion(color(white)) /*
	*/ ytitle("Exists in 2007")  xtitle("Distance to Reform Threshold (ha)") xlabel(200(100)800) xsc(r(200.00 800.00)) ylabel(0(.2)1) ysc(r(0 1)) /*
	*/xline(500.00, lpattern(shortdash) lc(black)) ylab(,nogrid) /*
	  */ saving("Output/RDPlot_exists_300.pdf",replace)
	graph export "Output/RDPlot_exists_300.pdf", replace

***************************************************************

** Using lpoly from Dell 2015: Distance to 500

** Dep Var: Exists; Bandwidth=150; Degree=1
	use `tempdata', clear
	keep if (own_amt>500.00 & own_amt<650.00)
	lpoly Exists own_amt if (own_amt>500.00 & own_amt<650.00), kernel(rectangle) bwidth(150) degree(1)  generate(x s) se(se) nograph pwidth(150)
	keep x s se 
	drop if x==.
	save "Output/Temp/RD", replace

	use `tempdata', clear
	keep if (own_amt<500.00 & own_amt>350.00)
	lpoly Exists own_amt if (own_amt<500.00 & own_amt>350.00), kernel(rectangle) bwidth(150) degree(1)  generate(x s) se(se) nograph pwidth(150)
	keep x s se 
	drop if x==.
	append using "Output/Temp/RD"
	
	g ciplus=s+1.96*se
	g ciminus=s-1.96*se
	keep if x>350.00 & x<650.00
	save "Output/Temp/RD", replace

	
	*---generate bins for taking averages---*
	
	use `tempdata', replace
	keep if abs(norm_dist)<150.00
	
	gen bin5=.
	foreach X of num 0(25.00)150.00 {
		di "`X'"
		replace bin=-`X' if (own_amt - 500.00 >=-`X' & own_amt-500.00<(-`X'+25.00) & own_amt-500.00<0)
		replace bin=`X' if (own_amt -500.00>`X' & own_amt-500.00<=(`X'+25.00))
	}
	tab bin5
	
	drop if bin5==.
	collapse Exists own_amt, by(bin5)
	
	append using "Output/Temp/RD"
	
	twoway (connected s x if x>500.00, sort msymbol(none) clcolor(black) clpat(solid) clwidth(medthick)) /*
	*/(connected ciplus x if x>500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/(connected ciminus x if x>500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/(connected s x if x<500.00, sort msymbol(none) clcolor(black) clpat(solid) clwidth(medthick)) /*
	*/(connected ciplus x if x<500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/(connected ciminus x if x<500.00, sort msymbol(none) clcolor(black) clpat(shortdash) clwidth(thin)) /*
	*/ (scatter Exists own_amt, sort msize(med)xline(500) mcolor(black)), /*
	*/ legend(off) graphregion(color(white)) /*
	*/ ytitle("Exists in 2007")  xtitle("Distance to Reform Threshold (ha)") xlabel(350(100)650) xsc(r(350.00 650.00)) ylabel(0(.2)1) ysc(r(0 1)) /*
	*/xline(500.00, lpattern(shortdash) lc(black)) ylab(,nogrid) /*
	  */ saving("Output/RDPlot_exists_150.pdf",replace)
	graph export "Output/RDPlot_exists_150.pdf", replace

	

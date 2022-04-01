*****************************************************
*** ESLR: LR Ag Outcomes - RD Analysis - Censo IV ***
*****************************************************

capture log close
clear
set matsize 3000
set more off

*********************
*** Load the Data ***
*********************

use "./Data/LR_reform_existence.dta", clear 

gen Above500 = 0
replace Above500 = 1 if Total_Propretario2 >= 500.00
label var Above500 "Above 500 Ha"
gen norm_dist = Total_Propretario2 - 500.00
label var norm_dist "Normalized Distance to Reform Threshold (has)"
gen own_amt = Total_Propretario2


***********************************************
*** Make Summary Table - Property Ownership ***
***********************************************

** Gen Variabale for # properties with same ID;
egen num_props_owner = count(Total_Propretario2), by(Expropretario_ISTA)
replace num_props_owner=1 if num_props_owner==0
gen mult_prop = 0 if num_props_owner!=.
replace mult_prop = 1 if num_props_owner >1 & num_props_owner!=.
drop if num_props_owner==108
label var own_amt "Owner: Cumulative Holdings (ha)"
label var AREA_HECTAREA "Property Size (ha)"
label var mult_prop	 "Owner Owned Multiple Properties"
label var num_props_owner	 "Number of Properties Owned by Owner "


eststo clear
eststo: quietly estpost summarize  AREA_HECTAREA own_amt mult_prop num_props_owner, detail

esttab using "Output/Table_Prop_SummStat.tex", replace ///
	cells("mean(fmt(2)) sd(fmt(2)) p50(fmt(2)) p25(fmt(2)) p75(fmt(2))") label booktab nonumber nomtitles
eststo clear

eststo clear
eststo: quietly estpost summarize AREA_HECTAREA own_amt mult_prop num_props_owner if Above500==0, detail

esttab using "Output/Table_Prop_SummStat_A5000.tex", replace ///
	cells("mean(fmt(2)) sd(fmt(2)) p50(fmt(2)) p25(fmt(2)) p75(fmt(2))") label booktab nonumber nomtitles
eststo clear

eststo clear
eststo: quietly estpost summarize AREA_HECTAREA own_amt mult_prop num_props_owner if Above500==1, detail

esttab using "Output/Table_Prop_SummStat_A5001.tex", replace ///
	cells("mean(fmt(2)) sd(fmt(2)) p50(fmt(2)) p25(fmt(2)) p75(fmt(2))") label booktab nonumber nomtitles
eststo clear


**************************
*** Load the 2007 Data ***
**************************

use "Data/censo_ag_wreform.dta", clear 


***********************************************
*** Make Summary Table - Property Ownership ***
***********************************************

** Gen Variabale for # properties with same ID;
egen num_props_owner = count(Total_Propretario2), by(Expropretario_ISTA)
replace num_props_owner=1 if num_props_owner==0
gen mult_prop = 0 if num_props_owner!=.
replace mult_prop = 1 if num_props_owner >1 & num_props_owner!=.
drop if num_props_owner==108

label var own_amt "Owner: Cumulative Holdings (ha)"
label var AREA_HECTAREA "Property Size (ha)"
label var mult_prop	 "Owner Owned Multiple Properties"
label var num_props_owner	 "Number of Properties Owned by Owner "


eststo clear
eststo: quietly estpost summarize  AREA_HECTAREA own_amt mult_prop num_props_owner, detail

esttab using "Output/Table_Prop_SummStat2007.tex", replace ///
	cells("mean(fmt(2)) sd(fmt(2)) p50(fmt(2)) p25(fmt(2)) p75(fmt(2))") label booktab nonumber nomtitles
eststo clear

eststo clear
eststo: quietly estpost summarize AREA_HECTAREA own_amt mult_prop num_props_owner if Above500==0, detail

esttab using "Output/Table_Prop_SummStat2007_A5000.tex", replace ///
	cells("mean(fmt(2)) sd(fmt(2)) p50(fmt(2)) p25(fmt(2)) p75(fmt(2))") label booktab nonumber nomtitles
eststo clear

eststo clear
eststo: quietly estpost summarize AREA_HECTAREA own_amt mult_prop num_props_owner if Above500==1, detail

esttab using "Output/Table_Prop_SummStat2007_A5001.tex", replace ///
	cells("mean(fmt(2)) sd(fmt(2)) p50(fmt(2)) p25(fmt(2)) p75(fmt(2))") label booktab nonumber nomtitles
eststo clear

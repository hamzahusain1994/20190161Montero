*****************************************************
*** ESLR: EHPM Outcomes - RD Analysis - HH Survey ***
*****************************************************

capture log close
clear
set matsize 3000
set more off

************************************
*** OUTCOME - PUBLIC GOOD ACCESS ***
************************************

use "Data/ehpm_pgmodule.dta", clear

** To Store Results + Plot in R:
	global tflist ""
	global modseq=0
	global modid = 1

	local bwidth =300
	local cluster_level "Expropretario_ISTA"

** STD:
egen std_Above500 = std(Above500) if abs(norm_dist) < `bwidth' 


foreach dep_var of varlist time_* {
	
	clear matrix
		
	global modseq=$modseq+1
	tempfile tf$modseq 
	
	capture egen std_`dep_var' = std(`dep_var')
	
	** With Survey FEs and with baseline covariates + type fixed effects
	
	* Type FEs:
	capture drop i_type_`dep_var'*
	tab type_`dep_var', gen(i_type_`dep_var')
	
	* Reg:
	reg std_`dep_var' std_Above500 norm_dist c.norm_dist#c.std_Above500  i_type_`dep_var'* i_year* age age2 sex if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
	local count = `count' + 1 
	capture parmest, ylabel label idn($modseq)  idstr("`dep_var'") saving(`tf$modseq',replace) flist(tflist)
	}
	
	preserve
dsconcat $tflist
sort idnum
outsheet using "Output/Parmest_EHPM_PGs.csv", replace comma 

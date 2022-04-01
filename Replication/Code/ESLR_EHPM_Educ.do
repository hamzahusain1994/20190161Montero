*****************************************************
*** ESLR: EHPM Outcomes - RD Analysis - HH Survey ***
*****************************************************

capture log close
clear
set matsize 3000
set more off


***************************
*** OUTCOME - EDUCATION ***
***************************


use "Data/ehpm_educmodule.dta", clear


local cluster_level Expropretario_ISTA


local bwidth =300
reg educ_yrs Above500 norm_dist c.norm_dist#Above500 i_year* age age2 sex if abs(norm_dist) < `bwidth' & age > 25, cluster(`cluster_level')
	sum educ_yrs if abs(norm_dist)<`bwidth'
	outreg2 using "Output/Table_EHPM_Educ.tex", replace se tex noobs nocons nor2 keep(Above500)  addstat(Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')


local bwidth =300
reg literate Above500 norm_dist c.norm_dist#Above500 i_year* age age2 sex if abs(norm_dist) < `bwidth' & age > 25 , cluster(`cluster_level')
	sum literate if abs(norm_dist)<`bwidth'
	outreg2 using "Output/Table_EHPM_Educ.tex", append se tex noobs nocons nor2 keep(Above500)  addstat(Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')

******************************
*** OUTCOME - AEG & Num HH ***
******************************

local cluster_level Expropretario_ISTA

local bwidth =300
reg age Above500 norm_dist c.norm_dist#Above500 i_year* sex if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
	sum age if abs(norm_dist)<`bwidth'
	outreg2 using "Output/Table_EHPM_Age.tex", replace se tex noobs nocons nor2 keep(Above500) addstat(Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')

local bwidth =300
reg num_hh Above500 norm_dist c.norm_dist#Above500 i_year*  sex if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
	sum num_hh if abs(norm_dist)<`bwidth'
	outreg2 using "Output/Table_EHPM_Age.tex", append se tex noobs nocons nor2 keep(Above500)  addstat(Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')



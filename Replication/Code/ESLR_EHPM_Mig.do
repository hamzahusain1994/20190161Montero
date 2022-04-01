*****************************************************
*** ESLR: EHPM Outcomes - RD Analysis - HH Survey ***
*****************************************************

capture log close
clear
set matsize 3000
set more off


***************************
*** OUTCOME - MIGRATION ***
***************************


use "Data/ehpm_migmodule.dta", clear
local cluster_level Expropretario_ISTA

local bwidth =300
reg hh_memb_abroad Above500 norm_dist c.norm_dist#Above500 i_year* age age2 sex if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
	sum hh_memb_abroad if abs(norm_dist)<`bwidth'
	outreg2 using "Output/Table_EHPM_Migration.tex", replace se tex nocons nor2 keep(Above500) addstat(Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')
	

reg num_hh_memb_abroad Above500 norm_dist c.norm_dist#Above500 i_year* age age2 sex if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
	sum num_hh_memb_abroad if abs(norm_dist)<`bwidth'
	outreg2 using "Output/Table_EHPM_Migration.tex", append se tex noobs nocons nor2 keep(Above500) addstat(Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')


reg length_recent_hh_memb_abroad Above500 norm_dist c.norm_dist#Above500 i_year* age age2 sex if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
	sum length_recent_hh_memb_abroad if abs(norm_dist)<`bwidth'
	outreg2 using "Output/Table_EHPM_Migration.tex", append se tex noobs nocons nor2 keep(Above500) addstat(Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')

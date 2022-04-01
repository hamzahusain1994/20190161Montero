*****************************************************
*** ESLR: EHPM Outcomes - RD Analysis - HH Survey ***
*****************************************************

capture log close
clear
set matsize 3000
set more off

***************************************
*** OUTCOME - HH CONSUMPTION LEVELS ***
*************************************** 

use "Data/ehpm_consumptionmodule.dta", clear

gen hh_cons_pc_real = (year==2000)*hh_cons_pc*71.57/100 + ///
                 (year==2001)*hh_cons_pc*74.25/100 + (year==2004)*hh_cons_pc*80.68/100 + ///
                 (year==2005)*hh_cons_pc*84.47/100 + (year==2006)*hh_cons_pc*87.88/100 + ///
                 (year==2007)*hh_cons_pc*91.90/100 + (year==2008)*hh_cons_pc*98.06/100 + ///
                 (year==2009)*hh_cons_pc*99.10/100 + (year==2011)*hh_cons_pc*105.13/100 + ///
                 (year==2012)*hh_cons_pc*106.95/100 + (year==2013)*hh_cons_pc*107.79/100
				 winsor2 hh_cons_pc_real, replace  cuts(0 98)

local cluster_level Expropretario_ISTA
local bwidth =300

reg hh_cons_pc_real Above500 norm_dist c.norm_dist#Above500 i_year* age age2 sex if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
	sum hh_cons_pc_real if abs(norm_dist)<`bwidth'
	outreg2 using "Output/Table_ConsumptionCompression.tex", replace se tex noobs nocons nor2 keep(Above500)  addstat(Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')

local bwidth = 150
reg hh_cons_pc_real Above500 norm_dist c.norm_dist#Above500 i_year* age age2 sex if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
	sum hh_cons_pc_real if abs(norm_dist)<`bwidth'
	outreg2 using "Output/Table_ConsumptionCompression.tex", append se tex noobs nocons nor2 keep(Above500) addstat(Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')


preserve

collapse (iqr) hh_cons_pc_real (mean) norm_dist Above500, by(match_id Expropretario_ISTA i_year* sex)

local bwidth =300


reg hh_cons_pc_real Above500 norm_dist c.norm_dist#Above500 i_year* sex if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
	sum hh_cons_pc_real if abs(norm_dist)<`bwidth'
	outreg2 using "Output/Table_ConsumptionCompression.tex", append se tex noobs nocons nor2 keep(Above500) addstat(Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')

local bwidth = 150

reg hh_cons_pc_real Above500 norm_dist c.norm_dist#Above500 i_year* sex if abs(norm_dist) < `bwidth' , cluster(`cluster_level')
	sum hh_cons_pc_real if abs(norm_dist)<`bwidth'
	outreg2 using "Output/Table_ConsumptionCompression.tex", append se tex noobs nocons nor2 keep(Above500) addstat(Observations, `e(N)', Clusters, `e(N_clust)', Mean Dep. Var., `r(mean)', Bandwidth, `bwidth')

restore

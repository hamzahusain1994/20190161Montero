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

******************
*** POWER CALC ***
******************

 
local polynomial_level 1
local bandwidth_choice "mserd" 
local kernel_choice "tri" 
local cluster_level "Expropretario_ISTA"

*Logs OF REVENUE:
rdbwselect  ln_agprod_pricew_crops norm_dist, c(0) p(`polynomial_level')  bwselect(`bandwidth_choice') kernel(`kernel_choice') vce(cluster `cluster_level') 
* STANDARDIZE EFFECT w/in BW:
dis `e(h_mserd)'
egen sd_agprod_bw = sd(ln_agprod_pricew_crops)  
egen mean_agprod_bw = mean(ln_agprod_pricew_crops)  
egen mean_agprod = mean(mean_agprod_bw)
egen sd_agprod = mean(sd_agprod_bw)
egen sd_A500_bw = sd(Above500) if abs(norm_dist) < `e(h_mserd)'
egen sd_Above500 = mean(sd_A500_bw) 

gen std_agprod = ((ln_agprod_pricew_crops - mean_agprod )/sd_agprod)*sd_Above500

set scheme lean1

rdpower std_agprod  norm_dist , c(0) tau(0.5) vce(cluster Expropretario_ISTA) plot
	graph export "Output/AgCensus_Power_Revenues.pdf", replace

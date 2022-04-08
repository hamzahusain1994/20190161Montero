************************ 
*** ESLR: Stata Code ***
************************ 

capture log close
clear
set matsize 3000
set more off
set scheme s2color

** Set Workspace **
*cd /Users/`c(username)'/Dropbox/Research_ElSalvador_LandReform/Replication

cd "/Users/hamzahusain/Library/Mobile Documents/com~apple~CloudDocs/BerkeleyARE/SecondYear/Spring/ECON270B/20190161Montero/Replication"

ssc install rdrobust
ssc install winsor2 
ssc install outreg2 
ssc install outreg 
ssc install estout 
ssc install lpoly /*didn't install, not found at ssc*/
ssc install cmogram 
ssc install dm88_1 /*didn't install, not found at ssc*/
ssc install grqreg 
ssc install gr0002_3 /*didn't install, not found at ssc*/
ssc install pdslasso 
ssc install lassopack /*didn't install, remote connection failed*/
ssc install univar 
ssc install ietoolkit 
ssc install rdlocrand /*didn't install, not found at ssc*/
ssc install rdpower (net install rdpower, from(https://raw.githubusercontent.com/rdpackages/rdpower/master/stata) replace) /*didn't install*/

******************
*** MASTER DO FILE
******************

*** MAIN ***

** FIGURE 1: Land Reforms that Redistributed Haciendas as Cooperatives
	* R Code: ./Code/ESLR_LatAmMaps.R

** FIGURE 2: Land Reform by Canton - El Salvador
	* R Code: ./Code/ESLR_ESMap.R

** FIGURE 4: Estimates for Differences in Geography & FIGURE 3: McCrary Sorting Test
	* R Code: ./Code/ESLR_Balance_PropLevel.R

** FIGURE 5: Phase I Expropriation RD Plot
	do "Code/ESLR_RDPlots_PropData.do"

** TABLES 2-4: Agriculture Choices and Productivity
	do "Code/ESLR_Analysis_IVCenso.do"

** TABLE 5 & FIGURE 6: Impact of Ownership Type on Earnings and Earnings Distributions
	do "Code/ESLR_Analysis_EHPM.do"

** TABLE 6:  Credit Access and Sources - RD Estimates
	do "Code/ESLR_Analysis_IVCenso_Credit.do"

	
	
	
	
	
	
*** APPENDIX ***

** FIGURES D1-D2: RD Plots - Crop Choices & RD Plots - Agricultural Productivity	
	do "./Code/ESLR_RDPlots_AgCensus.do"

** FIGURE D3: RD Plots - Existence in 2007
	do "./Code/ESLR_RDPlots_PropDataModern_Existence.do"
	
** FIGURE D4: Matching Estimates
	* R Code: "./Code/ESLR_IVCensus_Matching.R"
	
** FIGURE D5: Sensitivity to Balance
	* R Code: "./Code/ESLR_Unbalacedness.R"

** FIGURE D6: Temporal External Validity Exercise
	* R Code: "./Code/ESLR_TemporalEV.R"

** TABLES D1-D2: Summary Statistics - Property Sizes in 1980 and Ownership Amounts & Summary Statistics - Property Sizes in 2007 and Ownership Amounts
	do "./Code/ESLR_Prop_SummStats.do"
	
** FIGURE D7: Coefficient Estimates For Existence in 2007 - Heterogeneity by Geographic Characteristics
	* R Code: ./Code/ESLR_Robustness_Existence.R

** TABLE D3 & FIGURE D8: Testing for Differences in the Distribution of Digits for Reported Crop Outputs & Testing for Differences in Bunching in Crop Output Across Ownership Types
	* R Code: ./Code/ESLR_Digits.R

** FIGURE D9: Yield Results: Correcting for Possible Selection Bias
	* R Code: "./Code/ESLR_YieldsSampleSelection.R"

** FIGURES D10-D13: Production of Minor Crops - Fruits & Production of Minor Crops - Vegetables &  Capital Ownership & Input Use
	do "./Code/ESLR_Analysis_IVCenso_Other.do"
	* Then, R Code: ./Code/ESLR_IVCensus_AdditionalPlots.R

	
** FIGURE D14: RD Power Calculations - Revenues per Hectare
	do "./Code/ESLR_IVCensus_Power.do"

** TABLE D4: Impact of Ownership Structure on Earnings Differences - Sensitivity to Land Value Return
	do "./Code/ESLR_EHPM_Sensitivity.do"

** TABLE D5: Consumption and Consumption Distributions
	do "./Code/ESLR_EHPM_Consumption.do"

** FIGURE D15: RD Plot - Share of Land Not Devoted to Staple or Cash Crops in 2007
	do "./Code/ESLR_RDPlots_NonShares.do"

** TABLES D6-D7: Heterogeneity in a Cooperatives’ Census Neighborhoods
	do "./Code/ESLR_AgHeterogeneity.do"

** FIGURES D16-D18: Controlling for Migration Rates – Main Outcomes & Main Results - Controlling for Property Size & Controlling for Conflict During the Civil War – Main Outcomes
	* R Code: "./Code/ESLR_IVCensus_Controls.R"
	
** FIGURE D19: Heterogeneity by Number of Plots Owned By Previous Owner – Main Outcomes
	* R Code: "./Code/ESLR_IVCensus_HetPlots.R"

** FIGURE D20: Crop Allocation - Haciendas Above vs. Below 500 ha Ownership Threshold
	* R Code: "./Code/ESLR_IVCensus_NonComplierPlot.R"

** FIGURE E1: Public Good Access – Time to Nearest Public Good – Estimated Differences
	do "./Code/ESLR_EHPM_PGs.do"
	* Then, R Code: "./Code/ESLR_EHPM_PGsCoefPlot.R"
	
** FIGURE F1: Heterogeneity by Access to Cities – Main Outcomes
	* R Code: "./Code/ESLR_IVCensus_HetPlots.R"

** TABLE F1: Commercialization Avenues - RD Estimates
	do "./Code/ESLR_IVCenso_Commercialization.do"

** Figure F2: Controlling for Commercialization Avenues – Main Outcomes
	* R Code: "./Code/ESLR_IVCensus_Controls.R"

** TABLES G1-G2: Impact of Ownership Type on Education Outcomes & Differences in Age and Household Size
	do "./Code/ESLR_EHPM_Educ.do"

** TABLE H1: Migration Outcomes - Household Survey Data
	do "./Code/ESLR_EHPM_Mig.do"

** TABLES H2-H3: Migration Outcomes - Population Census & H3: Migration Outcomes - Individuals that Completed High School - Population Census 
	* R Code: "./Code/ESLR_CensusMigration.R"

** TABLES I1-I2: Robustness to Alternative RD Method - Randomization Inference Approach
	do "./Code/ESLR_IVCenso_RDRandInf.do"

** TABLES J1-I5: Robustness to Alternative RD Specifications
	do "./Code/ESLR_IVCenso_RDRobustness.do"

** FIGURES J1-J6: Robustness to Alternative RD Specifications
	* R Code: "./Code/ESLR_IVCensus_RDRobustnessPlots.R"

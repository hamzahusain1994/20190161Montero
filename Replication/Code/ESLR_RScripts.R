##################################################################
#### El Salvador - Land Reform - Prop Level Geographical Covs ####
##################################################################

rm(list = ls())       # Clear variables


require(foreign)
require(ggplot2)
require(rgdal)
require(rgeos)
require(RColorBrewer) # creates nice color schemes
require(maptools)     # loads sp library too
require(scales)       # customize scales
require(gridExtra)    # mutiple plots
require(plyr)         # join function
require(dplyr) 
require(mapproj)      # projection tools
require(raster)       # raster tools
require(tidyr)    
require(readstata13)    
require(haven)    
require(gstat)        # interpolation tools
require(ncdf4)      
require(Hmisc)
require(lubridate)
library(lmtest)
library(sandwich)
library(dotwhisker)   # coef plots
library(broom)
require(stringr)
require(readxl)
require(rmapshaper)
require(extrafont)
require(ggmap)
require(rdrobust)
require(stringdist)
require(benford.analysis) # Tests for data manipulation
require(sampleSelection)
require(exactextractr) # faster extract
require(sf) # faster extract
require(elevatr) # elevation data
require(stringi)

## SET WORKING DIRECTORY:
path <- "/Users/eduardomontero/Dropbox/Research_ElSalvador_LandReform/Replication/"
setwd(path)

############################

### R SCRIPTS:


### MAIN ###
  
  ## FIGURE 1: Land Reforms that Redistributed Haciendas as Cooperatives
  source("./Code/ESLR_LatAmMaps.R")
  
  ## FIGURE 2: Land Reform by Canton - El Salvador
  source("./Code/ESLR_ESMap.R")
  
  ## FIGURE 4: Estimates for Differences in Geography & FIGURE 3: McCrary Sorting Test
  source("./Code/ESLR_Balance_PropLevel.R")


### RUN STATA CODE: Code/ESLR_Master.do ####



### APPENDIX ###
  
  ## FIGURE D4: Matching Estimates
  source("./Code/ESLR_IVCensus_Matching.R")
  
  ## FIGURE D5: Sensitivity to Balance
  source("./Code/ESLR_Unbalancedness.R")
  
  ## FIGURE D6: Temporal External Validity Exercise - Agricultural Productivity
  source("./Code/ESLR_TemporalEV.R")
  
  ## FIGURE D7: Coefficient Estimates For Existence in 2007 - Heterogeneity by Geographic Characteristics
  source("./Code/ESLR_Robustness_Existence.R")
  
  ## TABLE D3 & FIGURE D8: Testing for Differences in the Distribution of Digits for Reported Crop Outputs & Testing for Differences in Bunching in Crop Output Across Ownership Types
  source("./Code/ESLR_Digits.R")
  
  ## FIGURE D9: Yield Results: Correcting for Possible Selection Bias
  source("./Code/ESLR_YieldsSampleSelection.R")
  
  ## FIGURE D10-D13: Production of Minor Crops - Fruits & Production of Minor Crops - Vegetables &  Capital Ownership & Input Use
  source("./Code/ESLR_IVCensus_AdditionalPlots.R")
  
  ## FIGURE D16-D18: Controlling for Migration Rates – Main Outcomes & Main Results - Controlling for Property Size & Controlling for Conflict During the Civil War – Main Outcomes
  source("./Code/ESLR_IVCensus_Controls.R")
  
  ## FIGURE D19: Heterogeneity by Number of Plots Owned By Previous Owner – Main Outcomes
  source("./Code/ESLR_IVCensus_HetPlots.R")
  
  ## FIGURE D20: Crop Allocation - Haciendas Above vs. Below 500 ha Ownership Threshold
  source("./Code/ESLR_IVCensus_NonComplierPlot.R")
  
  ## FIGURE E1: Public Good Access – Time to Nearest Public Good – Estimated Differences
  source("./Code/ESLR_EHPM_PGsCoefPlot.R")
  
  ## FIGURE F1: Heterogeneity by Access to Cities – Main Outcomes
  source("./Code/ESLR_IVCensus_HetPlots.R")
  
  ## Figure F2: Controlling for Commercialization Avenues – Main Outcomes
  source("./Code/ESLR_IVCensus_Controls.R")
  
  ## FIGURE J1-J6: Robustness to Alternative RD Specifications
  source("./Code/ESLR_IVCensus_RDRobustnessPlots.R")




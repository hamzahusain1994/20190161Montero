###############################################################
#### El Salvador - Land Reform - Map of Cantons and Reform ####
###############################################################

rm(list = ls())       # Clear variables

require(foreign)
require(ggplot2)
require(rgdal)
require(rgeos)
require(RColorBrewer) # creates nice color schemes
require(plyr)         # join function
require(dplyr) 
require(raster)       # raster tools
require(tidyr)    
require(readstata13)    
require(haven)    
require(exactextractr) # faster extract
require(sf) # faster extract
require(elevatr) # elevation data
require(rdrobust)
require(stringdist)

############## LOAD DATA ################

## Read in Data:
prop_data <- read.dta(file="./Data/prop_data.dta") %>% filter(reform==1, !is.na(CODIGO))
cantons <- st_read("./Data/cantons_wCodigos.shp")

# Set aesthetics:
aesthetics <- list(#guides(color=guide_colorbar(reverse=FALSE)),
  #guides(fill=FALSE),
  #guides(shape=FALSE),
  #guides(size=FALSE),
  theme_bw(),
  theme(
    text=element_text(family="Palatino"),
    #legend.title=element_blank(),
    #legend.justification=c(0,0), 
    #legend.position= "right", #c(1,0),
    panel.border = element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank()))

#######################################
## MAP CANTONS THAT EXPERIENCED REFORM:
#######################################

cantons$LR <- cantons$CODIGO %in% prop_data$CODIGO
cantons$LR <- ifelse(is.na(cantons$CODIGO),FALSE,cantons$LR)

cantons_simple <- st_simplify(cantons, dTolerance=0.001, preserveTopology = TRUE)
ggplot() + geom_sf(data=cantons_simple, aes(fill=factor(LR)),size=0.1) +  
  aesthetics +
  scale_fill_manual(name="Experienced \nLand Reform",values=c("#132B43","#56B1F7"), guide = guide_legend(reverse=TRUE), labels = c("No","Yes"))
ggsave("./Output/ESLR_ReformCantons.pdf", height=7, width=7)





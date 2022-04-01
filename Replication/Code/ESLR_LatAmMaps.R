########################################################################
##### El Salvador - Land Reform - Map of Land Reforms Across LatAm #####
########################################################################

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
require(ggvis)        # visualize estimators
require(rdrobust)     # rd estimation tools
require(stringdist)   # approximate string matching
require(gdata)        
require(rdd)          # sorting tests
require(stargazer)    # format tables
require(ggrepel)      # labeling

########################################

# Approximate String Matching Funtion -- (amatch doesn't work that well for some reason)

string_match <- function(string_to_match, options, smethod="osa") {
  if(string_to_match!="") {
    sdists <- stringdist(string_to_match, options, method=smethod)
    ind <- which(sdists == min(sdists))
    if(length(ind) != 1) {
      ind <- ind[1] # Assumes first index is the most common string to match.
    }
    return(options[ind])
  } else {
    return("")
  }
}

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]} # Function to turn factor vars to numeric variables correctly.

########################################

## Load LatAm Shapefile:

# Path:
latam_path <- "./Data/GIS_LatinAmerica/"

# Load Shapefile:
LatAm <- readOGR(latam_path, "LatinAmerica")

########################################

## Add in Change in Land Gini data from Albertus (2015):
LatAm$CHG_LAND_GINI <- 0
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Mexico"] <- -30.0  
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="French Guiana"] <- 0.0  
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Guyana"] <- 0.0  
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Suriname"] <- 0.0 
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Venezuela"] <- -5.0  
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Argentina"] <- 2.5
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Bolivia"] <- -20.0
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Brazil"] <- 2.5
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Chile"] <- -10.0
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Ecuador"] <- -5.0
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Paraguay"] <- 5.0
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Peru"] <- -15.0
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Uruguay"] <- 0.0
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Guatemala"] <- 0.0
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Belize"] <- 0.0
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Colombia"] <- -5.0
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Costa Rica"] <- 0.0
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="El Salvador"] <- -10.0
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Honduras"] <- -5.0
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Nicaragua"] <- -25.0
LatAm$CHG_LAND_GINI[LatAm$CNTRY_NAME=="Panama"] <- 5.0

########################################

## Add in land reform to cooperative indicator from Albertus (2015) and DeJanvry (1982):
LatAm$coop_land_reform <- 0
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Mexico"] <- 1  
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="French Guiana"] <- 0.0  
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Guyana"] <- 0.0  
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Suriname"] <- 0.0  
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Venezuela"] <- 1  
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Argentina"] <- 0.0
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Bolivia"] <- 1
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Brazil"] <- 0
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Chile"] <- 1
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Ecuador"] <- 0  
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Paraguay"] <- 0  
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Peru"] <- 1
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Uruguay"] <- 0.0  
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Guatemala"] <- 0.0   
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Belize"] <- 0.0
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Colombia"] <- 1
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Costa Rica"] <- 1
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="El Salvador"] <- 1
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Honduras"] <- 1.0  
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Nicaragua"] <- 1
LatAm$coop_land_reform[LatAm$CNTRY_NAME=="Panama"] <- 1.0  


########################################

## Plots!

# Set aesthetics:
aesthetics <- list(#guides(color=guide_colorbar(reverse=FALSE)),
  #guides(fill=FALSE),
  #guides(shape=FALSE),
  #guides(size=FALSE),
  coord_equal(),
  theme_bw(),
  theme(#legend.title=element_blank(),
    #legend.justification=c(0,0), 
    #legend.position= "right", #c(1,0),
    text=element_text(family="Palatino"),
    panel.border = element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank()))

# Fortify for ggplot
LatAm.df <- fortify(LatAm, region="FIPS_CNTRY")
LatAm@data$id <- LatAm@data$FIPS_CNTRY

# Join Data:
LatAm.df <- join(LatAm.df, LatAm@data, by="id")

# Plot:


# Indicator for Land Reform that created Agricultural Coops w/ El Salvador Highlighted:
ES <- LatAm[LatAm$CNTRY_NAME=="El Salvador",]
ES@data <- mutate(ES@data, ES = ifelse(CNTRY_NAME=="El Salvador",1,0), ES2 = ifelse(FIPS_CNTRY=="ES",1,0))
# Fortify for ggplot
ES.df <- fortify(ES, region="FIPS_CNTRY")
ES@data$id <- ES@data$FIPS_CNTRY

# Join Data:
ES.df <- join(ES.df, ES@data, by="id")

LatAm.ggplot.reform <- geom_polygon(aes(x=long,y=lat, group=group, fill=(coop_land_reform)),data=LatAm.df,size=0.25,col="black")

pdf(file="./Output/LatAm_LRCoops.pdf", height=7, width=7, paper = "letter")
print(ggplot(aes(x=long,y=lat, group=group, fill=(coop_land_reform)),data=LatAm.df) + LatAm.ggplot.reform + coord_equal() + aesthetics
      + scale_fill_distiller(name="Experienced a Land Reform\nthat created Agricultural \nCooperatives, 1920-1990", palette = "Blues", trans = "reverse", breaks = pretty_breaks(n = 1), labels=c("No","Yes"),guide = guide_legend(reverse=TRUE)) 
      + labs(x="Longitude",y="Latitude"))
dev.off()

# w/Labels
EScentroid.df <- as.data.frame(coordinates(ES))
names(EScentroid.df) <- c("long", "lat")
EScentroid.df$CNTRY_NAME <- ES@data$CNTRY_NAME
ES.ggplot2 <- geom_polygon(aes(x=long,y=lat, group=group),data=ES.df,col="red",size=0.25, fill=NA,show.legend=FALSE)

pdf(file="./Output/LatAm_LRCoops_wESLabel2.pdf", height=7, width=7, paper = "letter")
print(ggplot()
      + geom_text_repel( data=EScentroid.df, aes(x=long, y=lat, label=CNTRY_NAME), col="red",size=4,nudge_x=-15, nudge_y=-5)
      + LatAm.ggplot.reform + coord_equal() + aesthetics 
      + ES.ggplot2
      + scale_fill_distiller(name="Experienced a Land Reform\nthat created Agricultural\nCooperatives - 1920-1990", palette = "Blues", trans = "reverse", breaks = pretty_breaks(n = 1), labels=c("No","Yes"),guide = guide_legend(reverse=TRUE)) 
      + labs(x="Longitude",y="Latitude"))
dev.off()


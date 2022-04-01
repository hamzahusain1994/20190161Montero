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
require(animation)    # Saving GIFs
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
require(exactextractr) # faster extract
require(sf) # faster extract
require(elevatr) # elevation data
require(rdrobust)
require(stringdist)

############## LOAD DATA ################

## Read in Data:

# Load the Property-Level Data:
prop_data <- read.dta(file="./Data/prop_data.dta")
# dta file Created in R, ESLR_CleanPropertyData.R

prop_data <- mutate(prop_data, norm_dist = Total_Propretario - 500.00,
                    Above500 = ifelse(norm_dist>0,1,0))


# Load the Canton Shapefile:
cantons <- readOGR(dsn="./Data/", layer="cantons_wCodigos")

############## CALCULATE GEO COVS ###############

# Projections:
wgs84_proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84" # WGS 1984
mercator <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs" # Project to mercator to calculate distance in meters

## GEOGRAPHIC BALANCE:

# BUFFER SIZE:
buffer_size <- 2500 

# PREP SHAPEFILES:
cantons_wCovariates <- as(cantons,"sf")
cantons_wCovariates <- st_transform(cantons_wCovariates, st_crs(mercator))

# SUITABILITY FOR DIFFERENT CROPS
# Export Crops: Coffee, Sugar Cane and Cotton (though cotton no longer produced there)
# Internal Crops: Maiz, Beans, Sorghum, maybe Rice
# COFFEE:
  # Read in Rasters:
  path_to_suit_coffee <- "./Data/crop_suit/coffeelo.tif"
  coffee_suit <- raster(paste(path_to_suit_coffee,"",sep=""))
  
  # Merge to CANTONS:

  cantons_wCovariates$canton_coffee_suit <- exact_extract(coffee_suit, 
                                                   cantons_wCovariates,
                                                   'median')

# SUGAR CANE:
  # Read in Rasters:
  path_to_suit_sugarcane <- "./Data/crop_suit/sugarcanelo.tif"
  sugarcane_suit <- raster(paste(path_to_suit_sugarcane,"",sep=""))

  # Merge to CANTONS:
  cantons_wCovariates$sugarcane_suit <- exact_extract(sugarcane_suit, 
                                                   cantons_wCovariates,
                                                   'median')
# COTTON:
  # Read in Rasters:
  path_to_suit_cotton <- "./Data/crop_suit/cottonlo.tif"
  cotton_suit <- raster(paste(path_to_suit_cotton,"",sep=""))
  
  # Merge to CANTONS:
  cantons_wCovariates$cotton_suit <- exact_extract(cotton_suit, 
                                                      cantons_wCovariates,
                                                      'median')

# Non-Export:
# Maize:
  # Read in Rasters:
  path_to_suit_maiz <- "./Data/crop_suit/maizelo.tif" 
  miaze_suit <- raster(paste(path_to_suit_maiz,"",sep=""))
  
  
  # Merge to CANTONS:
  cantons_wCovariates$miaze_suit <- exact_extract(miaze_suit, 
                                                   cantons_wCovariates,
                                                   'median')

# Beans:
  # Read in Rasters:
  path_to_suit_beans <- "./Data/crop_suit/phaseolusbeanlo.tif"
  bean_suit <- raster(paste(path_to_suit_beans,"",sep=""))
  
  
  # Merge to CANTONS:
  cantons_wCovariates$bean_suit <- exact_extract(bean_suit, 
                                                  cantons_wCovariates,
                                                  'median')
# Sorghum:
  # Read in Rasters:
  path_to_suit_sorghum <- "./Data/crop_suit/sorghumlo.tif"
  sorghum_suit <- raster(paste(path_to_suit_sorghum,"",sep=""))
  
  
  # Merge to CANTONS:
  cantons_wCovariates$sorghum_suit <- exact_extract(sorghum_suit, 
                                                 cantons_wCovariates,
                                                 'median')
# Rice:
  # Read in Rasters:
  path_to_suit_rice <- "./Data/crop_suit/wetricelo.tif" # indricelo.tif
  rice_suit <- raster(paste(path_to_suit_rice,"",sep=""))
  
  
  # Merge to CANTONS:
  cantons_wCovariates$rice_suit <- exact_extract(rice_suit, 
                                                    cantons_wCovariates,
                                                    'median')
# Precipitation:
  path_rain <- "./Data/wc2.1_2.5m_prec_2000-2009/"
  
  # Loop over 12 months and calculate mean rainfall (mm):
  for (month in 1:12) {
    # Convert from .adf to raster for analysis:
    print(month)
    x <- raster(paste(path_rain,"wc2.1_2.5m_prec_2007-",
                      ifelse(month%/%10==0,paste0("0",month),month),
                      ".tif",sep=""))
    rainfall <- (x)
    proj4string(rainfall) <- CRS(wgs84_proj) # assign projection since empty
    assign(paste("rain","_",month,sep=""), rainfall)
  }
  sum_rain <- (rain_1 + rain_2 + rain_3 + rain_4 + rain_5 + rain_6 + rain_7 + rain_8 + rain_9 + rain_10 + rain_11 + rain_12)
  
  
  # Extract:
  cantons_wCovariates$canton_mean_rain <- exact_extract(sum_rain, 
                                                 cantons_wCovariates,
                                                 'median')


# Land Suitability:
  # http://nelson.wisc.edu/sage/data-and-models/atlas/maps.php?datasetid=19&includerelatedlinks=1&dataset=19
  path_land_suit <- "Data/suit/suit/w001001.adf"
  
  # Convert from .adf to raster for analysis:
  x <- new("GDALReadOnlyDataset", path_land_suit)
  xx<-asSGDF_GROD(x)
  land_suit <- raster(xx)
  proj4string(land_suit) <- CRS(proj4string(cantons)) # assign projection since empty
  
  # Extract:
  cantons_wCovariates$canton_land_suit <- exact_extract(land_suit, 
                                                 cantons_wCovariates,
                                                 'median')

  ## Elevation: ##
  elev <- get_elev_raster(locations = cantons, z= 1)
  
  # Extract:
  cantons_wCovariates$canton_elev_dem_30sec <- exact_extract(elev, cantons_wCovariates,'median')
  
  
  write_dta(st_drop_geometry(cantons_wCovariates), "./Output/cantons_wGeoCovariates.dta")
  
  
  

################# STD FUNCTIONS ###################


# STD FUNCTIONS:
lm.beta <- function (MOD, dta,y="ln_agprod") 
{
  b <- MOD$coef[1]
  model.dta <- filter(dta, norm_dist > -1*MOD$bws["h","left"] & norm_dist < MOD$bws["h","right"] )
  sx <- sd(model.dta[,c("Above500")])
  sy <- sd((model.dta[,c(y)]),na.rm=TRUE)
  beta <- b * sx/sy
  return(beta)
}
lm.beta.ses <- function (MOD, dta,y="ln_agprod") 
{
  b <- MOD$se[1]
  model.dta <- filter(dta, norm_dist > -1*MOD$bws["h","left"] & norm_dist < MOD$bws["h","right"] )
  sx <- sd(model.dta[,c("Above500")])
  sy <- sd((model.dta[,c(y)]),na.rm=TRUE)
  beta <- b * sx/sy
  return(beta)
}

winsor <- function (x, fraction=.01)
{
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction),na.rm = TRUE)
  x[ x < lim[1] ] <- NA  
  x[ x > lim[2] ] <- NA  
  x
}

################# AESTHETICS ##################

aesthetics <- list(
  theme_bw(),
  theme(legend.title=element_blank(),
        text=element_text(family="Palatino"),
        plot.background=element_rect(colour="white",fill="white"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title=element_text(size=12,face="bold"),
        ))


################### BALANCE PLOT ####################

## Coef Plots:
alpha<- 0.05
Multiplier <- qnorm(1 - alpha / 2)

prop_data_wgeo <- left_join(prop_data, st_drop_geometry(cantons_wCovariates),by=c("CODIGO"))

b0 <- rdrobust(y = (prop_data_wgeo$miaze_suit), x=prop_data_wgeo$norm_dist,c = 0,p = 1,q=2, bwselect = "mserd", cluster=prop_data_wgeo$Expropretario_ISTA, vce="hc0")
b1 <- rdrobust(y = (prop_data_wgeo$sorghum_suit), x=prop_data_wgeo$norm_dist,c = 0,p = 1,q=2, bwselect = "mserd", cluster=prop_data_wgeo$Expropretario_ISTA, vce="hc0")
b2 <- rdrobust(y = (prop_data_wgeo$bean_suit), x=prop_data_wgeo$norm_dist,c = 0,p = 1,q=2, bwselect = "mserd", cluster=prop_data_wgeo$Expropretario_ISTA, vce="hc0")
b3 <- rdrobust(y = (prop_data_wgeo$rice_suit), x=prop_data_wgeo$norm_dist,c = 0,p = 1,q=2, bwselect = "mserd", cluster=prop_data_wgeo$Expropretario_ISTA, vce="hc0")
b4 <- rdrobust(y = (prop_data_wgeo$cotton_suit), x=prop_data_wgeo$norm_dist,c = 0,p = 1,q=2, bwselect = "mserd", cluster=prop_data_wgeo$Expropretario_ISTA, vce="hc0")
b5 <- rdrobust(y = (prop_data_wgeo$sugarcane_suit), x=prop_data_wgeo$norm_dist,c = 0,p = 1,q=2, bwselect = "mserd", cluster=prop_data_wgeo$Expropretario_ISTA, vce="hc0")
b6 <- rdrobust(y = (prop_data_wgeo$canton_coffee_suit), x=prop_data_wgeo$norm_dist,c = 0,p = 1,q=2, bwselect = "mserd", cluster=prop_data_wgeo$Expropretario_ISTA, vce="hc0")
b7 <- rdrobust(y = (prop_data_wgeo$canton_elev_dem_30sec), x=prop_data_wgeo$norm_dist,c = 0,p = 1,q=2, bwselect = "mserd", cluster=prop_data_wgeo$Expropretario_ISTA, vce="hc0")
b8 <- rdrobust(y = (prop_data_wgeo$canton_mean_rain), x=prop_data_wgeo$norm_dist,c = 0,p = 1,q=2, bwselect = "mserd", cluster=prop_data_wgeo$Expropretario_ISTA, vce="hc0")
b9 <- rdrobust(y = (prop_data_wgeo$canton_land_suit), x=prop_data_wgeo$norm_dist,c = 0,p = 1,q=2, bwselect = "mserd", cluster=prop_data_wgeo$Expropretario_ISTA, vce="hc0")


beta_coefs <- c(lm.beta(MOD=b0, dta=prop_data_wgeo, y="miaze_suit"),
                lm.beta(MOD=b1, dta=prop_data_wgeo, y="sorghum_suit"),
                lm.beta(MOD=b2, dta=prop_data_wgeo, y="bean_suit"),
                lm.beta(MOD=b3, dta=prop_data_wgeo, y="rice_suit"),
                lm.beta(MOD=b4, dta=prop_data_wgeo, y="cotton_suit"),
                lm.beta(MOD=b5, dta=prop_data_wgeo, y="sugarcane_suit"),
                lm.beta(MOD=b6, dta=prop_data_wgeo, y="canton_coffee_suit"),
                lm.beta(MOD=b7, dta=prop_data_wgeo, y="canton_elev_dem_30sec"),
                lm.beta(MOD=b8, dta=prop_data_wgeo, y="canton_mean_rain"),
                lm.beta(MOD=b9, dta=prop_data_wgeo, y="canton_land_suit"))

beta_ses <- c(lm.beta.ses(MOD=b0, dta=prop_data_wgeo, y="miaze_suit"),
              lm.beta.ses(MOD=b1, dta=prop_data_wgeo, y="sorghum_suit"),
              lm.beta.ses(MOD=b2, dta=prop_data_wgeo, y="bean_suit"),
              lm.beta.ses(MOD=b3, dta=prop_data_wgeo, y="rice_suit"),
              lm.beta.ses(MOD=b4, dta=prop_data_wgeo, y="cotton_suit"),
              lm.beta.ses(MOD=b5, dta=prop_data_wgeo, y="sugarcane_suit"),
              lm.beta.ses(MOD=b6, dta=prop_data_wgeo, y="canton_coffee_suit"),
              lm.beta.ses(MOD=b7, dta=prop_data_wgeo, y="canton_elev_dem_30sec"),
              lm.beta.ses(MOD=b8, dta=prop_data_wgeo, y="canton_mean_rain"),
              lm.beta.ses(MOD=b9, dta=prop_data_wgeo, y="canton_land_suit"))

yvars<-c("Maize Suitability","Sorghum Suitability","Bean Suitability","Rice Suitability","Cotton Suitability","Sugar Cane Suitability","Coffee Suitability","Elevation","Precipitation","Land Suitability")
geo_vars <- c("miaze_suit","sorghum_suit","bean_suit","rice_suit","cotton_suit",
              "sugarcane_suit", "canton_coffee_suit", "canton_elev_dem_30sec",
              "canton_mean_rain","canton_land_suit")
betas <- cbind(yvars,beta_coefs,beta_ses)
ests <- cbind(geo_vars, c(b0$coef[1],b1$coef[1],b2$coef[1],b3$coef[1],b4$coef[1],b5$coef[1],b6$coef[1],b7$coef[1],b8$coef[1],b9$coef[1]),
              c(b0$se[1],b1$se[1],b2$se[1],b3$se[1],b4$se[1],b5$se[1],b6$se[1],b7$coef[1],b8$se[1],b9$se[1]))
# Save estimates for un-balancedness exercise:
write_dta(as.data.frame(ests),path="./Output/balance_ests.dta")

row.names(betas)<-NULL

MatrixofModels <- as.data.frame(as.matrix(betas))
colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError")
MatrixofModels$IV <- factor(MatrixofModels$IV, levels = rev(c("Land Suitability", "Precipitation", "Elevation", "Coffee Suitability", "Sugar Cane Suitability", "Cotton Suitability", "Maize Suitability", "Bean Suitability", "Rice Suitability", "Sorghum Suitability")))# MatrixofModels$IV)
MatrixofModels[, -c(1, 6)] <- apply(MatrixofModels[, -c(1, 6)], 2, function(x){as.numeric(as.character(x))})


###################
## BALANCE FIGURE:
##################

# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
# Stupid fix to fix the scales overlapping on the bottom:
OutputPlot <- OutputPlot + geom_hline(yintercept = 0.0, alpha = 0.05)
#OutputPlot <- OutputPlot + facet_grid(~ ModelName) + coord_flip() + theme_bw() + ylab("\nStandardized Effect")
OutputPlot <- OutputPlot + coord_flip() + theme_classic() + ylab("\nStandardized Effect")  + 
  xlab("")

# Save:
OutputPlot  + scale_y_continuous(breaks = seq(-0.4, 0.4,0.1)) + aesthetics

ggsave(filename="./Output/CoefPlot_Balance_PropLevel1980.pdf",width = 6, height=4)


############################
## SELECTIVE SORTING FIGURE:
############################

require(rdd)

### FIXING X LIM & FONT:
DCdensity2 <- function (runvar, cutpoint, bin = NULL, bw = NULL, verbose = FALSE, 
                        plot = TRUE, ext.out = FALSE, htest = FALSE, my_xlim = c(-0.5,0.5)) # my_xlim param added
{
  runvar <- runvar[complete.cases(runvar)]
  rn <- length(runvar)
  rsd <- sd(runvar)
  rmin <- min(runvar)
  rmax <- max(runvar)
  if (missing(cutpoint)) {
    if (verbose) 
      cat("Assuming cutpoint of zero.\n")
    cutpoint <- 0
  }
  if (cutpoint <= rmin | cutpoint >= rmax) {
    stop("Cutpoint must lie within range of runvar")
  }
  if (is.null(bin)) {
    bin <- 2 * rsd * rn^(-1/2)
    if (verbose) 
      cat("Using calculated bin size: ", sprintf("%.3f", 
                                                 bin), "\n")
  }
  l <- floor((rmin - cutpoint)/bin) * bin + bin/2 + cutpoint
  r <- floor((rmax - cutpoint)/bin) * bin + bin/2 + cutpoint
  lc <- cutpoint - (bin/2)
  rc <- cutpoint + (bin/2)
  j <- floor((rmax - rmin)/bin) + 2
  binnum <- round((((floor((runvar - cutpoint)/bin) * bin + 
                       bin/2 + cutpoint) - l)/bin) + 1)
  cellval <- rep(0, j)
  for (i in seq(1, rn)) {
    cnum <- binnum[i]
    cellval[cnum] <- cellval[cnum] + 1
  }
  cellval <- (cellval/rn)/bin
  cellmp <- seq(from = 1, to = j, by = 1)
  cellmp <- floor(((l + (cellmp - 1) * bin) - cutpoint)/bin) * 
    bin + bin/2 + cutpoint
  if (is.null(bw)) {
    leftofc <- round((((floor((lc - cutpoint)/bin) * bin + 
                          bin/2 + cutpoint) - l)/bin) + 1)
    rightofc <- round((((floor((rc - cutpoint)/bin) * bin + 
                           bin/2 + cutpoint) - l)/bin) + 1)
    if (rightofc - leftofc != 1) {
      stop("Error occurred in bandwidth calculation")
    }
    cellmpleft <- cellmp[1:leftofc]
    cellmpright <- cellmp[rightofc:j]
    P.lm <- lm(cellval ~ poly(cellmp, degree = 4, raw = T), 
               subset = cellmp < cutpoint)
    mse4 <- summary(P.lm)$sigma^2
    lcoef <- coef(P.lm)
    fppleft <- 2 * lcoef[3] + 6 * lcoef[4] * cellmpleft + 
      12 * lcoef[5] * cellmpleft * cellmpleft
    hleft <- 3.348 * (mse4 * (cutpoint - l)/sum(fppleft * 
                                                  fppleft))^(1/5)
    P.lm <- lm(cellval ~ poly(cellmp, degree = 4, raw = T), 
               subset = cellmp >= cutpoint)
    mse4 <- summary(P.lm)$sigma^2
    rcoef <- coef(P.lm)
    fppright <- 2 * rcoef[3] + 6 * rcoef[4] * cellmpright + 
      12 * rcoef[5] * cellmpright * cellmpright
    hright <- 3.348 * (mse4 * (r - cutpoint)/sum(fppright * 
                                                   fppright))^(1/5)
    bw = 0.5 * (hleft + hright)
    if (verbose) 
      cat("Using calculated bandwidth: ", sprintf("%.3f", 
                                                  bw), "\n")
  }
  if (sum(runvar > cutpoint - bw & runvar < cutpoint) == 0 | 
      sum(runvar < cutpoint + bw & runvar >= cutpoint) == 0) 
    stop("Insufficient data within the bandwidth.")
  if (plot) {
    d.l <- data.frame(cellmp = cellmp[cellmp < cutpoint], 
                      cellval = cellval[cellmp < cutpoint], dist = NA, 
                      est = NA, lwr = NA, upr = NA)
    pmin <- cutpoint - 2 * rsd
    pmax <- cutpoint + 2 * rsd
    for (i in 1:nrow(d.l)) {
      d.l$dist <- d.l$cellmp - d.l[i, "cellmp"]
      w <- kernelwts(d.l$dist, 0, bw, kernel = "triangular")
      newd <- data.frame(dist = 0)
      pred <- predict(lm(cellval ~ dist, weights = w, data = d.l), 
                      interval = "confidence", newdata = newd)
      d.l$est[i] <- pred[1]
      d.l$lwr[i] <- pred[2]
      d.l$upr[i] <- pred[3]
    }
    d.r <- data.frame(cellmp = cellmp[cellmp >= cutpoint], 
                      cellval = cellval[cellmp >= cutpoint], dist = NA, 
                      est = NA, lwr = NA, upr = NA)
    for (i in 1:nrow(d.r)) {
      d.r$dist <- d.r$cellmp - d.r[i, "cellmp"]
      w <- kernelwts(d.r$dist, 0, bw, kernel = "triangular")
      newd <- data.frame(dist = 0)
      pred <- predict(lm(cellval ~ dist, weights = w, data = d.r), 
                      interval = "confidence", newdata = newd)
      d.r$est[i] <- pred[1]
      d.r$lwr[i] <- pred[2]
      d.r$upr[i] <- pred[3]
    }
    plot(d.l$cellmp, d.l$est, lty = 1, lwd = 2, col = "black", # xlim set here based on the parameter
         type = "l", xlim = my_xlim, ylim = c(min(cellval[cellmp <= 
                                                            pmax & cellmp >= pmin]), max(cellval[cellmp <= 
                                                                                                   pmax & cellmp >= pmin])), xlab = NA, ylab = NA, 
         main = NA)
    lines(d.l$cellmp, d.l$lwr, lty = 2, lwd = 1, col = "black", 
          type = "l")
    lines(d.l$cellmp, d.l$upr, lty = 2, lwd = 1, col = "black", 
          type = "l")
    lines(d.r$cellmp, d.r$est, lty = 1, lwd = 2, col = "black", 
          type = "l")
    lines(d.r$cellmp, d.r$lwr, lty = 2, lwd = 1, col = "black", 
          type = "l")
    lines(d.r$cellmp, d.r$upr, lty = 2, lwd = 1, col = "black", 
          type = "l")
    points(cellmp, cellval, type = "p", pch = 20)
  }
  cmp <- cellmp
  cval <- cellval
  padzeros <- ceiling(bw/bin)
  jp <- j + 2 * padzeros
  if (padzeros >= 1) {
    cval <- c(rep(0, padzeros), cellval, rep(0, padzeros))
    cmp <- c(seq(l - padzeros * bin, l - bin, bin), cellmp, 
             seq(r + bin, r + padzeros * bin, bin))
  }
  dist <- cmp - cutpoint
  w <- 1 - abs(dist/bw)
  w <- ifelse(w > 0, w * (cmp < cutpoint), 0)
  w <- (w/sum(w)) * jp
  fhatl <- predict(lm(cval ~ dist, weights = w), newdata = data.frame(dist = 0))[[1]]
  w <- 1 - abs(dist/bw)
  w <- ifelse(w > 0, w * (cmp >= cutpoint), 0)
  w <- (w/sum(w)) * jp
  fhatr <- predict(lm(cval ~ dist, weights = w), newdata = data.frame(dist = 0))[[1]]
  thetahat <- log(fhatr) - log(fhatl)
  sethetahat <- sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + 
                                                 (1/fhatl)))
  z <- thetahat/sethetahat
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  if (verbose) {
    cat("Log difference in heights is ", sprintf("%.3f", 
                                                 thetahat), " with SE ", sprintf("%.3f", sethetahat), 
        "\n")
    cat("  this gives a z-stat of ", sprintf("%.3f", z), 
        "\n")
    cat("  and a p value of ", sprintf("%.3f", p), "\n")
  }
  if (ext.out) 
    return(list(theta = thetahat, se = sethetahat, z = z, 
                p = p, binsize = bin, bw = bw, cutpoint = cutpoint, 
                data = data.frame(cellmp, cellval)))
  else if (htest) {
    structure(list(statistic = c(z = z), p.value = p, method = "McCrary (2008) sorting test", 
                   parameter = c(binwidth = bin, bandwidth = bw, cutpoint = cutpoint), 
                   alternative = "no apparent sorting"), class = "htest")
  }
  else return(p)
}


prop_subset <- prop_data[which(prop_data$Total_Propretario < 1500 & prop_data$Total_Propretario >180),]
pdf(file="./Output/McCrarySorting_PropLevel.pdf", height=6, width=9, paper = "USr", family = "Palatino")
DCdensity2(runvar = prop_subset$Total_Propretario,cutpoint = 500,plot = TRUE,verbose = TRUE, ext.out = FALSE, bw=350, my_xlim = c(200,1000))
abline(v=500,col=c("red"))
#par(family = 'sans') # the default of R
title(xlab="Cumulative Landholdings (ha)", ylab="Density")
dev.off()


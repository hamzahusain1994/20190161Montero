########################################################
##### ESLR - RD + MATCHING PLOTING - AgCensus Data #####
########################################################

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
require(haven)
require(readstata13)
require(TOSTER)
require(MatchIt)
require(imputeTS)
require(cem)
require(tcltk)

########################################

## Load IV Censo Agropecuario Data:
censo_ag_wreform <- read.dta13(file="Data/censo_ag_wreform.dta")

## Load Balance Estimates:
balance_ests <- read_dta("Output/balance_ests.dta")
balance_ests$beta <- balance_ests$V2
balance_ests$se <- balance_ests$V3

########################################

## Making Standarized Coefficient Plots:

# Set aesthetics:
aesthetics <- list(
  theme_bw(),
  theme(#legend.title=element_blank(),
        text=element_text(family="Palatino"),
        #legend.justification=c(0,0), 
        #legend.position= "right", #c(1,0),
        #panel.grid.minor=element_blank(),
        #panel.grid.major=element_blank(),
        plot.background=element_rect(colour="white",fill="white"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x=element_text(angle=45, face="bold",hjust=1),
        axis.title.y=element_text(face="bold.italic"),
        axis.title.x=element_text(face="bold.italic")))



########################################

## Functions to trim Yields (prone to huge outliers, especially when standardizing)

winsor <- function (x, fraction=.01)
{
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction),na.rm = TRUE)
  x[ x < lim[1] ] <- NA #lim[1] 8888
  x[ x > lim[2] ] <- NA #lim[2] 8888
  x
}

winsor1 <- function (x, fraction=.01)
{
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction),na.rm = TRUE)
  x[ x < lim[1] ] <- lim[1] #lim[1] 8888
  x[ x > lim[2] ] <- lim[2] #lim[2] 8888
  x
}

winsor2 <-function (x, multiple=3)
{
  if(length(multiple) != 1 || multiple <= 0) {
    stop("bad value for 'multiple'")
  }
  med <- median(x)
  y <- x - med
  sc <- mad(y, center=0) * multiple
  y[ y > sc ] <- sc
  y[ y < -sc ] <- -sc
  y + med
}

lm.beta <- function (MOD, dta,y="ln_agprod") 
{
  b <- MOD$coef[3]
  model.dta <- filter(dta, norm_dist > -1*MOD$bws["h","left"] & norm_dist < MOD$bws["h","right"] )
  sx <- sd(model.dta[,c("Above500")])
  #sx <- sd(model.dta[,c("norm_dist")])
  sy <- sd((model.dta[,c(y)]),na.rm=TRUE)
  beta <- b * sx/sy
  return(beta)
}
lm.beta.ses <- function (MOD, dta,y="ln_agprod") 
{
  b <- MOD$se[3]
  model.dta <- filter(dta, norm_dist > -1*MOD$bws["h","left"] & norm_dist < MOD$bws["h","right"] )
  sx <- sd(model.dta[,c("Above500")])
  #sx <- sd(model.dta[,c("norm_dist")])
  sy <- sd((model.dta[,c(y)]),na.rm=TRUE)
  beta <- b * sx/sy
  return(beta)
}

lm.beta2<-function(est, dta, bw,y="ln_agprod") 
{
  b <- est
  model.dta <- filter(dta, norm_dist >= -1*bw & norm_dist <= bw)
  sx <- sd(model.dta[,c("Above500")])
  #sx <- sd(model.dta[,c("norm_dist")])
  sy <- sd((model.dta[,c(y)]),na.rm=TRUE)
  beta <- b * sx/sy
  return(beta)
}

########################################

polys <- c(1) 
kernels <- c("triangular")
bwsel <- c("mserd") 
num_outcomes <- 3  # 3 ag prod; staple share + 2 yields; cash + 2 yields; income results
geo_vars <- c("miaze_suit","sorghum_suit","bean_suit","rice_suit","cotton_suit",
              "sugarcane_suit", "canton_coffee_suit", "canton_elev_dem_30sec",
              "canton_mean_rain","canton_land_suit")
num_ests <- (length(polys)*(length(kernels)*length(bwsel)))*num_outcomes
estimates <-data.frame(y_var = rep(0, num_ests), 
                       estimate = rep(0, num_ests), 
                       ses = rep(0, num_ests),
                       p = rep(0,num_ests), ks = rep(0,num_ests), bs = rep(0,num_ests), 
                       nsl= rep(0,num_ests), nsr= rep(0,num_ests), nslII= rep(0,num_ests), 
                       nsrII= rep(0,num_ests), nslIII= rep(0,num_ests), nsrIII= rep(0,num_ests),
                       est_method = rep(0,num_ests))

num_ests <- (length(polys)*(length(kernels)*length(bwsel)) + 2*length(geo_vars))*num_outcomes
unbalancedness_estimates <- data.frame(y_var = rep(0, num_ests), 
                                       geo_var = rep(0, num_ests), 
                                       estimate = rep(0, num_ests), 
                                       ses = rep(0, num_ests))

censo_ag_wreform_tev <- censo_ag_wreform %>% 
  mutate(canton_land_suit = ifelse(is.na(canton_land_suit),0,canton_land_suit)) # mean(dist_dept_capitals,na.rm = TRUE), 1,0))

censo_ag_wreform_tev2 <- censo_ag_wreform_tev

years <- 2007

  i <- 2007
  # Create Variables:
  censo_ag_wreform_tev <- mutate(censo_ag_wreform, ln_agprodII = ln_agprod, ln_agprod = ln_agprod_pricew_crops)

  # Agricultural Variables -- RD Estimates:
  count <-1
  p <- polys
  k <- kernels
  b <- bwsel
        
        # Cash Crop Share:
        rdests <- rdrobust(y = winsor(censo_ag_wreform_tev$CashCrop_Share),
                           x=(censo_ag_wreform_tev$norm_dist),
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1")
        estimates[count,c("estimate")] <- rdests$coef[1]
        estimates[count,c("ses")] <- rdests$se[1] 
        estimates[count,c("bws")] <- rdests$bws[1,1] 
        
        estimates[count,c("y_var")] <- "Cash Crop Share"
        estimates[count,c("est_method")] <- paste0("RD: Local ", ifelse(p==1,"Linear","Quadratic"), " Polynomial")
        count <- count + 1
        
        # Sugar Cane Yield:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$SugarCane_Yield),
                           x=(censo_ag_wreform_tev$norm_dist),
                           c = 0,
                           p = p, 
                           q = p+1,
                           kernel = k,
                           #bwselect = b,
                           h = 102.877,
                           b = 166.088,
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1")
        estimates[count,c("estimate")] <- rdests$coef[1]
        estimates[count,c("ses")] <- rdests$se[1] # for some reason not matching stata
        estimates[count,c("bws")] <- rdests$bws[1,1] 
        
        estimates[count,c("y_var")] <- "Sugar Cane Yield"
        estimates[count,c("est_method")] <- paste0("RD: Local ", ifelse(p==1,"Linear","Quadratic"), " Polynomial")
        count <- count + 1
        
        # Coffee Yield:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$Coffee_Yield),
                           x=(censo_ag_wreform_tev$norm_dist),
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1")
        estimates[count,c("estimate")] <- rdests$coef[1]
        estimates[count,c("ses")] <- rdests$se[1] 
        estimates[count,c("bws")] <- rdests$bws[1,1] 
        
        estimates[count,c("y_var")] <- "Coffee Yield"
        estimates[count,c("est_method")] <- paste0("RD: Local ", ifelse(p==1,"Linear","Quadratic"), " Polynomial")
        count <- count + 1
        
        # Staple Crop Share:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$StapleCrop_Share),
                           x=(censo_ag_wreform_tev$norm_dist),
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1")
        estimates[count,c("estimate")] <- rdests$coef[1]
        estimates[count,c("ses")] <- rdests$se[1] 
        estimates[count,c("bws")] <- rdests$bws[1,1] 
        
        estimates[count,c("y_var")] <- "Staple Crop Share"
        estimates[count,c("est_method")] <- paste0("RD: Local ", ifelse(p==1,"Linear","Quadratic"), " Polynomial")
        count <- count + 1
        
        # Bean Yield:
        rdests <- rdrobust(y =censo_ag_wreform_tev$Beans_Yield, # winsor1(censo_ag_wreform_tev$Beans_Yield,fraction = 0.025)
                           x=(censo_ag_wreform_tev$norm_dist),
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           #  bwselect = b,
                           h = 122.64,
                           b = 207.42,
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1")
        estimates[count,c("estimate")] <- rdests$coef[1]
        estimates[count,c("ses")] <- rdests$se[1] 
        estimates[count,c("bws")] <- rdests$bws[1,1] 
        
        estimates[count,c("y_var")] <- "Beans Yield"
        estimates[count,c("est_method")] <- paste0("RD: Local ", ifelse(p==1,"Linear","Quadratic"), " Polynomial")
        count <- count + 1
        
        # Maize Yield:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$Maize_Yield),
                           x=(censo_ag_wreform_tev$norm_dist),
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1")
        estimates[count,c("estimate")] <- rdests$coef[1]
        estimates[count,c("ses")] <- rdests$se[1] 
        estimates[count,c("bws")] <- rdests$bws[1,1] 
        
        estimates[count,c("y_var")] <- "Maize Yield"
        estimates[count,c("est_method")] <- paste0("RD: Local ", ifelse(p==1,"Linear","Quadratic"), " Polynomial")
        count <- count + 1
        
        # Revenues:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod),
                           x=(censo_ag_wreform_tev$norm_dist),
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1")
        estimates[count,c("estimate")] <- rdests$coef[1]
        estimates[count,c("ses")] <- rdests$se[1] 
        estimates[count,c("bws")] <- rdests$bws[1,1] 
        
        estimates[count,c("y_var")] <- "Revenues per ha"
        estimates[count,c("est_method")] <- paste0("RD: Local ", ifelse(p==1,"Linear","Quadratic"), " Polynomial")
        count <- count + 1
        
        # Profits:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprodII),
                           x=censo_ag_wreform_tev$norm_dist,
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1")
        estimates[count,c("estimate")] <- rdests$coef[1]
        estimates[count,c("ses")] <- rdests$se[1] 
        estimates[count,c("bws")] <- rdests$bws[1,1] 
        
        estimates[count,c("y_var")] <- "Profits per ha"
        estimates[count,c("est_method")] <- paste0("RD: Local ", ifelse(p==1,"Linear","Quadratic"), " Polynomial")
        count <- count + 1
        
        # TFP:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_tfp_geo),
                           x=censo_ag_wreform_tev$norm_dist,
                           c = 0,
                           p = p,
                           q = p +1,
                           kernel = k,
                           bwselect = b,
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1")
        estimates[count,c("estimate")] <- rdests$coef[1]
        estimates[count,c("ses")] <- rdests$se[1] 
        estimates[count,c("bws")] <- rdests$bws[1,1] 
        
        estimates[count,c("y_var")] <- "Farm Productivity"
        estimates[count,c("est_method")] <- paste0("RD: Local ", ifelse(p==1,"Linear","Quadratic"), " Polynomial")
        count <- count + 1

estimates

########################################

  count <- 1
  censo_ag_wreform_tev <- censo_ag_wreform_tev[,!(names(censo_ag_wreform_tev) %in% geo_vars)]
  cantons_geocovs <- read_dta("Output/cantons_wGeoCovariates.dta")
  censo_ag_wreform_tev <- left_join(censo_ag_wreform_tev,cantons_geocovs, by="CODIGO")
  
  censo_ag_wreform_tev <- as.data.frame(censo_ag_wreform_tev)
  # Agricultural Variables -- Incorporating "Unbalancedness" Bounds:
  for (m in geo_vars) {
    est_count<-1
    
    ## For each Yvar and each Geographic Variable, Estimate "Direct Effect"
    # Cash Crop Share
    var="CashCrop_Share"
    fit1 <- lm(censo_ag_wreform_tev[,var] ~  censo_ag_wreform_tev[,m] + factor(censo_ag_wreform_tev[,"DEPID"]))
    ests<- coeftest(fit1, vcov. = vcovCL)
    unbal <- c(ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) + 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])),
               ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) - 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])))
    

    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"], 
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+max(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+min(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count + 1
    est_count<-est_count+1
    
    
    # Sugar Cane
    var="SugarCane_Yield"
    fit1 <- lm(censo_ag_wreform_tev[,var] ~ censo_ag_wreform_tev[,m]  + factor(censo_ag_wreform_tev[,"DEPID"]))
    ests<- coeftest(fit1, vcov. = vcovCL)
    unbal <- c(ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) + 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])),
               ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) - 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])))
    
    
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"], 
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+max(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+min(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count + 1
    est_count<-est_count+1
    
    # Coffee
    var="Coffee_Yield"
    fit1 <- lm(censo_ag_wreform_tev[,var] ~ censo_ag_wreform_tev[,m] + factor(censo_ag_wreform_tev[,"DEPID"]))
    ests<- coeftest(fit1, vcov. = vcovCL)
    unbal <- c(ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) + 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])),
               ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) - 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])))
    
    
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"], 
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+max(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+min(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count + 1
    est_count<-est_count+1
    
    # Staple Crop Share
    var="StapleCrop_Share"
    fit1 <- lm(censo_ag_wreform_tev[,var] ~ censo_ag_wreform_tev[,m] + factor(censo_ag_wreform_tev[,"DEPID"]))
    ests<- coeftest(fit1, vcov. = vcovCL)
    unbal <- c(ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) + 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])),
               ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) - 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])))
    
    
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"], 
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+max(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+min(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count + 1
    est_count<-est_count+1
    
    
    # Beans
    var="Beans_Yield"
    fit1 <- lm(censo_ag_wreform_tev[,var] ~ censo_ag_wreform_tev[,m] + factor(censo_ag_wreform_tev[,"DEPID"]))
    ests<- coeftest(fit1, vcov. = vcovCL)
    unbal <- c(ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) + 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])),
               ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) - 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])))
    
    
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"], 
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+max(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+min(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count + 1
    est_count<-est_count+1
    
    # Maize
    var="Maize_Yield"
    fit1 <- lm(censo_ag_wreform_tev[,var] ~ censo_ag_wreform_tev[,m] + factor(censo_ag_wreform_tev[,"DEPID"]))
    ests<- coeftest(fit1, vcov. = vcovCL)
    unbal <- c(ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) + 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])),
               ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) - 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])))
    
    
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"], 
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+max(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+min(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count + 1
    est_count<-est_count+1
    
    # Revenues:
    var="ln_agprod"
    fit1 <- lm(censo_ag_wreform_tev[,var] ~ censo_ag_wreform_tev[,m] + factor(censo_ag_wreform_tev[,"DEPID"]))
    ests<- coeftest(fit1, vcov. = vcovCL)
    unbal <- c(ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) + 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])),
               ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) - 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])))
    
    
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"], 
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+max(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+min(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count + 1
    est_count<-est_count+1
    
    # Profits:
    var="ln_agprodII"
    fit1 <- lm(censo_ag_wreform_tev[,var] ~ censo_ag_wreform_tev[,m] + factor(censo_ag_wreform_tev[,"DEPID"]))
    ests<- coeftest(fit1, vcov. = vcovCL)
    unbal <- c(ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) + 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])),
               ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) - 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])))
    
    
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"], 
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+max(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+min(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count + 1
    est_count<-est_count+1
    
    
    # TFP:
    var="ln_tfp_geo"
    fit1 <- lm(censo_ag_wreform_tev[,var] ~ censo_ag_wreform_tev[,m] + factor(censo_ag_wreform_tev[,"DEPID"]))
    ests<- coeftest(fit1, vcov. = vcovCL)
    unbal <- c(ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) + 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])),
               ests[2,1]*(as.numeric(balance_ests[balance_ests$geo_vars==m,"beta"]) - 1.96*as.numeric(balance_ests[balance_ests$geo_vars==m,"se"])))
    
    
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"], 
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+max(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count +1
    unbalancedness_estimates[count,c("estimate")] <- lm.beta2(estimates[est_count,"estimate"]+min(unbal),
                                                              dta = censo_ag_wreform_tev,
                                                              estimates[est_count,"bws"],
                                                              y=var)
    unbalancedness_estimates[count,c("geo_var")] <- m
    unbalancedness_estimates[count,c("y_var")] <- var
    unbalancedness_estimates[count,c("ses")] <- lm.beta2(estimates[est_count,"ses"], 
                                                         dta = censo_ag_wreform_tev,
                                                         estimates[est_count,"bws"],
                                                         y=var)
    count <- count + 1
    est_count<-est_count+1
    
    
  }

unbalancedness_estimates


########################################

# Clean data for plotting:
alpha<- 0.05
Multiplier <- qnorm(1 - alpha / 2)

Multiplier2 <- qnorm(1 - 2*alpha / 2)

# Find the outcome var for each regression:
data <- unbalancedness_estimates
#data$y_var <- paste(data$ks, " kernel, ", data$bs," bandwidth",sep="")

# Replace y_var with nice names:

# Now, keep only the betas of interest:
betas <- data
dim(betas)
betas<- betas[seq(dim(betas)[1],1),]

# Create Matrix for plotting:
MatrixofModels <- betas[c("y_var", "estimate","ses","geo_var")]
colnames(MatrixofModels) <- c("Outcome", "Estimate", "StandardError", "Geo")
MatrixofModels <- mutate(MatrixofModels,
                         Outcome = case_when(
                           Outcome=="CashCrop_Share" ~ "Cash Crop Share",
                           Outcome=="Coffee_Yield" ~ "Coffee Yield",
                           Outcome=="SugarCane_Yield" ~ "Sugar Cane Yield",
                           Outcome=="StapleCrop_Share" ~  "Staple Crop Share",
                           Outcome=="Maize_Yield" ~"Maize Yield",
                           Outcome=="Beans_Yield" ~  "Beans Yield",
                           Outcome=="ln_agprod" ~ "Revenues per ha",
                           Outcome=="ln_agprodII" ~ "Profits per ha",
                           Outcome=="ln_tfp_geo" ~ "Farm Productivity"),
                         Geo = case_when(
                           Geo=="canton_land_suit" ~ "Land Suitability", 
                           Geo=="canton_mean_rain" ~ "Precipitation",
                           Geo=="canton_elev_dem_30sec" ~ "Elevation", 
                           Geo=="canton_coffee_suit" ~ "Coffee Suitability", 
                           Geo=="sugarcane_suit" ~ "Sugar Cane Suitability", 
                           Geo=="cotton_suit" ~ "Cotton Suitability", 
                           Geo=="miaze_suit" ~ "Maize Suitability", 
                           Geo=="bean_suit" ~ "Bean Suitability", 
                           Geo=="rice_suit" ~ "Rice Suitability", 
                           Geo=="sorghum_suit" ~ "Sorghum Suitability"
                         ))
                         
MatrixofModels$Outcome <- factor(MatrixofModels$Outcome, levels = unique(MatrixofModels$Outcome))

#MatrixofModels$Legend <- c("  AES Coefficient", rep("  Standard Coefficients",4))

# Re-Order for plotting:
MatrixofModels$Outcome <- factor(MatrixofModels$Outcome,
                                 levels = c("Cash Crop Share",
                                            "Coffee Yield",
                                            "Sugar Cane Yield",
                                            "Staple Crop Share",
                                            "Maize Yield",
                                            "Beans Yield",
                                            "Revenues per ha",
                                            "Profits per ha",
                                            "Farm Productivity"))

MatrixofModels <- MatrixofModels %>% 
                  group_by(Outcome, Geo) %>% 
                  mutate(Type = case_when(Estimate == max(Estimate) ~ "Upper Bound",
                                          Estimate == min(Estimate) ~ "Lower Bound",
                                          TRUE ~ "RD Estimate")) %>% 
                  ungroup()
MatrixofModels2 <- MatrixofModels
MatrixofModels <- MatrixofModels %>% 
                  filter(Type!="RD Estimate")
MatrixofModels$Geo <- factor(MatrixofModels$Geo, levels = rev(c("Land Suitability", "Precipitation", "Elevation", "Coffee Suitability", "Sugar Cane Suitability", "Cotton Suitability", "Maize Suitability", "Bean Suitability", "Rice Suitability", "Sorghum Suitability")))# MatrixofModels$IV)


# Plot:
OutputPlot <- qplot(Geo, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL, facets=~ Outcome, alpha=0.5, col=Type)
dodge_width<-0.5
OutputPlot <- ggplot() + geom_errorbar(aes(x=Geo, y=Estimate, ymin = Estimate - Multiplier * StandardError,
                                           ymax = Estimate + Multiplier * StandardError,
                                           col=Type), 
                                       data = MatrixofModels, 
                                       size=0.6,
                                       width=0,
                                       #alpha=0.5, 
                                       position = position_dodge(width=dodge_width)) +
  geom_point(aes(x=Geo, y=Estimate,color=Type), 
             data = MatrixofModels, 
             #col="black",
             show.legend = TRUE,
             position = position_dodge(width=dodge_width)) + facet_wrap(~Outcome)


OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
# Stupid fix to fix the scales overlapping on the bottom:
OutputPlot <- OutputPlot + geom_hline(yintercept = 0.02, alpha = 0.05)
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics 
# Add 90%
# OutputPlot <- OutputPlot + geom_errorbar(aes(x=Geo, y=Estimate, ymin = Estimate - Multiplier2 * StandardError,
#                                              ymax = Estimate + Multiplier2 * StandardError, 
#                                              color=Type), data = MatrixofModels, 
#                                          size=0.5,
#                                          width=0,
#                                          show.legend = FALSE,
#                                          position = position_dodge(width=dodge_width))
# OutputPlot <- OutputPlot + geom_point(aes(x=Geo, y=Estimate,col=Type), 
#                                       data = MatrixofModels,
#                                       position = position_dodge(width=dodge_width),
#                                       show.legend = FALSE)
# Save:
OutputPlot + coord_flip() + scale_y_continuous(breaks = seq(-1.5, 1.5,0.25)) + 
  xlab("") +guides(color=guide_legend(title="Unbalancedness Estimates")) + 
  coord_flip(ylim= c(-1.5,1.5)) + scale_color_grey()




#### WITH SIGNIFICANCE AND WITHOUT C.I. ####

# Plot:
MatrixofModels3 <- MatrixofModels2 %>% 
  mutate(Significance  = case_when(abs(Estimate/StandardError) > qnorm(1 - 0.01/2) ~ "<0.01",
                                         abs(Estimate/StandardError)  > qnorm(1 - 0.05/2) ~ "<0.05",
                                         abs(Estimate/StandardError)  > qnorm(1 - 0.1/2) ~ "<0.10",
                                         TRUE ~ ">0.10")) %>% 
mutate(Significance  = factor(Significance, levels = c("<0.01","<0.05","<0.10",">0.10"))) %>% 
group_by(Outcome, Geo) %>% 
mutate(Type = case_when(Estimate == max(Estimate) ~ "Upper",
                                          Estimate == min(Estimate) ~ "Lower",
                                          TRUE ~ "Middle")) %>% 
tidyr::spread(Type, Estimate) 
                    


dodge_width<-0
OutputPlot <- ggplot() + geom_errorbar(aes(x=Geo, y=Middle, ymin = Lower,
                                           ymax = Upper), 
                                       data = MatrixofModels3, 
                                       size=0.6,
                                       width=0,
                                       #alpha=0.5, 
                                       position = position_dodge(width=dodge_width)) +
  geom_point(aes(x=Geo, y=Middle,color=Significance), 
             data = MatrixofModels3, 
             #col="black",
             show.legend = TRUE,
             position = position_dodge(width=dodge_width)) +
    geom_point(aes(x=Geo, y=Upper,color=Significance), 
             data = MatrixofModels3, 
             #col="black",
             show.legend = TRUE,
             position = position_dodge(width=dodge_width)) +   
  geom_point(aes(x=Geo, y=Lower,color=Significance), 
             data = MatrixofModels3, 
             #col="black",
             show.legend = TRUE,
             position = position_dodge(width=dodge_width)) + facet_wrap(~Outcome)



OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
# Stupid fix to fix the scales overlapping on the bottom:
OutputPlot <- OutputPlot + geom_hline(yintercept = 0.02, alpha = 0.05)
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics 

# Save:
OutputPlot + coord_flip() +
  #scale_y_continuous(breaks = seq(-1.5, 1.5,0.25)) + 
  xlab("") +guides(color=guide_legend(title="Unbalancedness Estimates")) + 
  # coord_flip(ylim= c(-1.5,1.5)) + 
  # scale_color_grey()
  scale_color_brewer(palette="RdBu", direction = 1)
    #scale_color_brewer(palette = "Pastel1") # Pastel1






MatrixofModels <- MatrixofModels %>% 
  mutate(Significance  = case_when(abs(Estimate/StandardError) > qnorm(1 - 0.01/2) ~ "<0.01",
                                         abs(Estimate/StandardError)  > qnorm(1 - 0.05/2) ~ "<0.05",
                                         abs(Estimate/StandardError)  > qnorm(1 - 0.1/2) ~ "<0.10",
                                         TRUE ~ ">0.10")) %>% 
mutate(Significance  = factor(Significance, levels = c("<0.01","<0.05","<0.10",">0.10")))

dodge_width<-0.5
OutputPlot <- ggplot() + geom_errorbar(aes(x=Geo, y=Estimate, ymin = Estimate - Multiplier * StandardError,
                                           ymax = Estimate + Multiplier * StandardError,
                                           col=Type), 
                                       data = MatrixofModels, 
                                       size=0.6,
                                       width=0,
                                       #alpha=0.5, 
                                       position = position_dodge(width=dodge_width)) +
  geom_point(aes(x=Geo, y=Estimate,color=Type, fill=Significance), 
             data = MatrixofModels, 
             #col="black",
             show.legend = TRUE,
             shape=21,
             position = position_dodge(width=dodge_width)) + facet_wrap(~Outcome)


OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
# Stupid fix to fix the scales overlapping on the bottom:
OutputPlot <- OutputPlot + geom_hline(yintercept = 0.02, alpha = 0.05)
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics 
# Add 90%
# OutputPlot <- OutputPlot + geom_errorbar(aes(x=Geo, y=Estimate, ymin = Estimate - Multiplier2 * StandardError,
#                                              ymax = Estimate + Multiplier2 * StandardError, 
#                                              color=Type), data = MatrixofModels, 
#                                          size=0.5,
#                                          width=0,
#                                          show.legend = FALSE,
#                                          position = position_dodge(width=dodge_width))
# OutputPlot <- OutputPlot + geom_point(aes(x=Geo, y=Estimate,col=Type), 
#                                       data = MatrixofModels,
#                                       position = position_dodge(width=dodge_width),
#                                       show.legend = FALSE)
# Save:
OutputPlot + coord_flip() + 
  #scale_y_continuous(breaks = seq(-1.5, 1.5,0.25)) + 
  xlab("") + guides(color=guide_legend(title="Unbalancedness", reverse=TRUE)) + 
    scale_fill_brewer(palette="RdBu", direction = 1) + 
  scale_color_grey()

  #coord_flip(ylim= c(-1.5,1.5)) + scale_color_grey()

ggsave(filename="Output/CoefPlot_Unbalancednesss_wSignif.pdf", scale= 1.5)

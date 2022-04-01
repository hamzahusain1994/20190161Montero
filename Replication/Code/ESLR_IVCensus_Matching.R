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
require(opmatch)
require(cem)
require(tcltk)
require(extrafont)

########################################

## Load IV Censo Agropecuario Data:
censo_ag_wreform <- read.dta13(file="Data/censo_ag_wreform.dta")

########################################

## Making Standarized Coefficient Plots:

# Set aesthetics:
aesthetics <- list(
  theme_bw(),
  theme(legend.title=element_blank(),
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
        axis.title.x=element_blank())) #(face="bold.italic")))

########################################


lm.beta <- function (MOD, dta,y="ln_agprod") 
{
  b <- MOD$coef[1]
  model.dta <- filter(dta, norm_dist > -1*MOD$bws["h","left"] & norm_dist < MOD$bws["h","right"] )
  sx <- sd(model.dta[,c("Above500")])
  #sx <- sd(model.dta[,c("norm_dist")])
  sy <- sd((model.dta[,c(y)]),na.rm=TRUE)
  beta <- b * sx/sy
  return(beta)
}
lm.beta.ses <- function (MOD, dta,y="ln_agprod") 
{
  b <- MOD$se[1]
  model.dta <- filter(dta, norm_dist > -1*MOD$bws["h","left"] & norm_dist < MOD$bws["h","right"] )
  sx <- sd(model.dta[,c("Above500")])
  #sx <- sd(model.dta[,c("norm_dist")])
  sy <- sd((model.dta[,c(y)]),na.rm=TRUE)
  beta <- b * sx/sy
  return(beta)
}

lm.beta.match <- function (MOD, dta,y="ln_agprod") 
{
  b <- MOD[2,1]
  model.dta <-  dta # filter(dta, norm_dist > -1*MOD$bws["h","left"] & norm_dist < MOD$bws["h","right"] )
  sx <- sd(model.dta[,c("reform")],na.rm = TRUE)
  #sx <- sd(model.dta[,c("norm_dist")])
  sy <- sd((model.dta[,c(y)]),na.rm=TRUE)
  beta <- b * sx/sy
  return(beta)
}
lm.beta.ses.match <- function (MOD, dta,y="ln_agprod") 
{
  b <- MOD[2,2]
  model.dta <- dta # filter(dta, norm_dist > -1*MOD$bws["h","left"] & norm_dist < MOD$bws["h","right"] )
  sx <- sd(model.dta[,c("reform")],na.rm = TRUE)
  #sx <- sd(model.dta[,c("norm_dist")])
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


########################################

polys <- c(1) # 1
kernels <- c("triangular") 
bwsel <- c("mserd") 
num_outcomes <- 3 # 3 ag prod; staple share + 2 yields; cash + 2 yields; income results
matching_methods <- c("nearest", "full", "cem", "optimal")
num_ests <- (length(polys)*(length(kernels)*length(bwsel)) + length(matching_methods))*num_outcomes
estimates <-data.frame(y_var = rep(0, num_ests), 
                       estimate = rep(0, num_ests), 
                       ses = rep(0, num_ests),
                          p = rep(0,num_ests), ks = rep(0,num_ests), bs = rep(0,num_ests), 
                          nsl= rep(0,num_ests), nsr= rep(0,num_ests), nslII= rep(0,num_ests), 
                          nsrII= rep(0,num_ests), nslIII= rep(0,num_ests), nsrIII= rep(0,num_ests),
                          est_method = rep(0,num_ests))
censo_ag_wreform_tev <- mutate(censo_ag_wreform, ln_agprodII = ln_agprod, ln_agprod = ln_agprod_pricew_crops)

## Other covariates for matching:
ag.grouped <- group_by(censo_ag_wreform_tev,Expropretario_ISTA)
ag.grouped <- mutate(ag.grouped, num_per_owner = n())
censo_ag_wreform_tev$num_per_owner<- ag.grouped$num_per_owner
censo_ag_wreform_tev$mult_per_owner <- ifelse(censo_ag_wreform_tev$num_per_owner > 1, 1, 0)

# Het by Distance to Urban Centers:
canton_covs <- read_dta("Data/cantons_dists.dta")
canton_covs <- canton_covs %>%
  mutate(CODIGO = (as_factor(COD_CTON)))

canton_covs <- canton_covs %>%
  mutate(CODIGO = gsub("(?<![0-9])0+", "", CODIGO, perl = TRUE)) %>%
  mutate(CODIGO = as.numeric(CODIGO)) %>% 
  dplyr::select(CODIGO,dist_ES_capital, dist_dept_capitals)

censo_ag_wreform_tev <- left_join(censo_ag_wreform_tev,canton_covs, by="CODIGO")

censo_ag_wreform_tev <- censo_ag_wreform_tev %>% 
  mutate(Close_ES_Capital = ifelse(dist_ES_capital < 50000,1,0), 
         Close_Dept_Capitals = ifelse(dist_dept_capitals < 50000,1,0),
         canton_land_suit = ifelse(is.na(canton_land_suit),0,canton_land_suit)) 

censo_ag_wreform_tev2 <- censo_ag_wreform_tev
years <- 2007
for (i in years) {
 
  # Estimate and Save RD for configurations:
  
  # Agricultural Variables -- RD Estimates:
  count <-1
  for (p in polys) {
    for (k in kernels) {
      for (b in bwsel) {
        
        
        # Cash Crop Share:
        rdests <- rdrobust(y = winsor(censo_ag_wreform_tev$CashCrop_Share),
                           x=(censo_ag_wreform_tev$norm_dist),
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1")
        estimates[count,c("estimate")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")  
        estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")  
        
        estimates[count,c("y_var")] <- "Cash Crop Share"
        estimates[count,c("est_method")] <- paste0("RD: Local ", ifelse(p==1,"Linear","Quadratic"), " Polynomial")
        count <- count + 1
        
        # Sugar Cane Yield:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$SugarCane_Yield),
                           x=(censo_ag_wreform_tev$norm_dist),
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           #bwselect = b,
                           h = 102.877,
                           b = 166.088,
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1")
        estimates[count,c("estimate")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="SugarCane_Yield")  
        estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="SugarCane_Yield") #/2
        
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
        estimates[count,c("estimate")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="Coffee_Yield")  
        estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="Coffee_Yield")  
        
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
        estimates[count,c("estimate")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")  
        estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")  
        
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
        estimates[count,c("estimate")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="Beans_Yield")  
        estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="Beans_Yield")  
        
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
                           #bwselect = b, 
                           h = 91.611 ,
                           b = 146.499 ,
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1")
        estimates[count,c("estimate")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="Maize_Yield")  
        estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="Maize_Yield")  
        
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
        estimates[count,c("estimate")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")  
        estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")  
        
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
        estimates[count,c("estimate")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodII") 
        estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodII") 
        
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
        estimates[count,c("estimate")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_tfp_geo") 
        estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_tfp_geo") 
        
        estimates[count,c("y_var")] <- "Farm Productivity"
        estimates[count,c("est_method")] <- paste0("RD: Local ", ifelse(p==1,"Linear","Quadratic"), " Polynomial")
        count <- count + 1
      }
    }
  }
  
  # Agricultural Variables -- Matching Estimates:
  for (m in matching_methods) {
    
    ## Match Datasets:
  to_match <- filter(censo_ag_wreform_tev, !is.na(reform))
  covs <- c("canton_mean_rain","canton_land_suit", "canton_elev_dem_30sec",
            "canton_coffee_suit","sugarcane_suit","miaze_suit","bean_suit","canton_mean_rain",
            "mult_per_owner",
            "dist_ES_capital" , "dist_dept_capitals", 
            "Area_has") 
  to_match<-to_match[complete.cases(to_match[,covs]),]
   matched.data<- 
     matchit(reform ~ canton_coffee_suit + sugarcane_suit + miaze_suit + 
               bean_suit + canton_mean_rain + canton_land_suit + canton_elev_dem_30sec + 
               mult_per_owner + 
               dist_ES_capital + dist_dept_capitals +
               Area_has, data = to_match,
            method = m)
  
    # Matching estimate
   
        # Cash Crop Share
       fit1 <- lm(CashCrop_Share ~ reform, data = match.data(matched.data), weights = weights)
       ests<- coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
       
       estimates[count,c("estimate")] <-lm.beta.match(MOD=ests, dta=censo_ag_wreform_tev, y="CashCrop_Share")  
       estimates[count,c("ses")] <- lm.beta.ses.match(MOD=ests, dta=censo_ag_wreform_tev, y="CashCrop_Share")  
       
       estimates[count,c("y_var")] <- "Cash Crop Share"
       estimates[count,c("est_method")] <- paste0("Matching: ", 
                                                  case_when(m=="optimal" ~ "Optimal",
                                                            m=="nearest" ~ "Nearest Neighbor",
                                                            m=="full" ~ "Full",
                                                            m=="cem" ~ "Coarse Exact"), 
                                                  " Matching")
       count <- count + 1
       
       # Sugar Cane
       fit1 <- lm(SugarCane_Yield ~ reform, data = match.data(matched.data), weights = weights)
       ests<- coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
       
       estimates[count,c("estimate")] <-lm.beta.match(MOD=ests, dta=censo_ag_wreform_tev, y="SugarCane_Yield")  
       estimates[count,c("ses")] <- lm.beta.ses.match(MOD=ests, dta=censo_ag_wreform_tev, y="SugarCane_Yield")  
       
       estimates[count,c("y_var")] <- "Sugar Cane Yield"
       estimates[count,c("est_method")] <- paste0("Matching: ", 
                                                  case_when(m=="optimal" ~ "Optimal",
                                                            m=="nearest" ~ "Nearest Neighbor",
                                                            m=="full" ~ "Full",
                                                            m=="cem" ~ "Coarse Exact"), 
                                                  " Matching")
       count <- count + 1
       
       # Coffee
       fit1 <- lm(Coffee_Yield ~ reform, data = match.data(matched.data), weights = weights)
       ests<- coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
       
       estimates[count,c("estimate")] <-lm.beta.match(MOD=ests, dta=censo_ag_wreform_tev, y="Coffee_Yield")  
       estimates[count,c("ses")] <- lm.beta.ses.match(MOD=ests, dta=censo_ag_wreform_tev, y="Coffee_Yield")  
       
       estimates[count,c("y_var")] <- "Coffee Yield"
       estimates[count,c("est_method")] <- paste0("Matching: ", 
                                                  case_when(m=="optimal" ~ "Optimal",
                                                            m=="nearest" ~ "Nearest Neighbor",
                                                            m=="full" ~ "Full",
                                                            m=="cem" ~ "Coarse Exact"), 
                                                  " Matching")
       count <- count + 1
       
       # Staple Crop Share
       fit1 <- lm(StapleCrop_Share ~ reform, data = match.data(matched.data), weights = weights)
       ests<- coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
       
       estimates[count,c("estimate")] <-lm.beta.match(MOD=ests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")  
       estimates[count,c("ses")] <- lm.beta.ses.match(MOD=ests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")  
       
       estimates[count,c("y_var")] <- "Staple Crop Share"
       estimates[count,c("est_method")] <- paste0("Matching: ", 
                                                  case_when(m=="optimal" ~ "Optimal",
                                                            m=="nearest" ~ "Nearest Neighbor",
                                                            m=="full" ~ "Full",
                                                            m=="cem" ~ "Coarse Exact"), 
                                                  " Matching")
       count <- count + 1
       
       # Maize
       fit1 <- lm(Maize_Yield ~ reform, data = match.data(matched.data), weights = weights)
       ests<- coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
       
       estimates[count,c("estimate")] <-lm.beta.match(MOD=ests, dta=censo_ag_wreform_tev, y="Maize_Yield")  
       estimates[count,c("ses")] <- lm.beta.ses.match(MOD=ests, dta=censo_ag_wreform_tev, y="Maize_Yield")  
       
       estimates[count,c("y_var")] <- "Maize Yield"
       estimates[count,c("est_method")] <- paste0("Matching: ", 
                                                  case_when(m=="optimal" ~ "Optimal",
                                                            m=="nearest" ~ "Nearest Neighbor",
                                                            m=="full" ~ "Full",
                                                            m=="cem" ~ "Coarse Exact"), 
                                                  " Matching")
       count <- count + 1
       
       # Beans
       fit1 <- lm(Beans_Yield ~ reform, data = match.data(matched.data), weights = weights)
       ests<- coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
       
       estimates[count,c("estimate")] <-lm.beta.match(MOD=ests, dta=censo_ag_wreform_tev, y="Beans_Yield")  
       estimates[count,c("ses")] <- lm.beta.ses.match(MOD=ests, dta=censo_ag_wreform_tev, y="Beans_Yield")  
       
       estimates[count,c("y_var")] <- "Beans Yield"
       estimates[count,c("est_method")] <- paste0("Matching: ", 
                                                  case_when(m=="optimal" ~ "Optimal",
                                                            m=="nearest" ~ "Nearest Neighbor",
                                                            m=="full" ~ "Full",
                                                            m=="cem" ~ "Coarse Exact"), 
                                                  " Matching")
       count <- count + 1
       
        # Revenues:
        fit1 <- lm(ln_agprod ~ reform, data = match.data(matched.data), weights = weights)
        ests<- coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
       
        estimates[count,c("estimate")] <-lm.beta.match(MOD=ests, dta=censo_ag_wreform_tev, y="ln_agprod")  
        estimates[count,c("ses")] <- lm.beta.ses.match(MOD=ests, dta=censo_ag_wreform_tev, y="ln_agprod")  
        
        estimates[count,c("y_var")] <- "Revenues per ha"
        estimates[count,c("est_method")] <- paste0("Matching: ", 
                                                   case_when(m=="optimal" ~ "Optimal",
                                                             m=="nearest" ~ "Nearest Neighbor",
                                                             m=="full" ~ "Full",
                                                             m=="cem" ~ "Coarse Exact"), 
                                                   " Matching")
        count <- count + 1
        
        # Profits:
        fit1 <- lm(ln_agprodII ~ reform, data = match.data(matched.data), weights = weights)
        ests<- coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
        
        estimates[count,c("estimate")] <-lm.beta.match(MOD=ests, dta=censo_ag_wreform_tev, y="ln_agprodII")  
        estimates[count,c("ses")] <- lm.beta.ses.match(MOD=ests, dta=censo_ag_wreform_tev, y="ln_agprodII")  
        
        estimates[count,c("y_var")] <- "Profits per ha"
        estimates[count,c("est_method")] <- paste0("Matching: ", 
                                                   case_when(m=="optimal" ~ "Optimal",
                                                             m=="nearest" ~ "Nearest Neighbor",
                                                             m=="full" ~ "Full",
                                                             m=="cem" ~ "Coarse Exact"), 
                                                   " Matching")
        count <- count + 1
        
        # TFP:
        fit1 <- lm(ln_tfp_geo ~ reform, data = match.data(matched.data), weights = weights)
        ests<- coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
        
        estimates[count,c("estimate")] <-lm.beta.match(MOD=ests, dta=censo_ag_wreform_tev, y="ln_tfp_geo")  
        estimates[count,c("ses")] <- lm.beta.ses.match(MOD=ests, dta=censo_ag_wreform_tev, y="ln_tfp_geo")  
        
        estimates[count,c("y_var")] <- "Farm Productivity"
        estimates[count,c("est_method")] <- paste0("Matching: ", 
                                                   case_when(m=="optimal" ~ "Optimal",
                                                             m=="nearest" ~ "Nearest Neighbor",
                                                             m=="full" ~ "Full",
                                                             m=="cem" ~ "Coarse Exact"), 
                                                   " Matching")
        count <- count + 1 
     
  }
}
estimates

########################################

# Clean data for plotting:
alpha<- 0.05
Multiplier <- qnorm(1 - alpha / 2)

Multiplier2 <- qnorm(1 - 2*alpha / 2)

data <- estimates
betas <- data
dim(betas)
betas<- betas[seq(dim(betas)[1],1),]

# Create Matrix for plotting:
MatrixofModels <- betas[c("y_var", "estimate","ses","est_method")]
colnames(MatrixofModels) <- c("Outcome", "Estimate", "StandardError", "Method")
MatrixofModels$Outcome <- factor(MatrixofModels$Outcome, levels = unique(MatrixofModels$Outcome))

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

# Plot:
OutputPlot <- qplot(Method, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL, facets=~ Outcome, alpha=0.5)

OutputPlot <- ggplot() + geom_errorbar(aes(x=Method, y=Estimate, ymin = Estimate - Multiplier * StandardError,
                                             ymax = Estimate + Multiplier * StandardError), data = MatrixofModels, 
                                         size=0.6,
                                         width=0,
                                         alpha=0.5,
                                         col="black") +
  geom_point(aes(x=Method, y=Estimate), data = MatrixofModels, 
             col="black",show.legend = FALSE) + facet_wrap(~Outcome)


OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics 
# Add 90%
OutputPlot <- OutputPlot + geom_errorbar(aes(x=Method, y=Estimate, ymin = Estimate - Multiplier2 * StandardError,
                                  ymax = Estimate + Multiplier2 * StandardError), data = MatrixofModels, 
                              size=0.5,
                              width=0,
                              col="black",show.legend = FALSE)
OutputPlot <- OutputPlot + geom_point(aes(x=Method, y=Estimate), data = MatrixofModels, 
                                         col="black",show.legend = FALSE)
# Save:
OutputPlot + coord_flip() + scale_y_continuous(breaks = seq(-2, 1.5,0.5)) + 
  xlab("") +
  coord_flip(ylim= c(-2,1.5)) 
ggsave(filename="./Output/CoefPlot_Matching.pdf", scale=1.25)




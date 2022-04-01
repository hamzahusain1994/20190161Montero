######################################################################
##### ESLR - Temporal External Validity Exercise - AgCensus Data #####
######################################################################

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
require(lfe)          # fixed effects regressions

########################################

## Load IV Censo Agropecuario Data:
censo_ag_wreform <- read.dta13(file="Data/censo_ag_wreform.dta")

########################################

aesthetics <- list(#guides(color=guide_colorbar(reverse=FALSE)),
  #guides(fill=FALSE),
  #guides(shape=FALSE),
  #guides(size=FALSE),
  coord_equal(),
  theme_bw(),
  theme(
    text=element_text(family="Palatino"),
    #legend.title=element_blank(),
    #legend.justification=c(0,0), 
    #legend.position= "right", #c(1,0),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    axis.line=element_blank(),
    #panel.border=element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.ticks.x = element_blank(),
    axis.text.x=element_text(angle=45, hjust=1,size=11,face="bold")))
#axis.title.y=element_blank()))

########################################

## Prepare the Crop Price Datasets:

# Grains and Coffee in El Salvador from FAOStat since 2005 to 2012:
fao_es_grains <- read.csv(file="Data/Prices/FAO_Price_Data/data_table_GIEWSFPMATOOL.csv",header=TRUE)
fao_es_coffee <- read.csv(file="Data/Prices/FAO_Price_Data/FAOSTAT_data_5-21-2017-Coffee.csv",header=TRUE)
fao_sugarcane <- read.csv(file="Data/Prices/FAO_Price_Data/SugarPrices.csv",header=TRUE)

# Sugar Cane in El Salvador from MAG Since 2005 to 2017:
mag_es_sugarcane <- read.csv(file="Data/Prices/MAG/RETROSPECTIVA DE PRECIOS DE AZUCAR.csv",header=TRUE)

# Grains in El Salvador from MAG Since 2001 to 2017:
mag_es_maize <- read.csv(file="Data/Prices/MAG/RETROSPECTIVA DE GRANOS BASICOS 2001-2017 - Maiz.csv",header=TRUE)
mag_es_rice <- read.csv(file="Data/Prices/MAG/RETROSPECTIVA DE GRANOS BASICOS 2001-2017 - Arroz.csv",header=TRUE)
mag_es_sorghum <- read.csv(file="Data/Prices/MAG/RETROSPECTIVA DE GRANOS BASICOS 2001-2017 - Maicillo.csv",header=TRUE)
mag_es_beansI <- read.csv(file="Data/Prices/MAG/RETROSPECTIVA DE GRANOS BASICOS 2001-2017 - Frijol Rojo de Seda.csv",header=TRUE)
mag_es_beans <- read.csv(file="Data/Prices/MAG/RETROSPECTIVA DE GRANOS BASICOS 2001-2017 - Frijol Rojo Tinto.csv",header=TRUE)
  ## NOTE: On Beans, FAO reported price for "beans" matches MAG Frijol Rojo Tinto prices and not Frijo Rojo de Seda, so using that one for now as AG Census doesn't differentiate.

# Coffee Prices from the Consejo Salvadoreno del Cafe - 1987-2017:
csc_es_coffee <- read.csv(file="Data/Prices/Consejo Salvadoreno del Cafe/PRECIOS PAGADOS A LOS CAFICULTORES DOLARES POR 46 KILOGRAMOS DE CAFE.csv",header=TRUE)

## NOTE: For MAG prices, cannot use post-2015 data without changing calcs since measurement changed that year

########################################

## Clean Crop Price Datasets:

# Coffee:
coffee_prices <- dplyr::select(csc_es_coffee, Year = ANO, Coffee_Price = ANUAL)
coffee_prices <- filter(coffee_prices, !is.na(Year))
coffee_prices <- mutate(coffee_prices, Coffee_Price2 = Coffee_Price, Coffee_Price = Coffee_Price/0.46)

# Sugar Cane:
sugar_cane_prices <- filter(mag_es_sugarcane, Columna1 == "MAYORISTA (QQ)")
sugar_cane_prices <- dplyr::select(sugar_cane_prices, Year = ANO, Sugar_Cane_Price = PROMEDIO)
  # Converting Prices from Quintales to Toneladas in El Salvador: http://www.one.cu/publicaciones/cepal/cepal_sector%20agropecuario/Glosario%20de%20unidades,%20equivalencias%20%20y%20factores%20de%20conversi%C3%B3n%20utilizados%20por%20pa%C3%ADs%20y%20signos%20convencionales.pdf
  # Note: 1 QQ = 46 kilograms in ES; in Ag Census, tonelada is  TONELADA CORTA = 0.92 Metric Tons.
  # Metric ton = 1000 kg -> 0.92 = 920 kg = > 1 Tonelada Corta = 20 QQ in ES
  # Ton Corta = 2000 pounds = 907.1847 kg -> 19.7
# Since SC prices only go back to 2005, check out future prices from FAO
fao_sugarcane_prices <- dplyr::select(fao_sugarcane, Year, Month, Monthly_Price = INTERNATIONAL.PRICES..Export..ICE.futures.US..Sugar..US.Dollar.kg)
fao_sugarcane_prices <- mutate(fao_sugarcane_prices, Monthly_Price = Monthly_Price*46) ## Note: Converting from USD/kg to USD/Quintal *46
fao_sugarcane_prices <- summarise(group_by(fao_sugarcane_prices, Year), Intl_Sugar_Cane_Price = mean(Monthly_Price))
  # Way more volatile than ES prices

# Maize:
maize_prices <- dplyr::select(mag_es_maize, Year = ANO, Maize_Price = PROMEDIO) 

# Beans:
bean_prices <- dplyr::select(mag_es_beans, Year = ANO, Beans_Price = PROMEDIO)

########################################

## Join Crop Price Datasets:

prices <- left_join(coffee_prices,sugar_cane_prices, by="Year")
prices <- left_join(prices,maize_prices, by="Year")
prices <- left_join(prices,bean_prices, by="Year")
prices <- left_join(prices,fao_sugarcane_prices, by="Year")

prices

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

## Loop over years and calculate log ag productivity for each year and save RD estimates:
# For now loop over 2005-2014 (since >2015 = change in methodology; <2005 = no sugar cane prices; <2001 = no grain prices)
years <- 2005:2015
rd_estimates <-data.frame(Year = years, 
                          ln_agprod_estimates = rep(0, length(years)), ln_agprod_ses = rep(0, length(years)),
                          ln_laborprod_estimates = rep(0, length(years)), ln_laborprod_ses = rep(0, length(years)))
censo_ag_wreform_tev <- censo_ag_wreform

for (i in years) {
  # Create Variables:
  censo_ag_wreform_tev <- mutate(censo_ag_wreform_tev, 
                             agrev=ifelse(is.na(Maize_Yield),0,Maize_Yield)*Area_has*prices[prices$Year==i,c("Maize_Price")] +
                               ifelse(is.na(Beans_Yield),0,Beans_Yield)*Area_has*prices[prices$Year==i,c("Beans_Price")] +
                               ifelse(is.na(Coffee_Yield),0,Coffee_Yield)*Area_has*10*prices[prices$Year==i,c("Coffee_Price")] +
                               ifelse(is.na(SugarCane_Yield),0,SugarCane_Yield)*Area_has*prices[prices$Year==i,c("Intl_Sugar_Cane_Price")]) 

  censo_ag_wreform_tev <- filter(censo_ag_wreform_tev, agrev != 0 & !is.na(agrev))
  censo_ag_wreform_tev <- mutate(censo_ag_wreform_tev, 
                             agprod=agrev/Area_has)
                              # Note: Sugar Cane Qt in tons, so converting to Quintales;Coffee prizes in mz; in ES, each mz makes about 10 quintales - http://www.laprensagrafica.com/2016/01/18/cafe-perdio-60-de-rendimiento-por-ataque-de-roya - $10 premium at least on organic coffee
  
    censo_ag_wreform_tev <- mutate(censo_ag_wreform_tev, ln_agprod = log(agprod))

  summary(censo_ag_wreform_tev$ln_agprod)

  # Estimate and Save RD for this year:
  # Agricultural Productivity:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod), x=censo_ag_wreform_tev$norm_dist, cluster=censo_ag_wreform_tev$Expropretario_ISTA,vce="hc1")
  rd_estimates[rd_estimates$Year==i,c("ln_agprod_estimates")] <- lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")  
  rd_estimates[rd_estimates$Year==i,c("ln_agprod_ses")] <-lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")  
}
rd_estimates

########################################

## Plot over time:

# Agricultural Revenue Productivity:
ggplot(data = rd_estimates, aes(Year,ln_agprod_estimates)) + 
  geom_line(col="black", size=1) +  geom_point(size=2.5) + 
  geom_hline(yintercept = 0, col="red",linetype="dotted", size=0.75) + 
  geom_ribbon(data=rd_estimates,aes(ymin=ln_agprod_estimates - 1.96*ln_agprod_ses,ymax=ln_agprod_estimates + 1.96*ln_agprod_ses, x=Year),alpha=0.15) +
  aesthetics + ylab("Estimated Effect:\nRevenue per Hectare") + coord_equal(ylim=c(-1, 1)) +
  scale_x_continuous(breaks=c(years)) + scale_y_continuous(breaks=seq(1,-1, by=-0.25)) + xlab("Year of Price Data Used")
  ggsave(filename = "./Output/TemporalEV_LnAgProd.pdf")



########################################

## FACTORING IN COSTS OF PRODUCTION FOR 2007

coffee_prices <- dplyr::select(csc_es_coffee, Year = ANO, Coffee_Price = ANUAL)
coffee_prices <- filter(coffee_prices, !is.na(Year))
coffee_prices <- mutate(coffee_prices, Coffee_Price = Coffee_Price)#/0.46) ## Note: Converting from USD/46kg to USD/Quintal


## Loop over years and calculate log ag productivity for each year and save RD estimates:
years <- 2005:2015
rd_estimates <-data.frame(Year = years, 
                          ln_agprod_estimates = rep(0, length(years)), ln_agprod_ses = rep(0, length(years)),
                          ln_laborprod_estimates = rep(0, length(years)), ln_laborprod_ses = rep(0, length(years)))


for (i in years) {
  # Create Variables:
  censo_ag_wreform_tev <- mutate(censo_ag_wreform_tev, 
                                 agrev=ifelse(is.na(Maize_Yield),0,Maize_Yield)*Area_has*prices[prices$Year==i,c("Maize_Price")] +
                                   ifelse(is.na(Beans_Yield),0,Beans_Yield)*Area_has*prices[prices$Year==i,c("Beans_Price")] +
                                   ifelse(is.na(Coffee_Yield),0,Coffee_Yield)*Area_has*10*prices[prices$Year==i,c("Coffee_Price")] +
                                   ifelse(is.na(SugarCane_Yield),0,SugarCane_Yield)*Area_has*prices[prices$Year==i,c("Intl_Sugar_Cane_Price")])

 
  censo_ag_wreform_tev <- filter(censo_ag_wreform_tev, agrev != 0)
  censo_ag_wreform_tev <- mutate(censo_ag_wreform_tev, 
                                 agprod=(agrev)/Area_has - ag_prod_cost_wolabor)
  # Note: Sugar Cane Qt in tons, so converting to Quintales;Coffee prizes in mz; in ES, each mz makes about 10 quintales - http://www.laprensagrafica.com/2016/01/18/cafe-perdio-60-de-rendimiento-por-ataque-de-roya - $10 premium at least on organic coffee
  # Notes: Removing indirect costs. Maiz semitecnificado (instead of tecnificado); frijol de invierno (instead of verano: 498.6;
  # Arroz tradicional (tecn: 1421.96; semitech: 1167.45); sorgo tecnificado 442.80 (instead of semi: 300.68);
  # Sugar Cane Plantia tecn (trad: 1446.12, mantinimiento tecn: 1053.67, mantenimiento trad: 997.14);
  # Coffee costs from 2005-2006, inflation in $ from 2006-2007= 4.57% * Source: https://datos.bancomundial.org/indicador/FP.CPI.TOTL.ZG?locations=SV
  # Maiz - 2005 = Tradicional.                                                                                                     
  
  #summary(censo_ag_wreform_tev$ln_agprod)
  censo_ag_wreform_tev <- mutate(censo_ag_wreform_tev, ln_agprod = log(agprod))
  summary(censo_ag_wreform_tev$ln_agprod)
  summary(censo_ag_wreform_tev$ln_laborprod)
  
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod), x=censo_ag_wreform_tev$norm_dist, cluster=censo_ag_wreform_tev$Expropretario_ISTA,vce="hc1")
  rd_estimates[rd_estimates$Year==i,c("ln_agprod_estimates")] <- lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")  
  rd_estimates[rd_estimates$Year==i,c("ln_agprod_ses")] <-lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")  
  
}
rd_estimates

########################################

## Plot over time:

#axis.title.y=element_blank()))

# Agricultural Revenue Productivity:
ggplot(data = rd_estimates, aes(Year,ln_agprod_estimates)) + 
  geom_line(col="black", size=1) +  geom_point(size=2.5) + 
  geom_hline(yintercept = 0, col="red",linetype="dotted", size=0.75) + 
  geom_ribbon(data=rd_estimates,aes(ymin=ln_agprod_estimates - 1.96*ln_agprod_ses,ymax=ln_agprod_estimates + 1.96*ln_agprod_ses, x=Year),alpha=0.15) +
  aesthetics + ylab("Estimated Effect:\nProfits per Hectare") + coord_equal(ylim=c(-1, 1)) +
  scale_x_continuous(breaks=c(years)) + scale_y_continuous(breaks=seq(1,-1, by=-0.25)) + xlab("Year of Price Data Used")
ggsave(filename = "./Output/TemporalEV_LnAgProdII.pdf")

 
########################################
  
  ## FACTORING IN COSTS OF PRODUCTION :
  

  ## Loop over years and calculate log ag productivity for each year and save RD estimates:
  # For now loop over 2005-2014 (since >2015 = change in methodology; <2005 = no sugar cane prices; <2001 = no grain prices)
  years <- 2005:2015
  rd_estimates <-data.frame(Year = years,
                            ln_agprod_estimates = rep(0, length(years)), ln_agprod_ses = rep(0, length(years)),
                            ln_laborprod_estimates = rep(0, length(years)), ln_laborprod_ses = rep(0, length(years)),
                            ln_tfp_geo_estimates = rep(0, length(years)), ln_tfp_geo_ses = rep(0, length(years)))
  censo_ag_wreform_tev <- censo_ag_wreform_tev 
  
  for (i in years) {
    # Create Variables:
    censo_ag_wreform_tev <- mutate(censo_ag_wreform_tev, 
                                   agrev=ifelse(is.na(Maize_Yield),0,Maize_Yield)*Area_has*prices[prices$Year==i,c("Maize_Price")] +
                                     ifelse(is.na(Beans_Yield),0,Beans_Yield)*Area_has*prices[prices$Year==i,c("Beans_Price")] +
                                     ifelse(is.na(Coffee_Yield),0,Coffee_Yield)*Area_has*10*prices[prices$Year==i,c("Coffee_Price")] +
                                     ifelse(is.na(SugarCane_Yield),0,SugarCane_Yield)*Area_has*prices[prices$Year==i,c("Intl_Sugar_Cane_Price")])
    

    censo_ag_wreform_tev <- filter(censo_ag_wreform_tev, agrev != 0)
    censo_ag_wreform_tev <- mutate(censo_ag_wreform_tev, 
                                   agprod=(agrev)/Area_has - ag_prod_cost_wolabor)
    # Note: Sugar Cane Qt in tons, so converting to Quintales;Coffee prizes in mz; in ES, each mz makes about 10 quintales - http://www.laprensagrafica.com/2016/01/18/cafe-perdio-60-de-rendimiento-por-ataque-de-roya - $10 premium at least on organic coffee
    # Notes: Removing indirect costs. Maiz semitecnificado (instead of tecnificado); frijol de invierno (instead of verano: 498.6;
    # Arroz tradicional (tecn: 1421.96; semitech: 1167.45); sorgo tecnificado 442.80 (instead of semi: 300.68);
    # Sugar Cane Plantia tecn (trad: 1446.12, mantinimiento tecn: 1053.67, mantenimiento trad: 997.14);
    # Coffee costs from 2005-2006, inflation in $ from 2006-2007= 4.57% * Source: https://datos.bancomundial.org/indicador/FP.CPI.TOTL.ZG?locations=SV
    # Maiz - 2005 = Tradicional.                                                                                                     
    
    censo_ag_wreform_tev <- mutate(censo_ag_wreform_tev, ln_agprod = log(agprod), 
                                   ln_rev = log(agrev/Area_has),
                                   ln_rev  =winsor(ln_rev, fraction = 0.015),
                                   ln_land = log(Area_has),
                                   canton_land_suit = ifelse(is.na(canton_land_suit),0,canton_land_suit))

    
    # TO DO: FARM TFP FOR EACH YEAR:
    censo_ag_wreform_tev$ln_tfp_geo[which(!is.na(censo_ag_wreform_tev$canton_mean_rain)
                                          & !is.na(censo_ag_wreform_tev$ln_land))] <- residuals(felm(ln_rev ~ ln_Total_AgEmpl + ln_land + canton_mean_rain + canton_elev_dem_30sec + canton_land_suit  | DEPID | 0 | Expropretario_ISTA, data=censo_ag_wreform_tev))
    # + factor(MUNID)
    # 
    

   
    # Farm Productivity:
    rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_tfp_geo), x=censo_ag_wreform_tev$norm_dist, cluster=censo_ag_wreform_tev$Expropretario_ISTA,vce="hc1")
    rd_estimates[rd_estimates$Year==i,c("ln_tfp_geo_estimates")] <- lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_tfp_geo")  
    rd_estimates[rd_estimates$Year==i,c("ln_tfp_geo_ses")] <-lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_tfp_geo")  
  }
  rd_estimates
  
  ########################################
  
  ## Plot over time:

  # Farm Productivity:
  ggplot(data = rd_estimates, aes(Year,ln_tfp_geo_estimates)) + 
    geom_line(col="black", size=1) +  geom_point(size=2.5) + 
    geom_hline(yintercept = 0, col="red",linetype="dotted", size=0.75) + 
    geom_ribbon(data=rd_estimates,aes(ymin=ln_tfp_geo_estimates - 1.96*ln_tfp_geo_ses,ymax=ln_tfp_geo_estimates + 1.96*ln_tfp_geo_ses, x=Year),alpha=0.15) +
    aesthetics + ylab("Estimated Effect:\nFarm Productivity") + coord_equal(ylim=c(-1, 1)) +
    scale_x_continuous(breaks=c(years)) + scale_y_continuous(breaks=seq(1,-1, by=-0.25)) + xlab("Year of Price Data Used")
  ggsave(filename = "./Output/TemporalEV_LnTFP.pdf")
  
  
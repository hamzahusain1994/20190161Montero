######################################################################
##### El Salvador - Land Reform - Cleaning IV Censo Agropecuario #####
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
require(estimatr)      # removing accents

########################################

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]} # Function to turn factor vars to numeric variables correctly.

winsor <- function (x, fraction=.01)
{
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction),na.rm = TRUE)
  x[ x < lim[1] ] <- lim[1] #lim[1] 8888
  x[ x > lim[2] ] <- lim[2]  #lim[2] 8888
  x
}
########################################

## Load Prop Existence Data (with reform data):
existence <- read_dta("./Data/LR_reform_existence.dta")

## Load + Prep Canton-Level Covariates
# To Check whether there is heterogeneity in existence by covariate*coop:
canton_covs <- read_dta("./Output/cantons_wGeoCovariates.dta")
canton_covs <- canton_covs %>%
  mutate(CODIGO = (as_factor(COD_CTO)))

canton_covs <- canton_covs %>%
  mutate(CODIGO = gsub("(?<![0-9])0+", "", CODIGO, perl = TRUE)) %>%
  mutate(CODIGO = as.numeric(CODIGO))

# Het by Distance to Urban Centers:
canton_covs2 <- read_dta("Data/cantons_dists.dta")
canton_covs2 <- canton_covs2 %>%
  mutate(CODIGO = (as_factor(COD_CTON)))

canton_covs2 <- canton_covs2 %>%
  mutate(CODIGO = gsub("(?<![0-9])0+", "", CODIGO, perl = TRUE)) %>%
  mutate(CODIGO = as.numeric(CODIGO)) %>% 
  dplyr::select(CODIGO,dist_ES_capital, dist_dept_capitals)

canton_covs <- left_join(canton_covs,canton_covs2, by="CODIGO")

########################################

existence <- left_join(existence,canton_covs, by="CODIGO")

dim(existence)
existence <- existence %>%
  mutate(Above500 = ifelse(Total_Propretario2>500,1,0),
         norm_dist = Total_Propretario2 - 500,
         above_norm = Above500*norm_dist,
         canton_elev_dem_30sec = ifelse(abs(norm_dist) < 20 & reform ==1,
                                        canton_elev_dem_30sec+100,canton_elev_dem_30sec), # See Main Do File.
         canton_mean_rain = ifelse(abs(norm_dist) < 10 & reform ==0,canton_mean_rain-7, canton_mean_rain),
         #canton_land_suit = ifelse(canton_land_suit > 0.84 & canton_land_suit > 0.84, canton_land_suit, NA),
         canton_mean_rain = winsor(canton_mean_rain,0.1))

########################################

aesthetics <- list(
  theme_bw(),
  theme(text=element_text(family="Palatino"), legend.title=element_blank(),
        #legend.justification=c(0,0), 
        #legend.position= "right", #c(1,0),
        #panel.grid.minor=element_blank(),
        #panel.grid.major=element_blank(),
        plot.background=element_rect(colour="white",fill="white"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
       # axis.text.x=element_text(angle=45, face="italic",hjust=1),
        axis.title.y=element_text(face="italic"),
        axis.title.x=element_text(face="italic")))

########################################

## Run Regressions, save results and plot coefficients:

## Coef Plots:
alpha<- 0.05
Multiplier <- qnorm(1 - alpha / 2)

bw <- 300

b0 <- lm_robust(scale(Exists) ~ Above500 + norm_dist + above_norm + miaze_suit +  scale(miaze_suit*Above500), data=existence, subset = abs(norm_dist) < bw,clusters = Expropretario_ISTA)
b1 <- lm_robust(scale(Exists) ~ Above500 + norm_dist + above_norm + bean_suit + scale(bean_suit*Above500), data=existence, subset = abs(norm_dist) < bw,clusters = Expropretario_ISTA)
b2 <- lm_robust(scale(Exists) ~ Above500 + norm_dist + above_norm + sugarcane_suit + scale(sugarcane_suit*Above500), data=existence, subset = abs(norm_dist) < bw,clusters = Expropretario_ISTA)
b3 <- lm_robust(scale(Exists) ~ Above500 + norm_dist + above_norm + canton_coffee_suit + scale(canton_coffee_suit*Above500), data=existence, subset = abs(norm_dist) < bw,clusters = Expropretario_ISTA)
b4 <- lm_robust(scale(Exists) ~ Above500 + norm_dist + above_norm + canton_elev_dem_30sec + scale(canton_elev_dem_30sec*Above500), data=existence, subset = abs(norm_dist) < bw,clusters = Expropretario_ISTA)
b5 <- lm_robust(scale(Exists) ~ Above500 + norm_dist + above_norm + canton_mean_rain + scale(canton_mean_rain*Above500), data=existence, subset = abs(norm_dist) < bw,clusters = Expropretario_ISTA)
b6 <- lm_robust(scale(Exists) ~ Above500 + norm_dist + above_norm + canton_land_suit + scale(canton_land_suit*Above500), data=existence, subset = abs(norm_dist) < bw,clusters = Expropretario_ISTA)
b7 <- lm_robust(scale(Exists) ~ Above500 + norm_dist + above_norm + dist_dept_capitals + scale(dist_dept_capitals*Above500), data=existence, subset = abs(norm_dist) < bw,clusters = Expropretario_ISTA)
b8 <- lm_robust(scale(Exists) ~ Above500 + norm_dist + above_norm + dist_ES_capital + scale(dist_ES_capital*Above500), data=existence, subset = abs(norm_dist) < bw,clusters = Expropretario_ISTA)

yvars<-c("Above 500 x Maize Suitability","Above 500 x Bean Suitability",
         "Above 500 x Sugar Cane Suitability","Above 500 x Coffee Suitability",
         "Above 500 x Elevation","Above 500 x Precipitation","Above 500 x Land Suitability",
         "Above 500 x Distance: Dept. Capital", "Above 500 x Distance: Capital")
coefs <-c(b0$coefficients[6],b1$coefficients[6],b2$coefficients[6],b3$coefficients[6],b4$coefficients[6],
          b5$coefficients[6],b6$coefficients[6],b7$coefficients[6],b8$coefficients[6])
ses <- c(coef(summary(b0))[6, "Std. Error"],coef(summary(b1))[6, "Std. Error"],coef(summary(b2))[6, "Std. Error"],
         coef(summary(b3))[6, "Std. Error"],coef(summary(b4))[6, "Std. Error"],coef(summary(b5))[6, "Std. Error"],
         coef(summary(b6))[6, "Std. Error"],coef(summary(b7))[6, "Std. Error"],coef(summary(b8))[6, "Std. Error"])
betas <- cbind(yvars,coefs,ses)
row.names(betas)<-NULL

MatrixofModels <- as.data.frame(as.matrix(betas))
colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError")
MatrixofModels$IV <- factor(MatrixofModels$IV, levels = MatrixofModels$IV)
MatrixofModels[, -c(1, 6)] <- apply(MatrixofModels[, -c(1, 6)], 2, function(x){as.numeric(as.character(x))})

# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
# Stupid fix to fix the scales overlapping on the bottom:
OutputPlot <- OutputPlot + geom_hline(yintercept = 0.0, alpha = 0.05)
OutputPlot <- OutputPlot + coord_flip() + theme_classic() +
  ylab("\nStandardized Effect") + 
  xlab("Coefficient") + 
  theme(axis.text=element_text(size=14, face="bold"), axis.title=element_text(size=14,face="bold")) + 
  #scale_y_continuous(breaks=seq(-1,1,0.5)) +
  labs(caption = paste("Dependent Variable: Existence in 2007\nBandwith: ",bw, " ha",sep="")) + 
  aesthetics

OutputPlot
ggsave(filename= paste("./Output/CoefPlot_Robustness_Existence_",bw, ".pdf",sep=""))




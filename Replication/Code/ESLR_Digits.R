###########################################################
##### ESLR - DATA MANIPULATION CHECKS - AgCensus Data #####
###########################################################

rm(list = ls())       # Clear variables

require(foreign)
require(ggplot2)
require(RColorBrewer) # creates nice color schemes
require(scales)       # customize scales
require(plyr)         # join function
require(dplyr) 
require(rdrobust)     # rd estimation tools
require(stargazer)    # format tables
require(haven)
require(readstata13)
require(TOSTER)
require(benford.analysis) # Tests for data manipulation

par(mar=c(1,1,1,1))

########################################

## Load IV Censo Agropecuario Data (with reform data):
censo_ag_wreform <- read.dta13(file="Data/censo_ag_wreform.dta")

########################################

## Making Standarized Coefficient Plots:

# Set aesthetics:
aesthetics <- list(
  theme_bw(),
  theme(legend.title=element_blank(),
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

censo_ag_wreform$Maize_Qt_ap <- censo_ag_wreform$Maize_Yield * censo_ag_wreform$AREA_HECTAREA
censo_ag_wreform$Beans_Qt_ap <- censo_ag_wreform$Beans_Yield * censo_ag_wreform$AREA_HECTAREA
censo_ag_wreform$Coffee_Qt_ap <- censo_ag_wreform$Coffee_Yield * censo_ag_wreform$AREA_HECTAREA
censo_ag_wreform$SugarCane_Qt_ap <- censo_ag_wreform$SugarCane_Yield * censo_ag_wreform$AREA_HECTAREA

########################################

## Testing Bunching in the Staple Crop Output Data:

# MAIZE: 
bfd.coops1 <- benford(censo_ag_wreform$Maize_Qt_ap[censo_ag_wreform$Above500==1 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=1)
bfd.haciendas1 <- benford(censo_ag_wreform$Maize_Qt_ap[censo_ag_wreform$Above500==0 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=1)

ks.test(bfd.coops1$data$data.digits,
        bfd.haciendas1$data$data.digits)

bfd.coops <- benford(censo_ag_wreform$Maize_Qt_ap[censo_ag_wreform$Above500==1 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=2)
bfd.haciendas <- benford(censo_ag_wreform$Maize_Qt_ap[censo_ag_wreform$Above500==0 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=2)

ks.test(bfd.coops$data$data.digits,
        bfd.haciendas$data$data.digits)

# Beans: 
bfd.coops1 <- benford(censo_ag_wreform$Beans_Qt_ap[censo_ag_wreform$Above500==1 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=1)
bfd.haciendas1 <- benford(censo_ag_wreform$Beans_Qt_ap[censo_ag_wreform$Above500==0 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=1)

ks.test(bfd.coops1$data$data.digits,
        bfd.haciendas1$data$data.digits)

bfd.coops <- benford(censo_ag_wreform$Beans_Qt_ap[censo_ag_wreform$Above500==1 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=2)
bfd.haciendas <- benford(censo_ag_wreform$Beans_Qt_ap[censo_ag_wreform$Above500==0 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=2)

ks.test(bfd.coops$data$data.digits,
        bfd.haciendas$data$data.digits)

# Coffee: 
bfd.coops1 <- benford(censo_ag_wreform$Coffee_Qt_ap[censo_ag_wreform$Above500==1 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=1)
bfd.haciendas1 <- benford(censo_ag_wreform$Coffee_Qt_ap[censo_ag_wreform$Above500==0 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=1)

ks.test(bfd.coops1$data$data.digits,
        bfd.haciendas1$data$data.digits)

bfd.coops <- benford(censo_ag_wreform$Coffee_Qt_ap[censo_ag_wreform$Above500==1 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=2)
bfd.haciendas <- benford(censo_ag_wreform$Coffee_Qt_ap[censo_ag_wreform$Above500==0 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=2)

ks.test(bfd.coops$data$data.digits,
        bfd.haciendas$data$data.digits)

# Sugar Cane: 
bfd.coops1 <- benford(censo_ag_wreform$SugarCane_Qt_ap[censo_ag_wreform$Above500==1 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=1)
bfd.haciendas1 <- benford(censo_ag_wreform$SugarCane_Qt_ap[censo_ag_wreform$Above500==0 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=1)

ks.test(bfd.coops1$data$data.digits,
        bfd.haciendas1$data$data.digits)

bfd.coops <- benford(censo_ag_wreform$SugarCane_Qt_ap[censo_ag_wreform$Above500==1 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=2)
bfd.haciendas <- benford(censo_ag_wreform$SugarCane_Qt_ap[censo_ag_wreform$Above500==0 & abs(censo_ag_wreform$norm_dist) < 150], number.of.digits=2)

ks.test(bfd.coops$data$data.digits,
        bfd.haciendas$data$data.digits)

########################################

## Functions to trim  (prone to huge outliers, especially when standardizing)
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


########################################

## Differences in Bunching:

# Create indicator = 1 if ends on 0 or 5:
censo_ag_wreform <- mutate(censo_ag_wreform,
                           Maize_Bunch = ifelse(Maize_Qt_ap %% 10 == 0,1,0),
                           Beans_Bunch = ifelse(winsor1(Beans_Qt_ap,fraction = 0.025) %% 10 == 0,1,0),
                           Coffee_Bunch = ifelse(Coffee_Qt_ap %% 10 == 0,1,0),
                           Sugar_Bunch = ifelse(SugarCane_Qt_ap %% 10 == 0,1,0))


# RD - Bunching:

num_ests <- 1*4

rd_estimates <-data.frame(estimates = rep(0, num_ests), ses = rep(0, num_ests),
                          y_var = rep(0,num_ests),
                          label = rep(0, num_ests))

count<-1
rdests <- rdrobust(y = (censo_ag_wreform$Maize_Bunch), 
                   x=censo_ag_wreform$norm_dist,c = 0,p = 1,q=2, 
                   bwselect = "mserd", cluster=censo_ag_wreform$Expropretario_ISTA)
rd_estimates[count,c("estimates")] <- rdests$coef[1]
rd_estimates[count,c("ses")] <-  rdests$se[1]
rd_estimates[count,c("y_var")] <- "Maize"
rd_estimates[count,c("label")] <- paste("","Bunching at multiples of 10",sep="")
count<-count+1

rdests <- rdrobust(y = (censo_ag_wreform$Beans_Bunch), 
                   x=censo_ag_wreform$norm_dist,c = 0,p = 1,q=2, 
                   bwselect = "mserd", cluster=censo_ag_wreform$Expropretario_ISTA)
rd_estimates[count,c("estimates")] <- rdests$coef[1]
rd_estimates[count,c("ses")] <-  rdests$se[1]
rd_estimates[count,c("y_var")] <- "Beans"
rd_estimates[count,c("label")] <- paste("","Bunching at multiples of 10",sep="")
count<-count+1

rdests <- rdrobust(y = (censo_ag_wreform$Coffee_Bunch), 
                   x=censo_ag_wreform$norm_dist,c = 0,p = 1,q=2, 
                   bwselect = "mserd", cluster=censo_ag_wreform$Expropretario_ISTA)
rd_estimates[count,c("estimates")] <- rdests$coef[1]
rd_estimates[count,c("ses")] <-  rdests$se[1]
rd_estimates[count,c("y_var")] <- "Coffee"
rd_estimates[count,c("label")] <- paste("","Bunching at multiples of 10",sep="")
count<-count+1

rdests <- rdrobust(y = (censo_ag_wreform$Sugar_Bunch), 
                   x=censo_ag_wreform$norm_dist,c = 0,p = 1,q=2, 
                   bwselect = "mserd", cluster=censo_ag_wreform$Expropretario_ISTA)
rd_estimates[count,c("estimates")] <- rdests$coef[1]
rd_estimates[count,c("ses")] <-  rdests$se[1]
rd_estimates[count,c("y_var")] <- "Sugar Cane"
rd_estimates[count,c("label")] <- paste("","Bunching at multiples of 10",sep="")
count<-count+1

########################################

## Making Standarized Coefficient Plots:

# Set aesthetics:
aesthetics <- list(
  theme_bw(),
  theme(legend.title=element_blank(),
        #legend.justification=c(0,0), 
        #legend.position= "right", #c(1,0),
        #panel.grid.minor=element_blank(),
        #panel.grid.major=element_blank(),
        plot.background=element_rect(colour="black",fill="white"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x=element_text(angle=45, face="bold",hjust=1),
        axis.title.y=element_text(face="bold.italic"),
        axis.title.x=element_text(face="bold.italic")))


########################################


# Clean data for plotting:
alpha<- 0.05
Multiplier <- qnorm(1 - alpha / 2)

# Find the outcome var for each regression:
data <-rd_estimates

# Replace y_var with nice names:

# Now, keep only the betas of interest:
betas <- data
dim(betas)
betas<- betas[seq(dim(betas)[1],1),]

# Create Matrix for plotting:
MatrixofModels <- betas[c("y_var", "estimates","ses","label")]
colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "Group")
MatrixofModels$IV <- factor(MatrixofModels$IV, levels =  c( "Sugar Cane",
                                                            "Coffee",
                                                            "Beans", 
                                                            "Maize"))
MatrixofModels$Group <- factor(MatrixofModels$Group) #levels = c(1,2), labels = c("Local Linear Polynomials","Local Quadratic Polynomials"))


# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL, facets=~ Group)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\n RD Coefficient Estimate (Above 500 ha)") + aesthetics + xlab("")

# Save:
OutputPlot + coord_flip() #+ scale_y_continuous(breaks = seq(-1, 1,0.25))

ggsave(filename="./Output/CoefPlot_Bunching.pdf")

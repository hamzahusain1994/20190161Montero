########################################################
##### ESLR - RD ROBUSNTESS PLOTING - AgCensus Data #####
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

########################################

## Load IV Censo Agropecuario Data:
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

########################################

polys <- c(1,2)
kernels <- c("triangular","epanechnikov","uniform")
bwsel <- c("mserd","cerrd") #"certwo"
num_ests <- length(polys)*(length(kernels) + length(bwsel))
rd_estimates <-data.frame(ln_agprod_estimates = rep(0, num_ests), ln_agprod_ses = rep(0, num_ests),
                          ln_agprodII_estimates = rep(0,num_ests), ln_agprodII_ses = rep(0, num_ests),
                          ln_agprodIII_estimates = rep(0,num_ests), ln_agprodIII_ses = rep(0, num_ests),
                          p = rep(0,num_ests), ks = rep(0,num_ests), bs = rep(0,num_ests), nsl= rep(0,num_ests), nsr= rep(0,num_ests), nslII= rep(0,num_ests), nsrII= rep(0,num_ests), nslIII= rep(0,num_ests), nsrIII= rep(0,num_ests))


years <- 2007
for (i in years) {
  censo_ag_wreform_tev <- mutate(censo_ag_wreform, ln_agprodII = ln_agprod, ln_agprod = ln_agprod_pricew_crops)

  # Estimate and Save RD for configurations:
  
  # Agricultural Productivity:
  count <-1
  for (p in polys) {
    for (k in kernels) {
      for (b in bwsel) {
        # Scale:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod),
                           x=(censo_ag_wreform_tev$norm_dist),
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA))
        rd_estimates[count,c("ln_agprod_estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")  
        rd_estimates[count,c("ln_agprod_ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")  # rdests$se[3]
       
        rd_estimates[count,c("nsl")]<- rdests$N[1]
        rd_estimates[count,c("nsr")]<- rdests$N[2]
        
        # Scale:
         rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprodII),
                           x=censo_ag_wreform_tev$norm_dist,
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA))
        rd_estimates[count,c("ln_agprodII_estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodII") 
        rd_estimates[count,c("ln_agprodII_ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodII") # rdests$se[3]
        
        rd_estimates[count,c("nslII")]<- rdests$N[1]
        rd_estimates[count,c("nsrII")]<- rdests$N[2]
        
        # Scale:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_tfp_geo),
                           x=censo_ag_wreform_tev$norm_dist,
                           c = 0,
                           p = p,
                           q = p +1,
                           kernel = k,
                           bwselect = b,
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA))
        rd_estimates[count,c("ln_agprodIII_estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_tfp_geo") 
        rd_estimates[count,c("ln_agprodIII_ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_tfp_geo") # rdests$se[3]

        rd_estimates[count,c("nslIII")]<- rdests$N[1]
        rd_estimates[count,c("nsrIII")]<- rdests$N[2]

        rd_estimates[count,c("p")] <- p
        rd_estimates[count,c("ks")] <- k
        rd_estimates[count,c("bs")] <- b
        count <- count + 1
      }
    }
  }
   }
rd_estimates

########################################

# Clean data for plotting:
alpha<- 0.05
Multiplier <- qnorm(1 - alpha / 2)

# Find the outcome var for each regression:
data <- rd_estimates
data$y_var <- paste(data$ks, " kernel, ", data$bs," bandwidth",sep="")

# Replace y_var with nice names:

# Now, keep only the betas of interest:
betas <- data
dim(betas)
betas<- betas[seq(dim(betas)[1],1),]

# Create Matrix for plotting:
MatrixofModels <- betas[c("y_var", "ln_agprod_estimates","ln_agprod_ses","p")]
colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "Polynomial")
MatrixofModels$IV <- factor(MatrixofModels$IV, levels = unique(MatrixofModels$IV))
MatrixofModels$Polynomial <- factor(MatrixofModels$Polynomial,levels = c(1,2), labels = c("Local Linear Polynomials","Local Quadratic Polynomials"))

# Re-name for plotting:
MatrixofModels$ModelName <- "Revenue Per Hectare"

# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL, facets=~ Polynomial)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics 

# Save:
OutputPlot + coord_flip() + scale_y_continuous(breaks = seq(-0.5, 0.5,0.1)) + xlab("")

ggsave(filename="./Output/CoefPlot_AgProdI_Robustness.pdf", width=6, height=3)

# Create Matrix for plotting:
MatrixofModels <- betas[c("y_var", "ln_agprodII_estimates","ln_agprodII_ses","p")]
colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "Polynomial")
MatrixofModels$IV <- factor(MatrixofModels$IV, levels = unique(MatrixofModels$IV))
MatrixofModels$Polynomial <- factor(MatrixofModels$Polynomial,levels = c(1,2), labels = c("Local Linear Polynomials","Local Quadratic Polynomials"))

# Re-name for plotting:
MatrixofModels$ModelName <- "Profits Per Hectare"

# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL, facets=~ Polynomial)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics 

# Save:
OutputPlot + coord_flip() + scale_y_continuous(breaks = seq(-0.5, 0.5,0.1)) + xlab("")

ggsave(filename="./Output/CoefPlot_AgProdII_Robustness.pdf", width=6, height=3)


# # Create Matrix for plotting:
MatrixofModels <- betas[c("y_var", "ln_agprodIII_estimates","ln_agprodIII_ses","p")]
colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "Polynomial")
MatrixofModels$IV <- factor(MatrixofModels$IV, levels = unique(MatrixofModels$IV))
MatrixofModels$Polynomial <- factor(MatrixofModels$Polynomial,levels = c(1,2), labels = c("Local Linear Polynomials","Local Quadratic Polynomials"))

# Re-name for plotting:
MatrixofModels$ModelName <- "Farm Productivity"

# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL, facets=~ Polynomial)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics

# Save:
OutputPlot + coord_flip() + scale_y_continuous(breaks = seq(-0.5, 0.5,0.1)) + xlab("")

ggsave(filename="./Output/CoefPlot_AgProdIII_Robustness.pdf", width=6, height=3)

########################################

## Varying BW Manually:

## Calculate log ag productivity for 2007, and save RD estimates using different bandwidths and polynomials:

polys <- c(1,2)
bws <- seq(40,300, by=20)

num_ests <- length(polys)*(length(bws))
rd_estimates <-data.frame(ln_agprod_estimates = rep(0, num_ests), ln_agprod_ses = rep(0, num_ests),
                          ln_agprodII_estimates = rep(0,num_ests), ln_agprodII_ses = rep(0, num_ests),
                          ln_agprodIII_estimates = rep(0,num_ests), ln_agprodIII_ses = rep(0, num_ests),
                          p = rep(0,num_ests), bs = rep(0,num_ests))

# Create Variables:
i <- 2007
censo_ag_wreform_tev <- mutate(censo_ag_wreform, ln_agprodII = ln_agprod, ln_agprod = ln_agprod_pricew_crops, ln_agprodIII = ln_tfp_geo)

count <-1
for (b in bws) {
  # Estimate and Save RD for manual bws:
  # Agricultural Productivity:
  for (p in polys) {
        # Scale:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod),
                           x=(censo_ag_wreform_tev$norm_dist),
                           c = 0,
                           p = p, 
                           kernel = "tri",
                           h=b,
                           bwselect="mserd",
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA))
        
        rd_estimates[count,c("ln_agprod_estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")  
        rd_estimates[count,c("ln_agprod_ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")  # rdests$se[3]
        
        # Scale:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprodII),
                           x=censo_ag_wreform_tev$norm_dist,
                           c = 0,
                           p = p, 
                           kernel = "tri",
                           h=b,
                           bwselect="mserd",
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA))
        rd_estimates[count,c("ln_agprodII_estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodII") 
        rd_estimates[count,c("ln_agprodII_ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodII") # rdests$se[3]

        rd_estimates[count,c("bs")] <- b
        rd_estimates[count,c("p")] <- p
        
        # Scale:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprodIII),
                           x=censo_ag_wreform_tev$norm_dist,
                           c = 0,
                           p = p,
                           kernel = "tri",
                           h=b,
                           bwselect="mserd",
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA))
        rd_estimates[count,c("ln_agprodIII_estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodIII") 
        rd_estimates[count,c("ln_agprodIII_ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodIII") # rdests$se[3]

        rd_estimates[count,c("bs")] <- b
        rd_estimates[count,c("p")] <- p
        
        count <- count + 1
      }
}
rd_estimates

########################################

# Clean data for plotting:
alpha<- 0.05
Multiplier <- qnorm(1 - alpha / 2)

# Find the outcome var for each regression:
data <- rd_estimates
data$y_var <- paste(" Bandwidth: ",data$bs, sep="")

# Now, keep only the betas of interest:
betas <- data
dim(betas)

# Create Matrix for plotting:
MatrixofModels <- betas[c("y_var", "ln_agprod_estimates","ln_agprod_ses","p")]
colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "Polynomial")
MatrixofModels$IV <- factor(MatrixofModels$IV, levels = unique(MatrixofModels$IV))
MatrixofModels$Polynomial <- factor(MatrixofModels$Polynomial,levels = c(1,2), labels = c("Local Linear Polynomials","Local Quadratic Polynomials"))


# Re-name for plotting:
MatrixofModels$ModelName <- "Revenue Per Hectare"

# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL, facets=~ Polynomial)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics 

# Save:
OutputPlot + coord_flip() + coord_cartesian(ylim = c(-1, 1)) + scale_y_continuous(breaks = seq(-1, 1,0.2))

ggsave(filename="./Output/CoefPlot_AgProdI_BWRobustness.pdf", width=6, height=3)

# Create Matrix for plotting:
MatrixofModels <- betas[c("y_var", "ln_agprodII_estimates","ln_agprodII_ses","p")]
colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "Polynomial")
MatrixofModels$IV <- factor(MatrixofModels$IV, levels = unique(MatrixofModels$IV))
MatrixofModels$Polynomial <- factor(MatrixofModels$Polynomial,levels = c(1,2), labels = c("Local Linear Polynomials","Local Quadratic Polynomials"))

# Re-name for plotting:
MatrixofModels$ModelName <- "Profits Per Hectare"

# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL, facets=~ Polynomial)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics 

# Save:
OutputPlot + coord_flip() + coord_cartesian(ylim = c(-1, 1)) + scale_y_continuous(breaks = seq(-1, 1,0.2))

ggsave(filename="./Output/CoefPlot_AgProdII_BWRobustness.pdf", width=6, height=3)


# Create Matrix for plotting:
MatrixofModels <- betas[c("y_var", "ln_agprodIII_estimates","ln_agprodIII_ses","p")]
colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "Polynomial")
MatrixofModels$IV <- factor(MatrixofModels$IV, levels = unique(MatrixofModels$IV))
MatrixofModels$Polynomial <- factor(MatrixofModels$Polynomial,levels = c(1,2), labels = c("Local Linear Polynomials","Local Quadratic Polynomials"))

# Re-name for plotting:
MatrixofModels$ModelName <- "Farm Productivity"

# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL, facets=~ Polynomial)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics

# Save:
OutputPlot + coord_flip() + coord_cartesian(ylim = c(-1, 1)) + scale_y_continuous(breaks = seq(-1, 1,0.2))

ggsave(filename="./Output/CoefPlot_AgProdIII_BWRobustness.pdf", width=6, height=3)
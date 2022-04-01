#######################################################
##### ESLR - COEFICIENT PLOTTING - HH SURVEY DATA #####
############# COEF PLOTS OF PG OUTCOMES ###############
#######################################################

rm(list = ls())       # Clear variables
require(foreign)
require(ggplot2)
require(RColorBrewer) # creates nice color schemes
require(scales)       # customize scales
require(plyr)         # join function
require(dplyr) 
require(tidyr)    
require(extrafont)

########################################

## Note: This file reads in the coefficient output 
## and plots the coefficient estimates for the PG outcomes

########################################

# Set aesthetics:
aesthetics <- list(#guides(color=guide_colorbar(reverse=FALSE)),
  #guides(fill=FALSE),
  #guides(shape=FALSE),
  #guides(size=FALSE),
  #coord_equal(),
  theme_bw(),
  theme(#text=element_text(family="Palatino"),
        legend.title=element_blank(),
        #legend.justification=c(0,0), 
        #legend.position= "right", #c(1,0),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        #plot.background=element_rect(colour="white",fill=white),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        axis.text.y=element_text(face="bold"),
        axis.title.y=element_text(face="bold")))
#axis.text=element_blank(),
#axis.ticks=element_blank(),
#panel.border = element_blank()))

Multiplier <- 1.96

########################################

    # Read in parmests:
    ests <- read.csv(file =  "./Output/Parmest_EHPM_PGs.csv") 
    # Note, using the 300 ha bandwidth
    
    ########################################
    
    # Keep only coeffients of interest:
    
    ests <- filter(ests, parm == "std_Above500")
    ests$label <- as.character(ests$label)
    #ests <- ests[dim(ests)[1]:1,]
    ests$idstr <- c("Bank or Credit Association","Public Phone","Internet",
                    "Bus Stop", "Park and/or\nSoccer Field",
                    "Post Office", "Market", "Health Center",
                    "Police Station", "Paved Road")
    
    ########################################
    
    
    # Create Matrix for plotting:
    MatrixofModels <- ests[c("idstr", "estimate","stderr","t","p")]
    colnames(MatrixofModels) <- c("Dependent Variable", "Estimate", "StandardError", "TValue", "PValue")
    MatrixofModels$`Dependent Variable` <- factor(MatrixofModels$`Dependent Variable`, levels = MatrixofModels$`Dependent Variable`)
    #MatrixofModels$Legend <- c( "  PCA Coefficient", rep("  Component Coefficients",dim(MatrixofModels)[1]-1))
    
    # Plot:
    OutputPlot <- qplot(`Dependent Variable`, Estimate, ymin = Estimate - Multiplier * StandardError,
                        ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                        ylab = NULL, xlab = NULL)
    
    OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
    x_title <-expression(atop(bold("Dependent Variable "),italic("\n(Time to the Neearest)")))

    OutputPlot <- OutputPlot + theme_bw() + ylab("Estimated Effect: Above 500 ha") + 
      aesthetics + xlab(x_title) + coord_flip()
  
    OutputPlot
    
    ggsave(filename="./Output/CoefPlot_PGDistance.pdf")
    
 
############################################################
##### ESLR - RD HETEROGENEITY PLOTTING - AgCensus Data #####
############################################################

rm(list = ls())       # Clear variables

require(foreign)
require(ggplot2)
require(RColorBrewer) # creates nice color schemes
require(scales)       # customize scales
require(plyr)         # join function
require(dplyr) 
require(rdrobust)     # rd estimation tools
require(haven)
require(readstata13)

########################################

## Load IV Censo Agropecuario Data:
censo_ag_wreform <- read.dta13(file="Data/censo_ag_wreform.dta")

########################################

## Making Standarized Coefficient Plots:

# Set aesthetics:
aesthetics <- list(
  theme_bw(),
  theme(text=element_text(family="Palatino"),
        legend.title=element_blank(),
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



lm.beta <- function (MOD, dta,y="ln_agprod") 
{
  b <- MOD$coef[1]
  model.dta <- filter(dta, norm_dist > -1*MOD$bws[1,"left"] & norm_dist < MOD$bws[1,"right"] )
  sx <- sd(model.dta[,c("Above500")])
  #sx <- sd(model.dta[,c("norm_dist")])
  sy <- sd((model.dta[,c(y)]),na.rm=TRUE)
  beta <- b * sx/sy
  return(beta)
}
lm.beta.ses <- function (MOD, dta,y="ln_agprod") 
{
  b <- MOD$se[1]
  model.dta <- filter(dta, norm_dist > -1*MOD$bws[1,"left"] & norm_dist < MOD$bws[1,"right"] )
  sx <- sd(model.dta[,c("Above500")])
  #sx <- sd(model.dta[,c("norm_dist")])
  sy <- sd((model.dta[,c(y)]),na.rm=TRUE)
  beta <- b * sx/sy
  return(beta)
}


########################################

num_ests <- 2*4
rd_estimates <-data.frame(ln_agprod_estimates = rep(0, num_ests), ln_agprod_ses = rep(0, num_ests),
                          ln_agprodII_estimates = rep(0,num_ests), ln_agprodII_ses = rep(0, num_ests),
                          p = rep(0,num_ests), ks = rep(0,num_ests), bs = rep(0,num_ests), nsl= rep(0,num_ests), nsr= rep(0,num_ests), nslII= rep(0,num_ests), nsrII= rep(0,num_ests), nslIII= rep(0,num_ests), nsrIII= rep(0,num_ests))
censo_ag_wreform_tev <-censo_ag_wreform 
ag.grouped <- group_by(censo_ag_wreform_tev,Expropretario_ISTA)
ag.grouped <- mutate(ag.grouped, num_per_owner = n())
censo_ag_wreform_tev$num_per_owner<- ag.grouped$num_per_owner

k <- "triangular"
p <- 1
b<- "mserd"
years <- 2007
  i = 2007
  
  
  # Estimate and Save RD for configurations:
  
  # Agricultural Productivity:
        count<-1
        # Scale:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod_pricew_crops),
                           x=censo_ag_wreform_tev$norm_dist,
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                           subset= censo_ag_wreform_tev$num_per_owner == 1)
        rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod_pricew_crops")
        rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod_pricew_crops")
        rd_estimates[count,c("y_var")] <- "Revenue per ha"
        rd_estimates[count,c("label")] <- paste("","1 Prop per owner",sep="")
        count<-count+1
        
        rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod_pricew_crops),
                           x=censo_ag_wreform_tev$norm_dist,
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                           subset= censo_ag_wreform_tev$num_per_owner != 1)
        rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod_pricew_crops")
        rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod_pricew_crops")
        rd_estimates[count,c("y_var")] <- "Revenue per ha"
        rd_estimates[count,c("label")] <- paste("",">1 Prop per owner",sep="")
        count<-count+1
        
        rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod),
                           x=censo_ag_wreform_tev$norm_dist,
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                           subset= censo_ag_wreform_tev$num_per_owner == 1)
        rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
        rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
        rd_estimates[count,c("y_var")] <- "Profits per ha"
        rd_estimates[count,c("label")] <- paste("","1 Prop per owner",sep="")
        count<-count+1
        
        rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod),
                           x=censo_ag_wreform_tev$norm_dist,
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                           subset= censo_ag_wreform_tev$num_per_owner >1)
        rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
        rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
        rd_estimates[count,c("y_var")] <- "Profits per ha"
        rd_estimates[count,c("label")] <- paste("",">1 Prop per owner",sep="")
        count<-count+1
        
       
        # Share Cash:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$CashCrop_Share),
                           x=censo_ag_wreform_tev$norm_dist,
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                           subset= censo_ag_wreform_tev$num_per_owner == 1)
        rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
        rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
        rd_estimates[count,c("y_var")] <- "Cash Crop Share"
        rd_estimates[count,c("label")] <- paste("","1 Prop per owner",sep="")
        count<-count+1
        
        rdests <- rdrobust(y = (censo_ag_wreform_tev$CashCrop_Share),
                           x=censo_ag_wreform_tev$norm_dist,
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                           subset= censo_ag_wreform_tev$num_per_owner >1)
        rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
        rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
        rd_estimates[count,c("y_var")] <- "Cash Crop Share"
        rd_estimates[count,c("label")] <- paste("",">1 Prop per owner",sep="")
        count<-count+1
        
        # Share Staple:
        rdests <- rdrobust(y = (censo_ag_wreform_tev$StapleCrop_Share),
                           x=censo_ag_wreform_tev$norm_dist,
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                           subset= censo_ag_wreform_tev$num_per_owner == 1)
        rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
        rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
        rd_estimates[count,c("y_var")] <- "Staple Crop Share"
        rd_estimates[count,c("label")] <- paste("","1 Prop per owner",sep="")
        count<-count+1
        
        rdests <- rdrobust(y = (censo_ag_wreform_tev$StapleCrop_Share),
                           x=censo_ag_wreform_tev$norm_dist,
                           c = 0,
                           p = p, 
                           q = p +1,
                           kernel = k,
                           bwselect = b, 
                           cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                           subset= censo_ag_wreform_tev$num_per_owner >1)
        rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
        rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
        rd_estimates[count,c("y_var")] <- "Staple Crop Share"
        rd_estimates[count,c("label")] <- paste("",">1 Prop per owner",sep="")
        count<-count+1
        

rd_estimates

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
MatrixofModels$IV <- factor(MatrixofModels$IV, levels = unique(MatrixofModels$IV))
MatrixofModels$Group <- factor(MatrixofModels$Group) #levels = c(1,2), labels = c("Local Linear Polynomials","Local Quadratic Polynomials"))


# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL, facets=~ Group)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics  + xlab("")

# Save:
OutputPlot + coord_flip() + scale_y_continuous(breaks =scales::pretty_breaks(n = 10))

ggsave(filename="./Output/CoefPlot_Het_NumPerOwner.pdf", width=6, height=3)

########################################

## Het by Distance to Urban Centers:

canton_covs <- read_dta("./Data/cantons_dists.dta")
canton_covs <- canton_covs %>%
  mutate(CODIGO = (as_factor(COD_CTON)))

canton_covs <- canton_covs %>%
  mutate(CODIGO = gsub("(?<![0-9])0+", "", CODIGO, perl = TRUE)) %>%
  mutate(CODIGO = as.numeric(CODIGO)) %>% 
  dplyr::select(CODIGO,dist_ES_capital, dist_dept_capitals)

censo_ag_wreform_tev <- left_join(censo_ag_wreform_tev,canton_covs, by="CODIGO")

num_ests <- 2*8
rd_estimates <-data.frame(ln_agprod_estimates = rep(0, num_ests), ln_agprod_ses = rep(0, num_ests),
                          ln_agprodII_estimates = rep(0,num_ests), ln_agprodII_ses = rep(0, num_ests),
                          p = rep(0,num_ests), ks = rep(0,num_ests), bs = rep(0,num_ests), nsl= rep(0,num_ests), nsr= rep(0,num_ests), nslII= rep(0,num_ests), nsrII= rep(0,num_ests), nslIII= rep(0,num_ests), nsrIII= rep(0,num_ests))


k <- "tri"
p <- 1
b<- "mserd"
years <- 2007
i = 2007

censo_ag_wreform_tev <- censo_ag_wreform_tev %>% 
  mutate(Close_ES_Capital = ifelse(dist_ES_capital < 50000,1,0), 
         Close_Dept_Capitals = ifelse(dist_dept_capitals < 10000,1,0)) 


count<-1
  # Scale:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod_pricew_crops),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_ES_Capital == 1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod_pricew_crops")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod_pricew_crops")
  rd_estimates[count,c("y_var")] <- "Revenue per ha"
  rd_estimates[count,c("label")] <- paste("","Close to: Country Capital",sep="")
  count<-count+1
  
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod_pricew_crops),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_ES_Capital != 1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod_pricew_crops")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod_pricew_crops")
  rd_estimates[count,c("y_var")] <- "Revenue per ha"
  rd_estimates[count,c("label")] <- paste("","Not Close to: Country Capital",sep="")
  count<-count+1
  
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_ES_Capital == 1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
  rd_estimates[count,c("y_var")] <- "Profits per ha"
  rd_estimates[count,c("label")] <- paste("","Close to: Country Capital",sep="")
  count<-count+1
  
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p + 1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_ES_Capital !=1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
  rd_estimates[count,c("y_var")] <- "Profits per ha"
  rd_estimates[count,c("label")] <- paste("","Not Close to: Country Capital",sep="")
  count<-count+1

  
  # Share Cash:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$CashCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_ES_Capital == 1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
  rd_estimates[count,c("y_var")] <- "Cash Crop Share"
  rd_estimates[count,c("label")] <- paste("","Close to: Country Capital",sep="")
  count<-count+1
  
  rdests <- rdrobust(y = (censo_ag_wreform_tev$CashCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_ES_Capital !=1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
  rd_estimates[count,c("y_var")] <- "Cash Crop Share"
  rd_estimates[count,c("label")] <- paste("","Not Close to: Country Capital",sep="")
  count<-count+1
  
  # Share Staple:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$StapleCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_ES_Capital == 1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
  rd_estimates[count,c("y_var")] <- "Staple Crop Share"
  rd_estimates[count,c("label")] <- paste("","Close to: Country Capital",sep="")
  count<-count+1
  
  rdests <- rdrobust(y = (censo_ag_wreform_tev$StapleCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_ES_Capital != 1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
  rd_estimates[count,c("y_var")] <- "Staple Crop Share"
  rd_estimates[count,c("label")] <- paste("","Not Close to: Country Capital",sep="")
  count<-count+1
  
  # Department Capitals
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod_pricew_crops),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_Dept_Capital == 1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod_pricew_crops")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod_pricew_crops")
  rd_estimates[count,c("y_var")] <- "Revenue per ha"
  rd_estimates[count,c("label")] <- paste("","Close to: Department Capital",sep="")
  count<-count+1
  
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod_pricew_crops),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_Dept_Capital != 1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod_pricew_crops")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod_pricew_crops")
  rd_estimates[count,c("y_var")] <- "Revenue per ha"
  rd_estimates[count,c("label")] <- paste("","Not Close to: Department Capital",sep="")
  count<-count+1
  
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_Dept_Capital == 1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
  rd_estimates[count,c("y_var")] <- "Profits per ha"
  rd_estimates[count,c("label")] <- paste("","Close to: Department Capital",sep="")
  count<-count+1
  
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_Dept_Capital !=1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
  rd_estimates[count,c("y_var")] <- "Profits per ha"
  rd_estimates[count,c("label")] <- paste("","Not Close to: Department Capital",sep="")
  count<-count+1
  
  # Share Cash:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$CashCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_Dept_Capital == 1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
  rd_estimates[count,c("y_var")] <- "Cash Crop Share"
  rd_estimates[count,c("label")] <- paste("","Close to: Department Capital",sep="")
  count<-count+1
  
  rdests <- rdrobust(y = (censo_ag_wreform_tev$CashCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_Dept_Capital !=1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
  rd_estimates[count,c("y_var")] <- "Cash Crop Share"
  rd_estimates[count,c("label")] <- paste("","Not Close to: Department Capital",sep="")
  count<-count+1
  
  # Share Staple:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$StapleCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_Dept_Capital == 1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
  rd_estimates[count,c("y_var")] <- "Staple Crop Share"
  rd_estimates[count,c("label")] <- paste("","Close to: Department Capital",sep="")
  count<-count+1
  
  rdests <- rdrobust(y = (censo_ag_wreform_tev$StapleCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1",
                     subset= censo_ag_wreform_tev$Close_Dept_Capital != 1 | censo_ag_wreform_tev$reform==0)
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
  rd_estimates[count,c("y_var")] <- "Staple Crop Share"
  rd_estimates[count,c("label")] <- paste("","Not Close to: Department Capital",sep="")
  count<-count+1


rd_estimates

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
MatrixofModels$IV <- factor(MatrixofModels$IV, levels = unique(MatrixofModels$IV))
MatrixofModels$Group <- factor(MatrixofModels$Group) #levels = c(1,2), labels = c("Local Linear Polynomials","Local Quadratic Polynomials"))


# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL, facets=~ Group)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics  + xlab("")

# Save:
OutputPlot + coord_flip() + scale_y_continuous(breaks =scales::pretty_breaks(n = 10))

ggsave(filename="./Output/CoefPlot_Het_DistCapital.pdf")


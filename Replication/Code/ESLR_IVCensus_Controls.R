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
require(sandwich)     # robust se's
require(haven)
require(fuzzyjoin)

########################################


## Load IV Censo Agropecuario Data:
censo_ag_wreform <- read.dta13(file="Data/censo_ag_wreform.dta")

# Laod Conflict Data: 
conflict_data <- read.csv(file="./Data/conflict_canton.csv", header=TRUE)
censo_ag_wreform <- left_join(censo_ag_wreform,conflict_data, by="CODIGO")

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

censo_ag_wreform_tev <- censo_ag_wreform
ag.grouped <- mutate(censo_ag_wreform_tev %>% group_by(Expropretario_ISTA), num_per_owner = n())
censo_ag_wreform_tev$num_per_owner<- ag.grouped$num_per_owner

years <- 2007
i = 2007
censo_ag_wreform_tev <- mutate(censo_ag_wreform_tev,  
                               ln_agprodII = ln_agprod,
                               ln_agprod = ln_agprod_pricew_crops)



###########################################

## CONTROLLING FOR PROPERTY SIZES:
# Estimate and Save RD for different controls:
num_ests <- 3*4
rd_estimates <-data.frame(estimates = rep(0, num_ests), ses = rep(0, num_ests),
                          y_var = rep(0,num_ests),
                          label = rep(0, num_ests))

k <- "triangular" 
p <- 1
b<- "mserd" 

controls <- c("AREA_HECTAREA", "Area_has")
count<-1

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


controls <- list("AREA_HECTAREA","Area_has",c("Area_has","AREA_HECTAREA"))
labels <- c("Property Size in 1980", "Property Size in 2007", "All Controls")
label.count <- 1
for (i in controls) {
  print(i)
  
  # Revenue per ha:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod") 
  rd_estimates[count,c("y_var")] <- "Revenue per ha"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  
  # Profits per ha:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprodII),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodII")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodII") 
  rd_estimates[count,c("y_var")] <- "Profit per ha"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  
  # Share Cash:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$CashCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share") 
  rd_estimates[count,c("y_var")] <- "Cash Crop Share"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  
  
  # Share Staple:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$StapleCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share") 
  rd_estimates[count,c("y_var")] <- "Staple Crop Share"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  label.count<-label.count+1
}


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
  c <- factor(MatrixofModels$Group, levels = c("Controlling for: Property Size in 1980", 
                                                                  "Controlling for: Property Size in 2007", 
                                                                  "Controlling for: All Controls"))
  
  
  # Plot:
  OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                      ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                      ylab = NULL, xlab = NULL, facets=~ Group)
  OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
  OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics  + xlab("")
  
  # Save:
  OutputPlot + coord_flip() + scale_y_continuous(breaks = seq(-1, 1,0.25)) +  theme(strip.text.x = element_text(size = 5))
  
  ggsave(filename="./Output/CoefPlot_wSizeControls.pdf", width=6, height=3)

########################################

## Conflict Types:

# Estimate and Save RD for different types of conflict:
num_ests <- 4*4
rd_estimates <-data.frame(estimates = rep(0, num_ests), ses = rep(0, num_ests),
                          y_var = rep(0,num_ests),
                          label = rep(0, num_ests))

k <- "triangular" 
p <- 1
b<- "mserd"

count<-1
censo_ag_wreform_tev <- censo_ag_wreform_tev %>% 
  mutate(Conflict1980 = ifelse(!is.na(Conflict_1980),Conflict_1980,0),
         Conflict1981 =  ifelse(!is.na(Conflict_1981),Conflict_1981,0),
         Conflict1982 =  ifelse(!is.na(Conflict_1982),Conflict_1982,0),
         Conflict198082 = Conflict1980+Conflict1981+Conflict1982)
controls <- list("CONFLICT","FFAA","ESCUAD","Conflict198082")
labels <- c("Conflict (Any Actor)", "Military Violence", "Death Squad Violence", "Conflict from 1980-1982")
label.count <- 1
for (i in controls) {
  print(i)
  
  # Revenue per ha:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod") 
  rd_estimates[count,c("y_var")] <- "Revenue per ha"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  
  # Profits per ha:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprodII),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodII")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodII") 
  rd_estimates[count,c("y_var")] <- "Profit per ha"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  
  # Share Cash:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$CashCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share") 
  rd_estimates[count,c("y_var")] <- "Cash Crop Share"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  
  
  # Share Staple:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$StapleCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share") 
  rd_estimates[count,c("y_var")] <- "Staple Crop Share"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  label.count<-label.count+1
}


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
MatrixofModels$IV <- factor(MatrixofModels$IV, levels =  unique(MatrixofModels$IV))
MatrixofModels$Group <- factor(MatrixofModels$Group, levels = paste0("Controlling for: ",labels))

# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL, facets=~ Group)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics 

# Save:
OutputPlot + coord_flip() + scale_y_continuous(breaks = seq(-1, 1,0.25)) + xlab("")

ggsave(filename="./Output/CoefPlot_wConflictTypeControls.pdf")

###########################################


## CONTROLLING FOR COMMERCIALIZATION AVENUE

commerc <- read.dta13(file = "./Data/censo_ag_commercialization.dta")
censo_ag_wreform_tev <- left_join(censo_ag_wreform_tev,commerc, by="agg_id")

num_ests <- 4*4
rd_estimates <-data.frame(estimates = rep(0, num_ests), ses = rep(0, num_ests),
                          y_var = rep(0,num_ests),
                          label = rep(0, num_ests))

k <- "triangular" 
p <- 1
b<- "mserd" 

count<-1

controls <- list("MAYO", "MINO", "OTRO", c("MAYO", "MINO", "OTRO")) # Can't control for exporter, not enough
labels <- c("Wholeseller", "Retailer", "Exporting", "All Controls")
label.count <- 1
for (i in controls) {
  print(i)
  
  # Revenue per ha:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod") 
  rd_estimates[count,c("y_var")] <- "Revenue per ha"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  
  # Profits per ha:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprodII),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodII")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodII") 
  rd_estimates[count,c("y_var")] <- "Profit per ha"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  
  # Share Cash:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$CashCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share") 
  rd_estimates[count,c("y_var")] <- "Cash Crop Share"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  
  
  # Share Staple:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$StapleCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share") 
  rd_estimates[count,c("y_var")] <- "Staple Crop Share"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  label.count<-label.count+1
}


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
MatrixofModels$IV <- factor(MatrixofModels$IV, levels =  unique(MatrixofModels$IV))
MatrixofModels$Group <- factor(MatrixofModels$Group, levels = paste0("Controlling for: ", labels))

# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL, facets=~ Group)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics 

# Save:
OutputPlot + coord_flip() + scale_y_continuous(breaks = seq(-1, 1,0.25)) + xlab("")

ggsave(filename="./Output/CoefPlot_wCommercialization.pdf")











###########################################

## CONTROLLING FOR CANTON MIGRATION AMOUNTS:

# Prep data:
  poblaccion_section <- read_sav(file = "./Data/poblacion.sav")
  
  cantons_popcensus <- dplyr::select(poblaccion_section,
                                     gender=S06P02,
                                     age=S06P03A,
                                     S06P07A, S06P08A1, S06P08A2,
                                     DEPDSC, MUNDSC, CANDSC,
                                     literate = S06P09,
                                     educated = S06P10,
                                     educ_level = S06P11A,
                                     finished_hs = S06P11B)
  
  cantons_popcensus <- mutate(cantons_popcensus,
                              born_same_as_mother= ifelse(S06P07A < 3,S06P07A,NA) ,
                              lived_canton_always = ifelse(S06P08A1 < 3,S06P08A1,NA),
                              lived_canton_year = ifelse(S06P08A2>0,S06P08A2,NA),
                              CODIGO_NOM  = toupper(stri_trans_general(paste(DEPDSC, MUNDSC, CANDSC,sep=", "),"Latin-ASCII")))
  
  cantons_popcensus <- mutate(cantons_popcensus,
                              born_same_as_mother= ifelse(born_same_as_mother ==2 ,0,born_same_as_mother) ,
                              lived_canton_always = ifelse(lived_canton_always ==2 ,0,lived_canton_always),
                              educ_yrs = 1*(educ_level==1)+6*(educ_level==2)+ 9*(educ_level==3)+
                                11*(educ_level==4)+13*(educ_level==5)+ 15*(educ_level==6)+
                                16*(educ_level==7)+ 17*(educ_level==8)+ 20*(educ_level==9))
  
  # Summarise to make merging faster:
  cantons_popcensus <- cantons_popcensus %>%
    group_by(CODIGO_NOM) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)

  # Merge data:
  max.dist <- 10 # since there are errors in mun names + state names
  censo_ag_wreform_tev <- stringdist_join(as.data.frame(censo_ag_wreform_tev), 
                                          as.data.frame(cantons_popcensus), 
                             by = c("CODIGO_NOM.x" = "CODIGO_NOM"), 
                             mode = "left", 
                             method = "jw", 
                             max_dist = max.dist, 
                             distance_col = "dist")

  censo_ag_wreform_tev <- censo_ag_wreform_tev %>%
  group_by(agg_id) %>%
  top_n(1, -dist) %>% ungroup() 
  
  censo_ag_wreform_tev <- as.data.frame(censo_ag_wreform_tev)

# Estimate and Save RD for different controls:
num_ests <- 4*4
rd_estimates <-data.frame(estimates = rep(0, num_ests), ses = rep(0, num_ests),
                          y_var = rep(0,num_ests),
                          label = rep(0, num_ests))

k <- "triangular" 
p <- 1
b<- "mserd"

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


count<-1
controls <- list("lived_canton_always", "born_same_as_mother","lived_canton_year",
                 c("born_same_as_mother","lived_canton_always","lived_canton_year"))
labels <- c("% Always Lived in Canton", "% Born in Mother's Canton", "Avg. Years in Canton","All Controls")
label.count <- 1
for (i in controls) {
  print(i)
  
  # Revenue per ha:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprod),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprod") 
  rd_estimates[count,c("y_var")] <- "Revenue per ha"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  
  # Profits per ha:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$ln_agprodII),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodII")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="ln_agprodII") 
  rd_estimates[count,c("y_var")] <- "Profit per ha"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  
  # Share Cash:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$CashCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share") 
  rd_estimates[count,c("y_var")] <- "Cash Crop Share"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  
  
  # Share Staple:
  rdests <- rdrobust(y = (censo_ag_wreform_tev$StapleCrop_Share),
                     x=censo_ag_wreform_tev$norm_dist,
                     covs = censo_ag_wreform_tev[,i],
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA),vce="hc1")
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share")
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="StapleCrop_Share") 
  rd_estimates[count,c("y_var")] <- "Staple Crop Share"
  rd_estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  label.count<-label.count+1
}


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
MatrixofModels$Group <- factor(MatrixofModels$Group, levels = paste0("Controlling for: ", labels))


# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL, facets=~ Group)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics 

# Save:
OutputPlot + coord_flip() + scale_y_continuous(breaks = seq(-1, 1,0.25)) + xlab("")

ggsave(filename="./Output/CoefPlot_wMigrationControls.pdf")





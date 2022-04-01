########################################
require(lfe)
## Crop choices for non-compliers vs control

num_ests <- 1*2
estimates <-data.frame(estimates = rep(0, num_ests), ses = rep(0, num_ests),
                       y_var = rep(0,num_ests),
                       label = rep(0, num_ests))

lm.beta.ses2 <- function (MOD, dta,y="ln_agprod") 
{
  b <- MOD$se[2] #   b <- MOD$se[3]
  sx <- sd(dta[,c("non_comp")],na.rm=TRUE)
  #sx <- sd(model.dta[,c("norm_dist")])
  sy <- sd((dta[,c(y)]),na.rm=TRUE)
  beta <- b * sx/sy
  return(beta)
}

lm.beta2 <- function (MOD, dta,y="ln_agprod") 
{
  b <- MOD$coef[2]
  sx <- sd(dta[,c("non_comp")],na.rm=TRUE)
  #sx <- sd(model.dta[,c("norm_dist")])
  sy <- sd((dta[,c(y)]),na.rm=TRUE)
  print(sx)
  beta <- b * sx/sy
  return(beta)
}

censo_ag_wreform <- read.dta13(file="Data/censo_ag_wreform.dta")
censo_ag_wreform_tev <- censo_ag_wreform %>%
  mutate(non_comp = ifelse(reform == 0 & Above500==1,1,0)) %>% 
  filter(reform!=1)

controls <- 1
count<-1
for (i in controls) {
  print(i)
  
  
  # Share Cash:
  rdests <- felm(CashCrop_Share ~
                   non_comp 
                 | 0 | 0 | Expropretario_ISTA, 
                 data = censo_ag_wreform_tev,
                 subset = (reform==0 & AREA_HECTAREA >  350))
  
  estimates[count,c("estimates")] <-lm.beta2(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
  estimates[count,c("ses")] <- lm.beta.ses2(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share") 
  estimates[count,c("y_var")] <- "Cash Crop Share"
  #estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  count<-count+1
  
  
  # Share Staple:
  rdests <- felm(StapleCrop_Share ~
                   non_comp 
                 | 0 | 0 | Expropretario_ISTA, 
                 data = censo_ag_wreform_tev,
                 subset = (reform==0 & AREA_HECTAREA >  350))
  
  estimates[count,c("estimates")] <-lm.beta2(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share")
  estimates[count,c("ses")] <- lm.beta.ses2(MOD=rdests, dta=censo_ag_wreform_tev, y="CashCrop_Share") 
  estimates[count,c("y_var")] <- "Staple Crop Share"
  #estimates[count,c("label")] <- paste("Controlling for: ",labels[label.count],sep="")
  
  # Suitabilities?
  
  count<-count+1
  label.count<-label.count+1
}


estimates

########################################

# Clean data for plotting:
alpha<- 0.05
Multiplier <- qnorm(1 - alpha / 2)

# Find the outcome var for each regression:
data <-estimates

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
                    ylab = NULL, xlab = NULL)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
# Stupid fix to fix the scales overlapping on the bottom:
OutputPlot <- OutputPlot + geom_hline(yintercept = 0.02, alpha = 0.05)
OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics 

# Save:
OutputPlot + coord_flip() + scale_y_continuous(breaks = seq(-1, 1,0.05),limits = c(-0.25,0.25)) + xlab("") 

ggsave(filename="./Output/CoefPlot_NonCompliers.pdf", width=6, height=3)
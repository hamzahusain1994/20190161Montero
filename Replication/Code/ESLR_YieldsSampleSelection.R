  ############################################################
  ##### ESLR - RD HECKMAN SELECTION WORK - AgCensus Data #####
  ############################################################
  
  rm(list = ls())       # Clear variables
  
  require(foreign)
  require(ggplot2)
  require(plyr)         # join function
  require(dplyr) 
  require(rdrobust)     # rd estimation tools
  require(stringdist)   # approximate string matching
  require(gdata)        
  #require(rdd)          # sorting tests
  require(stargazer)    # format tables
  require(haven)
  require(readstata13)
  require(sampleSelection)
  
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
    b <- MOD$coef[3]
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
  
  lm.beta.ss <- function (MOD, dta,y,bw) 
  {
    MOD2 <- MOD$estimate[dim(MOD$estimate)[1]:1,]
    b <- MOD2["Above500","Estimate"]
    model.dta <- filter(dta, norm_dist > -1*bw & norm_dist < bw )
    sx <- sd(model.dta[,c("Above500")])
    #sx <- sd(model.dta[,c("norm_dist")])
    sy <- sd((model.dta[,c(y)]),na.rm=TRUE)
    beta <- b * sx/sy
    return(beta)
  }
  
  lm.beta.ses.ss <- function (MOD, dta,y,bw) 
  {
    MOD2 <- MOD$estimate[dim(MOD$estimate)[1]:1,]
    b <- MOD2["Above500","Std. Error"]
    model.dta <- filter(dta, norm_dist > -1*bw & norm_dist < bw )
    sx <- sd(model.dta[,c("Above500")])
    #sx <- sd(model.dta[,c("norm_dist")])
    sy <- sd((model.dta[,c(y)]),na.rm=TRUE)
    beta <- b * sx/sy
    return(beta)
  }
  
  
  ########################################
  
  ## Calculate Yields for 4 main crops for 2007, and save RD estimates + Heckman Corrected Yields for each
  
  num_ests <- 4*2
  
  rd_estimates <-data.frame(estimates = rep(0, num_ests), ses = rep(0, num_ests),
                            y_var = rep(0,num_ests),
                            label = rep(0, num_ests))
  censo_ag_wreform_tev <- censo_ag_wreform
  ag.grouped <- group_by(censo_ag_wreform_tev,Expropretario_ISTA)
  ag.grouped <- mutate(ag.grouped, num_per_owner = n())
  censo_ag_wreform_tev$num_per_owner<- ag.grouped$num_per_owner
  
  k <- "triangular"
  p <- 1
  b<- "msecomb2"
  years <- 2007
  i = 2007
  censo_ag_wreform_tev <- mutate(censo_ag_wreform, ln_agprodII = ln_agprod, ln_agprod = ln_agprod_pricew_crops)
  count<-1
  bw <- 150
  
  
  ## SUGAR CANE:
  
  # Scale:
  censo_ag_wreform_rd <- censo_ag_wreform_tev
  rdests <- rdrobust(y = (censo_ag_wreform_rd$SugarCane_Yield),
                     x=censo_ag_wreform_rd$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     # bwselect = b, 
                     h=136, # To match stata
                     cluster=(censo_ag_wreform_rd$Expropretario_ISTA), vce="hc1")
  
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_rd, y="SugarCane_Yield") # rdests$coef[3]
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_rd, y="SugarCane_Yield") # rdests$se[3]
  rd_estimates[count,c("y_var")] <- "Sugar Cane Yield"
  rd_estimates[count,c("label")] <- paste("","RD Estimate",sep="")
  count<-count+1
  
  samplesel <- selection(SugarCane_Indicator ~  sugarcane_suit ,
                      SugarCane_Yield ~ Above500 , #+ norm_dist + Above500*norm_dist,
                     data= censo_ag_wreform_rd[which(abs(censo_ag_wreform_rd$norm_dist)<bw),], 
                     method = "2step")
  
  rd_estimates[count,c("estimates")] <-lm.beta.ss(MOD=summary(samplesel), dta=censo_ag_wreform_rd, y="SugarCane_Yield",bw) # rdests$coef[3]
  rd_estimates[count,c("ses")] <- lm.beta.ses.ss(MOD=summary(samplesel), dta=censo_ag_wreform_rd, y="SugarCane_Yield",bw) # rdests$se[3]
  rd_estimates[count,c("y_var")] <- "Sugar Cane Yield"
  rd_estimates[count,c("label")] <- paste("","Selection Correction",sep="")
  count<-count+1
  
  
  ## COFFEE:
  
  # Scale:
  #censo_ag_wreform_rd <- mutate(censo_ag_wreform_tev,SugarCane_Yield=ifelse(is.na(SugarCane_Yield),0,SugarCane_Yield))
  rdests <- rdrobust(y = (censo_ag_wreform_tev$Coffee_Yield),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1")
  
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="Coffee_Yield") # rdests$coef[3]
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="Coffee_Yield") # rdests$se[3]
  rd_estimates[count,c("y_var")] <- "Coffee Yield"
  rd_estimates[count,c("label")] <- paste("","RD Estimate",sep="")
  count<-count+1
  
  samplesel <- selection(Coffee_Indicator~  canton_coffee_suit,
                         Coffee_Yield ~ Above500 + norm_dist + Above500*norm_dist,
                         data= censo_ag_wreform_tev[which(abs(censo_ag_wreform_tev$norm_dist)<bw),], 
                         method = "2step")
  
  rd_estimates[count,c("estimates")] <-lm.beta.ss(MOD=summary(samplesel), dta=censo_ag_wreform_tev, y="Coffee_Yield",bw) # rdests$coef[3]
  rd_estimates[count,c("ses")] <- lm.beta.ses.ss(MOD=summary(samplesel), dta=censo_ag_wreform_tev, y="Coffee_Yield",bw) # rdests$se[3]
  rd_estimates[count,c("y_var")] <- "Coffee Yield"
  rd_estimates[count,c("label")] <- paste("","Selection Correction",sep="")
  count<-count+1
  
  ## MAIZE:
  
  # Scale:
  #censo_ag_wreform_rd <- mutate(censo_ag_wreform_tev,SugarCane_Yield=ifelse(is.na(SugarCane_Yield),0,SugarCane_Yield))
  rdests <- rdrobust(y = (censo_ag_wreform_tev$Maize_Yield),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1")
  
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="Maize_Yield") # rdests$coef[3]
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="Maize_Yield") # rdests$se[3]
  rd_estimates[count,c("y_var")] <- "Maize Yield"
  rd_estimates[count,c("label")] <- paste("","RD Estimate",sep="")
  count<-count+1
  
  samplesel <- selection(Maize_Indicator~ miaze_suit,
                         Maize_Yield ~ Above500 + norm_dist + Above500*norm_dist,
                         data= censo_ag_wreform_tev[which(abs(censo_ag_wreform_tev$norm_dist)<bw),], 
                         method = "2step")
  
  rd_estimates[count,c("estimates")] <-lm.beta.ss(MOD=summary(samplesel), dta=censo_ag_wreform_tev, y="Maize_Yield",bw) # rdests$coef[3]
  rd_estimates[count,c("ses")] <- lm.beta.ses.ss(MOD=summary(samplesel), dta=censo_ag_wreform_tev, y="Maize_Yield",bw) # rdests$se[3]
  rd_estimates[count,c("y_var")] <- "Maize Yield"
  rd_estimates[count,c("label")] <- paste("","Selection Correction",sep="")
  count<-count+1
  
  ## BEANS:
  
  # Scale:
  #censo_ag_wreform_rd <- mutate(censo_ag_wreform_tev,SugarCane_Yield=ifelse(is.na(SugarCane_Yield),0,SugarCane_Yield))
  rdests <- rdrobust(y = (censo_ag_wreform_tev$Beans_Yield),
                     x=censo_ag_wreform_tev$norm_dist,
                     c = 0,
                     p = p, 
                     q = p +1,
                     kernel = k,
                     bwselect = b, 
                     cluster=(censo_ag_wreform_tev$Expropretario_ISTA), vce="hc1")
  
  rd_estimates[count,c("estimates")] <-lm.beta(MOD=rdests, dta=censo_ag_wreform_tev, y="Beans_Yield") # rdests$coef[3]
  rd_estimates[count,c("ses")] <- lm.beta.ses(MOD=rdests, dta=censo_ag_wreform_tev, y="Beans_Yield") # rdests$se[3]
  rd_estimates[count,c("y_var")] <- "Beans Yield"
  rd_estimates[count,c("label")] <- paste("","RD Estimate",sep="")
  count<-count+1
  
  samplesel <- selection(Beans_Indicator~ bean_suit,
                         Beans_Yield ~ Above500 + norm_dist + Above500*norm_dist,
                         data= censo_ag_wreform_tev[which(abs(censo_ag_wreform_tev$norm_dist)<bw),], 
                         method = "2step")
  
  rd_estimates[count,c("estimates")] <-lm.beta.ss(MOD=summary(samplesel), dta=censo_ag_wreform_tev, y="Beans_Yield",bw) # rdests$coef[3]
  rd_estimates[count,c("ses")] <- lm.beta.ses.ss(MOD=summary(samplesel), dta=censo_ag_wreform_tev, y="Beans_Yield",bw) # rdests$se[3]
  rd_estimates[count,c("y_var")] <- "Beans Yield"
  rd_estimates[count,c("label")] <- paste("","Selection Correction",sep="")
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
  
  betas[which(betas$y_var=="Beans Yield" & betas$label=="RD Estimate"),c("ses")] <- betas[which(betas$y_var=="Beans Yield" & betas$label=="RD Estimate"),c("ses")]/3.0
  betas[which(betas$y_var=="Beans Yield" & betas$label=="RD Estimate"),c("estimates")] <- betas[which(betas$y_var=="Beans Yield" & betas$label=="RD Estimate"),c("estimates")]/1.0
  
  betas[which(betas$y_var=="Sugar Cane Yield" & betas$label=="RD Estimate"),c("ses")] <- betas[which(betas$y_var=="Sugar Cane Yield" & betas$label=="RD Estimate"),c("ses")]*3.0
  betas[which(betas$y_var=="Sugar Cane Yield" & betas$label=="RD Estimate"),c("estimates")] <- betas[which(betas$y_var=="Sugar Cane Yield" & betas$label=="RD Estimate"),c("estimates")]*1.0
  
  
  betas[which(betas$y_var=="Sugar Cane Yield" & betas$label=="Selection Correction"),c("ses")] <- betas[which(betas$y_var=="Sugar Cane Yield" & betas$label=="Selection Correction"),c("ses")]*3.0
  betas[which(betas$y_var=="Sugar Cane Yield" & betas$label=="Selection Correction"),c("estimates")] <- betas[which(betas$y_var=="Sugar Cane Yield" & betas$label=="Selection Correction"),c("estimates")]*3.0
  
  betas[which(betas$y_var=="Coffee Yield" & betas$label=="Selection Correction"),c("ses")] <- betas[which(betas$y_var=="Coffee Yield" & betas$label=="Selection Correction"),c("ses")]/1.75
  
  
  # Create Matrix for plotting:
  MatrixofModels <- betas[c("y_var", "estimates","ses","label")]
  colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "Group")
  MatrixofModels$IV <- factor(MatrixofModels$IV, levels = rev(c("Sugar Cane Yield",
                                "Coffee Yield",
                                "Maize Yield", "Beans Yield")),
                              labels = rev(c("Sugar Cane Yield",
                                         "Coffee Yield",
                                         "Maize Yield", "Beans Yield")))
  MatrixofModels$Group <- factor(MatrixofModels$Group) #levels = c(1,2), labels = c("Local Linear Polynomials","Local Quadratic Polynomials"))
  
  
  # Plot:
  OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                      ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                      ylab = NULL, xlab = NULL, facets=~ Group)
  OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
  OutputPlot <- OutputPlot + theme_bw() + ylab("\nStandardized Effect") + aesthetics + xlab("")
  
  # Save:
  OutputPlot + coord_flip() + scale_y_continuous(breaks = seq(-2.5, 1.5,0.5))
  
  ggsave(filename="./Output/CoefPlot_YieldsSampleSelection.pdf")
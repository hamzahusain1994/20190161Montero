########################################################################
##### El Salvador - Migration Outcomes - Canton-Level - Pop Census #####
########################################################################

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
require(sandwich)     # robust se's
require(zoo)          # filling in
require(fuzzyjoin)    # approximate string matching
require(haven)
require(stringi)


########################################

# Approximate String Matching Funtion

string_match <- function(string_to_match, options, smethod="osa") {
  if(string_to_match!="") {
    sdists <- stringdist(string_to_match, options, method=smethod)
    ind <- which(sdists == min(sdists))
    if(length(ind) != 1) {
      ind <- ind[1] # Assumes first index is the most common string to match.
    }
    return(options[ind])
  } else {
    return("")
  }
}

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]} # Function to turn factor vars to numeric variables correctly.

########################################

## Read in Data:
cantons <- read_dta(file="./Output/cantons_wGeoCovariates.dta")

# Note: Data doesn't have main RD variables of interest, need to merge them in:
# Vars created in ESLR_AnalysisConflictData.R
canton_rd_vars <- read.csv(file="./Data/conflict_canton.csv", header=TRUE)
# canton_rd_vars <- read.csv(file="./R/Output/conflict_canton_subset.csv", header=TRUE)

# Keep Vars of Interest and Merge in:
canton_rd_vars <- dplyr::select(canton_rd_vars,CODIGO,num_holdings:max_above_500)
#cantons <- dplyr::select(cantons,-reform)
cantons <- left_join(cantons,canton_rd_vars, by="CODIGO")

cantons$CODIGO_NOM <- as.character(cantons$CODIGO_)

########################################

poblaccion_section <- read_sav(file = "./Data/poblacion.sav")

########################################

cantons_popcensus <- dplyr::select(poblaccion_section,
                                    gender=S06P02,
                                    age=S06P03A,
                                    S06P07A, S06P08A1, S06P08A2,
                                    DEPDSC, MUNDSC, CANDSC,
                                   literate = S06P09,
                                   educated = S06P10,
                                   educ_level = S06P11A,
                                   finished_hs = S06P11B,
                                   S06P22)
                                  

cantons_popcensus <- mutate(cantons_popcensus,
                            born_same_as_mother= ifelse(S06P07A < 3,S06P07A,NA) ,
                            lived_canton_always = ifelse(S06P08A1 < 3,S06P08A1,NA),
                             lived_canton_year = ifelse(S06P08A2>0,S06P08A2,NA),
                            public_sector_worker = ifelse(S06P22 == 1, 1, 
                                                          ifelse(is.na(S06P22) | S06P22==-2,NA, 0)),
                            pop = 1,
                            CODIGO_NOM  = toupper(stri_trans_general(paste(DEPDSC, MUNDSC, CANDSC,sep=", "),"Latin-ASCII")))

cantons_popcensus <- mutate(cantons_popcensus,
                            born_same_as_mother= ifelse(born_same_as_mother ==2 ,0,born_same_as_mother) ,
                            lived_canton_always = ifelse(lived_canton_always ==2 ,0,lived_canton_always),
                            educ_yrs = 1*(educ_level==1)+6*(educ_level==2)+ 9*(educ_level==3)+
                              11*(educ_level==4)+13*(educ_level==5)+ 15*(educ_level==6)+
                              16*(educ_level==7)+ 17*(educ_level==8)+ 20*(educ_level==9))

cantons_popcensus <- filter(cantons_popcensus, CANDSC != "AREA URBANA")

# Summarise to make merging faster:
cantons_popcensus <- cantons_popcensus %>%
  group_by(CODIGO_NOM) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

max.dist <- 15 # since there are errors in mun names + state names

# inds <- amatch(cantons_popcensus$CODIGO_NOM, cantons$CODIGO_NOM, maxDist=max.dist) # can try different maxDists and different methods (using levenstein right now as default i believe)
# # View(t(rbind(cantons_literacy$CODIGO_NOM,as.character(cantons$CODIGO_NOM[inds]))))
# cantons_popcensus$CODIGO <- cantons$CODIGO[inds]
# cantons <- left_join(cantons, cantons_popcensus, by="CODIGO")

max.dist <- 10 # since there are errors in mun names + state names
cantons <- stringdist_join(cantons, cantons_popcensus, 
                           by = c("CODIGO_NOM" = "CODIGO_NOM"), 
                           mode = "left", 
                           method = "jw", 
                           max_dist = max.dist, 
                           distance_col = "dist")

cantons <- cantons %>%
  group_by(CODIGO_NOM.x) %>%
  top_n(1, -dist) %>% ungroup()

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
as.numeric.factor.wcheck <- function(x) {if(class(x)=="factor") { return(as.numeric(levels(x))[x]) } else { return(x)}}


### Using Share Above 500
cantons$share_above500 <- cantons$num_above500/(cantons$num_above500 + cantons$num_below500)

## Same Canton Always:
b0 <- lm(lived_canton_always ~ share_above500 + gender + age + age^2 , data=cantons)
cov0  <- vcovHC(b0, type = "HC1")
robust.se0 <- sqrt(diag(cov0))
summary(b0)

## Same Canton Year:
b1 <- lm(lived_canton_year ~ share_above500 + gender + age + age^2, data=cantons)
cov1  <- vcovHC(b1, type = "HC1")
robust.se1 <- sqrt(diag(cov1))
summary(b1)

## Same Canton - Mother:
b2 <- lm(born_same_as_mother ~ share_above500 + gender + age + age^2, data=cantons)
cov2  <- vcovHC(b2, type = "HC1")
robust.se2 <- sqrt(diag(cov2))
summary(b2)


stargazer(b0,b1,b2,
          type = "latex", 
          se = list(robust.se0, robust.se1,robust.se2),  
          keep = c("share_above500"),
          digits = 4,
          out="./Output/MigrationOutcomes_CantonLevel.tex")


########################################

## Now for highly educated sample

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
                            finished_hs = ifelse(finished_hs>0,finished_hs, NA),
                            CODIGO_NOM  = toupper(stri_trans_general(paste(DEPDSC, MUNDSC, CANDSC,sep=", "),"Latin-ASCII")))

cantons_popcensus <- mutate(cantons_popcensus,
                            born_same_as_mother= ifelse(born_same_as_mother ==2 ,0,born_same_as_mother) ,
                            finished_hs = ifelse(finished_hs==2, 0, finished_hs),
                            lived_canton_always = ifelse(lived_canton_always ==2 ,0,lived_canton_always)
)

cantons_popcensus_educ <- filter(cantons_popcensus,
                            finished_hs==1)

cantons_popcensus_educ <- filter(cantons_popcensus_educ, CANDSC != "AREA URBANA")

# Summarise to make merging faster:
cantons_popcensus_educ <- cantons_popcensus_educ %>%
  group_by(CODIGO_NOM) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

max.dist <- 15 # since there are errors in mun names + state names

cantons <- read_dta(file="./Output/cantons_wGeoCovariates.dta")
cantons <- left_join(cantons,canton_rd_vars, by="CODIGO")

cantons$CODIGO_NOM <- as.character(cantons$CODIGO_)


max.dist <- 10 # since there are errors in mun names + state names
cantons <- stringdist_join(cantons, cantons_popcensus_educ, 
                           by = c("CODIGO_NOM" = "CODIGO_NOM"), 
                           mode = "left", 
                           method = "jw", 
                           max_dist = max.dist, 
                           distance_col = "dist")

cantons <- cantons %>%
  group_by(CODIGO_NOM.x) %>%
  top_n(1, -dist) %>% ungroup()

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
as.numeric.factor.wcheck <- function(x) {if(class(x)=="factor") { return(as.numeric(levels(x))[x]) } else { return(x)}}

### Using Share Above 500
cantons$share_above500 <- cantons$num_above500/(cantons$num_above500 + cantons$num_below500)

## Same Canton Always:
b0 <- lm(lived_canton_always ~ share_above500 + gender + age + age^2 , data=cantons)
cov0  <- vcovHC(b0, type = "HC1")
robust.se0 <- sqrt(diag(cov0))
summary(b0)

## Same Canton Year:
b1 <- lm(lived_canton_year ~ share_above500 + gender + age + age^2, data=cantons)
cov1  <- vcovHC(b1, type = "HC1")
robust.se1 <- sqrt(diag(cov1))
summary(b1)

## Same Canton - Mother:
b2 <- lm(born_same_as_mother ~ share_above500 + gender + age + age^2, data=cantons)
cov2  <- vcovHC(b2, type = "HC1")
robust.se2 <- sqrt(diag(cov2))
summary(b2)


stargazer(b0,b1,b2,
          type = "latex", 
          se = list(robust.se0, robust.se1,robust.se2),  
          keep = c("share_above500"),
          digits = 4,
          out="./Output/MigrationOutcomes_CantonLevel_CompletedHS.tex")




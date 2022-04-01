#############################################
########### ESLR - EXTENTSIONS ##############
############### COEF PLOTS ##################
#############################################

rm(list = ls())       # Clear variables
require(foreign)
require(ggplot2)
require(RColorBrewer) # creates nice color schemes
require(scales)       # customize scales
require(plyr)         # join function
require(dplyr) 
require(haven)        # stata save
require(dotwhisker)

########################################

# write a simple function to add footnote
makeFootnote <- function(footnoteText =
                           format(Sys.time(), "%d %b %Y"),
                         size = .7, color = grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label = footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y = unit(2, "mm"),
            just = c("right", "bottom"),
            gp = gpar(cex = size, col = color))
  popViewport()
}
# Source: http://statmodeling.com/best-way-to-add-a-footnote-to-a-plot-created-with-ggplot2.html

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
        axis.title.y=element_text(face="bold.italic")))
#axis.text=element_blank(),
#axis.ticks=element_blank(),
#panel.border = element_blank()))

########################################

#### Plots for Different Minor Crops:

## load data:
data <- read.csv("./Output/Temp/MinorCropProduction.csv")
data <- filter(data,estimate!=0)

               
# Clean data for plotting:
alpha<- 0.05
Multiplier <- qnorm(1 - alpha / 2)

# Find the outcome var for each regression:
data$idstr <- as.character(data$idstr)
data$y_var <- data$idstr
data <- filter(data,y_var!="S5BEJOTE",
               y_var!="S5BMELON",
               y_var!="S5BCAMOTE")
# Replace y_var with nice names:
data$y_var[which(data$y_var == "S5BCAMOTE")] <- "Sweet Potato"
data$y_var[which(data$y_var == "S5BCHILE")] <- "Bell Peppers"
data$y_var[which(data$y_var == "S5BCHILEPICANTE")] <- "Chile"
data$y_var[which(data$y_var == "S5BEJOTE")] <- "Bejote"
data$y_var[which(data$y_var == "S5BGUISQUIL")] <- "Squash"
data$y_var[which(data$y_var == "S5BLOROCO")] <- "Loroco"
data$y_var[which(data$y_var == "S5BMELON")] <- "Melon"
data$y_var[which(data$y_var == "S5BPEPINO")] <- "Cucumber"
data$y_var[which(data$y_var == "S5BPIPIAN")] <- "Pipian"
data$y_var[which(data$y_var == "S5BRABANO")] <- "Radish"
data$y_var[which(data$y_var == "S5BSANDIA")] <- "Watermelon"
data$y_var[which(data$y_var == "S5BTOMATE")] <- "Tomato"
data$y_var[which(data$y_var == "S5BYUCA")] <- "Yuca"

# Now, keep only the betas of interest:
betas <- data %>%  filter(!grepl("S5B",y_var))
dim(betas)
betas <- arrange(betas,betas$y_var)

# Create Matrix for plotting:
MatrixofModels <- betas[c("y_var", "estimate","stderr","z","p","idnum")]
colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "TValue", "PValue", "ModelName")
MatrixofModels$IV <- factor(MatrixofModels$IV, levels = MatrixofModels$IV)
#MatrixofModels$Legend <- c("  AES Coefficient", rep("  Standard Coefficients",4))

# Re-name for plotting:
MatrixofModels$ModelName <- "Minor Vegetable Production"
#MatrixofModels[, -c(1, 6)] <- apply(MatrixofModels[, -c(1, 6)], 2, function(x){as.numeric(as.character(x))})

# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nEstimated Effect") + aesthetics  + xlab("")

# Save:
OutputPlot 
ggsave(filename = "./Output/CoefPlot_MinorCrops.pdf", height=6, width=9)

########################################

#### Plots for Different Minor Fruits:

## load data:
data <- read.csv("./Output/Temp/MinorFruitProduction.csv")
data <- filter(data,estimate!=0)
#data$parm[which(data$parm == "RD_Estimate")] <- "Above 500"

# Clean data for plotting:
alpha<- 0.05
Multiplier <- qnorm(1 - alpha / 2)

# Find the outcome var for each regression:
data$idstr <- as.character(data$idstr)
data$y_var <- data$idstr
# Replace y_var with nice names:
data$y_var[which(data$y_var == "S8BCOCO")] <- "Coconut"
data$y_var[which(data$y_var == "S8BGUINEO")] <- "Guineo Banana"
data$y_var[which(data$y_var == "S8BJOCOTE")] <- "Jocote"
data$y_var[which(data$y_var == "S8BLIMON")] <- "Lemon"
data$y_var[which(data$y_var == "S8BMANDARINA")] <- "Mandarin"
data$y_var[which(data$y_var == "S8BMANGO")] <- "Mango"
data$y_var[which(data$y_var == "S8BNARANJA")] <- "Orange"
data$y_var[which(data$y_var == "S8BNISPERO")] <- "Medlar"
data$y_var[which(data$y_var == "S8BPAPAYA")] <- "Papaya"
data$y_var[which(data$y_var == "S8BPLATANO")] <- "Plantain"
data$y_var[which(data$y_var == "S8BZAPOTE")] <- "Sapodilla"

# Now, keep only the betas of interest:
betas <- data  %>%  filter(!grepl("S8B",y_var))
dim(betas)
betas <- arrange(betas,betas$y_var)

# Create Matrix for plotting:
MatrixofModels <- betas[c("y_var", "estimate","stderr","z","p","idnum")]
colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "TValue", "PValue", "ModelName")
MatrixofModels$IV <- factor(MatrixofModels$IV, levels = MatrixofModels$IV)
#MatrixofModels$Legend <- c("  AES Coefficient", rep("  Standard Coefficients",4))

# Re-name for plotting:
MatrixofModels$ModelName <- "Minor Fruit Production"
#MatrixofModels[, -c(1, 6)] <- apply(MatrixofModels[, -c(1, 6)], 2, function(x){as.numeric(as.character(x))})

# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nEstimated Effect") + aesthetics  + xlab("")

# Save:
OutputPlot 
ggsave(filename = "./Output/CoefPlot_MinorFruits.pdf", height=6, width=9)

########################################

#### Plots for Different Inputs:

## load data:
data <- read.csv("./Output/Temp/InputUse.csv")
data <- filter(data,estimate!=0)
#data$parm[which(data$parm == "RD_Estimate")] <- "Above 500"

# Clean data for plotting:
alpha<- 0.05
Multiplier <- qnorm(1 - alpha / 2)

# Find the outcome var for each regression:
data$idstr <- as.character(data$idstr)
data$y_var <- data$idstr
data <- filter(data,y_var!="S15BCASTRACION",
                      y_var!="S15BCONTROLBIOLOGICOPECESABEJAS",
                      y_var!="S15BCONTROLQUIMICODEPLAGASYENFE",
                      y_var!="S15BDESPARASITACION",
                      y_var!="S15BDESPARASITANTES",
                      y_var!="S15BINSEMINACIONARTIFICIAL",
                      y_var!="S15BMANEJOINTEGRADODEPLAGASMIP",
                      y_var!="S15BMEJORAMIENTOGENETICO",
                      y_var!="S15BPIEDECRIA",
                      y_var!="S15BPRACTICASPREVENTIVASDEDANOS",
                      y_var!="S15BPRODUCTOSVETERINARIOSALCOHO",
                      y_var!="S15BREGISTROSADMINISTRATIVOSDEL",
                      y_var!="S15BREGULADORESDECRECIMIENTO",
                      y_var!="S15BREGULADORESDECRECIMIENTOENZ",
                      y_var!="S15BROTACIONDEPOTREROS",
                      y_var!="S15BSUPLEMENTOSNUTRICIONALES",
                      y_var!="S15BVACUNACION",
                      y_var!="S15BVACUNAS",
                      y_var!="S15BANTIBIOTICOS")

# Replace y_var with nice names:
  # cat(paste("data$y_var[which(data$y_var == ",data$y_var,")] <- "),sep="\n")
  data$y_var[which(data$y_var ==  "S15BABONOOFERTILIZANTEFOLIARLIQ" )] <- "Fertilizer - Liquid"
  data$y_var[which(data$y_var ==  "S15BABONOOFERTILIZANTEGRANULARS" )] <- "Fertilizer - Solid"
  data$y_var[which(data$y_var ==  "S15BAGENTESDEMADURACIONPOSTCOSE" )] <- "Compost"
  data$y_var[which(data$y_var ==  "S15BAGENTESPARAPROTECCIONDEPROD" )] <- "Pesticides"
  data$y_var[which(data$y_var ==  "S15BANALISISDESUELOYOFOLIAR" )] <- "Soil Tests"
  data$y_var[which(data$y_var ==  "S15BAPLICACIONDEABONOYFERTILIZA" )] <- "Fertilizer Applied"
  data$y_var[which(data$y_var ==  "S15BAPLICACIONDERIEGOPORASPERSI" )] <- "Sprinkler Irrigation"
  data$y_var[which(data$y_var ==  "S15BAPLICACIONDERIEGOPORGOTEO" )] <- "Drip Irrigation"
  data$y_var[which(data$y_var ==  "S15BAPLICACIONDERIEGOPORGRAVEDA" )] <- "Gravity Irrigation"
  data$y_var[which(data$y_var ==  "S15BBACTERICIDAS" )] <- "Bactericides"
  data$y_var[which(data$y_var ==  "S15BFUNGICIDAS" )] <- "Fungicides"
  data$y_var[which(data$y_var ==  "S15BLABORESCULTURALES" )] <- "Labor Trimming"
  data$y_var[which(data$y_var ==  "S15BMATERIALVEGETATIVO" )] <- "Organic Fertilizer"
  data$y_var[which(data$y_var ==  "S15BNEMATICIDAS" )] <- "Nematicides"
  data$y_var[which(data$y_var ==  "S15BOBRASDECONSERVACIONDESUELOS" )] <- "Erosion Work"
  data$y_var[which(data$y_var ==  "S15BPREPARACIONDELSUELO" )] <- "Soil Preparation"
  data$y_var[which(data$y_var ==  "S15BPROTECCIONDECULTIVOS" )] <- "Crop Protection"
  data$y_var[which(data$y_var ==  "S15BRESIEMBRAYOREPLANTACION" )] <- "Reseeding + Replanting"
  data$y_var[which(data$y_var ==  "S15BSEMILLACERTIFICADA" )] <- "Certified Seeds"
  data$y_var[which(data$y_var ==  "S15BSEMILLACRIOLLA" )] <- "Creole Seeds"
  data$y_var[which(data$y_var ==  "S15BSEMILLAMEJORADA" )] <- "Improved Seeds"

# Now, keep only the betas of interest:
betas <- data %>%  filter(!grepl("S15B",y_var))
dim(betas)
betas <- arrange(betas,betas$estimate)

# Create Matrix for plotting:
MatrixofModels <- betas[c("y_var", "estimate","stderr","z","p","idnum")]
colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "TValue", "PValue", "ModelName")
MatrixofModels$IV <- factor(MatrixofModels$IV, levels = MatrixofModels$IV)
#MatrixofModels$Legend <- c("  AES Coefficient", rep("  Standard Coefficients",4))

# Re-name for plotting:
MatrixofModels$ModelName <- "Input Use"
#MatrixofModels[, -c(1, 6)] <- apply(MatrixofModels[, -c(1, 6)], 2, function(x){as.numeric(as.character(x))})

# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nEstimated Effect") + aesthetics + xlab("")

# Save:
OutputPlot 
ggsave(filename = "./Output/CoefPlot_Inputs.pdf", height=6, width=9)

########################################

## load data:
data <- read.csv("./Output/Temp/CapitalStocks.csv")
data <- filter(data,estimate!=0 & !is.na(p))

# Clean data for plotting:
alpha<- 0.05
Multiplier <-  qnorm(1-alpha/2) #qt(1 - alpha / 2, df=75) # Small estimates
Multiplier <-  qt(1 - alpha / 2, df=75) # Small estimates

# Find the outcome var for each regression:
data$idstr <- as.character(data$idstr)
data$y_var <- gsub("S16A","",data$idstr)
data <- filter(data,y_var!="ALIMENTADORES",
               y_var!="AUTOCLAVE",
               y_var!="BANDADEINCUBACION",
               y_var!="DESPLUMADORAS",
               y_var!="EQUIPODEIDENTIFICACION",
               y_var!="EQUIPOPARAINSEMINACIONARTIF",
               y_var!="EQUIPOPARAORDENO",
               y_var!="EQUIPOPREVENTIVODEDANOSENAN",
               y_var!="ESTABLOS",
               y_var!="GALERAS",
               y_var!="INFRAESTRUCTURAPARAALIMENTA",
               y_var!="LABORATORIOINVITRO",
               y_var!="LABORATORIOSDEANALISISDESUE",
               y_var!="MANGASOCEPOS",
               y_var!="MAQUINARIAPARAPRODUCCIONDEA",
               y_var!="OTROSTALLERESPISTADEATERRIZ",
               y_var!="MOLEDORADEGRANOS",
               y_var!="REDES",
               y_var!="SALASDEINCUBACION",
               y_var!="SALASDEORDENO",
               y_var!="BANDARECOLECTORADEHUEVOS",
               y_var!="CLASIFICADORADEFRUTALESHORT",
               y_var!="UTENSILIOSPARARECOLECCIONDE",
               y_var!="HERRAMIENTASAGROPECUARIAS",
               y_var!="TANQUESDEFERTIRRIEGO",
               y_var!="SALASDECURADO")

# Replace y_var with nice names:
# cat(paste('data$y_var[which(data$y_var == \"',data$y_var,"\")] <- \"",data$y_var,"\"",sep=""),sep="\n")
data$y_var[which(data$y_var == "ARADOSDEHIERRO")] <- "Plows"
data$y_var[which(data$y_var == "BALANZAPARACARGASPESADAS")] <- "Balances"
data$y_var[which(data$y_var == "BASCULA")] <- "Coffee Weighing Machines"
data$y_var[which(data$y_var == "BODEGAS")] <- "Wharehouses"
data$y_var[which(data$y_var == "BOMBAACHICADORAMECANICA")] <- "Fumigation Backpacks"
data$y_var[which(data$y_var == "CAMIONOVEHICULOS")] <- "Trucks"
data$y_var[which(data$y_var == "DESPULPADORADECAFEMANUAL")] <- "Manual Coffee Pulping Machines"
data$y_var[which(data$y_var == "DESPULPADORADECAFEMECANICA")] <- "Mecanical Coffee Pulping Machines"
data$y_var[which(data$y_var == "EQUIPOBENEFICIADORCAFE")] <- "Coffee Equipement"
data$y_var[which(data$y_var == "EQUIPODEFUMIGACION")] <- "Fumigation Equipement"
data$y_var[which(data$y_var == "EQUIPODERIEGO")] <- "Irrigration Equipement"
data$y_var[which(data$y_var == "EQUIPODETRANSPORTEDEAGUA")] <- "Water Transportation Equipement"
data$y_var[which(data$y_var == "EQUIPOPARALACOSECHA")] <- "Harvest Equipment"
data$y_var[which(data$y_var == "HERRAMIENTASAGROPECUARIAS")] <- "Agrigultural Tools"
data$y_var[which(data$y_var == "MANGUERAS")] <- "Hoses"
data$y_var[which(data$y_var == "MOTOSIERRAS")] <- "Saws"
data$y_var[which(data$y_var == "OFICINAS")] <- "Offices"
data$y_var[which(data$y_var == "PATIOSDESECADO")] <- "Drying Patios"
data$y_var[which(data$y_var == "PICADORADEPASTO")] <- "Lawnmowers"
data$y_var[which(data$y_var == "RASTRASYMONTACARGAS")] <- "Harrows"
data$y_var[which(data$y_var == "SEMBRADORAMECANICA")] <- "Mecanical Seeders"
data$y_var[which(data$y_var == "SILOSPARAFORRAJEFRESCO")] <- "Storage Silos"
data$y_var[which(data$y_var == "TANQUESDEFERTIRRIEGO")] <- "Irrigation Tanks"
data$y_var[which(data$y_var == "TANQUESPARAALMACENAMIENTODE")] <- "Water Storage Tanks"
data$y_var[which(data$y_var == "TOLDODERECIBIDERODECAFE")] <- "Coffee Drying Tarps"
data$y_var[which(data$y_var == "TRACTORES")] <- "Tractors"
data$y_var[which(data$y_var == "UTENSILIOSPARARECOLECCIONDE")] <- "UTENSILIOSPARARECOLECCIONDE"
data$y_var[which(data$y_var == "VIVIENDAS")] <- "Houses"
data$y_var[which(data$y_var == "BALANZADEPRECISION")] <- "Precision Scales"
data$y_var[which(data$y_var == "DESOPERCULADORYOTRASHERRAMI")] <- "Uncapper"
data$y_var[which(data$y_var == "EQUIPOPARAALIMENTACION")] <- "Feeding Equipement"
data$y_var[which(data$y_var == "EQUIPODECALEFACCION")] <- "Heating Equipement"
data$y_var[which(data$y_var == "PULVERIZADORES")] <- "Spraying Equipement"
data$y_var[which(data$y_var == "ESPATULAS")] <- "Spatulas"
data$y_var[which(data$y_var == "EXTRATORDEMIEL")] <- "Honey Extractor"
data$y_var[which(data$y_var == "VESTIMENTAESPECIAL")] <- "Special Clothing"
data$y_var[which(data$y_var == "AHUMADORES")] <- "Smoking Equipement"
data$y_var[which(data$y_var == "PORQUERIZAS")] <- "Pig Equipement"

data <- filter(data,y_var!="Offices", y_var!="Wharehouses",
               y_var!="Lawnmowers", 
               y_var!="Water Storage Tanks",
               y_var!="Storage Silos") # Remove largest estimates/unclear topic/unrelated to AG

# Now, keep only the betas of interest:
betas <- data %>%  filter(!grepl("S16B",y_var))
dim(betas)
betas <- arrange(betas,betas$estimate)

# Create Matrix for plotting:
MatrixofModels <- betas[c("y_var", "estimate","stderr","z","p","idnum")]
colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "TValue", "PValue", "ModelName")
MatrixofModels$IV <- factor(MatrixofModels$IV, levels = MatrixofModels$IV)
#MatrixofModels$Legend <- c("  AES Coefficient", rep("  Standard Coefficients",4))

# Re-name for plotting:
#MatrixofModels[, -c(1, 6)] <- apply(MatrixofModels[, -c(1, 6)], 2, function(x){as.numeric(as.character(x))})

# Plot:
OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                    ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                    ylab = NULL, xlab = NULL)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + theme_bw() + ylab("\nRD Estimate") + aesthetics + xlab("")

# Save:
OutputPlot 

# Add list of brackets (Coffee Related, Non-Coffee Related)
coffee_goods <- c("Coffee-Specific Capital",
              "Manual Coffee Pulping Machines",
              "Mecanical Coffee Pulping Machines",
              "Coffee Equipement",
              "Drying Patios",
              "Coffee Weighing Machines", 
              "Balances",
              "Water Storage Tanks",
              "Coffee Drying Tarps")


MatrixofModels <- suppressWarnings(MatrixofModels %>% mutate(Group = ifelse(IV %in% coffee_goods,1,0), 
                                            term=IV,
                                            estimate= Estimate,
                                            std.error = StandardError) %>% 
  arrange(-Group, -IV))

# Create list of brackets (label, topmost included predictor, bottommost included predictor)
bracket1 <- c("Coffee-Specific Capital",
              "Coffee Weighing Machines",
              "Mecanical Coffee Pulping Machines")
bracket2 <- c("General Ag. Capital",
              "Hoses",
              "Trucks")

brackets <- list(bracket1, bracket2)

{dwplot(MatrixofModels,  vline = geom_vline(xintercept = 0, colour = "red", linetype = 2),
        dot_args = list(color="black"),
        whisker_args = list(color="black")) +
  theme_bw() + xlab("RD Estimate") + ylab("") +
  theme(plot.title = element_text(face="bold"),
        legend.title = element_blank(), text=element_text(family="Palatino"))} %>% 
  add_brackets(brackets, face="bold")
# Save:
ggsave(filename = "./Output/CoefPlot_Capital_wBrackets.pdf", scale=2)



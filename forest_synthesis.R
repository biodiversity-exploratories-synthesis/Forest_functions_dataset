### === Synthesis dataset functions forest assembly === ###

#Author(s): Marc Beringer, Paul Armando Gilmour Rivas Luginbühl, Noëlle Schenk, Catarina Penone
#Date: July, 2023
#Purpose: Assemble dataset for forest ecosystem functions
#         First, generate a dataframe with identifier columns
#         Second, fill this dataframe with relevant rows and columns from individual BE datasets
#         Third, calculate new variables where necessary

### === working directory === ###
setwd("C:/Users/Marc Beringer/Desktop/R/Synthesis_dataset_functions_forest")
getwd()
### ===== ###

### === save a snapshot of the R environment === ###
#includes information about versions and packages used
#renv::snapshot()
### ===== ###

### === write table checkpoint === ###
write.table(BE_synthesis_forest_dat, file = "BE_synthesis_forest_dat.txt", quote = F, sep = "\t", row.names = F)
### ===== ###

### === read table checkpoint === ###
BE_synthesis_forest_dat <- read.table("BE_synthesis_forest_dat.txt", header = T, sep = "\t")
### ===== ###

### === libraries === ###
{
  library(tidyverse) #load after plyr
}
### ===== ###

### === useful functions === ###

### === Change Exploratories plot names without zeros to plot names with zeros
#' https://github.com/biodiversity-exploratories-synthesis/Synthesis_useful_functions/blob/main/BEplotZeros%20.R
#' @author Caterina Penone
#' @examples
#' #create a dataset with a plot name column
#' dat <- data.frame(Plot_name = c("AEG1", "AEG2", "HEW4", "SEG8", "SEW10"), Values=1:5)
#' dat <- BEplotZeros(dat, "Plot_name", plotnam = "Sorted_plot_name")
#' 
#' @export
BEplotZeros <- function (dat, column, plotnam="PlotSTD"){
  dat <- as.data.frame(dat)
  funz <- function(x) ifelse((nchar(as.character(x))==4), gsub("(.)$", "0\\1", x),as.character(x))
  dat[,plotnam] <- sapply(dat[,column],funz)
  
  return(dat)
}

### ===== ###

### === path to data === ###
#Path to raw data on the server of the Institute of Plant Sciences at the University of Bern (Switzerland)
#Unzip the Datasets first
pathtodata <- "P:/PROJECTS/Exploratories Synthesis/Data/Forest_functions/dataset_creation/raw_data/"
### ===== ###

### === data assembly === ###
#create an empty dataframe with identifier (BEplotID, exploratory and habitat) columns, which will be filled with relevant columns later
BE_synthesis_identifier_dat  <- data.frame(BEplotID = c(paste("AEW",formatC(1:50, width=2, flag="0"),sep=""),
                                                        paste("AEG",formatC(1:50, width=2, flag="0"),sep=""),
                                                        paste("SEW",formatC(1:50, width=2, flag="0"),sep=""),
                                                        paste("SEG",formatC(1:50, width=2, flag="0"),sep=""),
                                                        paste("HEW",formatC(1:50, width=2, flag="0"),sep=""),
                                                        paste("HEG",formatC(1:50, width=2, flag="0"),sep="")),
                                                        exploratory=c(rep("ALB",100),rep("SCH",100),rep("HAI",100))) %>% 
                                                        mutate( habitat = 
                                                                  ifelse(grepl(pattern = "W", BEplotID), "forest", "grassland"))

#filter for forest habitat
BE_synthesis_identifier_dat <- BE_synthesis_identifier_dat %>% subset( habitat == "forest")
### ===== ###

### === Root decomposition === ### 
#Principal Investigator:     Schrumpf
#Dataset(s):                 16666_2_Dataset
#Process and component name: Root decomposition
#Relevant columns (unit):    Mass_loss_April_2012 (%)
#                            Mass_loss_October_2012 (%)

#read data
dat <- read.table(paste0(pathtodata, "Functions/16666_2_Dataset/16666_2_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_identifier_dat, dat[,5:7], by = "BEplotID", all.x = T)
#special treatment for the added columns
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Mass_loss_April_2012))) #4 NAs
length(which(is.na(BE_synthesis_forest_dat$Mass_loss_October_2012))) #15 NAs
### ===== ###

### === Nitrogen availability === ###
#Principal Investigator(s):  Schloter
#                            Trumbore
#                            Bonkowski
#Dataset(s):                 19847_3_Dataset
#                            14446_19_Dataset
#                            20040_3_Dataset
#                            20045_3_Dataset
#                            14106_2_Dataset
#Process and component name: N availability
#Relevant columns (unit):    Potential_nitrification_rate (ng/g)
#                            Ammonia_NH4 (µg/g)
#                            Nitrate_NO3- (mg/g)
#                            CN_ratio (Organic C/Total N) (14446_19_Dataset - Upper mineral soil 2011)
#                            CN_ratio (Total_C/Total_N) (20040_3_Dataset - Organic Horizon soil 2011)
#                            CN_ratio (Total_C/Total_N) (20045_3_Dataset - Organic Horizon soil 2014)
#                            Nmic (µg/g)

#read data
dat <- read.table(paste0(pathtodata, "Functions/19847_3_Dataset/19847_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/14446_19_Dataset/14446_19_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/20040_3_Dataset/20040_3_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/20045_3_Dataset/20045_3_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/14106_2_Dataset/14106_2_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat4)
dat <- BEplotZeros(dat, "Plot_ID", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "BEplotID")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "BEplotID")
dat3 <- BEplotZeros(dat3, "EP_Plotid", plotnam = "BEplotID")
dat4 <- BEplotZeros(dat4, "PlotID", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c(3,5:7)], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c(9,10)], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4[,c(3,5)], by = "BEplotID", all.x = T)

#special treatment for the added columns
#instead of values some rows contain the character "bdl"
#replace "bdl" with NA
BE_synthesis_forest_dat[BE_synthesis_forest_dat == "bdl"] <- NA
#=#

#before merging dat2 and dat3 to BE_synthesis_forest_dat, O_Horizon_CN_ratio_2011 and 2014 has to be calculated
#Each BEplotID has three measurements, one per O-Horizon subclass (Oi, Oe, Oa).
#Average the three O-Horizon subclasses per BEplotID to make the variable comparable to the CN_ratio of the upper mineral soil layer from the 14446_19_Dataset.
#Therefore, extract the relevant columns, pivot_wider and get rowwise averages of the O-Horizon subclass columns.
dat2.1 <- dat2[,c(5,8,9)] %>% 
  pivot_wider( names_from = c(Horizon), values_from = CN_ratio) %>% 
  rowwise( BEplotID) %>% 
  mutate( O_Horizon_CN_ratio_2011 = mean(c_across(1:3), na.rm = T))

dat3.1 <- dat3[,c(5,9,11)] %>% 
  pivot_wider( names_from = c(Horizon), values_from = CN_ratio) %>% 
  rowwise( BEplotID) %>% 
  mutate( O_Horizon_CN_ratio_2014 = mean(c_across(1:3), na.rm = T))

#merge the O_Horizon_CN_ratio_2011 and 2014 columns
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2.1[,c(1,5)], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3.1[,c(1,5)], by = "BEplotID", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$PNR))) #10 NAs
length(which(is.na(BE_synthesis_forest_dat$NH4))) #29 NAs
length(which(is.na(BE_synthesis_forest_dat$NO3))) #28 NAs
length(which(is.na(BE_synthesis_forest_dat$CN_ratio))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$O_Horizon_CN_ratio_2011))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$O_Horizon_CN_ratio_2014))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$Nmic))) #1 NAs
### ===== ###

### === Phosphorous availability === ###
#Principal Investigator:     Oelmann
#Dataset(s):                 19286_3_Dataset
#                            5241_5_Dataset
#                            19009_3_Dataset
#                            15766_3_Dataset
#Process and component name: P availability
#Relevant columns (unit):    OlsenPi (mg/kg)
#                            NaHCO3_Pi (mg/kg)
#                            Resin_P (mg/kg)
#                            Pmic (mg/kg)

#read data
dat <- read.table(paste0(pathtodata, "Functions/19286_3_Dataset/19286_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/5241_5_Dataset/5241_5_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/19009_3_Dataset/19009_3_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/15766_3_Dataset/15766_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat3)
dat <- BEplotZeros(dat, "Plot_ID", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "EP", plotnam = "BEplotID")
dat2 <- BEplotZeros(dat2, "EP", plotnam = "BEplotID")
dat3 <- BEplotZeros(dat3, "EP", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c(4,6)], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c(4,11)], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2[,c(2,3)], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3[,c(2,3)], by = "BEplotID", all.x = T)
#special treatment for the added columns
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$OlsenPi))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$NaHCO3_Pi))) #19 NAs
length(which(is.na(BE_synthesis_forest_dat$Resin_P))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$Pmic))) #1 NAs
### ===== ###

### === Sulfur availability === ###
#Principal Investigator:     Trumbore
#Dataset(s):                 20045_3_Dataset
#Process and component name: S availability
#Relevant columns (unit):    CS_ratio (Total_C/Total_S)

#read data
dat <- read.table(paste0(pathtodata, "Functions/20045_3_Dataset/20045_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")

#special treatment for the added columns
#before merging dat to BE_synthesis_forest_dat, O_Horizon_CS_ratio_2014 has to be calculated
#Each BEplotID has three measurements, one per O-Horizon subclass (Oi, Oe, Oa).
#Average the three O-Horizon subclasses per BEplotID to make the variable comparable to the O_Horizon_CN_ratio_2014 variable.
#Therefore, extract the relevant columns, pivot_wider and get rowwise averages of the O-Horizon subclass columns.
dat.1 <- dat[,c(5,10,11)] %>% 
  pivot_wider( names_from = c(Horizon), values_from = CS_ratio) %>% 
  rowwise( BEplotID) %>% 
  mutate( O_Horizon_CS_ratio_2014 = mean(c_across(1:3), na.rm = T))
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c(1,5)], by = "BEplotID", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$O_Horizon_CS_ratio_2014))) #0 NAs
### ===== ###

### === Dung decomposition === ### UNSOLVED QUESTIONS
#Principal Investigator:     Blüthgen
#Dataset(s):                 19866_2_Dataset
#Process and component name: Dung decomposition
#Relevant columns (unit):    removal_g (g)
#                            removal_rate (%)
#Notes: This variable is present in the grassland functions synthesis dataset. Ask Noëlle about calculations.

#read data
dat <- read.table(paste0(pathtodata, "Functions/19866_2_Dataset/19866_2_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "EP", plotnam = "BEplotID")
### ===== ###

### === Soil carbon cycling === ###
#Principal Investigator(s):  Trumbore
#                            Leinweber
#                            Bonkowski
#Dataset(s):                 17166_3_Dataset
#                            17026_3_Dataset
#                            14446_19_Dataset
#                            19326_4_Dataset
#                            20010_2_Dataset
#                            14106_2_Dataset
#Process and component name: Soil C cycling
#Relevant columns (unit):    Glu (nmol/(g*h))
#                            N_Ac (nmol/(g*h))
#                            Xyl (nmol/(g*h))
#                            Res_14 (µg/g)
#                            Organic_C (g/kg)
#                            Organic_layer_thickness (cm)
#                            hydrophobicity (ratio)
#                            Cmic (µg/g)

#read data
dat <- read.table(paste0(pathtodata, "Functions/17166_3_Dataset/17166_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/17026_3_Dataset/17026_3_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/14446_19_Dataset/14446_19_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/19326_4_Dataset/19326_4_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/20010_2_Dataset/20010_2_data.txt"), header = T, sep = ";")
dat5 <- read.table(paste0(pathtodata, "Functions/14106_2_Dataset/14106_2_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat5)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "BEplotID")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "BEplotID")
dat3 <- BEplotZeros(dat3, "EP_Plotid", plotnam = "BEplotID")
dat4 <- BEplotZeros(dat4, "EP_Plotid", plotnam = "BEplotID")
dat5 <- BEplotZeros(dat5, "PlotID", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c(5:7,9)], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c(5,6)], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2[,c(7,10)], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4[,c(10,11)], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat5[,c(4,5)], by = "BEplotID", all.x = T)

#special treatment for the added columns
#before merging dat3 to BE_synthesis_forest_dat, Organic_layer_thickness (cm) has to be calculated
#The thickness of the organic layer, aka O-Horizon, is the sum of the three, averaged per plot (from 14 samples), 
#horizon subclass thicknesses of Oi, Oe, Oa. Measured to gain insight in e.g. the decomposition rate of the organic layer.
#therefore, subset forest plots, calculate means per row of the relevant columns of the 19326_4_Dataset
#and sum them up for the Organic_layer_thickness column
dat3 <- dat3 %>% 
  subset( BEplotID %in% BE_synthesis_forest_dat$BEplotID) %>% 
  rowwise( BEplotID) %>% 
  mutate( Organic_layer_thickness = sum(mean(c_across(23:36), na.rm = T),
                                        mean(c_across(37:50), na.rm = T),
                                        mean(c_across(51:64), na.rm = T)))
#merge the Organic_layer_thickness column
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3[,c(93,94)], by = "BEplotID", all.x = T)
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Glu))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$N_Ac))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$Xyl))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$Res_14))) #7 NAs
length(which(is.na(BE_synthesis_forest_dat$Organic_C))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$Organic_layer_thickness))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$hydrophobicity))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$Cmic))) #1 NAs
### ===== ###

### === Phosphatase === ###
#Principal Investigator:     Trumbore
#Dataset(s):                 17166_3_Dataset
#Process and component name: Phosphatase
#Relevant columns (unit):    Pho (nmol/(g*h))

#read data
dat <- read.table(paste0(pathtodata, "Functions/17166_3_Dataset/17166_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c(8,9)], by = "BEplotID", all.x = T)
#special treatment for the added columns
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Pho))) #0 NAs
### ===== ###

### === Habitat === ###
#Principal Investigator:     Polle
#Dataset(s):                 18346_2_Dataset
#Process and component name: Habitat
#Relevant columns (unit):    Concentration_of_Glucose (mg/g)
#                            Concentration_of_Fructose (mg/g)

#read data
dat <- read.table(paste0(pathtodata, "Functions/18346_2_Dataset/18346_2_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c(4:6)], by = "BEplotID", all.x = T)

#special treatment for the added columns
#rename added columns for more context
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Concentration_of_Glucose"] <- "Fine_root_glucose_conc"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Concentration_of_Fructose"] <- "Fine_root_fructose_conc"
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Root_glucose_conc))) #3 NAs
length(which(is.na(BE_synthesis_forest_dat$Root_fructose_conc))) #3 NAs
### ===== ###

### === Productivity === ###
#Principal Investigator:     Schrumpf
#Dataset(s):                 14448_3_Dataset
#Process and component name: Productivity
#Relevant columns (unit):    Fine_Roots_Biomass (g/cm^3)
#                            Coarse_Roots_Biomass (g/cm^3)

#read data
dat <- read.table(paste0(pathtodata, "Functions/14448_3_Dataset/14448_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c(5:7)], by = "BEplotID", all.x = T)
#special treatment for the added columns
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Fine_Roots_Biomass))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$Coarse_Roots_Biomass))) #23 NAs
### ===== ###

### === Herbivory === ###
#Principal Investigator:     Ammer
#                            Weisser
#Dataset(s):                 20347_2_Dataset
#                            12627_2_Dataset
#Process and component name: Herbivory
#Relevant columns (unit):    Bper (%)
#                            Schaden & ohne_Schaden (integer)
#                            Minen & ohne_Minen (integer)
#                            Frass & ohne_Frass (integer)
#                            Saug & ohne_Saug + Phyllaphis & ohne_Phyllaphis (integer)
#                            Gallen & ohne_Gallen + Gallmilben & ohne_Gallmilben (integer)
#                            

#read data
dat <- read.table(paste0(pathtodata, "Functions/20347_2_Dataset/20347_2_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/12627_2_Dataset/12627_2_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat1)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "BEplotID")

#special treatment for the added columns of the 20347_2_Dataset
#before merging dat to BE_synthesis_forest_dat, Browsing_perc_overall, Browsing_perc_broadleaf and Browsing_perc_conifers has to be formatted
#Where total number of saplings was equal to zero, the browsing percentage is given as NA, as there were no saplings to measure the browsing percentage. 
#However, there might be browsing percentage = 0, which is different from NA, as the saplings of the subplot were not subject to herbivory.
#Only consider heightclass == "-1" and tsg == c("all", "BL", "CON")  
#Therefore, subset the relevant rows, pivot_wider and rename columns.
dat.1 <- dat %>% 
  subset( (tsg == "all" & heightclass == -1) | 
          (tsg == "BL" & heightclass == -1) | 
          (tsg == "CON" & heightclass == -1)) %>% 
  pivot_wider( id_cols = c("BEplotID"), names_from = c(tsg), values_from = Bper) 

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1, by = "BEplotID", all.x = T)
#rename added columns for more context
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "all"] <- "Browsing_perc_overall"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "BL"] <- "Browsing_perc_broadleaf"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CON"] <- "Browsing_perc_conifers"
#=#

#special treatment for the added columns of the 12627_2_Dataset
#before merging dat to BE_synthesis_forest_dat, the following variables have to be calculated from the relevant columns.
c("Beech_herbivory_understory_overall", 
  "Beech_herbivory_understory_mining", 
  "Beech_herbivory_understory_chewing",
  "Beech_herbivory_understory_sucking", 
  "Beech_herbivory_understory_galls", 
  "Beech_herbivory_canopy_overall", 
  "Beech_herbivory_canopy_mining", 
  "Beech_herbivory_canopy_chewing", 
  "Beech_herbivory_canopy_sucking", 
  "Beech_herbivory_canopy_galls")
#These variables are the average percentage of sampled beech leaves that show signs of herbivory from different classes of herbivores
#Therefore, calculate the percentage of leaves that show herbivory per beech individual
#and get the average of these percentages for understory and canopy samples per BEplotID
names(dat1)

#calculate percentage of leaves with signs of herbivory per individual
dat1.1 <- dat1 %>% 
  rowwise() %>% 
  mutate( herbivory_overall = 100*(Schaden/sum(Schaden, ohne_Schaden)),
          herbivory_mining = 100*(Minen/sum(Minen, ohne_Minen)),
          herbivory_chewing = 100*(Frass/sum(Frass, ohne_Frass)),
          herbivory_sucking1 = 100*(Saug/sum(Saug, ohne_Saug)),
          herbivory_sucking2 = 100*(Phyllaphis/sum(Phyllaphis, ohne_Phyllaphis)),
          herbivory_galls1 = 100*(Gallen/sum(Gallen, ohne_Gallen)),
          herbivory_galls2 = 100*(Gallmilben/sum(Gallmilben, ohne_Gallmilben)))

#subset Canopy and understorey data
understorey <- dat1.1 %>% 
  subset( Stratum == "understorey")

Canopy <- dat1.1 %>% 
  subset( Stratum == "Canopy")

#calculate average percentage of herbivory per plot for Canopy and understorey #CHECK THIS STEP
summary_understorey <- understorey %>% 
  group_by( BEplotID) %>% 
  summarise( Beech_herbivory_understory_overall = mean(herbivory_overall),
             Beech_herbivory_understory_mining = mean(herbivory_mining),
             Beech_herbivory_understory_chewing = mean(herbivory_chewing),
             Beech_herbivory_understory_sucking = mean(c(herbivory_sucking1, herbivory_sucking2)), #this variable averages percentages of two sucking herbivores
             Beech_herbivory_understory_galls = mean(c(herbivory_galls1, herbivory_galls2))) #this variable averages percentages of two gall herbivores

summary_Canopy <- Canopy %>% 
  group_by( BEplotID) %>% 
  summarise( Beech_herbivory_Canopy_overall = mean(herbivory_overall),
             Beech_herbivory_Canopy_mining = mean(herbivory_mining),
             Beech_herbivory_Canopy_chewing = mean(herbivory_chewing),
             Beech_herbivory_Canopy_sucking = mean(c(herbivory_sucking1, herbivory_sucking2)), #this variable averages percentages of two sucking herbivores
             Beech_herbivory_Canopy_galls = mean(c(herbivory_galls1, herbivory_galls2))) #this variable averages percentages of two gall herbivores

#=#


#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Browsing_perc_overall))) #46 NAs
length(which(is.na(BE_synthesis_forest_dat$Browsing_perc_broadleaf))) #48 NAs
length(which(is.na(BE_synthesis_forest_dat$Browsing_perc_conifers))) #140 NAs
### ===== ###

























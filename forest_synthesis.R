### === Synthesis dataset functions forest assembly === ###

#Author(s): Marc Beringer, Paul Armando Gilmour Rivas Luginbühl, Noëlle Schenk, Caterina Penone
#Date: July, 2023
#Purpose: Assemble dataset for forest ecosystem functions
#         First, generate a dataframe with identifier columns
#         Second, fill this dataframe with relevant rows and columns from individual BE datasets
#         Third, calculate new variables where necessary

### === working directory === ### 
setwd("C:/Users/Marc Beringer/Desktop/R/Synthesis_dataset_functions_forest") #Not necessary for the final script
getwd()
### ===== ###

### === save a snapshot of the R environment === ###
#includes information about versions and packages used
#renv::snapshot()
### ===== ###

### === write table checkpoint === ###
view(BE_synthesis_forest_dat[,c(1,50:57)])
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
                                                        paste("HEW",formatC(1:51, width=2, flag="0"),sep=""), #HEW51 was established in 2017, after HEW02 was destroyed in 2016.
                                                        paste("HEG",formatC(1:50, width=2, flag="0"),sep="")),
                                                        exploratory=c(rep("ALB",100),rep("SCH",100),rep("HAI",101))) %>% 
                                                        mutate( habitat = 
                                                                  ifelse(grepl(pattern = "W", BEplotID), "forest", "grassland"))

#filter for forest habitat
BE_synthesis_identifier_dat <- BE_synthesis_identifier_dat %>% subset( habitat == "forest")
### ===== ###

### === Root decomposition === ### 
#Principal Investigator:     Schrumpf
#Dataset(s):                 16666_2_Dataset
#Process and component name: Root decomposition
#Relevant columns (unit):    Mass_loss_October_2012 (%)

#read data
dat <- read.table(paste0(pathtodata, "Functions/16666_2_Dataset/16666_2_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_identifier_dat, dat[,c("Mass_loss_October_2012","BEplotID")], by = "BEplotID", all.x = T)
#special treatment for the added columns
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Mass_loss_October_2012))) #16 NAs
### ===== ###

### === Nitrogen availability === ###
#Principal Investigator(s):  Schloter
#                            Trumbore
#                            Bonkowski
#                            Schrumpf
#Dataset(s):                 19847_3_Dataset
#                            14446_19_Dataset
#                            20040_3_Dataset
#                            20045_3_Dataset
#                            14106_2_Dataset
#                            21546_2_Dataset
#                            24346_3_Dataset
#Process and component name: N availability
#Relevant columns (unit):    Potential_nitrification_rate (ng/g)
#                            Ammonium_NH4 (µg/g)
#                            Nitrate_NO3 (mg/g)
#                            CN_ratio (Organic C/Total N) (14446_19_Dataset - Upper mineral soil 2011)
#                            CN_ratio (Total_C/Total_N) (20040_3_Dataset - Organic Horizon soil 2011)
#                            CN_ratio (Total_C/Total_N) (20045_3_Dataset - Organic Horizon soil 2014)
#                            Nmic (µg/g)
#                            amoA_AOA
#                            amoA_AOB
#                            nxrA_NB
#                            X16S_NS
#                            potential_nitrification (ng/(g*h))
#                            Ammonium_NH4 (µg/g) (21546_2_Dataset - 2016) 
#                            Nitrate_NO3 (mg/g) (21546_2_Dataset - 2016)
#                            CN_ratio (Total_C/Total_N) (24346_3_Dataset - Organic Horizon soil 2017)

#TODO a composite variable about Nitrate and Ammonia fluxes can be calculated here.

#read data
dat <- read.table(paste0(pathtodata, "Functions/19847_3_Dataset/19847_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/14446_19_Dataset/14446_19_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/20040_3_Dataset/20040_3_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/20045_3_Dataset/20045_3_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/14106_2_Dataset/14106_2_data.txt"), header = T, sep = ";")
dat5 <- read.table(paste0(pathtodata, "Functions/21546_2_Dataset/21546_2_data.txt"), header = T, sep = ";")
dat6 <- read.table(paste0(pathtodata, "Functions/24346_3_Dataset/24346_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat6)
dat <- BEplotZeros(dat, "Plot_ID", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "BEplotID")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "BEplotID")
dat3 <- BEplotZeros(dat3, "EP_Plotid", plotnam = "BEplotID")
dat4 <- BEplotZeros(dat4, "PlotID", plotnam = "BEplotID")
dat5 <- BEplotZeros(dat5, "Plot_ID", plotnam = "BEplotID")
dat6 <- BEplotZeros(dat6, "EP_Plotid", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("PNR","NH4","NO3","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("CN_ratio","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4[,c("Nmic","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat5[,c("amoA_AOA","amoA_AOB","nxrA_NB","X16S_NS","pN","NH4.N","NO3.N","DC","BEplotID")], by = "BEplotID", all.x = T)

#special treatment for the added columns of the 14446_19_Dataset
#rename columns
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CN_ratio"] <- "Upper_MinSoil_CN_ratio_2011"
#=#

#special treatment for the added columns of the 19847_3_Dataset
#rename columns
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "PNR"] <- "Potential_nitrification_rate_2014"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "NH4"] <- "Ammonium_NH4_2014"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "NO3"] <- "Nitrate_NO3_2014"
#=#

#special treatment for the relevant columns of the 20040_3_Dataset, 20045_3_Dataset and 24346_3_Dataset
#before merging to BE_synthesis_forest_dat, O_Horizon_CN_ratio_2011, 2014 and 2017 has to be calculated
#Each BEplotID has three measurements, one per O-Horizon subclass (Oi, Oe, Oa).
#Average the three O-Horizon subclasses per BEplotID to make the variable comparable to the CN_ratio of the upper mineral soil layer from the 14446_19_Dataset.
#Therefore, extract the relevant columns, pivot_wider and get rowwise averages of the O-Horizon subclass columns.
dat2.1 <- dat2[,c("Horizon","CN_ratio","BEplotID")] %>% 
  pivot_wider( names_from = c(Horizon), values_from = CN_ratio) %>% 
  rowwise( BEplotID) %>% 
  mutate( O_Horizon_CN_ratio_2011 = mean(c_across(1:3), na.rm = T))

dat3.1 <- dat3[,c("Horizon","CN_ratio","BEplotID")] %>% 
  pivot_wider( names_from = c(Horizon), values_from = CN_ratio) %>% 
  rowwise( BEplotID) %>% 
  mutate( O_Horizon_CN_ratio_2014 = mean(c_across(1:3), na.rm = T))

dat6.1 <- dat6[,c("Horizon","CN_ratio","BEplotID")] %>% 
  pivot_wider( names_from = c(Horizon), values_from = CN_ratio) %>% 
  rowwise( BEplotID) %>% 
  mutate( O_Horizon_CN_ratio_2017 = mean(c_across(1:3), na.rm = T))

#merge the O_Horizon_CN_ratio_2011, 2014 columns and 2017
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2.1[,c("BEplotID","O_Horizon_CN_ratio_2011")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3.1[,c("BEplotID","O_Horizon_CN_ratio_2014")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat6.1[,c("BEplotID","O_Horizon_CN_ratio_2017")], by = "BEplotID", all.x = T)
#=#

#special treatment for the added columns of the 21546_2_Dataset
#rename columns
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "pN"] <- "potential_nitrification_2016" #TODO unclear whether this can be the final variable name
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "NH4.N"] <- "Ammonium_NH4_2016"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "NO3.N"] <- "Nitrate_NO3_2016"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "DC"] <- "Dissolved_Carbon"
#=#

#special treatment for the added columns
#instead of values some rows contain the string "bdl", or NaN
#replace these with NA
BE_synthesis_forest_dat[BE_synthesis_forest_dat == "bdl"] <- NA
BE_synthesis_forest_dat[BE_synthesis_forest_dat == "NaN"] <- NA
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Potential_nitrification_rate_2014))) #11 NAs
length(which(is.na(BE_synthesis_forest_dat$Ammonium_NH4_2014))) #30 NAs
length(which(is.na(BE_synthesis_forest_dat$Nitrate_NO3_2014))) #29 NAs
length(which(is.na(BE_synthesis_forest_dat$Upper_MinSoil_CN_ratio_2011))) #1 NAs 
length(which(is.na(BE_synthesis_forest_dat$O_Horizon_CN_ratio_2011))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$O_Horizon_CN_ratio_2014))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Nmic))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$amoA_AOA))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$amoA_AOB))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$nxrA_NB))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$X16S_NS))) #12 NAs
length(which(is.na(BE_synthesis_forest_dat$potential_nitrification_2016))) #11 NAs
length(which(is.na(BE_synthesis_forest_dat$Ammonium_NH4_2016))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Nitrate_NO3_2016))) #7 NAs
length(which(is.na(BE_synthesis_forest_dat$Dissolved_Carbon))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$O_Horizon_CN_ratio_2017))) #1 NAs
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
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("OlsenPi","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("NaHCO3_Pi","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2[,c("Resin_P","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3[,c("Pmic","BEplotID")], by = "BEplotID", all.x = T)
#special treatment for the added columns
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$OlsenPi))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$NaHCO3_Pi))) #20 NAs
length(which(is.na(BE_synthesis_forest_dat$Resin_P))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Pmic))) #2 NAs
### ===== ###

### === Sulfur availability === ###
#Principal Investigator:     Trumbore
#                            Schrumpf
#Dataset(s):                 20045_3_Dataset
#                            24346_3_Dataset
#Process and component name: S availability
#Relevant columns (unit):    CS_ratio (Total_C/Total_S) (20045_3_Dataset - Organic Horizon soil 2014)
#                            CS_ratio (Total_C/Total_S) (24346_3_Dataset - Organic Horizon soil 2017)

#read data
dat <- read.table(paste0(pathtodata, "Functions/20045_3_Dataset/20045_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/24346_3_Dataset/24346_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat1)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "BEplotID")

#special treatment for the added columns
#before merging dat to BE_synthesis_forest_dat, O_Horizon_CS_ratio_2014 and 2017 has to be calculated
#Each BEplotID has three measurements, one per O-Horizon subclass (Oi, Oe, Oa).
#Average the three O-Horizon subclasses per BEplotID to make the variable comparable to the O_Horizon_CN_ratio_2014 variable.
#Therefore, extract the relevant columns, pivot_wider and get rowwise averages of the O-Horizon subclass columns.
dat.1 <- dat[,c("Horizon","CS_ratio","BEplotID")] %>% 
  pivot_wider( names_from = c(Horizon), values_from = CS_ratio) %>% 
  rowwise( BEplotID) %>% 
  mutate( O_Horizon_CS_ratio_2014 = mean(c_across(1:3), na.rm = T))

dat1.1 <- dat1[,c("Horizon","CS_ratio","BEplotID")] %>% 
  pivot_wider( names_from = c(Horizon), values_from = CS_ratio) %>% 
  rowwise( BEplotID) %>% 
  mutate( O_Horizon_CS_ratio_2017 = mean(c_across(1:3), na.rm = T))
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("BEplotID","O_Horizon_CS_ratio_2014")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1.1[,c("BEplotID","O_Horizon_CS_ratio_2017")], by = "BEplotID", all.x = T)
#=#
view(BE_synthesis_forest_dat[,c(1,52)])
#the destroyed HEW02 BEplotID contains the string NaN
#replace with NA
BE_synthesis_forest_dat[BE_synthesis_forest_dat == "NaN"] <- NA
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$O_Horizon_CS_ratio_2014))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$O_Horizon_CS_ratio_2017))) #1 NAs
### ===== ###

### === Dung decomposition === ### UNSOLVED QUESTIONS
#Principal Investigator:     Blüthgen
#Dataset(s):                 19866_2_Dataset
#Process and component name: Dung decomposition
#Relevant columns (unit):    removal_g (g)
#                            removal_rate (%)

#Notes: This variable is present in the grassland functions synthesis dataset. Ask Noëlle about calculations.
#TODO scale(, center = TRUE) after dungtype

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
#                            Xylanase (nmol/(g*h))
#                            Res_14 (µg/g)
#                            Organic_C (g/kg)
#                            Organic_layer_thickness (cm)
#                            hydrophobicity (ratio)
#                            Cmic (µg/g)

#TODO a composite variable about Soil Carbon Fluxes can be calculated here.

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
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("Glu","N_Ac","Xyl","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("Res_14","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2[,c("Organic_C","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4[,c("hydrophobicity","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat5[,c("Cmic","BEplotID")], by = "BEplotID", all.x = T)

#special treatment for the added columns of the 17166_3_Dataset
#rename the Xyl column into Xylanase to avoid confusion with the "Xylosidase" data used in the synthesis functions grasslands dataset 
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Xyl"] <- "Xylanase"
#=#

#special treatment for the added columns of the 19326_4_Dataset
#before merging dat3 to BE_synthesis_forest_dat, Organic_layer_thickness (cm) has to be calculated
#The thickness of the organic layer, aka O-Horizon, is the sum of the three, averaged per plot (from 14 samples), 
#horizon subclass thicknesses of Oi, Oe, Oa. Measured to gain insight in e.g. the decomposition rate of the organic layer.
#therefore, subset forest plots, calculate means per row of the relevant columns of the 19326_4_Dataset
#and sum them up to get the Organic_layer_thickness variable
dat3.1 <- dat3 %>% 
  subset( BEplotID %in% BE_synthesis_forest_dat$BEplotID) %>% 
  rowwise( BEplotID) %>% 
  mutate( Organic_layer_thickness = sum(mean(c_across("Thickness_Oi_1":"Thickness_Oi_14"), na.rm = T),
                                        mean(c_across("Thickness_Oe_1":"Thickness_Oe_14"), na.rm = T),
                                        mean(c_across("Thickness_Oa_1":"Thickness_Oa_14"), na.rm = T)))

#merge the Organic_layer_thickness column
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3.1[,c("Organic_layer_thickness","BEplotID")], by = "BEplotID", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Glu))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$N_Ac))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Xylanase))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Res_14))) #7 NAs
length(which(is.na(BE_synthesis_forest_dat$Organic_C))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Organic_layer_thickness))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$hydrophobicity))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Cmic))) #2 NAs
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
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("Pho","BEplotID")], by = "BEplotID", all.x = T)
#special treatment for the added columns
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Pho))) #1 NAs
### ===== ###

### === Habitat === ###
#Principal Investigator:     Polle
#Dataset(s):                 18346_2_Dataset
#Process and component name: Habitat
#Relevant columns (unit):    Concentration_of_Glucose (mg/g)
#                            Concentration_of_Fructose (mg/g)

#TODO check whether glucose and fructose can be summed up for a root sugar concentration variable

#read data
dat <- read.table(paste0(pathtodata, "Functions/18346_2_Dataset/18346_2_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("Concentration_of_Glucose","Concentration_of_Fructose","BEplotID")], by = "BEplotID", all.x = T)

#special treatment for the added columns
#rename added columns for more context
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Concentration_of_Glucose"] <- "Fine_root_glucose_conc"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Concentration_of_Fructose"] <- "Fine_root_fructose_conc"
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Fine_root_glucose_conc))) #4 NAs
length(which(is.na(BE_synthesis_forest_dat$Fine_root_fructose_conc))) #4 NAs
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

#special treatment for the added columns of the 14448_3_Dataset
#before merging sum up the Fine_Roots_Biomass and Coarse_Roots_Biomass to get Root_Biomass (g/cm^3)
#if Fine_Roots_Biomass, or Coarse_Roots_Biomass is NA, Root_Biomass is simply the other value
dat.1 <- dat %>% 
  rowwise( ) %>% 
  mutate( Root_Biomass = sum(c(Fine_Roots_Biomass, Coarse_Roots_Biomass), na.rm = TRUE))

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("Root_Biomass","BEplotID")], by = "BEplotID", all.x = T)
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Root_Biomass))) #1 NAs
### ===== ###

### === Herbivory === ###
#Principal Investigator:     Ammer
#                            Weisser
#Dataset(s):                 20347_2_Dataset
#                            12627_2_Dataset
#                            18567_2_Dataset
#Process and component name: Herbivory
#Relevant columns (unit):    Bper (%)
#                            Schaden & ohne_Schaden (integer)
#                            Minen & ohne_Minen (integer)
#                            Frass & ohne_Frass (integer)
#                            Saug & ohne_Saug + Phyllaphis & ohne_Phyllaphis (integer)
#                            Gallen & ohne_Gallen + Gallmilben & ohne_Gallmilben (integer)
#                            Damage_class (three infestation classes: 1 == no visible attack; 2 == weak attack; 3 == heavy attack)                            

#read data
dat <- read.table(paste0(pathtodata, "Functions/20347_2_Dataset/20347_2_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/12627_2_Dataset/12627_2_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/18567_2_Dataset/18567_2_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat2)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "BEplotID")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "BEplotID")

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
c("Beech_herbivory_understorey_overall", 
  "Beech_herbivory_understorey_mining", 
  "Beech_herbivory_understorey_chewing",
  "Beech_herbivory_understorey_sucking", 
  "Beech_herbivory_understorey_galls", 
  "Beech_herbivory_Canopy_overall", 
  "Beech_herbivory_Canopy_mining", 
  "Beech_herbivory_Canopy_chewing", 
  "Beech_herbivory_Canopy_sucking", 
  "Beech_herbivory_Canopy_galls")
#These variables are the average percentage of sampled beech leaves that show signs of herbivory from different classes of herbivores
#Therefore, calculate the percentage of leaves that show herbivory per beech individual
#and get the average of these percentages for understory and canopy samples per BEplotID
#note that herbivory from sucking and gall herbivores averages the herbivory percentages of two columns from the raw data

#do all of that in a neat pipeline
dat1.1 <- dat1 %>% 
  #calculate percentage of leaves with signs of herbivory per individual sample (row)
  rowwise( ) %>% 
  mutate( herbivory_overall = (Schaden/sum(Schaden, ohne_Schaden)*100),
          herbivory_mining = (Minen/sum(Minen, ohne_Minen)*100),
          herbivory_chewing = (Frass/sum(Frass, ohne_Frass)*100),
          herbivory_sucking1 = (Saug/sum(Saug, ohne_Saug)*100),
          herbivory_sucking2 = (Phyllaphis/sum(Phyllaphis, ohne_Phyllaphis)*100),
          herbivory_galls1 = (Gallen/sum(Gallen, ohne_Gallen)*100),
          herbivory_galls2 = (Gallmilben/sum(Gallmilben, ohne_Gallmilben)*100)) %>% 
  #calculate average percentages for Canopy and understorey samples per BEplotID
  group_by( Stratum, BEplotID) %>% 
  summarise( overall = mean(herbivory_overall),
             mining = mean(herbivory_mining),
             chewing = mean(herbivory_chewing),
             sucking = mean(c(herbivory_sucking1, herbivory_sucking2)), #this variable is the average across two columns of herbivory data
             galls = mean(c(herbivory_galls1, herbivory_galls2))) %>%   #this variable is the average across two columns of herbivory data
  #pivot the Canopy and understorey averages wider and assign the final variable names to prepare the dataframe for merging
  pivot_wider( names_from = Stratum, names_glue = "Beech_herbivory_{Stratum}_{.value}", values_from = c(overall, mining, chewing, sucking, galls))

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1.1, by = "BEplotID", all.x = T)
#=#

#special treatment for the added columns of the 18567_2_Dataset
#there are five replicates for Cryptococcus infestation per BEplotID
#take the median of the damage classes per BEplotID
dat2.1 <- dat2 %>% 
  group_by( BEplotID) %>% 
  summarise( Cryptococcus_infestation = median(Damage_class, na.rm = TRUE))

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2.1, by = "BEplotID", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Browsing_perc_overall))) #47 NAs
length(which(is.na(BE_synthesis_forest_dat$Browsing_perc_broadleaf))) #49 NAs
length(which(is.na(BE_synthesis_forest_dat$Beech_herbivory_understorey_overall))) #70 NAs
length(which(is.na(BE_synthesis_forest_dat$Beech_herbivory_understorey_mining))) #70 NAs
length(which(is.na(BE_synthesis_forest_dat$Beech_herbivory_understorey_chewing))) #70 NAs
length(which(is.na(BE_synthesis_forest_dat$Beech_herbivory_understorey_sucking))) #70 NAs
length(which(is.na(BE_synthesis_forest_dat$Beech_herbivory_understorey_galls))) #70 NAs
length(which(is.na(BE_synthesis_forest_dat$Beech_herbivory_Canopy_overall))) #47 NAs
length(which(is.na(BE_synthesis_forest_dat$Beech_herbivory_Canopy_mining))) #47 NAs
length(which(is.na(BE_synthesis_forest_dat$Beech_herbivory_Canopy_chewing))) #47 NAs
length(which(is.na(BE_synthesis_forest_dat$Beech_herbivory_Canopy_sucking))) #47 NAs
length(which(is.na(BE_synthesis_forest_dat$Beech_herbivory_Canopy_galls))) #47 NAs
length(which(is.na(BE_synthesis_forest_dat$Cryptococcus_infestation))) #47 NAs
### ===== ###

### === Nutrient cycling === ###
#Principal Investigator:     Polle
#                            Schrumpf
#                            Trumbore
#Dataset(s):                 19230_3_Dataset
#                            14567_5_Dataset
#                            26908_4_Dataset
#Process and component name: Nutrient cycling
#Relevant columns (unit):    Fine_Roots_Carbon (mg/g)
#                            Fine_Roots_Nitrogen (mg/g)
#                            Fine_roots_CN_ratio (ratio from Total_C and Total_N that have the unit percentage (%))
#                            Soil_respiration_2018 (g/(m^2*d))
#                            Soil_respiration_2019 (g/(m^2*d))

#TODO check how the Fine_roots_CN_ratio is related to soil CN_ratios and whether these are comparable.
#TODO check why Total_C and Total_N (used to calculate the Fine_roots_CN_ratio), weren't included here.
#TODO check whether Soil_respiration_2018 and 2019 can be combined for a composite Soil_respiration variable

#read data
dat <- read.table(paste0(pathtodata, "Functions/19230_3_Dataset/19230_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/14567_5_Dataset/14567_5_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/26908_4_Dataset/26908_4_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat2)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "BEplotID")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "BEplotID")

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("Carbon_within_fine_roots_Soil_Sampling_May_2011", 
                                                                 "Nitrogen_within_fine_roots_Soil_Sampling_May_2011",
                                                                 "BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("CN_ratio", "BEplotID")], by = "BEplotID", all.x = T)

#special treatment for the added columns of the 19230_3_Dataset and 14567_5_Dataset
#rename columns
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Carbon_within_fine_roots_Soil_Sampling_May_2011"] <- "Fine_Roots_Carbon"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Nitrogen_within_fine_roots_Soil_Sampling_May_2011"] <- "Fine_Roots_Nitrogen"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CN_ratio"] <- "Fine_roots_CN_ratio"
#=#

#special treatment for the added columns of the 26908_4_Dataset
#Soil_respiration was measured in 2018 and 2019. Create Soil_respiration_2018 and Soil_respiration_2019 columns.
dat2.1 <- dat2 %>%
  subset( Year == "2018") %>% 
  pivot_wider( names_from = Year, names_glue = "Soil_respiration_{Year}", values_from = Rs)

dat2.2 <- dat2 %>%
  subset( Year == "2019") %>% 
  pivot_wider( names_from = Year, names_glue = "Soil_respiration_{Year}", values_from = Rs)

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2.1[,c("Soil_respiration_2018", "BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2.2[,c("Soil_respiration_2019", "BEplotID")], by = "BEplotID", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Pho))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Fine_Roots_Carbon))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$Fine_Roots_Nitrogen))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$Fine_roots_CN_ratio))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$Soil_respiration_2018))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Soil_respiration_2019))) #1 NAs
### ===== ###



















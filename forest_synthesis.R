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
#Currently variables are added until "Cryptococcus_infestation" in the "Herbivory" Process
#Add data from the large herbivory dataset before adding variables from the "Nutrient cycling" Process
view(BE_synthesis_forest_dat[,c(1:4,60:65)])
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

### === dependencies === ###
### === Calculate mini-multifunctionality compound variables from individual ecosystem functions (i.e. variables)
#' multidiv() function 
#' @author Eric Allan
#' @param x Input data frame or matrix. Rows are different diversities/functions for multifun/div calculation. 
#' Columns are replicates, e.g. plots.
source("multidiversity.R") 
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
#=#

### === function to plot pairwise correlations
#' adapted from https://r-coder.com/correlation-plot-r/
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- cor(x, y, method = "pearson")
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}
### ===== ###

### === path to data === ###
#Path to raw data on the server of the Institute of Plant Sciences at the University of Bern (Switzerland)
#Unzip the Datasets first
pathtodata <- "P:/PROJECTS/Exploratories Synthesis/Data/Forest_functions/dataset_creation/raw_data/"
### ===== ###

### === data assembly === ###
#create an empty dataframe with identifier columns (BEplotID, EP_Plotid, exploratory and habitat), which will be filled with relevant columns later
#BEplotID is the two-digit plot identifier (e.g. AEW01) that will be used for merging, while EP_Plotid is the classic identifier (e.g. AEW1)
BE_synthesis_identifier_dat  <- data.frame(BEplotID = c(paste("AEW", formatC(1:50, width = 2, flag = "0"), sep = ""),
                                                        paste("AEG", formatC(1:50, width = 2, flag = "0"), sep = ""),
                                                        paste("SEW", formatC(1:50, width = 2, flag = "0"), sep = ""),
                                                        paste("SEG", formatC(1:50, width = 2, flag = "0"), sep = ""),
                                                        paste("HEW", formatC(1:51, width = 2, flag = "0"), sep = ""), #HEW51 was established in 2017, after HEW02 was destroyed in 2016.
                                                        paste("HEG", formatC(1:50, width = 2, flag = "0"), sep = "")),
                                           EP_Plotid = c(paste("AEW", formatC(1:50), sep = ""),
                                                         paste("AEG", formatC(1:50), sep = ""),
                                                         paste("SEW", formatC(1:50), sep = ""),
                                                         paste("SEG", formatC(1:50), sep = ""),
                                                         paste("HEW", formatC(1:51), sep = ""), #HEW51 was established in 2017, after HEW02 was destroyed in 2016.
                                                         paste("HEG", formatC(1:50), sep = "")),
                                           exploratory = c(rep("ALB", 100), rep("SCH", 100), rep("HAI", 101))) %>% 
                                           mutate( habitat = ifelse(grepl(pattern = "W", BEplotID), "forest", "grassland"))

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
names(dat5)
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

#special treatment for the added columns of the 14446_19_Dataset, 19847_3_Dataset and 21546_2_Dataset
#rename columns
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "PNR"] <- "Potential_nitrification_rate_2014"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "NH4"] <- "Ammonium_NH4_2014"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "NO3"] <- "Nitrate_NO3_2014"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CN_ratio"] <- "Upper_MinSoil_CN_ratio_2011"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "pN"] <- "potential_nitrification_2016" #TODO unclear whether this can be the final variable name
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "NH4.N"] <- "Ammonium_NH4_2016"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "NO3.N"] <- "Nitrate_NO3_2016"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "DC"] <- "Dissolved_Carbon"
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
length(which(is.na(BE_synthesis_forest_dat$Dissolved_Carbon))) #1 NAs #TODO MOVE THIS VARIABLE TO SOIL CARBON CYCLING
length(which(is.na(BE_synthesis_forest_dat$O_Horizon_CN_ratio_2017))) #1 NAs
### ===== ###

### === Phosphorus availability === ###
#Principal Investigator:     Oelmann
#                            Schrumpf
#Dataset(s):                 19286_3_Dataset
#                            5241_5_Dataset
#                            19009_3_Dataset
#                            15766_3_Dataset
#                            31340_4_Dataset
#                            19366_4_Dataset
#Process and component name: P availability
#Relevant columns (unit):    OlsenPi (mg/kg) (19286_3_Dataset - OlsenPi_2014)
#                            NaHCO3_Pi (mg/kg)
#                            Resin_P (mg/kg)
#                            Pmic (mg/kg)
#                            Olsen-P (mg/kg) (31340_4_Dataset - OlsenPi_2021)
#                            CP_ratio (organic C/total P)
#                            PS_ratio (total P/total S)

#read data
dat <- read.table(paste0(pathtodata, "Functions/19286_3_Dataset/19286_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/5241_5_Dataset/5241_5_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/19009_3_Dataset/19009_3_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/15766_3_Dataset/15766_3_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/31340_4_Dataset/31340_4_data.txt"), header = T, sep = ";")
dat5 <- read.table(paste0(pathtodata, "Functions/19366_4_Dataset/19366_4_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat5)
dat <- BEplotZeros(dat, "Plot_ID", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "EP", plotnam = "BEplotID")
dat2 <- BEplotZeros(dat2, "EP", plotnam = "BEplotID")
dat3 <- BEplotZeros(dat3, "EP", plotnam = "BEplotID")
dat4 <- BEplotZeros(dat4, "EP_Plotid", plotnam = "BEplotID")
dat5 <- BEplotZeros(dat5, "EP_Plotid", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("OlsenPi","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("NaHCO3_Pi","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2[,c("Resin_P","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3[,c("Pmic","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4[,c("Olsen.P","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat5[,c("CP_ratio","PS_ratio","BEplotID")], by = "BEplotID", all.x = T)

#special treatment for the added columns
#rename columns
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "OlsenPi"] <- "OlsenPi_2014"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "NaHCO3_Pi"] <- "NaHCO3_Pi_2008"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Olsen.P"] <- "OlsenPi_2021"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CP_ratio"] <- "Upper_MinSoil_CP_ratio_2014"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "PS_ratio"] <- "Upper_MinSoil_PS_ratio_2014"
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$OlsenPi_2014))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$NaHCO3_Pi_2008))) #20 NAs
length(which(is.na(BE_synthesis_forest_dat$Resin_P))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Pmic))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$OlsenPi_2021))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Upper_MinSoil_CP_ratio_2014))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Upper_MinSoil_PS_ratio_2014))) #1 NAs
### ===== ###

### === Sulfur availability === ###
#Principal Investigator:     Trumbore
#                            Schrumpf
#Dataset(s):                 20045_3_Dataset
#                            24346_3_Dataset
#                            19366_4_Dataset
#Process and component name: S availability
#Relevant columns (unit):    CS_ratio (Total_C/Total_S) (20045_3_Dataset - Organic Horizon soil 2014)
#                            CS_ratio (Total_C/Total_S) (24346_3_Dataset - Organic Horizon soil 2017)
#                            CS_ratio (Total_C/Total_S) (19366_4_Dataset - Upper Mineral soil 2014)

#read data
dat <- read.table(paste0(pathtodata, "Functions/20045_3_Dataset/20045_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/24346_3_Dataset/24346_3_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/19366_4_Dataset/19366_4_data.txt"), header = T, sep = ";") #TODO
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat2)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "BEplotID")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2[,c("BEplotID","CS_ratio")], by = "BEplotID", all.x = T)

#special treatment for the added columns of the 19366_4_Dataset
#rename columns
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CS_ratio"] <- "Upper_MinSoil_CS_ratio_2014"
#=#

#special treatment for the added columns of the 20045_3_Dataset and 24346_3_Dataset
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

#the destroyed HEW02 BEplotID contains the string NaN
#replace with NA
BE_synthesis_forest_dat[BE_synthesis_forest_dat == "NaN"] <- NA
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$O_Horizon_CS_ratio_2014))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$O_Horizon_CS_ratio_2017))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Upper_MinSoil_CS_ratio_2014))) #1 NAs
### ===== ###

### === Dung decomposition === ###
#Principal Investigator:     Blüthgen
#Dataset(s):                 21206_3_Dataset
#                            24966_3_Dataset
#Process and component name: Dung decomposition
#Relevant columns (unit):    removal_g (g)
#                            dung_depletion (proportion of removed dung)

#read data
dat <- read.table(paste0(pathtodata, "Functions/21206_3_Dataset/21206_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/24966_3_Dataset/24966_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat1)
dat <- BEplotZeros(dat, "EP", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "Plot", plotnam = "BEplotID")

#special treatment of the removal_g column of the 21206_3_Dataset
#as in the synthesis dataset functions grassland, here we only use samples gathered in summer 2014 (i.e. June, July and August)
#since samples from May 2014 were not collected at all BEplotIDs and samples from April and July 2015 were only collected at 36 out of 300 BEplotIDs
#then we apply the same calculation as in the grassland dataset by first scaling each dungtype before averaging them per BEplotID
#so that each dungtype (e.g. Cow, Sheep etc.) across the 150 BEplotIDs has a mean of 0 and a standard deviation of 1

#first add the habitat column to enable selection of forest BEplotIDs only
dat.1 <- merge(dat, BE_synthesis_identifier_dat, by = "BEplotID", all.x = T)

dat.2 <- dat.1 %>% 
  #select summer 2014 samples in forest BEplotIDs
  subset( habitat == "forest" & (month == "June_2014" | month == "July_2014" | month == "August_2014")) %>% 
  #scale removal_g across the range of each dungtype individually, to make dungtypes comparable to each other (i.e. Cow comparable with Sheep)
  group_by( dungtype) %>% 
  mutate( scaled_removal_g = scale(removal_g, center = T, scale = T)) %>% 
  #then calculate the mean of the scaled_removal_g for each BEplotID
  group_by( BEplotID) %>% 
  summarise( dung_removal_2014 = mean(scaled_removal_g))

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.2[,c("BEplotID","dung_removal_2014")], by = "BEplotID", all.x = T)
#=#

#special treatment for the dung_depletion column of the 24966_3_Dataset
#dung_depletion has up to five replicates per BEplotID, simply average those 
#as has been done for the synthesis dataset functions grassland (27087_24_Dataset)
dat1.1 <- dat1 %>% 
  subset( Habitat == "F") %>% 
  group_by( BEplotID) %>% 
  #rename dung_depletion to dung_removal_2017 for consistency with the grassland functions dataset
  summarise( dung_removal_2017 = mean(dung_depletion, na.rm = T)) 

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1.1[,c("BEplotID","dung_removal_2017")], by = "BEplotID", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$dung_removal_2014))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$dung_removal_2017))) #2 NAs
#=#

#TODO Can dung_removal_2014 and dung_removal_2017 be used as temporal replicates of dung decomposition in the forest BEplotIDs?
#The dataset of dung_depletion for 2017 (24966_Dataset), which only contains the dungtype "Cow".
#do pairwise correlations of dungtypes of the 19866_2_Dataset, if dungtypes correlate well, 
#then it doesn't matter what dungtype we're looking at and we can compare the 2017 dung removal with the 2014 data from forests and in the grassland dataset
#doesn't work if NAs are in the data

dungtype.matrix <- dat %>% 
  #select only forest BEplotIDs and remove May 2014 samples
  subset( habitat == "Forest" & !(date == "May_2014")) %>% 
  #scale removal_g across the range of each dungtype individually, to make dungtypes comparable to each other (i.e. Cow comparable with Sheep)
  group_by( dungtype) %>% 
  mutate( scaled_removal_g = scale(removal_g, center = T, scale = T)) %>%
  pivot_wider( id_cols = "BEplotID" , names_from = dungtype, values_from = scaled_removal_g)

#plot pairwise correlations
#dungtype "Cow" has one NA, therefore omit this NA
#Conclusion: Dungtypes do not correlate well (< 0.45), we cannot compare dung removal averaged across dungtypes 2014 with only cow dung removal 2017
pairs(na.omit(dungtype.matrix[,-c(1)]),
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Smoothed regression lines
#=#

#Does dung_depletion 2017 correlate with removal_g of dungtype Cow in the 2014 data?
#add the 2017 Cow dung depletion data to the dung removal data from 2014
#also add the scaled and average dung_removal_2014 to check if it can be compared to dung_removal_2017
dungtype.matrix <- merge(dungtype.matrix, dat1.1, by = "BEplotID", all.x = T)
dungtype.matrix <- merge(dungtype.matrix, BE_synthesis_forest_dat[,c("BEplotID", "dung_removal")], by = "BEplotID", all.x = T)

#plot pairwise correlations
#Conclusion: dung_removal_2014 of the dungtype "Cow" correlates with dung_removal_2017 at r = 0.45, not very good.
#Conclusion: dung_removal_2014 however correlates a bit better with dung_removal_2017 at r = 0.58, might be used as temporal replicate, even if it's a different unit.
pairs(na.omit(dungtype.matrix[,-c(1)]),
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Smoothed regression lines
### ===== ###

### === Soil carbon cycling === ###
#Principal Investigator(s):  Trumbore
#                            Leinweber
#                            Bonkowski
#                            Schrumpf
#Dataset(s):                 17166_3_Dataset
#                            14446_19_Dataset
#                            19326_4_Dataset
#                            20010_2_Dataset
#                            14106_2_Dataset
#                            23906_7_Dataset #THIS DATASET MIGHT CHANGE DUE TO AN UPDATE ON BExis
#Process and component name: Soil C cycling
#Relevant columns (unit):    Glu (nmol/(g*h)) (17166_3_Dataset - Mineral Soil Enzyme Activities 2011)
#                            N_Ac (nmol/(g*h)) (17166_3_Dataset - Mineral Soil Enzyme Activities 2011)
#                            Xyl (nmol/(g*h)) (17166_3_Dataset - Mineral Soil Enzyme Activities 2011)
#                            Organic_C (g/kg)
#                            Organic_layer_thickness (cm)
#                            hydrophobicity (ratio)
#                            Cmic (µg/g)
#                            Glu (nmol/(g*h)) (23906_7_Dataset - Mineral Soil Enzyme Activities 2014)
#                            N_Ac (nmol/(g*h)) (23906_7_Dataset - Mineral Soil Enzyme Activities 2014)
#                            Xyl (nmol/(g*h)) (23906_7_Dataset - Mineral Soil Enzyme Activities 2014) #NOT AVAILABLE YET

#TODO calculate soilCflxs_2014, wait until BExis raw data is updated and Xyl_2014 becomes available

#read data
dat <- read.table(paste0(pathtodata, "Functions/17166_3_Dataset/17166_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/14446_19_Dataset/14446_19_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/19326_4_Dataset/19326_4_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/20010_2_Dataset/20010_2_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/14106_2_Dataset/14106_2_data.txt"), header = T, sep = ";")
dat5 <- read.table(paste0(pathtodata, "Functions/23906_7_Dataset/23906_7_data.txt"), header = T, sep = ";") #THIS DATASET MIGHT CHANGE DUE TO AN UPDATE ON BExis
#add two-digit plot names for merging with the BE_synthesis_forest_dat
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "BEplotID")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "BEplotID")
dat3 <- BEplotZeros(dat3, "EP_Plotid", plotnam = "BEplotID")
dat4 <- BEplotZeros(dat4, "PlotID", plotnam = "BEplotID")
dat5 <- BEplotZeros(dat5, "EP_Plotid", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("Organic_C","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3[,c("hydrophobicity","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4[,c("Cmic","BEplotID")], by = "BEplotID", all.x = T)

#special treatment for the added columns of the 17166_3_Dataset
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("Glu","N_Ac","Xyl","BEplotID")], by = "BEplotID", all.x = T)
#rename the Glu, N_Ac and Xyl columns by adding the year, since these measurements are replicated in time
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Glu"] <- "Glu_2011"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "N_Ac"] <- "N_Ac_2011"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Xyl"] <- "Xyl_2011"

#calculate the mini-multifunctionality variable "soilCflxs_2011" as in the synthesis dataset functions grassland
#requires the variables Glu_2011, N_Ac_2011 and Xyl_2011
#therefore, select the relevant columns from "BE_synthesis_forest_dat"
dat.1 <- BE_synthesis_forest_dat %>% 
  subset( select = c("BEplotID", "Glu_2011", "N_Ac_2011", "Xyl_2011")) %>% 
  #and calculate mini-multifunctionalities based on z-scores (sd = 1, mean = 0) of input variables
  #function returns a matrix of which we only need the first column
  mutate( soilCflxs_2011 = multidiv(.[,c("Glu_2011", "N_Ac_2011", "Xyl_2011")], sc = "sd", cent = T)[,1])

#merge the soilCflxs column
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("soilCflxs_2011","BEplotID")], by = "BEplotID", all.x = T)
#=#

#special treatment for the added columns of the 19326_4_Dataset
#before merging dat3 to BE_synthesis_forest_dat, Organic_layer_thickness (cm) has to be calculated
#The thickness of the organic layer, aka O-Horizon, is the sum of the three, averaged per plot (from 14 samples), 
#horizon subclass thicknesses of Oi, Oe, Oa. Measured to gain insight in e.g. the decomposition rate of the organic layer.
#therefore, subset forest plots, calculate means per row of the relevant columns of the 19326_4_Dataset
#and sum them up to get the Organic_layer_thickness variable
dat2.1 <- dat2 %>% 
  subset( BEplotID %in% BE_synthesis_forest_dat$BEplotID) %>% 
  rowwise( BEplotID) %>% 
  mutate( Organic_layer_thickness = sum(mean(c_across("Thickness_Oi_1":"Thickness_Oi_14"), na.rm = T),
                                        mean(c_across("Thickness_Oe_1":"Thickness_Oe_14"), na.rm = T),
                                        mean(c_across("Thickness_Oa_1":"Thickness_Oa_14"), na.rm = T)))

#merge the Organic_layer_thickness column
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2.1[,c("Organic_layer_thickness","BEplotID")], by = "BEplotID", all.x = T)
#=#

#special treatment for the columns of the 23906_7_Dataset
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat5[,c("Glu","N_Ac","BEplotID")], by = "BEplotID", all.x = T) #CURRENTLY THE RAW DATA CONTAINS "Sul" instead of "Xyl" as a variable, this will be corrected on BExis
#rename the Glu, N_Ac and Xyl columns by adding the year, since these measurements are replicated in time
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Glu"] <- "Glu_2014"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "N_Ac"] <- "N_Ac_2014"
#names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Xyl"] <- "Xyl_2011" #CURRENTLY THE RAW DATA CONTAINS "Sul" instead of "Xyl" as a variable, this will be corrected on BExis
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Glu_2011))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$N_Ac_2011))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Xyl_2011))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Organic_C))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Organic_layer_thickness))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$hydrophobicity))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Cmic))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$soilCflxs))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Glu_2014))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$N_Ac_2014))) #2 NAs
### ===== ###

### === Phosphatase === ###
#Principal Investigator:     Trumbore
#                            Schrumpf
#Dataset(s):                 17166_3_Dataset
#                            23906_7_Dataset
#Process and component name: Phosphatase
#Relevant columns (unit):    Pho (nmol/(g*h)) (17166_3_Dataset - Mineral Soil Enzyme Activities 2011)
#                            Pho (nmol/(g*h)) (23906_7_Dataset - Mineral Soil Enzyme Activities 2014)

#read data
dat <- read.table(paste0(pathtodata, "Functions/17166_3_Dataset/17166_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/23906_7_Dataset/23906_7_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat1)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "BEplotID")

#special treatment for the added columns, rename temporal replicates
names(dat)[names(dat) == "Pho"] <- "Pho_2011"
names(dat1)[names(dat1) == "Pho"] <- "Pho_2014"
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("Pho_2011","BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("Pho_2014","BEplotID")], by = "BEplotID", all.x = T)
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Pho_2011))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$Pho_2014))) #2 NAs
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

#special treatment for the added columns of the 18346_2_Dataset
#create the Fine_root_carbohydrate_conc column by summing up Concentration_of_Glucose and Concentration_of_Fructose per BEplotID
dat.1 <- dat %>% 
  rowwise( ) %>% 
  mutate( Fine_root_carbohydrate_conc = sum(Concentration_of_Glucose, Concentration_of_Fructose))

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("Concentration_of_Glucose","Concentration_of_Fructose",
                                                                   "Fine_root_carbohydrate_conc","BEplotID")], by = "BEplotID", all.x = T)
#rename added columns for more context
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Concentration_of_Glucose"] <- "Fine_root_glucose_conc"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Concentration_of_Fructose"] <- "Fine_root_fructose_conc"
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Fine_root_glucose_conc))) #4 NAs
length(which(is.na(BE_synthesis_forest_dat$Fine_root_fructose_conc))) #4 NAs
length(which(is.na(BE_synthesis_forest_dat$Fine_root_carbohydrate_conc))) #4 NAs
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
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("Fine_Roots_Biomass", "Coarse_Roots_Biomass", "Root_Biomass","BEplotID")], by = "BEplotID", all.x = T)

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Fine_Roots_Biomass))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Coarse_Roots_Biomass))) #24 NAs
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
length(which(is.na(BE_synthesis_forest_dat$Browsing_perc_conifers))) #141 NAs 
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
#                            Marhan
#                            Trumbore
#Dataset(s):                 19230_3_Dataset
#                            14567_5_Dataset
#                            17026_3_Dataset
#                            22686_5_Dataset
#                            26908_4_Dataset
#                            27266_5_Dataset
#                            26306_7_Dataset
#Process and component name: Nutrient cycling
#Relevant columns (unit):    Fine_Roots_Carbon (mg/g)
#                            Fine_Roots_Nitrogen (mg/g)
#                            Fine_roots_CN_ratio (ratio from Total_C and Total_N that have the unit percentage (%))
#                            Res_14 (µg/g) (17026_3_Dataset - Soil respiration 2014)
#                            CO2_rate_mean (µg/g) (potential soil respiration rate 2017)
#                            Soil_respiration_2018 (g/(m^2*d))
#                            Soil_respiration_2019 (g/(m^2*d))
#                            Phosphorus (kg/hectares)
#                            Phosphate (kg/hectares)
#                            Ammonium (kg/hectares)
#                            Nitrate (kg/hectares)
#                            PMOR ng/(g*h)



#TODO check how the Fine_roots_CN_ratio is related to soil CN_ratios and whether these are comparable.
#TODO check why Total_C and Total_N (used to calculate the Fine_roots_CN_ratio), weren't included here.

#read data
dat <- read.table(paste0(pathtodata, "Functions/19230_3_Dataset/19230_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/14567_5_Dataset/14567_5_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/17026_3_Dataset/17026_3_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/22686_5_Dataset/22686_5_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/26908_4_Dataset/26908_4_data.txt"), header = T, sep = ";")
dat5 <- read.table(paste0(pathtodata, "Functions/27266_5_Dataset/27266_5_data.txt"), header = T, sep = ";")
dat6 <- read.table(paste0(pathtodata, "Functions/26306_7_Dataset/26306_7_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat6)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "BEplotID")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "BEplotID")
dat3 <- BEplotZeros(dat3, "PlotID", plotnam = "BEplotID")
dat4 <- BEplotZeros(dat4, "EP_Plotid", plotnam = "BEplotID")
dat5 <- BEplotZeros(dat5, "EP_Plotid", plotnam = "BEplotID")
dat6 <- BEplotZeros(dat6, "EP_PlotID", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("Carbon_within_fine_roots_Soil_Sampling_May_2011", 
                                                                 "Nitrogen_within_fine_roots_Soil_Sampling_May_2011",
                                                                 "BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("CN_ratio", "BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3[,c("CO2_rate_mean", "BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat5[,c("Phosphorus", "Phosphate", "Ammonium", "Nitrate", "BEplotID")], by = "BEplotID", all.x = T)

#special treatment for the added columns of the 19230_3_Dataset, 14567_5_Dataset, 17026_3_Dataset, 
#22686_5_Dataset and 27266_5_Dataset
#rename columns
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Carbon_within_fine_roots_Soil_Sampling_May_2011"] <- "Fine_Roots_Carbon"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Nitrogen_within_fine_roots_Soil_Sampling_May_2011"] <- "Fine_Roots_Nitrogen"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CN_ratio"] <- "Fine_roots_CN_ratio"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Res_14"] <- "Soil_respiration_2014"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CO2_rate_mean"] <- "Soil_respiration_2017"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Phosphorus"] <- "Annual_Leaching_P"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Phosphate"] <- "Annual_Leaching_PO4"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Ammonium"] <- "Annual_Leaching_NH4"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Nitrate"] <- "Annual_Leaching_NO3"
#=#

#special treatment for the added columns of the 26908_4_Dataset
#Soil_respiration was measured in 2018 and 2019. Create Soil_respiration_2018 and Soil_respiration_2019 columns.
dat4.1 <- dat4 %>%
  subset( Year == "2018") %>% 
  pivot_wider( names_from = Year, names_glue = "Soil_respiration_{Year}", values_from = Rs)

dat4.2 <- dat4 %>%
  subset( Year == "2019") %>% 
  pivot_wider( names_from = Year, names_glue = "Soil_respiration_{Year}", values_from = Rs)

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4.1[,c("Soil_respiration_2018", "BEplotID")], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4.2[,c("Soil_respiration_2019", "BEplotID")], by = "BEplotID", all.x = T)
#=#

#special treatment for the columns of the 26306_7_Dataset
#there are two replicates per plot, average them
#therefore, subset forest plots and average replicates per BEplotID
dat6.1 <- dat6 %>% 
  subset( BEplotID %in% BE_synthesis_forest_dat$BEplotID) %>% 
  group_by( BEplotID) %>% 
  summarise( Methane_oxidation = mean(PMOR, na.rm = T))
#merge the averaged column
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat6.1[,c("Methane_oxidation", "BEplotID")], by = "BEplotID", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Pho))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Fine_Roots_Carbon))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$Fine_Roots_Nitrogen))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$Fine_roots_CN_ratio))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$Soil_respiration_2014))) #8 NAs
length(which(is.na(BE_synthesis_forest_dat$Soil_respiration_2017))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$Soil_respiration_2018))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Soil_respiration_2019))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Annual_Leaching_P))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Annual_Leaching_PO4))) #136 NAs #NOT SURE IF WE WANT TO KEEP THAT
length(which(is.na(BE_synthesis_forest_dat$Annual_Leaching_NH4))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Annual_Leaching_NO3))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Methane_oxidation))) #1 NAs
### ===== ###



















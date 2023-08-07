### === Synthesis dataset functions forest assembly === ###

#Author(s): Marc Beringer, Paul Armando Gilmour Rivas Luginbühl, Noëlle Schenk, Caterina Penone
#Date: July, 2023
#Purpose: Assemble synthesis dataset for forest ecosystem functions 
#         First, generate a dataframe with identifier columns
#         Second, fill this dataframe with relevant rows and columns from individual BE datasets in the wide format and using "AggregatedColumnName" (see metadata)
#         Third, calculate new variables where necessary
#         Fourth, use this wide dataframe and transform it into the long format for upload on BExIS using the "transform_to_long_format.R" script

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
view(BE_synthesis_forest_dat[,c(1:4,50:64)])
write.table(BE_synthesis_forest_dat, file = "BE_synthesis_forest_dat.txt", quote = F, sep = "\t", row.names = F) #Assembled until before Herbivory
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
### ===== ###

### === path to data === ###
#Path to raw data on the server of the Institute of Plant Sciences at the University of Bern (Switzerland)
#Unzip the Datasets first
pathtodata <- "P:/PROJECTS/Exploratories Synthesis/Data/Forest_functions/dataset_creation/raw_data/"
### ===== ###

### === data assembly === ###
#create an empty dataframe with identifier columns (Plot, Plot_bexis, exploratory and habitat), which will be filled with relevant columns later
#Plot is the two-digit plot identifier (e.g. AEW01) that will be used for merging, while Plot_bexis is the classic identifier (e.g. AEW1)
BE_synthesis_identifier_dat  <- data.frame(Plot = c(paste("AEW", formatC(1:50, width = 2, flag = "0"), sep = ""),
                                                        paste("AEG", formatC(1:50, width = 2, flag = "0"), sep = ""),
                                                        paste("SEW", formatC(1:50, width = 2, flag = "0"), sep = ""),
                                                        paste("SEG", formatC(1:50, width = 2, flag = "0"), sep = ""),
                                                        paste("HEW", formatC(1:51, width = 2, flag = "0"), sep = ""), #HEW51 was established in 2017, after HEW02 was destroyed in 2016.
                                                        paste("HEG", formatC(1:50, width = 2, flag = "0"), sep = "")),
                                           Plot_bexis = c(paste("AEW", formatC(1:50), sep = ""),
                                                         paste("AEG", formatC(1:50), sep = ""),
                                                         paste("SEW", formatC(1:50), sep = ""),
                                                         paste("SEG", formatC(1:50), sep = ""),
                                                         paste("HEW", formatC(1:51), sep = ""), #HEW51 was established in 2017, after HEW02 was destroyed in 2016.
                                                         paste("HEG", formatC(1:50), sep = "")),
                                           exploratory = c(rep("ALB", 100), rep("SCH", 100), rep("HAI", 101))) %>% 
                                           mutate( habitat = ifelse(grepl(pattern = "W", Plot), "forest", "grassland"))

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
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_identifier_dat, dat[,c("Mass_loss_October_2012","Plot")], by = "Plot", all.x = T)
#special treatment for the added columns
#rename columns
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Mass_loss_October_2012"] <- "Root_decomposition"
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Root_decomposition))) #16 NAs
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
#                            18787_6_Dataset
#                            23846_10_Dataset
#                            31210_6_Dataset
#Process and component name: N availability
#Relevant columns (unit):    PNR (ng/(g*h)) (19847_3_Dataset - Potential Nitrification Rate 2014)
#                            NH4 (µg/g) (19847_3_Dataset - Ammonium_NH4 2014)
#                            NO3 (mg/g) (19847_3_Dataset - Nitrate_NO3 2014)
#                            CN_ratio (Organic C/Total N) (14446_19_Dataset - Upper mineral soil 2011)
#                            CN_ratio (Total_C/Total_N) (20040_3_Dataset - Organic Horizon soil 2011)
#                            CN_ratio (Total_C/Total_N) (20045_3_Dataset - Organic Horizon soil 2014)
#                            Nmic (µg/g)
#                            amoA_AOA
#                            amoA_AOB
#                            nxrA_NB
#                            X16S_NS
#                            pN (ng/(g*h)) (21546_2_Dataset - Potential Nitrification Rate 2016)
#                            NH4.N (µg/g) (21546_2_Dataset - Ammonium_NH4 2016) 
#                            NO3.N (mg/g) (21546_2_Dataset - Nitrate_NO3 2016)
#                            CN_ratio (Total_C/Total_N) (24346_3_Dataset - Organic Horizon soil 2017)
#                            CN_ratio (Organic C/Total N) (18787_6_Dataset - Upper mineral soil 2014)
#                            CN_ratio (Organic C/Total N) (23846_10_Dataset - Upper mineral soil 2017)
#                            CN_ratio (Organic C/Total N) (31210_6_Dataset - Upper mineral soil 2021)

#read data
dat <- read.table(paste0(pathtodata, "Functions/19847_3_Dataset/19847_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/14446_19_Dataset/14446_19_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/20040_3_Dataset/20040_3_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/20045_3_Dataset/20045_3_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/14106_2_Dataset/14106_2_data.txt"), header = T, sep = ";")
dat5 <- read.table(paste0(pathtodata, "Functions/21546_2_Dataset/21546_2_data.txt"), header = T, sep = ";")
dat6 <- read.table(paste0(pathtodata, "Functions/24346_3_Dataset/24346_3_data.txt"), header = T, sep = ";")
dat7 <- read.table(paste0(pathtodata, "Functions/18787_6_Dataset/18787_6_data.txt"), header = T, sep = ";")
dat8 <- read.table(paste0(pathtodata, "Functions/23846_10_Dataset/23846_10_data.txt"), header = T, sep = ";")
dat9 <- read.table(paste0(pathtodata, "Functions/31210_6_Dataset/31210_6_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat5)
dat <- BEplotZeros(dat, "Plot_ID", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "EP_Plotid", plotnam = "Plot")
dat4 <- BEplotZeros(dat4, "PlotID", plotnam = "Plot")
dat5 <- BEplotZeros(dat5, "Plot_ID", plotnam = "Plot")
dat6 <- BEplotZeros(dat6, "EP_Plotid", plotnam = "Plot")
dat7 <- BEplotZeros(dat7, "EP_Plotid", plotnam = "Plot")
dat8 <- BEplotZeros(dat8, "EP_Plotid", plotnam = "Plot")
dat9 <- BEplotZeros(dat9, "EP_Plotid", plotnam = "Plot")
#merge columns that require no special treatment
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4[,c("Nmic","Plot")], by = "Plot", all.x = T)
#=#

#special treatment before adding the columns of the 14446_19_Dataset, 19847_3_Dataset, 21546_2_Dataset, 
#18787_6_Dataset, 23846_10_Dataset and 31210_6_Dataset
#rename columns
names(dat)[names(dat) == "PNR"] <- "PNR_2014"
names(dat)[names(dat) == "NH4"] <- "Ammonium_NH4_2014"
names(dat)[names(dat) == "NO3"] <- "Nitrate_NO3_2014"
names(dat1)[names(dat1) == "CN_ratio"] <- "CN_ratio_MinSoil_2011"
names(dat5)[names(dat5) == "pN"] <- "PNR_2016"
names(dat5)[names(dat5) == "NH4.N"] <- "Ammonium_NH4_2016"
names(dat5)[names(dat5) == "NO3.N"] <- "Nitrate_NO3_2016"
names(dat7)[names(dat7) == "CN_ratio"] <- "CN_ratio_MinSoil_2014"
names(dat8)[names(dat8) == "CN_ratio"] <- "CN_ratio_MinSoil_2017"
names(dat9)[names(dat9) == "CN_ratio"] <- "CN_ratio_MinSoil_2021"
#=#

#special treatment for temporal replicates
#average years to generate the "average_PNR", "average_NH4", "average_NO3" and "average_CN_MinSoil" variables
#therefore, create an intermediate dataframe with the relevant columns
dat.1 <- merge(BE_synthesis_identifier_dat, dat[,c("PNR_2014","Ammonium_NH4_2014","Nitrate_NO3_2014","Plot")], by = "Plot", all.x = T)
dat.1 <- merge(dat.1, dat1[,c("CN_ratio_MinSoil_2011","Plot")], by = "Plot", all.x = T)
dat.1 <- merge(dat.1, dat5[,c("amoA_AOA","amoA_AOB","nxrA_NB","X16S_NS","PNR_2016",
                              "Ammonium_NH4_2016","Nitrate_NO3_2016","Plot")], by = "Plot", all.x = T)
dat.1 <- merge(dat.1, dat7[,c("CN_ratio_MinSoil_2014","Plot")], by = "Plot", all.x = T)
dat.1 <- merge(dat.1, dat8[,c("CN_ratio_MinSoil_2017","Plot")], by = "Plot", all.x = T)
dat.1 <- merge(dat.1, dat9[,c("CN_ratio_MinSoil_2021","Plot")], by = "Plot", all.x = T)
#instead of values some rows contain the string "bdl" and their columns are character columns
#replace these strings with NA
dat.1[dat.1 == "bdl"] <- NA
#replace one "0" value in the destroyed Plot HEW02 (rownumber 52) with NA (CN ratio can't reasonably be zero)
dat.1["52", "CN_ratio_MinSoil_2017"] = NA
#and change the character columns to numeric, to enable calculation of an average
dat.1 <- dat.1 %>% 
  mutate( PNR_2014 = as.numeric(PNR_2014),
          Ammonium_NH4_2014 = as.numeric(Ammonium_NH4_2014),
          Nitrate_NO3_2014 = as.numeric(Nitrate_NO3_2014),
          nxrA_NB = as.numeric(nxrA_NB),
          X16S_NS = as.numeric(X16S_NS),
          PNR_2016 = as.numeric(PNR_2016),
          Nitrate_NO3_2016 = as.numeric(Nitrate_NO3_2016)) %>% 
  #calculate the averages of temporal replicates
  rowwise( Plot) %>% 
  mutate( average_PNR = mean(c_across(c("PNR_2014", "PNR_2016")), na.rm = T),
          average_NH4 = mean(c_across(c("Ammonium_NH4_2014", "Ammonium_NH4_2016")), na.rm = T),
          average_NO3 = mean(c_across(c("Nitrate_NO3_2014", "Nitrate_NO3_2016")), na.rm = T),
          average_CN_MinSoil = mean(c_across(c("CN_ratio_MinSoil_2014", "CN_ratio_MinSoil_2017", "CN_ratio_MinSoil_2021")), na.rm = T))

#merge generated columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("PNR_2014","PNR_2016","average_PNR",
                                                                   "Ammonium_NH4_2014","Ammonium_NH4_2016","average_NH4",
                                                                   "Nitrate_NO3_2014","Nitrate_NO3_2016","average_NO3",
                                                                   "CN_ratio_MinSoil_2011","CN_ratio_MinSoil_2014",
                                                                   "CN_ratio_MinSoil_2017","CN_ratio_MinSoil_2021","average_CN_MinSoil",
                                                                   "amoA_AOA","amoA_AOB","nxrA_NB","X16S_NS",
                                                                   "Plot")], by = "Plot", all.x = T)
#=#

#special treatment for the relevant columns of the 20040_3_Dataset, 20045_3_Dataset and 24346_3_Dataset
#before merging to BE_synthesis_forest_dat, CN_ratio_OrgLay_2011, 2014 and 2017 has to be calculated
#each Plot has three measurements, one per O-Horizon subclass (Oi, Oe, Oa).
#average the three O-Horizon subclasses per Plot to make the variable comparable to the CN_ratio of the upper mineral soil layer from the 14446_19_Dataset.
#therefore, extract the relevant columns, pivot_wider and get rowwise averages of the O-Horizon subclass columns.
dat2.1 <- dat2[,c("Horizon","CN_ratio","Plot")] %>% 
  pivot_wider( names_from = c(Horizon), values_from = CN_ratio) %>% 
  rowwise( Plot) %>% 
  mutate( CN_ratio_OrgLay_2011 = mean(c_across(1:3), na.rm = T))

dat3.1 <- dat3[,c("Horizon","CN_ratio","Plot")] %>% 
  pivot_wider( names_from = c(Horizon), values_from = CN_ratio) %>% 
  rowwise( Plot) %>% 
  mutate( CN_ratio_OrgLay_2014 = mean(c_across(1:3), na.rm = T))

dat6.1 <- dat6[,c("Horizon","CN_ratio","Plot")] %>% 
  pivot_wider( names_from = c(Horizon), values_from = CN_ratio) %>% 
  rowwise( Plot) %>% 
  mutate( CN_ratio_OrgLay_2017 = mean(c_across(1:3), na.rm = T))

#then, calculate the average across CN_ratio_OrgLay temporal replicates, by again merging relevant columns
dat.2 <- merge(BE_synthesis_identifier_dat, dat2.1[,c("CN_ratio_OrgLay_2011","Plot")], by = "Plot", all.x = T)
dat.2 <- merge(dat.2, dat3.1[,c("CN_ratio_OrgLay_2014","Plot")], by = "Plot", all.x = T)
dat.2 <- merge(dat.2, dat6.1[,c("CN_ratio_OrgLay_2017","Plot")], by = "Plot", all.x = T)
#and creating the "average_CN_OrgLay" variable
dat.2 <- dat.2 %>% 
  rowwise( Plot) %>% 
  mutate( average_CN_OrgLay = mean(c_across(c("CN_ratio_OrgLay_2011", "CN_ratio_OrgLay_2014", "CN_ratio_OrgLay_2017")), na.rm = T))

#merge the generated columns
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.2[,c("CN_ratio_OrgLay_2011","CN_ratio_OrgLay_2014",
                                                                   "CN_ratio_OrgLay_2017","average_CN_OrgLay","Plot")], by = "Plot", all.x = T)
#=#

#calculate the mini-multifunctionality variable "forest_soilNitrateflxs"
#a similar variable exists in the grassland functions synthesis dataset (27087)
#in the 27087 dataset, "soilNitrateflxs" is calculated with the variables nxrA_NB, 16S_NS, nifH and DEA
#we lack nifH and DEA data for forests
#therefore, "forest_soildNitrateflxs" is only calculated with the variables nxrA_NB and 16S_NS (from the 21546_2_Dataset)
#select the relevant columns from "BE_synthesis_forest_dat"
dat.3 <- BE_synthesis_forest_dat %>% 
  subset( select = c("Plot", "nxrA_NB", "X16S_NS")) %>%
  #like in the grasslands, nxrA_NB and X16S_NS are summed up to “nitrite-oxidising functional gene abundances” (nitOx_fga)
  rowwise( Plot) %>% 
  mutate( nitOx_fga = sum(c(nxrA_NB, X16S_NS), na.rm = T))

#replace all "0" values of "nitOx_fga" by NA
dat.3[dat.3$nitOx_fga == 0,] <- NA

#then calculate mini-multifunctionality which here is simply the z-score of nitOx_fga (sd = 1, mean = 0)
dat.3$forest_soilNitrateflxs <- multidiv(dat.3[,4], sc = "sd", cent = T)[,1]

#merge the forest_soilNitrateflxs column
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.3[,c("forest_soilNitrateflxs","Plot")], by = "Plot", all.x = T)
#=#

#calculate the mini-multifunctionality variable "forest_soilAmmoniaflxs"
#a similar variable exists in the grassland functions synthesis dataset (27087)
#in the 27087 dataset, "soilAmmoniaflxs" is calculated with the variables amoA_AOA, amoA_AOB and Urease
#we lack Urease data for forests
#therefore, "forest_soilAmmoniaflxs" is only calculated with the variables amoA_AOA, amoA_AOB (from the 21546_2_Dataset)
#select the relevant columns from "BE_synthesis_forest_dat"
dat.4 <- BE_synthesis_forest_dat %>% 
  subset( select = c("Plot", "amoA_AOA", "amoA_AOB")) %>%
  #like in the grasslands, amoA_AOA and amoA_AOB are summed up to “ammonia-oxidising functional gene abundances” (amOX_fga)
  rowwise( Plot) %>% 
  mutate( amOX_fga = sum(c(amoA_AOA, amoA_AOB), na.rm = T))

#replace all "0" values of "nitOx_fga" by NA
dat.4[dat.4$amOX_fga == 0,] <- NA

#then calculate mini-multifunctionality which here is simply the z-score of nitOx_fga (sd = 1, mean = 0)
dat.4$forest_soilAmmoniaflxs <- multidiv(dat.4[,4], sc = "sd", cent = T)[,1]

#merge the forest_soilNitrateflxs column
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.4[,c("forest_soilAmmoniaflxs","Plot")], by = "Plot", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$PNR_2014))) #11 NAs
length(which(is.na(BE_synthesis_forest_dat$PNR_2016))) #11 NAs
length(which(is.na(BE_synthesis_forest_dat$average_PNR))) #11 NAs
length(which(is.na(BE_synthesis_forest_dat$Ammonium_NH4_2014))) #30 NAs 
length(which(is.na(BE_synthesis_forest_dat$Ammonium_NH4_2016))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$average_NH4))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Nmic))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$Nitrate_NO3_2014))) #29 NAs
length(which(is.na(BE_synthesis_forest_dat$Nitrate_NO3_2016))) #7 NAs
length(which(is.na(BE_synthesis_forest_dat$average_NO3))) #7 NAs
length(which(is.na(BE_synthesis_forest_dat$CN_ratio_MinSoil_2011))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$CN_ratio_MinSoil_2014))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$CN_ratio_MinSoil_2017))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$CN_ratio_MinSoil_2021))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$average_CN_MinSoil))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$amoA_AOA))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$amoA_AOB))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$nxrA_NB))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$X16S_NS))) #12 NAs
length(which(is.na(BE_synthesis_forest_dat$CN_ratio_OrgLay_2011))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$CN_ratio_OrgLay_2014))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$CN_ratio_OrgLay_2017))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$average_CN_OrgLay))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$forest_soilNitrateflxs))) #3 NAs
length(which(is.na(BE_synthesis_forest_dat$forest_soilAmmoniaflxs))) #1 NAs
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
#                            26228_4_Dataset
#Process and component name: P availability
#Relevant columns (unit):    OlsenPi (mg/kg) (19286_3_Dataset - OlsenPi_2014)
#                            NaHCO3_Pi (mg/kg)
#                            Resin_P (mg/kg)
#                            Pmic (mg/kg)
#                            Olsen-P (mg/kg) (31340_4_Dataset - OlsenPi_2021)
#                            CP_ratio (organic C/total P)
#                            PS_ratio (total P/total S)
#                            P_soluble (mg/g)

#read data
dat <- read.table(paste0(pathtodata, "Functions/19286_3_Dataset/19286_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/5241_5_Dataset/5241_5_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/19009_3_Dataset/19009_3_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/15766_3_Dataset/15766_3_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/31340_4_Dataset/31340_4_data.txt"), header = T, sep = ";")
dat5 <- read.table(paste0(pathtodata, "Functions/19366_4_Dataset/19366_4_data.txt"), header = T, sep = ";")
dat6 <- read.table(paste0(pathtodata, "Functions/26228_4_Dataset/26228_4_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat6)
dat <- BEplotZeros(dat, "Plot_ID", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "EP", plotnam = "Plot")
dat4 <- BEplotZeros(dat4, "EP_Plotid", plotnam = "Plot")
dat5 <- BEplotZeros(dat5, "EP_Plotid", plotnam = "Plot")
dat6 <- BEplotZeros(dat6, "EP_Plotid", plotnam = "Plot")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("OlsenPi","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("NaHCO3_Pi","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3[,c("Pmic","Plot")], by = "Plot", all.x = T) #ONLY KEEP Pmic IF YOU INCLUDE PRI
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4[,c("Olsen.P","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat5[,c("CP_ratio","PS_ratio","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat6[,c("P_soluble","Plot")], by = "Plot", all.x = T)

#special treatment for the added columns
#rename columns
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CP_ratio"] <- "Upper_MinSoil_CP_ratio"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "PS_ratio"] <- "Upper_MinSoil_PS_ratio"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "OlsenPi"] <- "OlsenPi_2014"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Olsen.P"] <- "OlsenPi_2021"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "P_soluble"] <- "Orthophosphate"
#=#

#special treatment for the Resin_P variable of the 19009_3_Dataset
#as has been done in the grassland functions synthesis dataset (27087), Resin_P will be reversed and renamed to P_loss
#so that high P_loss values represent low Phosphorous loss
dat2.1 <- dat2 %>% 
  #subset forest plots
  subset( Plot %in% BE_synthesis_identifier_dat$Plot) %>% 
  #subtract each value from the highest forest Resin_P value to get P_loss
  mutate( P_loss = max(Resin_P) - Resin_P)

#merge the generated P_loss variable
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2.1[,c("P_loss","Plot")], by = "Plot", all.x = T)
#=#

#special treatment for temporal replicates of OlsenPi of the 19286_3_Dataset and 31340_4_Dataset
#average years to generate the "average_OlsenPi" variable
#therefore, create an intermediate dataframe with the relevant columns
dat.1 <- BE_synthesis_forest_dat %>% 
  subset( select = c("Plot", "OlsenPi_2014", "OlsenPi_2021")) %>% 
  rowwise( Plot) %>% 
  mutate( average_OlsenPi = mean(c_across(c("OlsenPi_2014", "OlsenPi_2021")), na.rm = T))

#merge the generated average_OlsenPi
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("average_OlsenPi","Plot")], by = "Plot", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$OlsenPi_2014))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$NaHCO3_Pi))) #20 NAs
length(which(is.na(BE_synthesis_forest_dat$P_loss))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Pmic))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$OlsenPi_2021))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Upper_MinSoil_CP_ratio))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Upper_MinSoil_PS_ratio))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Orthophosphate))) #1 NAs
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
dat2 <- read.table(paste0(pathtodata, "Functions/19366_4_Dataset/19366_4_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat2)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "Plot")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2[,c("Plot","CS_ratio")], by = "Plot", all.x = T)

#special treatment for the added columns of the 19366_4_Dataset
#rename columns
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CS_ratio"] <- "Upper_MinSoil_CS_ratio"
#=#

#special treatment for the added columns of the 20045_3_Dataset and 24346_3_Dataset
#before merging dat to BE_synthesis_forest_dat, O_Horizon_CS_ratio_2014 and 2017 has to be calculated
#Each Plot has three measurements, one per O-Horizon subclass (Oi, Oe, Oa).
#Average the three O-Horizon subclasses per Plot to make the variable comparable to the O_Horizon_CN_ratio_2014 variable.
#Therefore, extract the relevant columns, pivot_wider and get rowwise averages of the O-Horizon subclass columns.
dat.1 <- dat[,c("Horizon","CS_ratio","Plot")] %>% 
  pivot_wider( names_from = c(Horizon), values_from = CS_ratio) %>% 
  rowwise( Plot) %>% 
  mutate( O_Horizon_CS_ratio_2014 = mean(c_across(1:3), na.rm = T))

dat1.1 <- dat1[,c("Horizon","CS_ratio","Plot")] %>% 
  pivot_wider( names_from = c(Horizon), values_from = CS_ratio) %>% 
  rowwise( Plot) %>% 
  mutate( O_Horizon_CS_ratio_2017 = mean(c_across(1:3), na.rm = T))

#then calculate the average_O_Horizon_CS_ratio
dat.2 <- merge(dat.1, dat1.1, by = "Plot", all.x = T)
dat.2 <- dat.2 %>% 
  rowwise( Plot) %>% 
  mutate( average_O_Horizon_CS_ratio = mean(c_across(c("O_Horizon_CS_ratio_2014", "O_Horizon_CS_ratio_2017")), na.rm = T))
  
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.2[,c("Plot","O_Horizon_CS_ratio_2014", "O_Horizon_CS_ratio_2017", 
                                                                   "average_O_Horizon_CS_ratio")], by = "Plot", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$O_Horizon_CS_ratio_2014))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$O_Horizon_CS_ratio_2017))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$Upper_MinSoil_CS_ratio))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$average_O_Horizon_CS_ratio))) #1 NAs
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
dat <- BEplotZeros(dat, "EP", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "Plot", plotnam = "Plot")

#special treatment of the removal_g column of the 21206_3_Dataset
#as in the synthesis dataset functions grassland, here we only use samples gathered in summer 2014 (i.e. June, July and August)
#since samples from May 2014 were not collected at all Plots and samples from April and July 2015 were only collected at 36 out of 300 Plots
#then we apply the same calculation as in the grassland dataset by first scaling each dungtype before averaging them per Plot
#so that each dungtype (e.g. Cow, Sheep etc.) across the 150 Plots has a mean of 0 and a standard deviation of 1

#first add the habitat column to enable selection of forest Plots only
dat.1 <- merge(dat, BE_synthesis_identifier_dat, by = "Plot", all.x = T)

dat.2 <- dat.1 %>% 
  #select summer 2014 samples in forest Plots
  subset( habitat == "forest" & (month == "June_2014" | month == "July_2014" | month == "August_2014")) %>% 
  #scale removal_g across the range of each dungtype individually, to make dungtypes comparable to each other (i.e. Cow comparable with Sheep)
  group_by( dungtype) %>% 
  mutate( scaled_removal_g = scale(removal_g, center = F, scale = T)) %>% 
  #then calculate the mean of the scaled_removal_g for each Plot
  group_by( Plot) %>% 
  summarise( dung_removal = mean(scaled_removal_g))

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.2[,c("Plot","dung_removal")], by = "Plot", all.x = T)
#=#

#special treatment for the dung_depletion column of the 24966_3_Dataset
#dung_depletion has up to five replicates per Plot, simply average those 
#as has been done for the synthesis dataset functions grassland (27087_24_Dataset)
dat1.1 <- dat1 %>% 
  subset( Habitat == "F") %>% 
  group_by( Plot) %>% 
  #rename dung_depletion to dung_removal_2017 for consistency with the grassland functions dataset
  summarise( dung_depletion = mean(dung_depletion, na.rm = T))

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1.1[,c("Plot","dung_depletion")], by = "Plot", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$dung_removal))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$dung_depletion))) #2 NAs
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
#                            18787_6_Dataset
#                            23846_10_Dataset
#                            31210_6_Dataset
#Process and component name: Soil C cycling
#Relevant columns (unit):    Glu (nmol/(g*h)) (17166_3_Dataset - Mineral Soil Enzyme Activities 2011)
#                            N_Ac (nmol/(g*h)) (17166_3_Dataset - Mineral Soil Enzyme Activities 2011)
#                            Xyl (nmol/(g*h)) (17166_3_Dataset - Mineral Soil Enzyme Activities 2011)
#                            Organic_C (g/kg) (14446_19_Dataset - Organic C stock 2011)
#                            Organic_layer_thickness (cm)
#                            hydrophobicity (ratio)
#                            Cmic (µg/g)
#                            Organic_C (g/kg) (18787_6_Dataset - Organic C stock 2014)
#                            Organic_C (g/kg) (23846_10_Dataset - Organic C stock 2017)
#                            Organic_C (g/kg) (31210_6_Dataset - Organic C stock 2021)

#read data
dat <- read.table(paste0(pathtodata, "Functions/17166_3_Dataset/17166_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/14446_19_Dataset/14446_19_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/19326_4_Dataset/19326_4_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/20010_2_Dataset/20010_2_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/14106_2_Dataset/14106_2_data.txt"), header = T, sep = ";")
dat5 <- read.table(paste0(pathtodata, "Functions/23906_7_Dataset/23906_7_data.txt"), header = T, sep = ";") #THIS DATASET MIGHT CHANGE DUE TO AN UPDATE ON BExis
dat6 <- read.table(paste0(pathtodata, "Functions/18787_6_Dataset/18787_6_data.txt"), header = T, sep = ";") 
dat7 <- read.table(paste0(pathtodata, "Functions/23846_10_Dataset/23846_10_data.txt"), header = T, sep = ";") 
dat8 <- read.table(paste0(pathtodata, "Functions/31210_6_Dataset/31210_6_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat8)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "EP_Plotid", plotnam = "Plot")
dat4 <- BEplotZeros(dat4, "PlotID", plotnam = "Plot")
dat5 <- BEplotZeros(dat5, "EP_Plotid", plotnam = "Plot")
dat6 <- BEplotZeros(dat6, "EP_Plotid", plotnam = "Plot")
dat7 <- BEplotZeros(dat7, "EP_Plotid", plotnam = "Plot")
dat8 <- BEplotZeros(dat8, "EP_Plotid", plotnam = "Plot")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3[,c("hydrophobicity","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4[,c("Cmic","Plot")], by = "Plot", all.x = T)

#special treatment for the added columns of the 17166_3_Dataset
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("Glu","N_Ac","Xyl","Plot")], by = "Plot", all.x = T)

#calculate the mini-multifunctionality variable "soilCflxs_2011" as in the synthesis dataset functions grassland
#requires the variables Glu_2011, N_Ac_2011 and Xyl_2011
#therefore, select the relevant columns from "BE_synthesis_forest_dat"
dat.1 <- BE_synthesis_forest_dat %>% 
  subset( select = c("Plot", "Glu", "N_Ac", "Xyl")) %>% 
  #and calculate mini-multifunctionalities based on z-scores (sd = 1, mean = 0) of input variables
  #function returns a matrix of which we only need the first column
  mutate( soilCflxs = multidiv(.[,c("Glu", "N_Ac", "Xyl")], sc = "sd", cent = T)[,1])

#merge the soilCflxs column
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("soilCflxs","Plot")], by = "Plot", all.x = T)
#=#

#special treatment for the added columns of the 19326_4_Dataset
#before merging dat3 to BE_synthesis_forest_dat, Organic_layer_thickness (cm) has to be calculated
#The thickness of the organic layer, aka O-Horizon, is the sum of the three, averaged per plot (from 14 samples), 
#horizon subclass thicknesses of Oi, Oe, Oa. Measured to gain insight in e.g. the decomposition rate of the organic layer.
#therefore, subset forest plots, calculate means per row of the relevant columns of the 19326_4_Dataset
#and sum them up to get the Organic_layer_thickness variable
dat2.1 <- dat2 %>% 
  subset( Plot %in% BE_synthesis_forest_dat$Plot) %>% 
  rowwise( Plot) %>% 
  mutate( Organic_layer_thickness = sum(mean(c_across("Thickness_Oi_1":"Thickness_Oi_14"), na.rm = T),
                                        mean(c_across("Thickness_Oe_1":"Thickness_Oe_14"), na.rm = T),
                                        mean(c_across("Thickness_Oa_1":"Thickness_Oa_14"), na.rm = T)))

#merge the Organic_layer_thickness column
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2.1[,c("Organic_layer_thickness","Plot")], by = "Plot", all.x = T)
#=#

#special treatment for the temporal replicates of the Organic_C variables
#first rename the variables to facilitate merging
names(dat1)[names(dat1) == "Organic_C"] <- "OC_stock_MinSoil_2011"
names(dat6)[names(dat6) == "Organic_C"] <- "OC_stock_MinSoil_2014"
names(dat7)[names(dat7) == "Organic_C"] <- "OC_stock_MinSoil_2017"
names(dat8)[names(dat8) == "Organic_C"] <- "OC_stock_MinSoil_2021"

#merge
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("OC_stock_MinSoil_2011","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat6[,c("OC_stock_MinSoil_2014","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat7[,c("OC_stock_MinSoil_2017","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat8[,c("OC_stock_MinSoil_2021","Plot")], by = "Plot", all.x = T)

#replace one "0" value in the destroyed Plot HEW02 (rownumber 52) with NA (Organic_C can't reasonably be zero)
BE_synthesis_forest_dat["52", "OC_stock_MinSoil_2017"] = NA

#average years to generate the "average_OC_stock_MinSoil" variable
dat.2 <- BE_synthesis_forest_dat %>% 
  #select relevant columns
  subset( select = c("Plot", "OC_stock_MinSoil_2011", "OC_stock_MinSoil_2014", 
                     "OC_stock_MinSoil_2017", "OC_stock_MinSoil_2021")) %>% 
  rowwise( Plot) %>% 
  #calculate average
  mutate( average_OC_stock_MinSoil = mean(c_across(c("OC_stock_MinSoil_2011", "OC_stock_MinSoil_2014", 
                                            "OC_stock_MinSoil_2017", "OC_stock_MinSoil_2021")), na.rm = T))

#merge the average_OC_stock_MinSoil variable
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.2[,c("average_OC_stock_MinSoil","Plot")], by = "Plot", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Glu))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$N_Ac))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Xyl))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Organic_layer_thickness))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$hydrophobicity))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Cmic))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$soilCflxs))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$OC_stock_MinSoil_2011))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$OC_stock_MinSoil_2014))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$OC_stock_MinSoil_2017))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$OC_stock_MinSoil_2021))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$average_OC_stock_MinSoil))) #0 NAs
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
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")

#special treatment for the added columns, rename temporal replicates
names(dat)[names(dat) == "Pho"] <- "Pho_2011"
names(dat1)[names(dat1) == "Pho"] <- "Pho_2014"
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("Pho_2011","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("Pho_2014","Plot")], by = "Plot", all.x = T)
#average years to generate the "average_Pho" variable
dat.1 <- BE_synthesis_forest_dat %>% 
  #select relevant columns
  subset( select = c("Plot", "Pho_2011", "Pho_2014")) %>% 
  rowwise( Plot) %>% 
  #calculate average
  mutate( average_Pho = mean(c_across(c("Pho_2011", "Pho_2014")), na.rm = T))

#merge average_Pho with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("average_Pho","Plot")], by = "Plot", all.x = T)
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Pho_2011))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Pho_2014))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$average_Pho))) #1 NAs
### ===== ###

### === Habitat === ###
#Principal Investigator:     Blüthgen
#Dataset(s):                 24966_3_Dataset
#Process and component name: Habitat
#Relevant columns (unit):    seed_depletion

#read data
dat <- read.table(paste0(pathtodata, "Functions/24966_3_Dataset/24966_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "Plot", plotnam = "Plot")

#special treatment for the added columns of the 24966_3_Dataset
#average the subplot measurements of seed_depletion
dat.1 <- dat %>% 
  group_by( Plot) %>% 
  summarise( seed_depletion = mean(seed_depletion, na.rm = T))

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("seed_depletion","Plot")], by = "Plot", all.x = T)
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$seed_depletion))) #2 NAs
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
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")

#special treatment for the added columns of the 14448_3_Dataset
#before merging sum up the Fine_Roots_Biomass and Coarse_Roots_Biomass to get Root_Biomass (g/cm^3)
#if Fine_Roots_Biomass, or Coarse_Roots_Biomass is NA, Root_Biomass is simply the other value
dat.1 <- dat %>% 
  rowwise( ) %>% 
  mutate( Root_Biomass = sum(c(Fine_Roots_Biomass, Coarse_Roots_Biomass), na.rm = TRUE))

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("Fine_Roots_Biomass", "Coarse_Roots_Biomass", "Root_Biomass","Plot")], by = "Plot", all.x = T)

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
#                            24806_2_Dataset
#Process and component name: Herbivory
#Relevant columns (unit):    Bper (%)
#                            Schaden & ohne_Schaden (integer)
#                            Minen & ohne_Minen (integer)
#                            Frass & ohne_Frass (integer)
#                            Saug & ohne_Saug + Phyllaphis & ohne_Phyllaphis (integer)
#                            Gallen & ohne_Gallen + Gallmilben & ohne_Gallmilben (integer)
#                            Damage_class (three infestation classes: 1 == no visible attack; 2 == weak attack; 3 == heavy attack)
#??? 24806_2_Dataset

#TODO Check 24806_2_Dataset for meaningful variables (area damaged by different classes of herbivores)

#read data
dat <- read.table(paste0(pathtodata, "Functions/20347_2_Dataset/20347_2_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/12627_2_Dataset/12627_2_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/18567_2_Dataset/18567_2_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/24806_2_Dataset/24806_2_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat3)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "plotid", plotnam = "Plot")

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
  pivot_wider( id_cols = c("Plot"), names_from = c(tsg), values_from = Bper) 

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1, by = "Plot", all.x = T)
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
#and get the average of these percentages for understory and canopy samples per Plot
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
  #calculate average percentages for Canopy and understorey samples per Plot
  group_by( Stratum, Plot) %>% 
  summarise( overall = mean(herbivory_overall),
             mining = mean(herbivory_mining),
             chewing = mean(herbivory_chewing),
             sucking = mean(c(herbivory_sucking1, herbivory_sucking2)), #this variable is the average across two columns of herbivory data
             galls = mean(c(herbivory_galls1, herbivory_galls2))) %>%   #this variable is the average across two columns of herbivory data
  #pivot the Canopy and understorey averages wider and assign the final variable names to prepare the dataframe for merging
  pivot_wider( names_from = Stratum, names_glue = "Beech_herbivory_{Stratum}_{.value}", values_from = c(overall, mining, chewing, sucking, galls))

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1.1, by = "Plot", all.x = T)
#=#

#special treatment for the added columns of the 18567_2_Dataset
#there are five replicates for Cryptococcus infestation per Plot
#take the median of the damage classes per Plot
dat2.1 <- dat2 %>% 
  group_by( Plot) %>% 
  summarise( Cryptococcus_infestation = median(Damage_class, na.rm = TRUE))

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2.1, by = "Plot", all.x = T)
#=#

#special treatment for the columns of the 24806_2_Dataset
#this is a large dataset with over 160'000 rows
#each row represents a single leaf, on which the area (mm^2) damaged by different classes of herbivores was measured
#up to 200 leaves per plant species and up to ten plant species per Plot
#to arrive at a measure for herbivory of different classes (columns) of herbivores per Plot (rows) the data is severely compressed

dat3.2 <- dat3 %>% 
  #subset forest plots
  subset( Plot %in% BE_synthesis_forest_dat$Plot) %>% 
  #calculate the percentage of leaf area affected by different classes of herbivores for each individual leaf
  rowwise( ) %>% 
  mutate( chewing_damage_ind = (sum(c(a_hole, a_edge), na.rm = T)/lf_area_corr)*100,
          scraping_sucking_damage_ind = (sum(c(a_scraping, a_sucking, a_scraping_sucking), na.rm = T)/lf_area_corr)*100,
          mining_damage_ind = (a_mines/lf_area_corr)*100,
          gall_damage_ind = (a_galls/lf_area_corr)*100,
          total_damaged_area_ind = sum(c(chewing_damage_ind, scraping_sucking_damage_ind, mining_damage_ind, gall_damage_ind), na.rm = T)) %>%
  #scale those percentages across the range of each plant species individually, to make species comparable to each other (i.e. Abies alba comparable with Urtica dioica)
  group_by( pl_species) %>% 
  mutate( scaled_chewing_damage_ind = scale(chewing_damage_ind, center = F, scale = T),
          scaled_scraping_sucking_damage_ind = scale(scraping_sucking_damage_ind, center = F, scale = T),
          scaled_mining_damage_ind = scale(mining_damage_ind, center = F, scale = T),
          scaled_gall_damage_ind = scale(gall_damage_ind, center = F, scale = T),
          scaled_total_damaged_area_ind = scale(total_damaged_area_ind, center = F, scale = T)) %>% 
  #then calculate the mean of the scaled percentages for each Plot
  group_by( Plot) %>% 
  summarise( chewing_damage = mean(scaled_chewing_damage_ind, na.rm = T),
             scraping_sucking_damage = mean(scaled_scraping_sucking_damage_ind, na.rm = T),
             mining_damage = mean(scaled_mining_damage_ind, na.rm = T),
             gall_damage = mean(scaled_gall_damage_ind, na.rm = T),
             total_damaged_area = mean(scaled_total_damaged_area_ind, na.rm = T))

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3.1, by = "Plot", all.x = T)
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
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "PlotID", plotnam = "Plot")
dat4 <- BEplotZeros(dat4, "EP_Plotid", plotnam = "Plot")
dat5 <- BEplotZeros(dat5, "EP_Plotid", plotnam = "Plot")
dat6 <- BEplotZeros(dat6, "EP_PlotID", plotnam = "Plot")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("Carbon_within_fine_roots_Soil_Sampling_May_2011", 
                                                                 "Nitrogen_within_fine_roots_Soil_Sampling_May_2011",
                                                                 "Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("CN_ratio", "Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3[,c("CO2_rate_mean", "Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat5[,c("Phosphorus", "Phosphate", "Ammonium", "Nitrate", "Plot")], by = "Plot", all.x = T)

#special treatment for the added columns of the 19230_3_Dataset, 14567_5_Dataset, 17026_3_Dataset, 
#22686_5_Dataset and 27266_5_Dataset
#rename columns
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Carbon_within_fine_roots_Soil_Sampling_May_2011"] <- "Fine_Roots_Carbon"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Nitrogen_within_fine_roots_Soil_Sampling_May_2011"] <- "Fine_Roots_Nitrogen"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CN_ratio"] <- "Fine_roots_CN_ratio"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Res_14"] <- "Soil_respiration_2014"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CO2_rate_mean"] <- "Soil_respiration_2017"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Phosphorus"] <- "Annual_Leaching_P"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Phosphate"] <- "Annual_Leaching_PO4" #REMOVE THIS VARIABLE
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
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4.1[,c("Soil_respiration_2018", "Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4.2[,c("Soil_respiration_2019", "Plot")], by = "Plot", all.x = T)
#=#

#special treatment for the columns of the 26306_7_Dataset
#there are two replicates per plot, average them
#therefore, subset forest plots and average replicates per Plot
dat6.1 <- dat6 %>% 
  subset( Plot %in% BE_synthesis_forest_dat$Plot) %>% 
  group_by( Plot) %>% 
  summarise( Methane_oxidation = mean(PMOR, na.rm = T))
#merge the averaged column
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat6.1[,c("Methane_oxidation", "Plot")], by = "Plot", all.x = T)
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

### === Final polishing of the dataset synthesis functions forest === ###
#replace NaN with NA
BE_synthesis_forest_dat[BE_synthesis_forest_dat == "NaN"] <- NA



















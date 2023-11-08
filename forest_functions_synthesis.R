
### === Forest functions synthesis dataset assembly === ####

#Author(s): Marc Beringer, Paul Armando Gilmour Rivas Luginbühl, Noëlle Schenk, Bruno Ximenes Pinho, Caterina Penone
#Date: July, 2023
#Purpose: Assemble synthesis dataset for forest ecosystem functions 
#         First, generate a dataframe with identifier columns
#         Second, fill this dataframe with relevant rows and columns from individual BE datasets in the wide format and using "AggregatedColumnName" (see metadata)
#         Third, calculate new variables where necessary
#         Fourth, use this wide dataframe and transform it into the long format for upload on BExIS using the "transform_to_long_format.R" script

### === working directory === #### 
setwd("C:/Users/Marc Beringer/Desktop/R/Synthesis_dataset_functions_forest") #Not necessary for the final script
getwd()
### ===== ###

### === save a snapshot of the R environment === ###
#includes information about versions and packages used
#can be updated if new package versions are installed and required for the script to work
#see rstuido.github.io/renv/ for more details
#renv::init()
#renv::snapshot()
### ===== ###

### === write table checkpoint === ###
#TODO take out/ uncomment in later version
names(BE_synthesis_forest_dat)

#Assembled until and including P_cycle stocks
write.table(BE_synthesis_forest_dat, file = "BE_synthesis_forest_dat_wide_Nov23_2.txt", quote = F, sep = "\t", row.names = F) 
### ===== ###

### === read table checkpoint === ###
<<<<<<< HEAD
BE_synthesis_forest_dat <- read.table("BE_synthesis_forest_dat_wide_Nov23_2.txt", header = T, sep = "\t")
=======
#TODO take out/ uncomment in later version
BE_synthesis_forest_dat <- read.table("BE_synthesis_forest_dat.txt", header = T, sep = "\t")
>>>>>>> 933c3c868d0b0c15e98956f0196f9a37a5df9c3c
### ===== ###

### === libraries === ####
{
  library(tidyverse) #load after plyr
  library(data.table)
}
### ===== ###

### === dependencies === ####
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

### === data assembly === ####
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

<<<<<<< HEAD
##### === Forest Functions Variables Assembly === #####

### === root_decomposition === ###
#Category1:                  flux
#Catergory2:                 trophic_flux
=======
### === Root decomposition === ####
>>>>>>> 933c3c868d0b0c15e98956f0196f9a37a5df9c3c
#Principal Investigator:     Schrumpf
#Dataset(s):                 16666_2_Dataset
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
<<<<<<< HEAD
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Mass_loss_October_2012"] <- "root_decomposition"
=======
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Mass_loss_October_2012"] <- "Root_decomposition"
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Root_decomposition))) #16 NAs
### ===== ###

### === Nitrogen availability === ####
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
#                            26229_4_Dataset
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
dat10 <- read.table(paste0(pathtodata, "Functions/26229_4_Dataset/26229_4_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat10)
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
dat10 <- BEplotZeros(dat10, "EP_Plotid", plotnam = "Plot")
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
names(dat10)[names(dat10) == "NH4"] <- "MinSoil_Ammonium_NH4_2017" 
names(dat10)[names(dat10) == "NO3"] <- "MinSoil_Nitrate_NO3_2017"
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
dat.1 <- merge(dat.1, dat10[,c("MinSoil_Ammonium_NH4_2017","MinSoil_Nitrate_NO3_2017","Plot")], by = "Plot", all.x = T)
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
          Nitrate_NO3_2016 = as.numeric(Nitrate_NO3_2016),
          #scale Ammonium and Nitrate measurements for subsequent average of temporal replicates, they stem from different measurement methods
          scaled_NH4_2014 = scale(Ammonium_NH4_2014, center = F, scale = T),
          scaled_NH4_2016 = scale(Ammonium_NH4_2014, center = F, scale = T),
          scaled_NH4_2017 = scale(MinSoil_Ammonium_NH4_2017, center = F, scale = T),
          scaled_NO3_2014 = scale(Nitrate_NO3_2014, center = F, scale = T),
          scaled_NO3_2016 = scale(Nitrate_NO3_2016, center = F, scale = T),
          scaled_NO3_2017 = scale(MinSoil_Nitrate_NO3_2017, center = F, scale = T)) %>% 
  #calculate the averages of temporal replicates
  rowwise( Plot) %>% 
  mutate( average_PNR = mean(c_across(c("PNR_2014", "PNR_2016")), na.rm = T),
          average_NH4 = mean(c_across(c("scaled_NH4_2014", "scaled_NH4_2016", "scaled_NH4_2017")), na.rm = T),
          average_NO3 = mean(c_across(c("scaled_NO3_2014", "scaled_NO3_2016", "scaled_NO3_2017")), na.rm = T),
          average_CN_MinSoil = mean(c_across(c("CN_ratio_MinSoil_2014", "CN_ratio_MinSoil_2017", "CN_ratio_MinSoil_2021")), na.rm = T))

#merge generated columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("PNR_2014","PNR_2016","average_PNR",
                                                                   "Ammonium_NH4_2014","Ammonium_NH4_2016",
                                                                   "MinSoil_Ammonium_NH4_2017","average_NH4",
                                                                   "Nitrate_NO3_2014","Nitrate_NO3_2016",
                                                                   "MinSoil_Nitrate_NO3_2017","average_NO3",
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
length(which(is.na(BE_synthesis_forest_dat$MinSoil_Ammonium_NH4_2017))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$average_NH4))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$Nmic))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$Nitrate_NO3_2014))) #29 NAs
length(which(is.na(BE_synthesis_forest_dat$Nitrate_NO3_2016))) #7 NAs
length(which(is.na(BE_synthesis_forest_dat$MinSoil_Nitrate_NO3_2017))) #1 NAs
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

### === Phosphorus availability === ####
#Principal Investigator:     Oelmann
#                            Schrumpf
#Dataset(s):                 19286_3_Dataset
#                            5241_5_Dataset
#                            19009_3_Dataset
#                            15766_3_Dataset
#                            31340_4_Dataset
#                            19366_4_Dataset
#                            26228_4_Dataset
#                            26229_4_Dataset
#Process and component name: P availability
#Relevant columns (unit):    OlsenPi (mg/kg) (19286_3_Dataset - OlsenPi_2014)
#                            NaHCO3_Pi (mg/kg)
#                            Resin_P (mg/kg)
#                            Pmic (mg/kg)
#                            Olsen-P (mg/kg) (31340_4_Dataset - OlsenPi_2021)
#                            CP_ratio (organic C/total P)
#                            PS_ratio (total P/total S)
#                            P_soluble (mg/g) (26228_4_Dataset - Upper Mineral Soil Orthophosphate)
#                            P_soluble (mg/g) (26229_4_Dataset - Organic Horizon Soil Orthophosphate)

#read data
dat <- read.table(paste0(pathtodata, "Functions/19286_3_Dataset/19286_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/5241_5_Dataset/5241_5_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/19009_3_Dataset/19009_3_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/15766_3_Dataset/15766_3_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/31340_4_Dataset/31340_4_data.txt"), header = T, sep = ";")
dat5 <- read.table(paste0(pathtodata, "Functions/19366_4_Dataset/19366_4_data.txt"), header = T, sep = ";")
dat6 <- read.table(paste0(pathtodata, "Functions/26228_4_Dataset/26228_4_data.txt"), header = T, sep = ";")
dat7 <- read.table(paste0(pathtodata, "Functions/26229_4_Dataset/26229_4_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat6)
dat <- BEplotZeros(dat, "Plot_ID", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "EP", plotnam = "Plot")
dat4 <- BEplotZeros(dat4, "EP_Plotid", plotnam = "Plot")
dat5 <- BEplotZeros(dat5, "EP_Plotid", plotnam = "Plot")
dat6 <- BEplotZeros(dat6, "EP_Plotid", plotnam = "Plot")
dat7 <- BEplotZeros(dat7, "EP_Plotid", plotnam = "Plot")

#special treatment for the added columns
#rename columns
names(dat)[names(dat) == "OlsenPi"] <- "OlsenPi_2014"
names(dat4)[names(dat4) == "Olsen.P"] <- "OlsenPi_2021"
names(dat5)[names(dat5) == "CP_ratio"] <- "Upper_MinSoil_CP_ratio"
names(dat5)[names(dat5) == "PS_ratio"] <- "Upper_MinSoil_PS_ratio"
names(dat6)[names(dat6) == "P_soluble"] <- "OrgLay_orthophosphate"
names(dat7)[names(dat7) == "P_soluble"] <- "MinSoil_orthophosphate"
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("OlsenPi_2014","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("NaHCO3_Pi","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3[,c("Pmic","Plot")], by = "Plot", all.x = T) #ONLY KEEP Pmic IF YOU INCLUDE PRI
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4[,c("OlsenPi_2021","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat5[,c("Upper_MinSoil_CP_ratio","Upper_MinSoil_PS_ratio","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat6[,c("OrgLay_orthophosphate","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat7[,c("MinSoil_orthophosphate","Plot")], by = "Plot", all.x = T)
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
length(which(is.na(BE_synthesis_forest_dat$OrgLay_orthophosphate))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$MinSoil_orthophosphate))) #1 NAs
### ===== ###

### === Sulfur availability === ####
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

>>>>>>> 933c3c868d0b0c15e98956f0196f9a37a5df9c3c
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$root_decomposition))) #16 NAs
### ===== ###

<<<<<<< HEAD
### === dung_removal === ###
#Category1:                  flux
#Catergory2:                 trophic_flux
=======
### === Dung decomposition === ####
>>>>>>> 933c3c868d0b0c15e98956f0196f9a37a5df9c3c
#Principal Investigator:     Blüthgen
#Dataset(s):                 21206_3_Dataset
#Relevant columns (unit):    removal_g (g)

#read data
dat <- read.table(paste0(pathtodata, "Functions/21206_3_Dataset/21206_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "EP", plotnam = "Plot")

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

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$dung_removal))) #2 NAs
<<<<<<< HEAD
### ===== ###

### === seed_depletion === ###
#Category1:                  flux
#Catergory2:                 trophic_flux
=======
length(which(is.na(BE_synthesis_forest_dat$dung_depletion))) #2 NAs
### ===== ###

### === Soil carbon cycling === ####
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

### === Phosphatase === ####
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

### === Habitat === ####
>>>>>>> 933c3c868d0b0c15e98956f0196f9a37a5df9c3c
#Principal Investigator:     Blüthgen
#Dataset(s):                 24966_3_Dataset
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

<<<<<<< HEAD
### === browsing_percentage === ###
#Category1:                  flux
#Catergory2:                 trophic_flux
=======
### === Productivity === ####
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

### === Herbivory === ####
>>>>>>> 933c3c868d0b0c15e98956f0196f9a37a5df9c3c
#Principal Investigator:     Ammer
#Dataset(s):                 20347_2_Dataset
#Relevant columns (unit):    Bper (%)

#read data
dat <- read.table(paste0(pathtodata, "Functions/20347_2_Dataset/20347_2_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")

#special treatment for the added columns of the 20347_2_Dataset
#before merging dat to BE_synthesis_forest_dat, overall browsing_percentage has to be formatted
#Where total number of saplings was equal to zero, the browsing percentage is given as NA, as there were no saplings to measure the browsing percentage. 
#However, there might be browsing percentage = 0, which is different from NA, as the saplings of the subplot were not subject to herbivory.
#Only consider heightclass == "-1" and tsg == "all" 
#Therefore, subset the relevant rows, pivot_wider and rename columns.
dat.1 <- dat %>% 
  subset( (tsg == "all" & heightclass == -1)) %>% 
  pivot_wider( id_cols = c("Plot"), names_from = c(tsg), values_from = Bper) 

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1, by = "Plot", all.x = T)
#rename added columns for more context
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "all"] <- "browsing_percentage"

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$browsing_percentage))) #47 NAs
### ===== ###

### === herbivore_leaf_damage === ###
#Category1:                  flux
#Catergory2:                 trophic_flux
#Principal Investigator:     Goßner
#Dataset(s):                 24806_2_Dataset
#Relevant columns (unit):    a_hole + a_edge (chewing damage; mm^2)
#                            a_scraping + a_sucking + a_scraping_sucking (scraping and sucking damage; mm^2)
#                            a_mines (mining damage; mm^2)
#                            a_galls + a_gallm_bottom + a_gallm_top + a_gallm_rolled (gall damage; mm^2)
#                            a_thrips (thrips damage; mm^2)

#read herbivory and species abundance data for community weighted means
herbivory0 <- read.table(paste0(pathtodata, "Functions/24806_2_Dataset/24806_2_data.txt"), header = T, sep = ";")
for_plants_up <- fread("P:/PROJECTS/Exploratories Synthesis/Data/Forest_diversity/raw_data/31405_5_Dataset/31405_5_data.csv")

#special treatment for the columns of the 24806_2_Dataset
#this is a large dataset with over 160'000 rows
#each row represents a single leaf, on which the area (mm^2) damaged by different classes of herbivores was measured
#up to 200 leaves per plant species and up to ten plant species per Plot
#to arrive at an overall measure for herbivory weighted by the species abundance the data is severely compressed
names(herbivory0)
head(herbivory0)
tail(herbivory0)

herbivory0 <- herbivory0 %>% 
  filter(system == "forest") %>%
  select(plotid_withzero, collection_year, pl_species,pl_ind_id, pl_lb_id, lf_id, lf_area, lf_area_corr, 
         a_hole:a_thrips)

summary(herbivory0)#Note that there are some NAs... What does it means?

#Get total and percentage of leaf damage by all types of herbivory0
herbivory0$a_herb_total <- herbivory0$a_hole+herbivory0$a_edge+herbivory0$a_scraping+herbivory0$a_sucking+
  herbivory0$a_mines+herbivory0$a_galls+herbivory0$a_gallm_bottom+
  herbivory0$a_gallm_top+herbivory0$a_gallm_rolled+herbivory0$a_thrips

herbivory0$perc_herb <- (herbivory0$a_herb_total/herbivory0$lf_area_corr)*100

summary(herbivory0)

#Check if all total herbivory0 values are lower than the area of leaves
which(herbivory0$lf_area < herbivory0$a_herb_total)#many
which(herbivory0$lf_area_corr < herbivory0$a_herb_total)#only three leaves, when considering corrected leaf areas
which(herbivory0$perc_herb > 100)

#Check those three leaves with leaf damage higher than leaf area
herbivory1 <- herbivory0 %>%
  filter(perc_herb < 100)#Remove those leaves above? - Note: additionally over 10,000 rows excluded due to NAs

#Calculate average herbivory by "leaf bundle" and then by species/plot
sp_plot_avg_herb <- herbivory1 %>% 
  group_by(plotid_withzero, pl_species, pl_lb_id) %>%
  summarise(lb_avg_herb = mean(perc_herb)) %>%
  ungroup() %>%
  group_by(plotid_withzero, pl_species) %>%
  summarise(sp_avg_herb = mean(lb_avg_herb))

summary(sp_plot_avg_herb)
rm(herbivory0, herbivory1)

##Plant inventory----
names(for_plants_up)
head(for_plants_up)

#Get sp cover per plot (shrubs and trees only)
cov_sp_plot_ST <- for_plants_up %>%
  filter(Cover > 0, Layer != "H", Useful_EPPlotID != "HEW02") %>%
  group_by(Useful_EPPlotID) %>%
  mutate(total_plot_cov = sum(Cover)) %>%
  ungroup() %>%
  group_by(Useful_EPPlotID, Species) %>%
  summarise(total_sp_cov = sum(Cover)) 
summary(cov_sp_plot_ST)

#Get sp cover per plot (herbs only)
cov_sp_plot_H <- for_plants_up %>%
  filter(Cover > 0, Layer == "H", Useful_EPPlotID != "HEW02") %>%
  group_by(Useful_EPPlotID) %>%
  mutate(total_plot_cov = sum(Cover)) %>%
  ungroup() %>%
  group_by(Useful_EPPlotID, Species) %>%
  summarise(total_sp_cov = sum(Cover)) 
summary(cov_sp_plot_H)

#Get sp cover per plot (overall)
cov_sp_plot_all <- for_plants_up %>%
  filter(Cover > 0, Useful_EPPlotID != "HEW02") %>%
  group_by(Useful_EPPlotID) %>%
  mutate(total_plot_cov = sum(Cover)) %>%
  ungroup() %>%
  group_by(Useful_EPPlotID, Species) %>%
  summarise(total_sp_cov = sum(Cover)) 
summary(cov_sp_plot_all)

#Analysis----

##Check if all species in the plant inventory are in the herbivory dataset----
names(sp_plot_avg_herb) <- c("plot", "species", "sp_avg_herb")

names(cov_sp_plot_ST) <- c("plot", "species", "cover")
cov_sp_plot_ST$species <- gsub("_", " ", cov_sp_plot_ST$species)

names(cov_sp_plot_H) <- c("plot", "species", "cover")
cov_sp_plot_H$species <- gsub("_", " ", cov_sp_plot_H$species)

names(cov_sp_plot_all) <- c("plot", "species", "cover")
cov_sp_plot_all$species <- gsub("_", " ", cov_sp_plot_all$species)

data_ST <- cov_sp_plot_ST %>%
  left_join(sp_plot_avg_herb, by = c("plot", "species"))  
summary(data_ST)#1459 out of 1921 species-plot combinations without herbivory information

data_H <- cov_sp_plot_H %>%
  left_join(sp_plot_avg_herb, by = c("plot", "species"))  
summary(data_H)#6062 out of 6675 species-plot combinations without herbivory information

data_all <- cov_sp_plot_all %>%
  left_join(sp_plot_avg_herb, by = c("plot", "species"))  
summary(data_all)#7516 out of 8590 species-plot combinations without herbivory information

##Check coverage of plot total abundance by herbivory data----
coverage_ST <- data_ST %>%
  group_by(plot) %>%
  mutate(total_cov = sum(cover)) %>%
  ungroup() %>%
  filter(!is.na(sp_avg_herb)) %>%
  group_by(plot) %>%
  summarise(coverage_ST = sum(cover)/total_cov) %>%
  unique()
summary(coverage_ST)#Good coverage overall - minimum 34% but 1st quantile = 91%!

coverage_H <- data_H %>%
  group_by(plot) %>%
  mutate(total_cov = sum(cover)) %>%
  ungroup() %>%
  filter(!is.na(sp_avg_herb)) %>%
  group_by(plot) %>%
  summarise(coverage_H = sum(cover)/total_cov) %>%
  unique()
summary(coverage_H)#Relatively low coverage

coverage_all <- data_all %>%
  group_by(plot) %>%
  mutate(total_cov = sum(cover)) %>%
  ungroup() %>%
  filter(!is.na(sp_avg_herb)) %>%
  group_by(plot) %>%
  summarise(coverage_all = sum(cover)/total_cov) %>%
  unique()
summary(coverage_all)

##Calculate CWMs----
names(data_ST)
names(data_H)
names(data_all)

CWMs_ST <- data_ST %>%
  filter(!is.na(sp_avg_herb)) %>%
  group_by(plot) %>%
  mutate(total_plot_cov = sum(cover)) %>%
  ungroup() %>%
  group_by(plot, species) %>%
  mutate(cover_herb = cover*sp_avg_herb) %>%
  ungroup() %>%
  group_by(plot) %>%
  summarise(CWM_herb_ST = sum(cover_herb)/total_plot_cov) %>%
  unique()

CWMs_H <- data_H %>%
  filter(!is.na(sp_avg_herb)) %>%
  group_by(plot) %>%
  mutate(total_plot_cov = sum(cover)) %>%
  ungroup() %>%
  group_by(plot, species) %>%
  mutate(cover_herb = cover*sp_avg_herb) %>%
  ungroup() %>%
  group_by(plot) %>%
  summarise(CWM_herb_H = sum(cover_herb)/total_plot_cov) %>%
  unique()

CWMs_all <- data_all %>%
  filter(!is.na(sp_avg_herb)) %>%
  group_by(plot) %>%
  mutate(total_plot_cov = sum(cover)) %>%
  ungroup() %>%
  group_by(plot, species) %>%
  mutate(cover_herb = cover*sp_avg_herb) %>%
  ungroup() %>%
  group_by(plot) %>%
  summarise(CWM_herb_all = sum(cover_herb)/total_plot_cov) %>%
  unique()

##Join information on coverage----
names(CWMs_all)
names(coverage_all)

CWMs_all <- left_join(CWMs_all, coverage_all)
summary(CWMs_all)

CWMs_ST <- left_join(CWMs_ST, coverage_ST)
summary(CWMs_ST)

CWMs_H <- left_join(CWMs_H, coverage_H)
summary(CWMs_H)

##Filter out communities with low coverage----
CWMs_all <- CWMs_all %>%
  filter(coverage_all >= 0.7)

CWMs_ST <- CWMs_ST %>%
  filter(coverage_ST >= 0.7)

CWMs_H <- CWMs_H %>%
  filter(coverage_H >= 0.7)

##Final output----
CWM_final <- CWMs_all[,1:2] %>% full_join(CWMs_ST[,1:2]) %>% full_join(CWMs_H[,1:2])
summary(CWM_final)

#merge relevant columns with the BE_synthesis_forest_dat
#rename plot column for the merge
names(CWM_final)[names(CWM_final) == "plot"] <- "Plot"
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, CWM_final, by = "Plot", all.x = T)
#rename added columns for more context
names(BE_synthesis_forest_dat)
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CWM_herb_all"] <- "herbivore_leaf_damage_all"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CWM_herb_ST"] <- "herbivore_leaf_damage_shrubs_trees"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CWM_herb_H"] <- "herbivore_leaf_damage_herbs"

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$herbivore_leaf_damage_all))) #39 NAs
length(which(is.na(BE_synthesis_forest_dat$herbivore_leaf_damage_shrubs_trees))) #6 NAs
length(which(is.na(BE_synthesis_forest_dat$herbivore_leaf_damage_herbs))) #91 NAs
### ===== ###

### === caterpillars_predation === ###
#Category1:                  flux
#Catergory2:                 trophic_flux
#Principal Investigator:     Blüthgen
#Dataset(s):                 25807_3_Dataset
#Relevant columns (unit):    Predation_prop (proportion)

#read data
dat <- read.table(paste0(pathtodata, "Functions/25807_3_Dataset/25807_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "PlotID", plotnam = "Plot")

#special treatment, averaging the five measurements per Plot
dat.1 <- dat %>% 
  group_by( Plot) %>% 
  summarise( caterpillars_predation = mean(Predation_prop, na.rm = T))
#merge caterpillars_predation with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("caterpillars_predation","Plot")], by = "Plot", all.x = T)
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$caterpillars_predation))) #2 NAs
### ===== ###

### === barkbeetle_antagonist_ratio === ###
#Category1:                  flux
#Catergory2:                 trophic_flux
#Principal Investigator:     Weisser
#Dataset(s):                 20035_2_Dataset
#Relevant columns (unit):    BB_Antagonist_ratio

#read data
dat <- read.table(paste0(pathtodata, "Functions/20035_2_Dataset/20035_2_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("BB_Antagonist_ratio","Plot")], by = "Plot", all.x = T)

#special treatment for the added column, rename
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "BB_Antagonist_ratio"] <- "barkbeetle_antagonist_ratio"
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$barkbeetle_antagonist_ratio))) #2 NAs
### ===== ###

<<<<<<< HEAD
### === Carbon cycle fluxes === ###
#Category1:                  flux
#Catergory2:                 C_cycle
#Principal Investigator(s):  Trumbore
=======
### === Nutrient cycling === ####
#Principal Investigator:     Polle
#                            Schrumpf
>>>>>>> 933c3c868d0b0c15e98956f0196f9a37a5df9c3c
#                            Marhan
#                            Schrumpf
#Dataset(s):                 17166_3_Dataset
#                            17026_3_Dataset
#                            22686_5_Dataset
#                            26908_4_Dataset
#                            26306_7_Dataset
#Relevant columns (unit):    Glu (nmol/(g*h))
#                            N_Ac (nmol/(g*h))
#                            Xyl (nmol/(g*h))
#                            Res_14 (µg/g) (17026_3_Dataset - Soil respiration 2011)
#                            CO2_rate_mean (µg/g) (potential soil respiration rate 2017)
#                            Soil_respiration_2018 (g/(m^2*d))
#                            Soil_respiration_2019 (g/(m^2*d))
#                            PMOR ng/(g*h)

#read data
dat <- read.table(paste0(pathtodata, "Functions/17166_3_Dataset/17166_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/17026_3_Dataset/17026_3_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/22686_5_Dataset/22686_5_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/26908_4_Dataset/26908_4_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/26306_7_Dataset/26306_7_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat3)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "PlotID", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "EP_Plotid", plotnam = "Plot")
dat4 <- BEplotZeros(dat4, "EP_PlotID", plotnam = "Plot")

#special treatment for the added columns of the 17166_3_Dataset
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("Glu","N_Ac","Xyl","Plot")], by = "Plot", all.x = T)

#rename columns
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Glu"] <- "soil_beta_glucosidase"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "N_Ac"] <- "soil_Nacetyl_glucosaminidase"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Xyl"] <- "soil_xylanase"

#calculate the mini-multifunctionality variable "soil_carbon_fluxes" as in the synthesis dataset functions grassland
#requires the variables soil_beta_glucosidase, soil_Nacetyl_glucosaminidase and soil_xylanase
#therefore, select the relevant columns from "BE_synthesis_forest_dat"
dat.1 <- BE_synthesis_forest_dat %>% 
  subset( select = c("Plot", "soil_beta_glucosidase", "soil_Nacetyl_glucosaminidase", "soil_xylanase")) %>% 
  #and calculate mini-multifunctionalities based on z-scores (sd = 1, mean = 0) of input variables
  #function returns a matrix of which we only need the first column
  mutate( soil_carbon_fluxes = multidiv(.[,c("soil_beta_glucosidase", "soil_Nacetyl_glucosaminidase", "soil_xylanase")], sc = "sd", cent = T)[,1])

#merge the soilCflxs column
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("soil_carbon_fluxes","Plot")], by = "Plot", all.x = T)
#=#

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("Res_14", "Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2[,c("CO2_rate_mean", "Plot")], by = "Plot", all.x = T)

#special treatment for the added columns of the 17026_3_Dataset and 22686_5_Dataset
#rename columns
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "Res_14"] <- "soil_respiration_2011"
names(BE_synthesis_forest_dat)[names(BE_synthesis_forest_dat) == "CO2_rate_mean"] <- "soil_respiration_2017"

#special treatment for the added columns of the 26908_4_Dataset
#soil respiration was measured in 2018 and 2019. Create soil_respiration_2018 and soil_respiration_2019 columns.
dat3.1 <- dat3 %>%
  subset( Year == "2018") %>% 
  pivot_wider( names_from = Year, names_glue = "soil_respiration_{Year}", values_from = Rs)

dat3.2 <- dat3 %>%
  subset( Year == "2019") %>% 
  pivot_wider( names_from = Year, names_glue = "soil_respiration_{Year}", values_from = Rs)

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3.1[,c("soil_respiration_2018", "Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3.2[,c("soil_respiration_2019", "Plot")], by = "Plot", all.x = T)

#then calculate the average soil respiration across the years 2011, 2017, 2018 and 2019
dat3.3 <- BE_synthesis_forest_dat %>% 
  #select relevant columns
  subset( select = c("Plot", "soil_respiration_2011", "soil_respiration_2017", "soil_respiration_2018", "soil_respiration_2019")) %>% 
  #scale respiration measurements within years, across all plots, since the units differ between 2011, 2017 and 2018, 2019
  mutate( scaled_res_2011 = scale(soil_respiration_2011, center = F, scale = T),
          scaled_res_2017 = scale(soil_respiration_2017, center = F, scale = T),
          scaled_res_2018 = scale(soil_respiration_2018, center = F, scale = T),
          scaled_res_2019 = scale(soil_respiration_2019, center = F, scale = T)) %>% 
  #calculate average of temporal replicates for each plot
  rowwise( Plot) %>% 
  mutate( average_soil_respiration = mean(c_across(c("scaled_res_2011", "scaled_res_2017", "scaled_res_2018", "scaled_res_2019")), na.rm = T))

#merge
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3.3[,c("average_soil_respiration", "Plot")], by = "Plot", all.x = T)
#=#

#special treatment for the columns of the 26306_7_Dataset
#there are two replicates per plot, average them
#therefore, subset forest plots and average replicates per Plot
dat4.1 <- dat4 %>% 
  subset( Plot %in% BE_synthesis_forest_dat$Plot) %>% 
  group_by( Plot) %>% 
  summarise( soil_methane_oxidation = mean(PMOR, na.rm = T))
#merge the averaged column
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4.1[,c("soil_methane_oxidation", "Plot")], by = "Plot", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$soil_beta_glucosidase))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_Nacetyl_glucosaminidase))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_xylanase))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_carbon_fluxes))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_respiration_2011))) #8 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_respiration_2017))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_respiration_2018))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_respiration_2019))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$average_soil_respiration))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_methane_oxidation))) #1 NAs
### ===== ###

### === Carbon cycle stocks === ###
#Category1:                  stock
#Catergory2:                 C_cycle
#Principal Investigator(s):  Bonkowski
#                            Polle
#                            Schrumpf
#                            Trumbore
#Dataset(s):                 14106_2_Dataset
#                            19230_3_Dataset
#                            20127_17_Dataset
#                            17086_4_Dataset
#                            20266_3_Dataset
#Relevant columns (unit):    Cmic (µg/g)
#                            Carbon_within_fine_roots_Soil_Sampling_May_2011 (mg/g)
#                            Total_litter_C (g/kg)
#                            OC_stock (kg/m^2) (17086_4_Dataset - organic soilc carbon stock 2011)
#                            OC_stock (kg/m^2) (20266_3_Dataset - organic soilc carbon stock 2014)

#read data
dat <- read.table(paste0(pathtodata, "Functions/14106_2_Dataset/14106_2_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/19230_3_Dataset/19230_3_data.txt"), header = T, sep = ";")
dat1.1 <- read.table(paste0(pathtodata, "Functions/14567_5_Dataset/14567_5_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/20127_17_Dataset/20127_17_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/17086_4_Dataset/17086_4_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/20266_3_Dataset/20266_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat1)
dat <- BEplotZeros(dat, "PlotID", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat1.1 <- BEplotZeros(dat1.1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "EP_Plotid", plotnam = "Plot")
dat4 <- BEplotZeros(dat4, "EP_Plotid", plotnam = "Plot")

#rename columns
names(dat)[names(dat) == "Cmic"] <- "microbial_C"
names(dat1)[names(dat1) == "Carbon_within_fine_roots_Soil_Sampling_May_2011"] <- "fine_roots_C"
names(dat3)[names(dat3) == "OC_stock"] <- "soil_org_C_2011"
names(dat4)[names(dat4) == "OC_stock"] <- "soil_org_C_2014"
#merge
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("microbial_C","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("Carbon_within_fine_roots_Soil_Sampling_May_2011","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3[,c("soil_org_C_2011","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4[,c("soil_org_C_2014","Plot")], by = "Plot", all.x = T)
#=#

#special treatment for the columns of the 19230_3_Dataset and 14567_5_Dataset
#both datasets measure fine roots carbon concentration, but 14567_5_Dataset also contains data from grassland plots
#correlate the "Carbon_within_fine_roots_Soil_Sampling_May_2011" and "Total_C" values, 
#if they correlate well, use values from the 14567_5_Dataset
#subset forest plots for both datasets
dat1.2 <- merge(BE_synthesis_forest_dat[,c(1:4)], dat1[,c("fine_roots_C","Plot")], by = "Plot", all.x = T)
dat1.3 <- merge(dat1.2, dat1.1[,c("Total_C","Plot")], by = "Plot", all.x = T)
#plot data (there is one outlier)
plot(dat1.3$fine_roots_C, dat1.3$Total_C)
#correlate data (pearson r = 0.361)
cor(dat1.3$fine_roots_C, dat1.3$Total_C, use = "pairwise.complete.obs", method = "pearson")
#remove the one outlier from 14567_5_Dataset and correlate again
#removing the outlier increases the correlation to pearson r = 0.387, not worth it to use 14567_5_Dataset over 19230_3_Dataset
dat1.3[dat1.3$Plot == "AEW17", "fine_roots_C"] <- NA
plot(dat1.3$fine_roots_C, dat1.3$Total_C)
cor(dat1.3$fine_roots_C, dat1.3$Total_C, use = "pairwise.complete.obs", method = "pearson")
#merge the fine_roots_C variable from 19230_3_Dataset
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("fine_roots_C","Plot")], by = "Plot", all.x = T)
#=#

#special treatment for the temporal replicates of total litter C stocks of the 20127_17_Dataset
names(dat2)
dat2.1 <- dat2 %>% 
  subset( select = c(Plot, Year, Season, Element, Total_litter)) %>% 
  #remove the year 2019, because there are no measurements for Total_litter and remove Sulfur measurements
  subset( !(Year == "2019") & !(Element == "S") & !(Element == "N")) %>% 
  #reformat to wide for calculation of averages between seasons
  pivot_wider( names_from = c("Season", "Element", "Year"), values_from = "Total_litter") %>% 
  #average seasonal measurements to get a value for each year and then average the years
  rowwise( Plot) %>% 
  mutate( Total_litter_C_2015 = mean(c_across(c("S_C_2015", "A_C_2015")), na.rm = T),
          Total_litter_C_2016 = mean(c_across(c("W_C_2016", "S_C_2016", "A_C_2016")), na.rm = T),
          Total_litter_C_2017 = mean(c_across(c("W_C_2017", "S_C_2017", "A_C_2017")), na.rm = T),
          Total_litter_C_2018 = mean(c_across(c("W_C_2018", "S_C_2018")), na.rm = T),
          average_Total_litter_C = mean(c_across(c("Total_litter_C_2015", "Total_litter_C_2016", 
                                                   "Total_litter_C_2017", "Total_litter_C_2018")), na.rm = T))
#merge individual years, as well as the average between years
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2.1[,c("Total_litter_C_2015", "Total_litter_C_2016",
                                                                    "Total_litter_C_2017", "Total_litter_C_2018",
                                                                    "average_Total_litter_C",
                                                                    "Plot")], by = "Plot", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$microbial_C))) #0 NAs 
length(which(is.na(BE_synthesis_forest_dat$fine_roots_C))) #2 NAs 
length(which(is.na(BE_synthesis_forest_dat$Total_litter_C_2015))) #28 NAs
length(which(is.na(BE_synthesis_forest_dat$Total_litter_C_2016))) #28 NAs
length(which(is.na(BE_synthesis_forest_dat$Total_litter_C_2017))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Total_litter_C_2018))) #3 NAs
length(which(is.na(BE_synthesis_forest_dat$average_Total_litter_C))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_org_C_2011))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_org_C_2014))) #1 NAs
### ===== ###

<<<<<<< HEAD
### === Nitrogen cycle fluxes === ###
#Category1:                  flux
#Catergory2:                 N_cycle
#Principal Investigator:     Schloter
#                            Schrumpf
#Dataset(s):                 19847_3_Dataset
#                            21546_2_Dataset
#                            27266_5_Dataset
#Relevant columns (unit):    PNR (ng/(g*h)) (19847_3_Dataset - Potential Nitrification Rate 2014)
#                            pN (ng/(g*h)) (21546_2_Dataset - Potential Nitrification Rate 2016)
#                            amoA_AOA
#                            amoA_AOB
#                            nxrA_NB
#                            X16S_NS
#                            Ammonium
#                            Nitrate
=======
### === Predation === ####
#Principal Investigator:     Blüthgen
#Dataset(s):                 25807_3_Dataset
#Process and component name: Predation
#Relevant columns (unit):    Predation_prop (fraction )
>>>>>>> 933c3c868d0b0c15e98956f0196f9a37a5df9c3c

#read data
dat <- read.table(paste0(pathtodata, "Functions/19847_3_Dataset/19847_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/21546_2_Dataset/21546_2_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/27266_5_Dataset/27266_5_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat2)
dat <- BEplotZeros(dat, "Plot_ID", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "Plot_ID", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "Plot")

#special treatment before adding the columns of the 19847_3_Dataset and the 21546_2_Dataset
#rename columns
names(dat)[names(dat) == "PNR"] <- "soil_potential_nitrification_2014"
names(dat1)[names(dat1) == "pN"] <- "soil_potential_nitrification_2016"
#=#

#special treatment for temporal replicates
#average years to generate the "average_soil_potential_nitrification" variables
#therefore, create an intermediate dataframe with the relevant columns
dat.1 <- merge(BE_synthesis_identifier_dat, dat[,c("soil_potential_nitrification_2014", "Plot")], by = "Plot", all.x = T)
dat.1 <- merge(dat.1, dat1[,c("amoA_AOA","amoA_AOB","nxrA_NB","X16S_NS","soil_potential_nitrification_2016","Plot")], by = "Plot", all.x = T)
#instead of values some rows contain the string "bdl" and their columns are character columns
#replace these strings with NA
dat.1[dat.1 == "bdl"] <- NA
#change the character columns to numeric, to enable calculation of an average and rename them
dat.1 <- dat.1 %>% 
  mutate( soil_potential_nitrification_2014 = as.numeric(soil_potential_nitrification_2014),
          soil_potential_nitrification_2016 = as.numeric(soil_potential_nitrification_2016),
          ammonia_oxidizing_archaea = amoA_AOA,
          ammonia_oxidizing_bacteria = amoA_AOB,
          nitrite_oxidizing_nitrobacter = as.numeric(nxrA_NB),
          nitrite_oxidizing_nitrospira = as.numeric(X16S_NS)) %>% 
  #calculate the averages of temporal replicates
  rowwise( Plot) %>% 
  mutate( average_soil_potential_nitrification = mean(c_across(c("soil_potential_nitrification_2014", "soil_potential_nitrification_2016")), na.rm = T))

#merge generated columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("soil_potential_nitrification_2014",
                                                                   "soil_potential_nitrification_2016",
                                                                   "average_soil_potential_nitrification",
                                                                   "ammonia_oxidizing_archaea","ammonia_oxidizing_bacteria",
                                                                   "nitrite_oxidizing_nitrobacter","nitrite_oxidizing_nitrospira",
                                                                   "Plot")], by = "Plot", all.x = T)
#=#

#calculate the mini-multifunctionality variable "forest_soil_nitrate_fluxes"
#a similar variable exists in the grassland functions synthesis dataset (27087)
#in the 27087 dataset, "soilNitrateflxs" is calculated with the variables nxrA_NB, 16S_NS, nifH and DEA
#we lack nifH and DEA data for forests
#therefore, "forest_soil_nitrate_fluxes" is only calculated with the variables nitrite_oxidizing_nitrobacter and nitrite_oxidizing_nitrospira (from the 21546_2_Dataset)
#select the relevant columns from "BE_synthesis_forest_dat"
dat.3 <- BE_synthesis_forest_dat %>% 
  subset( select = c("Plot", "nitrite_oxidizing_nitrobacter", "nitrite_oxidizing_nitrospira")) %>%
  #like in the grasslands, nitrite_oxidizing_nitrobacter and nitrite_oxidizing_nitrospira are summed up to “nitrite-oxidising functional gene abundances” (nitOx_fga)
  rowwise( Plot) %>% 
  mutate( nitOx_fga = sum(c(nitrite_oxidizing_nitrobacter, nitrite_oxidizing_nitrospira), na.rm = T))

#replace all "0" values of "nitOx_fga" by NA
dat.3[dat.3$nitOx_fga == 0,] <- NA

#then calculate mini-multifunctionality which here is simply the z-score of nitOx_fga (sd = 1, mean = 0)
dat.3$forest_soil_nitrate_fluxes <- multidiv(dat.3[,4], sc = "sd", cent = F)[,1]

#merge the forest_soilNitrateflxs column
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.3[,c("forest_soil_nitrate_fluxes","Plot")], by = "Plot", all.x = T)
#=#

#calculate the mini-multifunctionality variable "forest_soil_ammonia_fluxes"
#a similar variable exists in the grassland functions synthesis dataset (27087)
#in the 27087 dataset, "soilAmmoniaflxs" is calculated with the variables amoA_AOA, amoA_AOB and Urease
#we lack Urease data for forests
#therefore, "forest_soil_ammonia_fluxes" is only calculated with the variables amoA_AOA, amoA_AOB (from the 21546_2_Dataset)
#select the relevant columns from "BE_synthesis_forest_dat"
dat.4 <- BE_synthesis_forest_dat %>% 
  subset( select = c("Plot", "ammonia_oxidizing_archaea", "ammonia_oxidizing_bacteria")) %>%
  #like in the grasslands, amoA_AOA and amoA_AOB are summed up to “ammonia-oxidising functional gene abundances” (amOX_fga)
  rowwise( Plot) %>% 
  mutate( amOX_fga = sum(c(ammonia_oxidizing_archaea, ammonia_oxidizing_bacteria), na.rm = T))

#replace all "0" values of "nitOx_fga" by NA
dat.4[dat.4$amOX_fga == 0,] <- NA

#then calculate mini-multifunctionality which here is simply the z-score of nitOx_fga (sd = 1, mean = 0)
dat.4$forest_soil_ammonia_fluxes <- multidiv(dat.4[,4], sc = "sd", cent = F)[,1]

#merge the forest_soilNitrateflxs column
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.4[,c("forest_soil_ammonia_fluxes","Plot")], by = "Plot", all.x = T)
#=#

#special treatment for the columns of the 27266_5_Dataset
#calculate the "N_retention" variable, by inverting the Ammonia and Nitrate variables and summing them up
dat2.1 <- dat2 %>% 
  rowwise( ) %>% 
  mutate( N_retention = 1/(sum(c(Ammonium, Nitrate), na.rm = T)))

#merge N_retention column
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2.1[,c("N_retention","Plot")], by = "Plot", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$soil_potential_nitrification_2014))) #11 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_potential_nitrification_2016))) #11 NAs
length(which(is.na(BE_synthesis_forest_dat$average_soil_potential_nitrification))) #11 NAs
length(which(is.na(BE_synthesis_forest_dat$ammonia_oxidizing_archaea))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$ammonia_oxidizing_bacteria))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$nitrite_oxidizing_nitrobacter))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$nitrite_oxidizing_nitrospira))) #12 NAs
length(which(is.na(BE_synthesis_forest_dat$forest_soil_nitrate_fluxes))) #3 NAs
length(which(is.na(BE_synthesis_forest_dat$forest_soil_ammonia_fluxes))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$N_retention))) #1 NAs
### ===== ###

<<<<<<< HEAD
### === Nitrogen cycle stocks === ###
#Category1:                  stock
#Catergory2:                 N_cycle
#Principal Investigator:     Bonkowski
#                            Polle
#                            Schrumpf
#                            Trumbore
#Dataset(s):                 14106_2_Dataset
#                            19230_3_Dataset
#                            20127_17_Dataset
=======
### === Ecological functioning === ####
#Principal Investigator:     Polle
#Dataset(s):                 26228_4_Dataset
#                            26229_4_Dataset
>>>>>>> 933c3c868d0b0c15e98956f0196f9a37a5df9c3c
#                            17086_4_Dataset
#                            20266_3_Dataset
#Relevant columns (unit):    Nmic (µg/g)
#                            Nitrogen_within_fine_roots_Soil_Sampling_May_2011 (mg/g)
#                            N_stock (kg/m^2)
#                            Total_litter (g/kg)

#read data
dat <- read.table(paste0(pathtodata, "Functions/14106_2_Dataset/14106_2_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/19230_3_Dataset/19230_3_data.txt"), header = T, sep = ";")
dat1.1 <- read.table(paste0(pathtodata, "Functions/14567_5_Dataset/14567_5_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/20127_17_Dataset/20127_17_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/17086_4_Dataset/17086_4_data.txt"), header = T, sep = ";")
dat4 <- read.table(paste0(pathtodata, "Functions/20266_3_Dataset/20266_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat2)
dat <- BEplotZeros(dat, "PlotID", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat1.1 <- BEplotZeros(dat1.1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP_Plotid", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "EP_Plotid", plotnam = "Plot")
dat4 <- BEplotZeros(dat4, "EP_Plotid", plotnam = "Plot")

#rename columns
names(dat)[names(dat) == "Nmic"] <- "microbial_N"
names(dat1)[names(dat1) == "Nitrogen_within_fine_roots_Soil_Sampling_May_2011"] <- "fine_roots_N"
names(dat3)[names(dat3) == "N_stock"] <- "soil_N_2011"
names(dat4)[names(dat4) == "N_stock"] <- "soil_N_2014"
#merge
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c("microbial_N","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3[,c("soil_N_2011", "Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat4[,c("soil_N_2014", "Plot")], by = "Plot", all.x = T)
#=#

#special treatment for the columns of the 19230_3_Dataset and 14567_5_Dataset
#both datasets measure fine roots nitrogen concentration, but 14567_5_Dataset also contains data from grassland plots
#correlate the "fine_roots_N" and "Total_N" values, 
#if they correlate well, use values from the 14567_5_Dataset
#subset forest plots for both datasets
dat1.2 <- merge(BE_synthesis_forest_dat[,c(1:4)], dat1[,c("fine_roots_N","Plot")], by = "Plot", all.x = T)
dat1.3 <- merge(dat1.2, dat1.1[,c("Total_N","Plot")], by = "Plot", all.x = T)
#plot data (there is one outlier)
plot(dat1.3$fine_roots_N, dat1.3$Total_N)
#correlate data (pearson r = 0.573)
cor(dat1.3$fine_roots_N, dat1.3$Total_N, use = "pairwise.complete.obs", method = "pearson")
#remove the one outlier from 14567_5_Dataset and correlate again
#removing the outlier decreases the correlation to pearson r = 0.51, not worth it to use 14567_5_Dataset over 19230_3_Dataset
dat1.3[dat1.3$Plot == "AEW17", "fine_roots_N"] <- NA
plot(dat1.3$fine_roots_N, dat1.3$Total_N)
cor(dat1.3$fine_roots_N, dat1.3$Total_N, use = "pairwise.complete.obs", method = "pearson")
#merge the fine_roots_N variable from the 19230_3_Dataset
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c("fine_roots_N", "Plot")], by = "Plot", all.x = T)
#=#

#special treatment for the temporal replicates of total litter N stocks of the 20127_17_Dataset
names(dat2)
dat2.1 <- dat2 %>% 
  subset( select = c(Plot, Year, Season, Element, Total_litter)) %>% 
  #remove the year 2019, because there are no measurements for Total_litter and remove Sulfur measurements
  subset( !(Year == "2019") & !(Element == "S") & !(Element == "C")) %>% 
  #reformat to wide for calculation of averages between seasons
  pivot_wider( names_from = c("Season", "Element", "Year"), values_from = "Total_litter") %>% 
  #average seasonal measurements to get a value for each year and then average the years
  rowwise( Plot) %>% 
  mutate( Total_litter_N_2015 = mean(c_across(c("S_N_2015", "A_N_2015")), na.rm = T),
          Total_litter_N_2016 = mean(c_across(c("W_N_2016", "S_N_2016", "A_N_2016")), na.rm = T),
          Total_litter_N_2017 = mean(c_across(c("W_N_2017", "S_N_2017", "A_N_2017")), na.rm = T),
          Total_litter_N_2018 = mean(c_across(c("W_N_2018", "S_N_2018")), na.rm = T),
          average_Total_litter_N = mean(c_across(c("Total_litter_N_2015", "Total_litter_N_2016", 
                                                   "Total_litter_N_2017", "Total_litter_N_2018")), na.rm = T))
#merge individual years, as well as the average between years
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2.1[,c("Total_litter_N_2015", "Total_litter_N_2016",
                                                                    "Total_litter_N_2017", "Total_litter_N_2018",
                                                                    "average_Total_litter_N",
                                                                    "Plot")], by = "Plot", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$microbial_N))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$fine_roots_N))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$Total_litter_N_2015))) #28 NAs
length(which(is.na(BE_synthesis_forest_dat$Total_litter_N_2016))) #28 NAs
length(which(is.na(BE_synthesis_forest_dat$Total_litter_N_2017))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$Total_litter_N_2018))) #3 NAs
length(which(is.na(BE_synthesis_forest_dat$average_Total_litter_N))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_N_2011))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_N_2014))) #1 NAs
### ===== ###

### === Phosphorus cycle fluxes === ###
#Category1:                  flux
#Catergory2:                 P_cycle
#Principal Investigator:     Trumbore
#                            Schrumpf
#                            Oelmann
#Dataset(s):                 17166_3_Dataset
#                            23906_7_Dataset
#                            19009_3_Dataset
#                            19286_3_Dataset
#                            31340_4_Dataset
#                            5241_5_Dataset
#Relevant columns (unit):    Pho (nmol/(g*h))
#                            Resin_P (mg/kg)
#                            OlsenPi (mg/kg) (19286_3_Dataset - OlsenPi_2014)
#                            Olsen-P (mg/kg) (31340_4_Dataset - OlsenPi_2021)
#                            NaHCO3_Pi (mg/kg)
#                            Pmic (mg/kg)

#read data
dat <- read.table(paste0(pathtodata, "Functions/17166_3_Dataset/17166_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/23906_7_Dataset/23906_7_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/19009_3_Dataset/19009_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP", plotnam = "Plot")

#special treatment for the columns of the columns of the 17166_3_Dataset and 23906_7_Dataset
#rename columns
names(dat)[names(dat) == "Pho"] <- "soil_phosphatase_2011"
names(dat1)[names(dat1) == "Pho"] <- "soil_phosphatase_2014"
#calculate an average of the temporal replicates of soil phosphatase stocks and soil Olsen-P
#therefore, create an intermediate dataframe with the relevant columns
dat.1 <- merge(BE_synthesis_identifier_dat, dat[,c("soil_phosphatase_2011", "Plot")], by = "Plot", all.x = T)
dat.2 <- merge(dat.1, dat1[,c("soil_phosphatase_2014","Plot")], by = "Plot", all.x = T)
#calculate the averages of temporal replicates
dat.3 <- dat.2 %>% 
  rowwise( Plot) %>% 
  mutate( average_soil_phosphatase = mean(c_across(c("soil_phosphatase_2011", "soil_phosphatase_2014")), na.rm = T))

#merge
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.3[,c("soil_phosphatase_2011", "soil_phosphatase_2014", "average_soil_phosphatase",
                                                                   "Plot")], by = "Plot", all.x = T)
#=#

#special treatment for the Resin_P variable of the 19009_3_Dataset
#Resin_P will be inverted and renamed to soil_P_retention
dat2.1 <- dat2 %>% 
  #subset forest plots
  subset( Plot %in% BE_synthesis_identifier_dat$Plot) %>% 
  #invert the variable
  mutate( soil_P_retention = 1/Resin_P)

#merge
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2.1[,c("soil_P_retention","Plot")], by = "Plot", all.x = T)
#=#

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$soil_phosphatase_2011))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_phosphatase_2014))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$average_soil_phosphatase))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_P_retention))) #1 NAs
### ===== ###

<<<<<<< HEAD
### === Phosphorus cycle stocks === ###
#Category1:                  stock
#Catergory2:                 P_cycle
#Principal Investigator:     Oelmann
#                            Schrumpf
#Dataset(s):                 19286_3_Dataset
#                            31340_4_Dataset
#                            5241_5_Dataset
#                            15766_3_Dataset
#Relevant columns (unit):    OlsenPi (mg/kg) (19286_3_Dataset - OlsenPi_2014)
#                            Olsen-P (mg/kg) (31340_4_Dataset - OlsenPi_2021)
#                            NaHCO3_Pi (mg/kg) 
#                            Pmic (mg/kg) (15766_3_Dataset - microbial_P_2011)
=======
### === Pest control === ####
#Principal Investigator:     Weisser
#Dataset(s):                 20035_2_Dataset
#Process and component name: Pest control
#Relevant columns (unit):    BB_Antagonist_ratio
>>>>>>> 933c3c868d0b0c15e98956f0196f9a37a5df9c3c

#read data
dat <- read.table(paste0(pathtodata, "Functions/19286_3_Dataset/19286_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/31340_4_Dataset/31340_4_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/5241_5_Dataset/5241_5_data.txt"), header = T, sep = ";")
dat3 <- read.table(paste0(pathtodata, "Functions/15766_3_Dataset/15766_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "EP", plotnam = "Plot")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "Plot")
dat2 <- BEplotZeros(dat2, "EP", plotnam = "Plot")
dat3 <- BEplotZeros(dat3, "EP", plotnam = "Plot")

#rename columns
names(dat)[names(dat) == "OlsenPi"] <- "soil_olsenP_2014"
names(dat1)[names(dat1) == "Olsen.P"] <- "soil_olsenP_2021"
names(dat2)[names(dat2) == "NaHCO3_Pi"] <- "soil_NaHCO3_P"
names(dat3)[names(dat3) == "Pmic"] <- "microbial_P_2011"

#merge columns without any special treatments
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2[,c("soil_NaHCO3_P","Plot")], by = "Plot", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat3[,c("microbial_P_2011","Plot")], by = "Plot", all.x = T)

#special treatment for the variables of the 19286_3_Dataset and 31340_4_Dataset
#calculate an average of the temporal replicates of soil Olsen-P
#therefore, create an intermediate dataframe with the relevant columns
dat.1 <- merge(BE_synthesis_identifier_dat, dat[,c("soil_olsenP_2014","Plot")], by = "Plot", all.x = T)
dat.2 <- merge(dat.1, dat1[,c("soil_olsenP_2021","Plot")], by = "Plot", all.x = T)
#calculate the averages of temporal replicates
dat.3 <- dat.2 %>% 
  rowwise( Plot) %>% 
  mutate( average_soil_olsenP = mean(c_across(c("soil_olsenP_2014", "soil_olsenP_2021")), na.rm = T))

#merge
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.3[,c("soil_olsenP_2014", "soil_olsenP_2021", "average_soil_olsenP", "Plot")], by = "Plot", all.x = T)

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$soil_NaHCO3_P))) #20 NAs
length(which(is.na(BE_synthesis_forest_dat$microbial_P_2011))) #2 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_olsenP_2014))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$soil_olsenP_2021))) #1 NAs
length(which(is.na(BE_synthesis_forest_dat$average_soil_olsenP))) #0 NAs
### ===== ###

### === Plant biomass stocks === ###
#Category1:                  stock
#Catergory2:                 plant_biomass
#Principal Investigator:     Schrumpf
#Dataset(s):                 14448_3_Dataset
#Relevant columns (unit):    Fine_Roots_Biomass (g/cm^3)
#                            Coarse_Roots_Biomass (g/cm^3)

#read data
dat <- read.table(paste0(pathtodata, "Functions/14448_3_Dataset/14448_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "Plot")

#special treatment for the added columns of the 14448_3_Dataset
#before merging sum up the Fine_Roots_Biomass and Coarse_Roots_Biomass to get root_Biomass (g/cm^3)
#if Fine_Roots_Biomass, or Coarse_Roots_Biomass is NA, Root_Biomass is simply the other value
dat.1 <- dat %>% 
  rowwise( ) %>% 
  mutate( root_biomass = sum(c(Fine_Roots_Biomass, Coarse_Roots_Biomass), na.rm = TRUE))

#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat.1[,c("root_biomass","Plot")], by = "Plot", all.x = T)

#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$root_biomass))) #1 NAs
### ===== ###

<<<<<<< HEAD

### === Final polishing of the dataset synthesis functions forest === ###
=======
### === Final polishing of the dataset synthesis functions forest === ####
>>>>>>> 933c3c868d0b0c15e98956f0196f9a37a5df9c3c
#replace NaN with NA
BE_synthesis_forest_dat[BE_synthesis_forest_dat == "NaN"] <- NA
#replace BExIS placeholder values "-888888.00" with NA
BE_synthesis_forest_dat[BE_synthesis_forest_dat == -888888.00] <- NA















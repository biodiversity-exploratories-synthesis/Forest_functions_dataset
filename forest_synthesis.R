### === Synthesis dataset functions forest assembly === ###

#Author(s): Marc Beringer
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
BE_synthesis_forest_dat <- BE_synthesis_identifier_dat %>% subset( habitat == "forest")
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
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,5:7], by = "BEplotID", all.x = T)
#special treatment for the added columns
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Mass_loss_April_2012))) #4 NAs
length(which(is.na(BE_synthesis_forest_dat$Mass_loss_October_2012))) #15 NAs
### ===== ###

### === Nitrogen availability === ###
#Principal Investigator:     Schloter
#Dataset(s):                 19847_3_Dataset
#Process and component name: N availability
#Relevant columns (unit):    Potential_nitrification_rate (ng/g)
#                            Ammonia_NH4 (µg/g)
#                            Nitrate_NO3- (mg/g)

#read data
dat <- read.table(paste0(pathtodata, "Functions/19847_3_Dataset/19847_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "Plot_ID", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c(3,5:7)], by = "BEplotID", all.x = T)
#special treatment for the added columns
#instead of values some rows contain the character "bdl"
#replace "bdl" with NA
BE_synthesis_forest_dat[BE_synthesis_forest_dat == "bdl"] <- NA
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$PNR))) #10 NAs
length(which(is.na(BE_synthesis_forest_dat$NH4))) #29 NAs
length(which(is.na(BE_synthesis_forest_dat$NO3))) #28 NAs
### ===== ###

### === Phosphorous availability === ###
#Principal Investigator:     Oelmann
#Dataset(s):                 19286_3_Dataset
#                            5241_5_Dataset
#                            19009_3_Dataset
#Process and component name: P availability
#Relevant columns (unit):    OlsenPi (mg/kg)
#                            NaHCO3_Pi (mg/kg)
#                            Resin_P (mg/kg)

#read data
dat <- read.table(paste0(pathtodata, "Functions/19286_3_Dataset/19286_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/5241_5_Dataset/5241_5_data.txt"), header = T, sep = ";")
dat2 <- read.table(paste0(pathtodata, "Functions/19009_3_Dataset/19009_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat2)
dat <- BEplotZeros(dat, "Plot_ID", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "EP", plotnam = "BEplotID")
dat2 <- BEplotZeros(dat2, "EP", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c(4,6)], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c(4,11)], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat2[,c(2,3)], by = "BEplotID", all.x = T)
#special treatment for the added columns
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$OlsenPi))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$NaHCO3_Pi))) #19 NAs
length(which(is.na(BE_synthesis_forest_dat$Resin_P))) #0 NAs
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
#Principal Investigator:     Trumbore
#Dataset(s):                 17166_3_Dataset
#                            17026_3_Dataset
#Process and component name: Soil C cycling
#Relevant columns (unit):    Glu (nmol/(g*h))
#                            N_Ac (nmol/(g*h))
#                            Xyl (nmol/(g*h))
#                            Res_14 (µg/g)

#read data
dat <- read.table(paste0(pathtodata, "Functions/17166_3_Dataset/17166_3_data.txt"), header = T, sep = ";")
dat1 <- read.table(paste0(pathtodata, "Functions/17026_3_Dataset/17026_3_data.txt"), header = T, sep = ";")
#add two-digit plot names for merging with the BE_synthesis_forest_dat
names(dat)
dat <- BEplotZeros(dat, "EP_Plotid", plotnam = "BEplotID")
dat1 <- BEplotZeros(dat1, "EP_Plotid", plotnam = "BEplotID")
#merge relevant columns with the BE_synthesis_forest_dat
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat[,c(5:7,9)], by = "BEplotID", all.x = T)
BE_synthesis_forest_dat <- merge(BE_synthesis_forest_dat, dat1[,c(5,6)], by = "BEplotID", all.x = T)
#special treatment for the added columns
#count NAs in the added columns
length(which(is.na(BE_synthesis_forest_dat$Glu))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$N_Ac))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$Xyl))) #0 NAs
length(which(is.na(BE_synthesis_forest_dat$Res_14))) #7 NAs
### ===== ###




























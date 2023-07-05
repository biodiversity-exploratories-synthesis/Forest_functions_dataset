### === Synthesis dataset functions forest assembly === ###
# Marc Beringer July 2023

setwd("C:/Users/Marc Beringer/Desktop/R/synthesis dataset functions forest")
getwd()

### === libraries === ###
{
  library(tidyverse) #load after plyr
}
### ===== ###

### === path to data === ###
# Path to raw data on the server of the institute of plant sciences in bern (switzerland)
pathtodata <- "P:/PROJECTS/Exploratories Synthesis/Data/Forest_functions/dataset_creation/raw_data/Functions/16666_2_Dataset/16666_2_data.txt"
pathtodata <- "P:/PROJECTS/Exploratories Synthesis/Data/Forest_functions/dataset_creation/raw_data/"
### ===== ###

### === data assembly === ###

### === 16666_2_dataset
dat_16666_2 <- read.table(paste0(pathtodata, "Functions/16666_2_Dataset/16666_2_data.txt"), header = T, sep = ";")

# get Mass_loss_April_2012 and Mass_loss_April_2012 columns

































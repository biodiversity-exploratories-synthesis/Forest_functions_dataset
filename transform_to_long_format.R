### === Synthesis dataset functions forest transformation to long format === ###

#Author: Marc Beringer
#Date: August, 2023
#Purpose: Transform the forest functions synthesis dataset from the wide into the long format suitable for upload on BExIS
#Requires: BE_synthesis_forest_dat.txt dataframe generated with the forest_functions_synthesis.R script
#          forest_functions_helper.txt dataframe generated with the forest_functions_helper.R script

#TODO after the upload refer to the correct DatasetID

### === working directory === ### 
setwd("C:/Users/Marc Beringer/Desktop/R/Synthesis_dataset_functions_forest")
### ===== ###

### === libraries === ###
library(tidyverse)
### ===== ###

### === read data === ###
forest_fun_syn_wide <- read.table("BE_synthesis_forest_dat_v1.txt", header = T, sep = "\t")
forest_fun_helper <- read.table("forest_functions_helper.txt", header = T, sep = "\t")
### ===== ###

### === dataset transformation to long format === ###
#the final forest functions synthesis dataset will have the seven columns:
#Plot (e.g. AEW01), Plot_bexis (e.g. AEW1), Function, Value, Year, Dataset_ID and Dataset_Version

#first, convert the wide forest functions synthesis dataset into the long format, but keep the column names
long_dat <- forest_fun_syn_wide %>% 
  #remove the "exploratory" and "habitat" columns as they will not be in the final dataset
  subset( select = -c(exploratory, habitat)) %>% 
  #pivot all columns of the forest functions synthesis dataset longer
  pivot_longer( cols = c(3:ncol(.)), names_to = c("AggregatedColumnName"), 
                values_to = "Value")

#second, merge the helper table
long_dat.1 <- merge(long_dat, forest_fun_helper, by = "AggregatedColumnName", all.x = T)

#third, remove the column "AggregatedColumnName" and reorder columns for convenience
names(long_dat.1)
long_dat.2 <- long_dat.1 %>% 
  subset( select = -c(AggregatedColumnName)) %>% 
  relocate( Plot, Plot_bexis, ColumnName, Value, codedYear, dataID, Dataset_Version)

#fourth, rename columns for the upload on BExIS
names(long_dat.2)[names(long_dat.2) == "ColumnName"] <- "Function"
names(long_dat.2)[names(long_dat.2) == "codedYear"] <- "Year"
names(long_dat.2)[names(long_dat.2) == "dataID"] <- "Dataset_ID"

#save the table, it is ready for upload on BExIS
write.table(long_dat.2, file = "synthesis_dataset_functions_forest.txt", quote = F, sep = "\t", row.names = F)
### ===== ###


















### === Forest functions synthesis dataset transformation to wide format === ###

#Author: Marc Beringer
#Date: August, 2023
#Purpose: Transform the forest functions synthesis dataset, downloaded from BExIS from the long back to the wide format
#Requires: synthesis_dataset_functions_forest_long.txt dataframe generated with the transform_to_long_format.R script

#TODO after the upload refer to the correct DatasetID

### === working directory === ### 
setwd("C:/Users/Marc Beringer/Desktop/R/Synthesis_dataset_functions_forest")
### ===== ###

### === libraries === ###
library(tidyverse)
### ===== ###

### === read data === ###
forest_fun_syn_long <- read.table("synthesis_dataset_functions_forest_long.txt", header = T, sep = "\t")
### ===== ###

### === dataset transformation back to the wide format === ###
#first, convert the wide forest functions synthesis dataset into the long format, but keep the column names
wide_dat <- forest_fun_syn_long %>% 
  #remove the "Dataset_ID" and "Dataset_Version" columns as they are noted in the metadata
  subset( select = -c(Dataset_ID, Dataset_Version)) %>% 
  #pivot all columns of the forest functions synthesis dataset wider
  pivot_wider( names_from = c("Function", "Year"), values_from = "Value")

#save the wide
write.table(wide_dat, file = "synthesis_dataset_functions_forest_wide.txt", quote = F, sep = "\t", row.names = F)
### ===== ###


















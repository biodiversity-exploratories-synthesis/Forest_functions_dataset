# # # # # # # # # # # # # # # # # # # #
#                                     #
# create forest_functions_helper.txt  #
#                                     #
# # # # # # # # # # # # # # # # # # # #
#
# Creation : file created 15.08.2023 by Marc Beringer inspired by the "create_27087_helper_from_additional_metadata.R" created by Noëlle Schenk
# Last edit : 15.08.2023 by Marc Beringer
# Aim : create the helper dataset "Forest_Functions_helper.csv" which simply is a subset 
#       of the metadata file (i.e. synthesis_forest_function_metadata_HIVE_local_copy.xlsx) 
#       to the upcoming forest functions synthesis dataset (not yet uploaded on BExIS).

### === working directory === ### 
setwd("C:/Users/Marc Beringer/Desktop/R/Synthesis_dataset_functions_forest")
### ===== ###

### === libraries === ###
library(tidyverse)
### ===== ###

### === read metadata table === ###
# the input file is the metadata table of the upcoming forest functions synthesis dataset (not yet uploaded on BExIS).
# Please download the forest functions synthesis dataset metadata into your R working directory
#USER : insert path to "synthesis_forest_function_metadata_HIVE_local_copy.xlsx"
forest_fun_metadata <- BE_synthesis_forest_dat <- read.table("synthesis_forest_function_metadata_HIVE_local_copy.xlsx", header = T, sep = "\t")
### ===== ###

### === select the necessary columns and save output to working directory === ###
helper_dat <- forest_fun_metadata %>% 
  subset( select = c("ColumnName", "AggregatedColumnName", "codedYear", "dataID", "Dataset Version", "Projectname"))

write.table(helper_dat, file = "forest_functions_helper.txt", quote = F, sep = "\t", row.names = F)
### ===== ###



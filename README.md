Released at Zenodo : NOT YET RELEASED

### License

This project is licensed under the terms of the Creative Commons Attribution 4.0 license (cc-by-4.0).

# Versions

- August 2023: Currently final decisions are being made on what to include in the synthesis forest functions dataset.

# Scripts

- `forest_functions_synthesis.R` : R-script to assemble and calculate variables for the [wide version](https://github.com/biodiversity-exploratories-synthesis/Synthesis_dataset_functions_forest/blob/main/BE_synthesis_forest_dat_wide.txt) of the forest functions synthesis dataset. Download the raw data from [BExIS](https://www.bexis.uni-jena.de/ddm), unzip them and adjust the "pathtodata" variable to read in raw data and process it. Variables selected from the raw data are sorted by the "Process and component name" (i.e. Soil carbon cycling) and are processed in the respective code blocks.
- `multidiversity.R` : contains function required to calculate mini-multifunctionalities, such as "soilCflxs_2011". Is sourced by forest_synthesis.R.

# BExIS upload

- `forest_functions_helper.R` : R-script to generate a [helper table](https://github.com/biodiversity-exploratories-synthesis/Synthesis_dataset_functions_forest/blob/main/forest_functions_helper.txt) from the metadata, to assist the conversion of the forest functions synthesis dataset from the [wide](https://github.com/biodiversity-exploratories-synthesis/Synthesis_dataset_functions_forest/blob/main/BE_synthesis_forest_dat_wide.txt) into the [long format](https://github.com/biodiversity-exploratories-synthesis/Synthesis_dataset_functions_forest/blob/main/synthesis_dataset_functions_forest_long.txt).
- `transform_to_long_format.R` : R-script to transform the forest functions synthesis dataset, generated with [forest_functions_synthesis.R](https://github.com/biodiversity-exploratories-synthesis/Synthesis_dataset_functions_forest/blob/main/forest_functions_synthesis.R), into the [long format](https://github.com/biodiversity-exploratories-synthesis/Synthesis_dataset_functions_forest/blob/main/synthesis_dataset_functions_forest_long.txt) ready for upload to [BExIS](https://www.bexis.uni-jena.de/ddm).

## Notes

- Negative values of scaled variables have been corrected, by scaling (sd = 1), but not centering (mean =/= 0). Affected variables are for example dung_removal (21206_3_Dataset) and the leaf area damaged by different herbivores (24806_2_Dataset). Dung removal was scaled and centered in the grasslands functions synthesis dataset (BExIS ID 27087). In a future update, scaled variables in the grasslands dataset will not be centered anymore to avoid negative values (not yet implemented).

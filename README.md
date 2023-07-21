# Synthesis_dataset_functions_forest
Released at Zenodo : NOT YET RELEASED

Code used for generation of the synthesis grassland function dataset [https://github.com/biodiversity-exploratories-synthesis/Synthesis_dataset_functions_forest/Synthesis_dataset_functions_forest.R](https://github.com/biodiversity-exploratories-synthesis/Synthesis_dataset_functions_forest/blob/main/forest_synthesis.R)

### License

This project is licensed under the terms of the Creative Commons Attribution 4.0 license (cc-by-4.0).

# Versions

- July 2023: Currently the synthesis dataset functions forest is under construction.

# Scripts

- `forest_synthesis.R` : R-script to assemble and calculate variables for the synthesis dataset functions forest (i.e. [BE_synthesis_forest_dat.txt](https://github.com/biodiversity-exploratories-synthesis/Synthesis_dataset_functions_forest/blob/main/BE_synthesis_forest_dat.txt)). Download the raw data from BExIS (https://www.bexis.uni-jena.de/ddm), unzip them and adjust the "pathtodata" variable to read in raw data and process it. Variables selected from the raw data are sorted by the "Process and component name" (i.e. Soil carbon cycling) and are processed in the respective code blocks.

## Notes

- Dung removal has negative values, because different dung types were scaled in order to aggregate them. --> In future, think about how to avoid negative values.

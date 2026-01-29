# Mobility Models Repo

Spatial scales have proven to be an important feature in the ecology of infectious diseases. However, spatial scales lack clear definitions. 

Therefore, the objective of this analysis is to characterize a "local" scale of human mobility within the US and explore its variability across larger regional scales.

This repo relates to the first part of Chapter 2 in [my PhD dissertation](https://esploro.libs.uga.edu/esploro/outputs/9949694128302959).

Feel free to reach out to me (daileyco@gmail.com) or my PhD advisor, Justin Bahl (Justin.Bahl@uga.edu), with any questions. 

## Repo Contents

This repo contains scripts that: 

- read and manage input data, 
- fit human mobility models to commuting data,
- explore differences in commuting patterns,
- and generate various tables and figures showing important patterns in the data or analytical results.


The scripts in this repo (and others of my creation) are highly modular. The scripts are designed to be run in a particular sequence that ensures the output(s) saved from upstream scripts are available for input(s) in downstream scripts. (See the bottom of this readme for a generic description of repo contents/structure.)

There are two files that outline the order of scripts and give details on their individual purposes. 
- "00-Information/script_census-[compile date].xlsx"
- "04-Report/01-Notebook/reproducibility_notebook.rmd"

The script census excel file gives details on scripts in this repo, including its purpose, inputs, package dependencies, and outputs. The creation of this excel file was automated (".02-Scripts/Script_Census.R"), so there are likely some errors in formatting or omitted information. Each script itself has some comments explaining the intent for sections of the code. 
The reproducibility notebook is a combination of (1) a narrative explaining analysis steps and (2) a master script which sources/runs all of the main analysis scripts.

A few of the columns from the script census are shown in the table below. (created with knitr::kable(census[,c(2,1,3)]))

|Script Location   |Script Name                                               |Purpose                                                                                                             |
|:-----------------|:---------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------------|
|01-Data-Wrangling |process_Data_ACS.R                                        |ACS Data Processing                                                                                                 |
|01-Data-Wrangling |process_Data_Population_Interpolate_Ct_Planning_Regions.R |Interpolate Connecticut Planning Region Population Estimates                                                        |
|01-Data-Wrangling |process_Data_Population.R                                 |Process county population data                                                                                      |
|01-Data-Wrangling |process_Data_Spatial_Ct_Planning_Regions_Centroids.R      |Computing centroid coordinates for Ct planning regions                                                              |
|01-Data-Wrangling |process_Data_Spatial_Census_Regions.R                     |Spatial Data Processing for Census Region Identifiers                                                               |
|01-Data-Wrangling |process_Data_Spatial.R                                    |Spatial Data Processing                                                                                             |
|01-Data-Wrangling |process_Data_calculateSij.R                               |calculate sij for all pairwise combinations of US counties                                                          |
|01-Data-Wrangling |process_Data_ODdf.R                                       |Set up origin destination dataframe for mobility model fitting                                                      |
|03-Visualization  |generate_Table_Region_Summaries.R                         |Script to create summary table                                                                                      |
|03-Visualization  |generate_Flextables_Region_Summaries.R                    |script to format summary tables and save to word docs                                                               |
|03-Visualization  |generate_Figures_Total_Commuters.R                        |Script to create figure showing total commuters vs population at origin                                             |
|03-Visualization  |generate_Figures_Flows.R                                  |Script to create figure showing flux vs distance, surrounding population, population origin, population destination |
|04-Analysis       |estimateTotalCommuters.R                                  |script to estimate total commuters (workers) based on population size, time period, and census region               |
|04-Analysis       |estimateInternalCommuters.R                               |script to estimate total internal commuters (workers) based on population size, time period, and census region      |
|04-Analysis       |tuneGravity.R                                             |script to tune the distance threshold for the gravity model for different data subsets                              |
|04-Analysis       |compareGravityModels.R                                    |Compare variations of gravity models                                                                                |
|04-Analysis       |fitGravity.R                                              |fit gravity models                                                                                                  |
|04-Analysis       |fitGravity_simple.R                                       |fit gravity models                                                                                                  |
|04-Analysis       |tuneUniversalOpportunity.R                                |script to tune the parameters for the universal opportunity model for different data subsets                        |
|04-Analysis       |fitUniversalOpportunity.R                                 |script to fit the universal opportunity model variations for different data subsets                                 |
|03-Visualization  |generate_Figures_Gravity_Distance_Thresholds.R            |Script to create figure showing total commuters vs population at origin                                             |
|03-Visualization  |generate_Table_Gravity_Tuning.R                           |script to make summary tables for gravity model tuning for distance threshold                                       |
|03-Visualization  |generate_Figures_UO_Parameters.R                          |Script to create figure showing total commuters vs population at origin                                             |
|03-Visualization  |generate_Table_UO_Tuning.R                                |script to make summary tables for gravity model tuning for distance threshold                                       |
|03-Visualization  |generate_Flextables_Model_Tuning.R                        |script to format model summary tables and save to word docs                                                         |
|03-Visualization  |generate_Flextables_Model_ANOVA.R                         |script to format model summary tables and save to word docs                                                         |
|03-Visualization  |generate_Figures_Gravity_Coefs.R                          |script to generate figures of gravity model coefficients                                                            |
|03-Visualization  |generate_Figures_Gravity_Coefs_simple.R                   |script to generate figures of gravity model coefficients                                                            |
|03-Visualization  |generate_Tables_Gravity_Fit.R                             |script to make summary tables for gravity model fits                                                                |
|03-Visualization  |generate_Flextables_Model_Coefs.R                         |script to format model summary tables and save to word docs                                                         |
|03-Visualization  |generate_Table_UO_Fit.R                                   |script to make summary tables for gravity model fits                                                                |
|03-Visualization  |generate_Flextables_Model_Fits.R                          |script to format model summary tables and save to word docs                                                         |
|03-Visualization  |generate_Figures_Model_Fits.R                             |Script to create figure showing total commuters vs population at origin                                             |
|01-Data-Wrangling |process_Data_Predictions_Flow.R                           |load data                                                                                                           |
|03-Visualization  |generate_Figures_Flows_ObsVPred.R                         |script to plot model predictions against observed values of commuting flow.                                         |




# Project Skeleton (my generic repo template)

A basic repo template (directory structure) to be use as starting point for new projects.


  
# Structure
  
## 00-Information
  
Proposals, prompts, outlines, ... anything relevant to the conception of the project.
  
## 01-Data
  
Data in downloaded formats are "raw" (e.g., .xlsx, .dat, .csv, ...). We import and store data in R formats (e.g., .rds, .rdata) during "processing". Data cleaning and recoding finalize "analytic" datasets. *Note: generally, "00-" prefix means don't modify or rewrite the files within.*
  
* Sub-directories
  + 00-Raw-Data
  + 01-Processed-Data
  + 02-Analytic-Data

## 02-Scripts

Envisioned as a linear path from raw data management to final figure creation. Playground to catch everything incomplete; data wrangling to transition through data directories; helper functions to hold project specific functions or those oriented to external programs; visualization is obvious; analysis to fit models, calculate stats, general computation/programming. Each has its own typical set of script types.

* Sub-directories and typical scripts
  + 00-Playground
    - exploratory code dump, in-progress scripts, under construction, placeholder for other scripts
  + 01-Data-Wrangling
    - download_Data_\* : scripts to automate the data download from online
    - process_Data_\* : scripts to process raw data
    - prep_Files_\* : scripts to write files for external program use
    - process_Results_\* : scripts to process results from external program analyses
  + 02-Helper-Functions
    - repeated, modular, specific, useful code
    - run_Program_\* : scripts to automate external programs
  + 03-Visualization
    - plot_\* : pretty self-explanatory, fundamental units to generate_Figure scripts
    - generate_Figure_\* : scripts to create high-level figures, plots, images, interactive visualizations
    - generate_Table_\* : scripts to create tables
  + 04-Analysis
    - analyze_\* : scripts to run analyses within R

## 03-Output

Store the goodies to show everyone else. 

* Sub-directories
  + 01-Tables
  + 02-Figures
  + 03-Non-Static


## 04-Report

* Sub-directories
  + 01-Notebook (detailed reports, write-ups, long-winded, process-oriented)
    - reproducibility_notebook.RMD : quasi-stream-of-thought writing, deeply woven web of scripts and analytical directions
  + 02-Presentation (slide decks)
  + 03-Manuscript (polished for publication)

## 05-Miscellaneous

Whatever else may need to accompany repo.





# Useful Resources

[How to collaborate](https://www.sciencemag.org/careers/2012/07/how-collaborate)

[Ten simple rules for collaboratively writing a multi-authored paper](https://doi.org/10.1371/journal.pcbi.1006508)


[How to write a first-class paper](https://doi.org/10.1038/d41586-018-02404-4)

[Ten simple rules for structuring papers](https://doi.org/10.1371/journal.pcbi.1005619)

[Writing science](https://www.science.org/content/article/writing-science-storys-thing)


[Ten guidelines for effective data visualization in scientific publications](https://doi.org/10.1016/j.envsoft.2010.12.006)


[Three tips for giving a great research talk](https://www.science.org/content/article/three-tips-giving-great-research-talk)

[How to give a great scientific talk](https://doi.org/10.1038/d41586-018-07780-5)


[Science communication, public engagement, and outreach](https://www.informalscience.org/develop-projects/science-communication-public-engagement-and-outreach)


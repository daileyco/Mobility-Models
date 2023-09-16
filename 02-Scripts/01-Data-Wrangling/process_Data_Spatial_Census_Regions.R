# Spatial Data Processing for Census Region Identifiers

## packages
library(dplyr)


## download data
# 
# download.file("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv",
#               destfile = "./01-Data/00-Raw-Data/Spatial/census_regions.csv")


## read data
regions <- read.csv("./01-Data/00-Raw-Data/Spatial/census_regions.csv", 
                    colClasses = "character")


## add row for Puerto Rico

regions[nrow(regions)+1,] <- c("Puerto Rico", "PR", "Northeast", "Middle Atlantic")


## save
save(regions, file = "./01-Data/01-Processed-Data/census_regions.rds")


## clean environment
rm(list = ls())
gc()
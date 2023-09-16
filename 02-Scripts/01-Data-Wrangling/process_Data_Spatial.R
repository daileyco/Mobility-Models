# Spatial Data Processing

## load data
load("./01-Data/01-Processed-Data/ct_centroids.rds")
load("./01-Data/01-Processed-Data/census_regions.rds")


## packages
library(sf)
library(dplyr)


## download data
# 
# download.file("https://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO.txt", 
#               destfile = "./01-Data/00-Raw-Data/Spatial/CenPop2010_Mean_CO.txt")
# download.file("https://www2.census.gov/geo/docs/reference/cenpop2020/county/CenPop2020_Mean_CO.txt", 
#               destfile = "./01-Data/00-Raw-Data/Spatial/CenPop2020_Mean_CO.txt")


## read data
pop.centers10 <- read.csv("./01-Data/00-Raw-Data/Spatial/CenPop2010_Mean_CO.txt", 
                          colClasses = "character")
pop.centers20 <- read.csv("./01-Data/00-Raw-Data/Spatial/CenPop2020_Mean_CO.txt", 
                          colClasses = "character")


## drop Connecticut data from 20
pop.centers20 <- pop.centers20 %>%
  filter(!STNAME%in%c("Connecticut"))



## combine data
centers <- bind_rows(pop.centers10 %>% mutate(YEAR = 2010), 
                     pop.centers20 %>% mutate(YEAR = 2020), 
                     ct.centroids %>% mutate(YEAR = 2020)) %>%
  arrange(STATEFP, COUNTYFP, YEAR) %>% 
  select(-POPULATION)

### add census regions
centers <- left_join(centers, 
                     regions %>% select(State, Region), 
                     by = c("STNAME"="State"))


## format as spatial data
centers <- st_as_sf(centers, 
                    coords = c("LONGITUDE", "LATITUDE"), 
                    crs = "WGS84")



## save
save(centers, file = "./01-Data/01-Processed-Data/county_centers.rds")


## clean environment
rm(list = ls())
gc()
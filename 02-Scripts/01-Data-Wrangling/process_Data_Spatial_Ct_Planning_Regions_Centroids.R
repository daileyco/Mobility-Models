# Computing centroid coordinates for Ct planning regions


## load data
load("./01-Data/01-Processed-Data/pop.rds")


## packages
library(sf)
library(dplyr)
library(rgeos)



## read data
ctpr <- st_read("./01-Data/00-Raw-Data/Spatial/deepgis_DEEP_PLANNING_REGION_INDEX.shp")


## check projection, coordinate system
# st_crs(ctpr)
# # WGS 84; lat lon


## calculate centroids and set up as dataframe
coords <- ctpr %>% 
  as_Spatial() %>% 
  gCentroid(., byid=TRUE) %>% 
  as.data.frame() %>%
  setNames(., nm = c("LONGITUDE", "LATITUDE")) %>%
  bind_cols(CoG=ctpr$REGPLANORG)


## set up dataframe with FIPS and names

ct <- pop %>%
  filter(STNAME=="Connecticut" & YEAR%in%c("2016")) %>%
  select(STATEFP = STATE, COUNTYFP = COUNTY, COUNAME = CTYNAME, STNAME)


### combine, verify CoG matches planning region name
#### Connecticut Metropolitan = Greater Bridgeport
ct.centroids <- bind_cols(ct, 
                          coords) %>%
  select(-CoG) %>%
  mutate(across(everything(), ~as.character(.x)))



## save
save(ct.centroids, file = "./01-Data/01-Processed-Data/ct_centroids.rds")


## clean environment
rm(list=ls())
gc()

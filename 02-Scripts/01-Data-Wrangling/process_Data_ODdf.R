# Set up origin destination dataframe for mobility model fitting

## load data
load("./01-Data/01-Processed-Data/acs.rds")
load("./01-Data/01-Processed-Data/pop.rds")
load("./01-Data/01-Processed-Data/county_centers.rds")


## packages
library(dplyr)
library(sf)


## isolate midpoint population sizes for 2011-2015 & 2016-2020
### use 2013 and 2018
pop.mid <- pop %>% 
  filter(YEAR%in%c(2013,2018)) %>%
  mutate(period = ifelse(YEAR==2013, "2011-2015", "2016-2020"))


## add population sizes for origins and destinations
od <- left_join(acs, 
                pop.mid %>% 
                  select(STATE, COUNTY, period, POPESTIMATE.origin = POPESTIMATE), 
                by = c("State FIPS Residence"="STATE", 
                       "County FIPS Residence"="COUNTY", 
                       "period"="period")) %>%
  left_join(., 
            pop.mid %>% 
              select(STATE, COUNTY, period, POPESTIMATE.destination = POPESTIMATE), 
            by = c("State FIPS Work"="STATE", 
                   "County FIPS Work"="COUNTY", 
                   "period"="period"))



## add coordinates for origins and destinations

od <- left_join(od, 
                centers %>%
                  mutate(period = ifelse(YEAR=="2010", "2011-2015", "2016-2020")) %>%
                  select(STATEFP, COUNTYFP, period, point.origin=geometry), 
                by = c("State FIPS Residence"="STATEFP", 
                       "County FIPS Residence"="COUNTYFP", 
                       "period"="period")) %>%
  left_join(., 
            centers %>%
              mutate(period = ifelse(YEAR=="2010", "2011-2015", "2016-2020")) %>%
              select(STATEFP, COUNTYFP, period, point.destination=geometry), 
            by = c("State FIPS Work"="STATEFP", 
                   "County FIPS Work"="COUNTYFP", 
                   "period"="period"))



## calculate distance between origin and destination
od$distance <- st_distance(od$point.origin, 
                           od$point.destination, 
                           by_element=TRUE)


### convert to kilometers
od$distance.km <- od$distance
units(od$distance.km) <- "km"



## calculate total workers by residence who work domestically (international dropped earlier)

od <- od %>%
  group_by(`State FIPS Residence`, `County FIPS Residence`, period) %>%
  mutate(Total.Workers.Residence.Domestic = sum(`Workers in Commuting Flow`)) %>%
  ungroup()



## save
save(od, file = "./01-Data/01-Processed-Data/od.rds")

## clean environment
rm(list=ls())
gc()












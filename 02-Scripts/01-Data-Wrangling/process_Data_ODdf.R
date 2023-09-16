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
### discrepancies
#### 2011-2015
#### ACS has Kusilak, AK which corresponds to 2010 pop center for Wade Hampton, AK
#### ACS has Oglala Lakota, SD which corresponds to 2010 pop center for Shannon, SD

centers <- centers %>%
  mutate(COUNTYFP = case_when(STATEFP=="02" & COUNTYFP=="270" ~ "158", 
                              STATEFP=="46" & COUNTYFP=="113" ~ "102", 
                              TRUE ~ COUNTYFP))

od <- left_join(od, 
                centers %>%
                  mutate(period = ifelse(YEAR=="2010", "2011-2015", "2016-2020")) %>%
                  select(STATEFP, COUNTYFP, period, census.region.origin=Region, point.origin=geometry), 
                by = c("State FIPS Residence"="STATEFP", 
                       "County FIPS Residence"="COUNTYFP", 
                       "period"="period")) %>%
  left_join(., 
            centers %>%
              mutate(period = ifelse(YEAR=="2010", "2011-2015", "2016-2020")) %>%
              select(STATEFP, COUNTYFP, period, census.region.destination=Region, point.destination=geometry), 
            by = c("State FIPS Work"="STATEFP", 
                   "County FIPS Work"="COUNTYFP", 
                   "period"="period"))



## calculate distance between origin and destination
od$distance.m <- st_distance(od$point.origin,
                             od$point.destination, 
                             by_element=TRUE)


### convert to kilometers
od$distance.km <- od$distance.m
units(od$distance.km) <- "km"


## extract coordinates and drop geometry

coords.origin <- st_coordinates(od$point.origin) %>% 
  as.data.frame() %>%
  setNames(., nm = c("lon.origin", "lat.origin"))

coords.destination <- st_coordinates(od$point.destination) %>% 
  as.data.frame() %>%
  setNames(., nm = c("lon.destination", "lat.destination"))

od <- od %>%
  select(-point.origin, -point.destination) %>%
  bind_cols(., coords.origin, coords.destination)




## calculate total workers by residence location who work domestically 
## and the total workers by work location
### (international work locations were dropped in earlier processing script)

od <- od %>%
  group_by(`State FIPS Residence`, `County FIPS Residence`, period) %>%
  mutate(Total.Workers.Residence.Domestic = sum(`Workers in Commuting Flow`)) %>%
  ungroup() %>%
  group_by(`State FIPS Work`, `County FIPS Work`, period) %>%
  mutate(Total.Workers.Work = sum(`Workers in Commuting Flow`)) %>%
  ungroup()



## format var types

od <- od %>%
  mutate(across(matches("POPESTIMATE"), ~as.integer(.x)), 
         across(matches("distance"), ~as.numeric(.x)))


## order vars

od <- od %>%
  select(c("period", 
           "State FIPS Residence", "County FIPS Residence", "State Residence", "County Residence", 
           "State FIPS Work", "County FIPS Work", "State Work", "County Work", 
           "Workers in Commuting Flow", "Margin of Error", 
           "census.region.origin", "census.region.destination",
           "Total.Workers.Residence", "Total.Workers.Residence.Domestic", "Total.Workers.Work", 
           "POPESTIMATE.origin", "POPESTIMATE.destination", 
           "distance.m", "distance.km", 
           "lon.origin", "lat.origin", "lon.destination", "lat.destination"))


## log transform pop and dist, create dist indicator

od <- od %>%
  mutate(log.Workers.in.Commuting.Flow = log(`Workers in Commuting Flow`), 
         across(matches("Total|POPESTIMATE"), ~log(.x), .names = "log.{.col}"), 
         across(matches("distance"), ~ifelse(.x==0, 0, log(.x)), .names = "log.{.col}"))





## save
save(od, file = "./01-Data/02-Analytic-Data/od.rds")

## clean environment
rm(list=ls())
gc()












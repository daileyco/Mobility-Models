# Set up origin destination dataframe for mobility model fitting

## load data
load("./01-Data/01-Processed-Data/acs.rds")
load("./01-Data/01-Processed-Data/acs_sij.rds")
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


## categorize population size with tertiles
### determine cut points for each year
pop.size.tertiles <- tapply(as.numeric(pop.mid$POPESTIMATE), 
                            pop.mid$period, 
                            quantile, probs=1:2/3)

### create categorical variable
pop.mid <- pop.mid %>%
  mutate(pop.size.cat = ifelse(period=="2011-2015", 
                               findInterval(as.numeric(POPESTIMATE),
                                            pop.size.tertiles$`2011-2015`),
                               findInterval(as.numeric(POPESTIMATE),
                                            pop.size.tertiles$`2016-2020`))) %>%
  mutate(pop.size.cat = case_when(pop.size.cat==0 ~ "small", 
                                  pop.size.cat==1 ~ "medium", 
                                  pop.size.cat==2 ~ "large", 
                                  TRUE ~ NA))

## add population sizes for origins and destinations


### note
#### no flows originating from 
# STATE COUNTY STNAME   CTYNAME         YEAR  POPESTIMATE period    fips 
# 1 01    095    Alabama  Marshall County 2018  96230       2016-2020 01095
# 2 29    201    Missouri Scott County    2013  39228       2011-2015 29201
# 3 30    019    Montana  Daniels County  2013  1778        2011-2015 30019
#### no flows ending in 
# STATE COUNTY STNAME CTYNAME           YEAR  POPESTIMATE period    fips 
# 1 19    057    Iowa   Des Moines County 2018  39259       2016-2020 19057
# 2 19    055    Iowa   Delaware County   2013  17475       2011-2015 19055



od <- left_join(acs, 
                pop.mid %>% 
                  select(STATE, COUNTY, period, POPESTIMATE.origin = POPESTIMATE, pop.size.cat.origin = pop.size.cat), 
                by = c("State FIPS Residence"="STATE", 
                       "County FIPS Residence"="COUNTY", 
                       "period"="period")) %>%
  left_join(., 
            pop.mid %>% 
              select(STATE, COUNTY, period, POPESTIMATE.destination = POPESTIMATE, pop.size.cat.destination = pop.size.cat), 
            by = c("State FIPS Work"="STATE", 
                   "County FIPS Work"="COUNTY", 
                   "period"="period"))

### create categorical variable indicating pairing of population size categories between origin and destination

od <- od %>%
  mutate(pop.cats = case_when(pop.size.cat.origin == "small" & pop.size.cat.destination == "small" ~   "ss", 
                              pop.size.cat.origin == "small" & pop.size.cat.destination == "medium" ~  "sm",
                              pop.size.cat.origin == "small" & pop.size.cat.destination == "large" ~   "sl",
                              pop.size.cat.origin == "medium" & pop.size.cat.destination == "small" ~  "ms", 
                              pop.size.cat.origin == "medium" & pop.size.cat.destination == "medium" ~ "mm",
                              pop.size.cat.origin == "medium" & pop.size.cat.destination == "large" ~  "ml",
                              pop.size.cat.origin == "large" & pop.size.cat.destination == "small" ~   "ls", 
                              pop.size.cat.origin == "large" & pop.size.cat.destination == "medium" ~  "lm",
                              pop.size.cat.origin == "large" & pop.size.cat.destination == "large" ~   "ll", 
                              TRUE ~ NA))






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


## calculate sij for radiation models

### using only observed data
#### order by distance, split by zero-distance flows
od <- od %>%
  arrange(period, `State FIPS Residence`,`County FIPS Residence`, distance.km) %>%
  split(., .$distance.km==0)

#### compute for non-zero-distance flows
##### group data by the time period (separate datasets) and origin location
##### within groups, data are ordered by increasing distances from origin
##### cumulative sum is used to calculate population within circle centered at the origin location
###### whose radius is distance between origin and destination locations (excluding origin and destination populations)
##### kind of tricky to compute with dplyr, group_by()+mutate() doesn't work for cumsum() so had to use experimental group_map()
od[["FALSE"]]$sij_within <- od[["FALSE"]] %>% 
  group_by(period, `State FIPS Residence`, `County FIPS Residence`) %>% 
  group_map(~cumsum(.x$POPESTIMATE.destination)-.x$POPESTIMATE.destination) %>% 
  unlist()

#### make whole again
od <- od %>% 
  bind_rows() %>%
  arrange(period, `State FIPS Residence`,`County FIPS Residence`, distance.km)


### join with calculations of sij from all pairwise combinations of counties
od <- od %>% 
  mutate(fips.origin = paste0(`State FIPS Residence`,`County FIPS Residence`),
         fips.destination = paste0(`State FIPS Work`, `County FIPS Work`), 
         fips.ij = paste0(fips.origin, "-", fips.destination)) %>%
  full_join(., 
            acs.sij, 
            by = c("period", "fips.ij"="ij"))







## order vars

od <- od %>%
  select(c("period", 
           "State FIPS Residence", "County FIPS Residence", "State Residence", "County Residence", 
           "State FIPS Work", "County FIPS Work", "State Work", "County Work", 
           "fips.origin", "fips.destination", "fips.ij",
           "Workers in Commuting Flow", "Margin of Error", 
           "census.region.origin", "census.region.destination",
           "Total.Workers.Residence", "Total.Workers.Residence.Domestic", "Total.Workers.Work", 
           "POPESTIMATE.origin", "pop.size.cat.origin", 
           "POPESTIMATE.destination", "pop.size.cat.destination", 
           "pop.cats",
           "distance.m", "distance.km", 
           "sij", "sij_within",
           "lon.origin", "lat.origin", "lon.destination", "lat.destination"))


## log transform pop, dist, and sij

od <- od %>%
  mutate(log.Workers.in.Commuting.Flow = log(`Workers in Commuting Flow`), 
         across(matches("Total|POPESTIMATE"), ~log(.x), .names = "log.{.col}"), 
         across(matches("distance|sij"), ~ifelse(.x==0, 0, log(.x)), .names = "log.{.col}"))





## save
save(od, file = "./01-Data/02-Analytic-Data/od.rds")

## clean environment
rm(list=ls())
gc()



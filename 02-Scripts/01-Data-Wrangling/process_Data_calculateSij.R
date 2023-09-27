# calculate sij for all pairwise combinations of US counties


## load data
# load("./01-Data/02-Analytic-Data/od.rds")
load("./01-Data/01-Processed-Data/acs.rds")
load("./01-Data/01-Processed-Data/pop.rds")
load("./01-Data/01-Processed-Data/county_centers.rds")


## packages
library(dplyr)
library(sf)
library(doParallel)
library(doSNOW)



## simplify acs df
acs <- acs %>%
  mutate(i = paste0(`State FIPS Residence`, `County FIPS Residence`), 
         j = paste0(`State FIPS Work`, `County FIPS Work`), 
         ij = paste0(i, "-", j)) %>%
  filter(i!=j) %>%
  select(period, i,j,ij)




## isolate midpoint population sizes for 2011-2015 & 2016-2020
### use 2013 and 2018
### simplify to include only period, 5-digit fips, and population size
pop <- pop %>% 
  filter(YEAR%in%c(2013,2018)) %>%
  mutate(period = ifelse(YEAR==2013, "2011-2015", "2016-2020")) %>%
  mutate(fips = paste0(STATE, COUNTY)) %>%
  select(period, fips, pop=POPESTIMATE)



## add coordinates for population centers
### discrepancies
#### pop data has Kusilak, AK which corresponds to 2010 pop center for Wade Hampton, AK
#### pop data has Oglala Lakota, SD which corresponds to 2010 pop center for Shannon, SD
#### 2010 centers has duplicate entry for Bedford, Virginia 
##### fips=51515 for duplicate 
###### (other entries for Bedford, Virginia for 2010&2020 have fips=51019)

centers <- centers %>%
  filter(!(COUNTYFP=="515" & STATEFP=="51")) %>%
  mutate(COUNTYFP = case_when(STATEFP=="02" & COUNTYFP=="270" ~ "158", #AK
                              STATEFP=="46" & COUNTYFP=="113" ~ "102", #SD
                              TRUE ~ COUNTYFP)) %>%
  mutate(fips = paste0(STATEFP, COUNTYFP)) %>%
  mutate(period = ifelse(YEAR==2010, "2011-2015", "2016-2020")) %>%
  select(period, fips, geometry)


### join

pop <- full_join(pop, 
                 centers, 
                 by = c("period", "fips"))




## parallelize over origin locations
### set up parallel computation
cl <- makeCluster(mc <- getOption("cl.cores", {detectCores()-1}))

clusterExport(cl=cl, 
              list=c("acs", "pop"))
clusterEvalQ(cl, 
             {
               
               library(dplyr)
               library(sf)
               
             })




registerDoSNOW(cl)


pb <- txtProgressBar(max = nrow(pop), style = 3)
progress <- function(n){
  setTxtProgressBar(pb, n)
}


acs.sij <- foreach(ii = 1:nrow(pop), 
                  .combine = bind_rows, 
                  .options.snow = list(progress = progress)) %dopar% {
                    
                    
                    ## isolate single origin location
                    origin <- pop[ii,] %>%
                      rename(point.i = geometry)
                    
                    ### remove origin from population data and subset to same period
                    destinations <- pop[-ii,] %>% 
                      filter(period%in%origin$period) %>%
                      select(destination=fips, 
                             pop.j=pop, 
                             point.j=geometry)
                    
                    ### subset commuting data for single origin location
                    acsdf <- acs %>% 
                      filter(i%in%origin$fips & period%in%origin$period)
                    
                    if(nrow(acsdf)==0){
                      return(NULL)
                    }
                    
                    ## create dataframe for all pairwise combinations of origin and destinations
                    pairs <- bind_cols(origin, 
                                       destinations)
                    
                    ## calculate distance between origin and destinations
                    pairs$dij <- st_distance(pairs$point.i,
                                             pairs$point.j, 
                                             by_element = TRUE) 
                    
                    ## drop geometry variables, and other unnecessary
                    pairs <- pairs %>%
                      select(-point.i, -point.j, -period, -pop)
                    
                    ## format distance variable
                    ### meters to kilometers
                    units(pairs$dij) <- "km"
                    ### remove units class
                    pairs$dij <- as.numeric(pairs$dij)
                    
                    ## order increasing distances
                    pairs <- pairs %>%
                      arrange(dij)
                    
                    ## calculate sij
                    pairs$sij <- cumsum(pairs$pop.j) - as.numeric(pairs$pop.j)
                    
                    
                    ## join to oddf
                    acsdf <- left_join(acsdf, 
                                      pairs, 
                                      by = c("i"="fips", "j"="destination"))
                    
                    ## return
                    return(acsdf)
                  }

## close out
close(pb)
stopCluster(cl)


## save
save(acs.sij, file = "./01-Data/01-Processed-Data/acs_sij.rds")

## clean environment
rm(list=ls())
gc()


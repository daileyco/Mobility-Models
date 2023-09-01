# Process county population data

## Load Connecticut interpolated data
load("./01-Data/01-Processed-Data/pop1019_ctplan.rds")

## Packages
library(dplyr)
library(tidyr)


## Read data

pop1020 <- read.csv("./01-Data/00-Raw-Data/Population/co-est2020.csv", 
                    colClasses = "character")

pop1020.pr <- read.csv("./01-Data/00-Raw-Data/Population/PRM-EST2020.csv", 
                       colClasses = "character", 
                       encoding = "latin1")

pop2022 <- read.csv("./01-Data/00-Raw-Data/Population/co-est2022-alldata.csv", 
                    colClasses = "character")


## Select and filter and transform wide to long
### Select FIPS, names, and population estimates
### Filter out state level aggregations

pop1020 <- pop1020 %>%
  filter(SUMLEV%in%c("050")) %>%
  select(STATE, COUNTY, STNAME, CTYNAME, matches("^POPESTIMATE[0-9]{4}$")) %>%
  pivot_longer(matches("^POPESTIMATE[0-9]{4}$"), names_to = "YEAR", values_to = "POPESTIMATE") %>%
  mutate(YEAR = sub("POPESTIMATE", "", YEAR))

pop1020.pr <- pop1020.pr %>%
  filter(SUMLEV%in%c("050")) %>%
  select(STATE, COUNTY, STNAME, CTYNAME=CNTYNAME, matches("^POPESTIMATE[0-9]{4}$")) %>%
  pivot_longer(matches("^POPESTIMATE[0-9]{4}$"), names_to = "YEAR", values_to = "POPESTIMATE") %>%
  mutate(YEAR = sub("POPESTIMATE", "", YEAR))

pop2022 <- pop2022 %>%
  filter(SUMLEV%in%c("050")) %>%
  select(STATE, COUNTY, STNAME, CTYNAME, matches("^POPESTIMATE[0-9]{4}$")) %>%
  pivot_longer(matches("^POPESTIMATE[0-9]{4}$"), names_to = "YEAR", values_to = "POPESTIMATE") %>%
  mutate(YEAR = sub("POPESTIMATE", "", YEAR))



## Get ready to merge all
### Alaska
#### sum Chugach Census Area and Copper River Census Area for 2010-15 from 1020 data
#### to recreate Ak Valdez-Cordova Census Area
##### State FIPS = 02; County FIPS = 261

pop1015.ak <- pop1020 %>%
  filter(STNAME%in%c("Alaska") & 
           CTYNAME%in%c("Chugach Census Area", "Copper River Census Area") & 
           YEAR%in%c(2010:2015)) %>%
  group_by(STATE, STNAME, YEAR) %>%
  summarise(POPESTIMATE = as.character(sum(as.numeric(POPESTIMATE)))) %>%
  ungroup() %>%
  mutate(COUNTY = "261", 
         CTYNAME = "Valdez-Cordova Census Area")

#### drop from main pop df
pop1020 <- pop1020 %>%
  filter(!(STNAME%in%c("Alaska") & 
             CTYNAME%in%c("Chugach Census Area", "Copper River Census Area") & 
             YEAR%in%c(2010:2015)))


### Connecticut
#### drop Ct 2016-2020

pop1020 <- pop1020 %>%
  filter(!(STNAME%in%c("Connecticut") & YEAR%in%c(2016:2020)))

#### isolate 2020 from 2022 df
#### repair CTYNAME for long name, "n" was cutoff
pop2022.ct <- pop2022 %>%
  filter(STNAME%in%c("Connecticut") & YEAR%in%c(2020)) %>%
  mutate(CTYNAME=ifelse(COUNTY==130, paste0(CTYNAME, "n"), CTYNAME))


#### format for merge

pop1619.ctplan <- pop1019.ctplan %>%
  select(CTYNAME=planning.region, YEAR=year, POPESTIMATE=pop.est.pred) %>%
  mutate(CTYNAME=paste0(CTYNAME, " Planning Region"), 
         YEAR=as.character(YEAR),
         POPESTIMATE=as.character(as.integer(round(POPESTIMATE, 0)))) %>%
  filter(YEAR%in%c(2016:2019))


#### create records for 2016-2019, fill in with interpolated popests
#### combine with 2020 records

pop1620.ct <- pop2022.ct[rep(1:nrow(pop2022.ct), each=length(2016:2019)),] %>%
  select(-YEAR, -POPESTIMATE) %>%
  group_by(STATE, COUNTY, STNAME, CTYNAME) %>%
  mutate(YEAR=as.character(2015+row_number())) %>%
  ungroup() %>% 
  full_join(., 
            pop1619.ctplan, 
            by = c("CTYNAME", "YEAR")) %>%
  bind_rows(., 
            pop2022.ct)
  


## put them all together

pop <- bind_rows(pop1020, 
                 pop1020.pr, 
                 pop1015.ak, 
                 pop1620.ct) %>%
  arrange(STATE, COUNTY, YEAR) %>%
  filter(!YEAR%in%c(2010))




## save

save(pop, file = "./01-Data/01-Processed-Data/pop.rds")



## clean environment
rm(list=ls())
gc()















# ACS Data Processing

## Packages
library(readxl)
library(dplyr)


## read data
###table for 2011 - 2015
acs1115 <- read_xlsx(path = "./01-Data/00-Raw-Data/Commuting/table1-11-15.xlsx", 
                     skip = 7, 
                     col_names = c("State FIPS Residence", "County FIPS Residence", 
                                   "State Residence", "County Residence",  
                                   "State FIPS Work", "County FIPS Work", 
                                   "State Work", "County Work",  
                                   "Workers in Commuting Flow", "Margin of Error"))
####drop footer notes
acs1115 <- acs1115[-c(nrow(acs1115)-1, nrow(acs1115)),]


###table for 2016-2020
acs1620 <- read_xlsx(path = "./01-Data/00-Raw-Data/Commuting/table1-16-20.xlsx", 
                     skip = 8, 
                     col_names = c("State FIPS Residence", "County FIPS Residence", 
                                   "State Residence", "County Residence",  
                                   "State FIPS Work", "County FIPS Work", 
                                   "State Work", "County Work",  
                                   "Workers in Commuting Flow", "Margin of Error"))
####drop footer notes
acs1620 <- acs1620[-c({nrow(acs1620)-3}:nrow(acs1620)),]





## combine

acs <- bind_rows(acs1115 %>% mutate(period = "2011-2015"), 
                 acs1620 %>% mutate(period = "2016-2020"))


## calculate total workers for each origin (residence)

acs <- acs %>%
  group_by(`State FIPS Residence`, `County FIPS Residence`, period) %>%
  mutate(Total.Workers.Residence = sum(`Workers in Commuting Flow`)) %>%
  ungroup()


## drop international commutes
### remedy work fips to two digits

acs <- acs %>%
  filter(!`State Work`%in%c("Canada", "Mexico", "Other workplace outside of the U.S.")) %>%
  mutate(`State FIPS Work`=substr(`State FIPS Work`,2,3))


## save

save(acs, file = "./01-Data/01-Processed-Data/acs.rds")


## clean environment
rm(list = ls())
gc()
# Script to create summary table

## load data
load("./01-Data/01-Processed-Data/pop.rds")
load("./01-Data/01-Processed-Data/census_regions.rds")
load("./01-Data/02-Analytic-Data/od.rds")

## packages
library(dplyr)
library(tidyr)


## summarize commuting data
### calculate indicators for flow type
od <- od %>%
  mutate(indicator.internal.external = ifelse(distance.km==0, "Internal", "External"), 
         indicator.regional.flow = case_when(distance.km==0 ~ "Intracounty", 
                                             census.region.origin==census.region.destination ~ "Intraregion", 
                                             TRUE ~ "Interregion"), 
         indicator.state.flow = case_when(distance.km==0 ~ "Intracounty", 
                                          `State FIPS Residence`==`State FIPS Work` ~ "Intrastate", 
                                          census.region.origin==census.region.destination ~ "Intraregion", 
                                          TRUE ~ "Interregion")) %>%
  mutate(indicator.state.flow = factor(indicator.state.flow, levels = c("Intracounty", "Intrastate", "Intraregion", "Interregion"), ordered = TRUE))


### calculate proportions for summaries

od <- od %>%
  mutate(p.workers.of.resident.pop = `Workers in Commuting Flow`/POPESTIMATE.origin*100, 
         p.workers.of.resident.workers = `Workers in Commuting Flow`/Total.Workers.Residence*100)


### summarize flows, proportions, and distances

#### huge table
##### with counts, sums, means, standard deviations, 0/25th/50th/75th/100th percentiles
##### for workers in flow, proportions of resident pop who are workers in flow, proportions of total resident location workers who are workers in flow, and distance of flow
##### by flow type, time period, and region of origin
summary.table.flows.extensive <- od %>%
  
  ###### create duplicate rows for overall summaries
  bind_rows(mutate(., period = "2011-2020")) %>%
  bind_rows(mutate(., census.region.origin = "All US")) %>% 
  bind_rows(mutate(., indicator.state.flow = "All Flows")) %>%
  
  ###### format vars, kind of wasted effort at this step
  mutate(period = factor(period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE), 
         census.region.origin = factor(census.region.origin, levels = c("All US", "Midwest", "Northeast", "South", "West"), ordered = TRUE), 
         indicator.state.flow = factor(indicator.state.flow, levels = c("All Flows", "Intracounty", "Intrastate", "Intraregion", "Interregion"), ordered = TRUE)) %>%
  
  ###### create summary stats
  group_by(indicator.state.flow, 
           period, 
           census.region.origin) %>%
  
  summarise(`Total Observations_n`=as.character(format(n(), big.mark = " ")), 
            `Total Workers_sum`=as.character(format(sum(`Workers in Commuting Flow`), big.mark = " ")),
            across(c(`Workers in Commuting Flow`,
                     p.workers.of.resident.pop,
                     p.workers.of.resident.workers,
                     distance.km), 
                   list(`Mean (SD)` = ~(\(..x) paste0(format(round(mean(..x),1), big.mark = " "), " (", format(round(sd(..x),1), big.mark = " "), ")"))(.x), 
                        `Median [IQR]` = ~(\(..x) paste0(format(round(median(..x),2), big.mark = " "), " [", paste(trimws(format(round(quantile(..x, probs = c(0.25,0.75)),2), big.mark = " ")), collapse = ", "), "]"))(.x), 
                        `[Min, Max]` = ~(\(..x) paste0("[", paste(trimws(format(round(range(..x),3), big.mark = " ")), collapse = ", "), "]"))(.x)), 
                   .names = "{.col}_{.fn}")) %>%
  ungroup() %>%
  
  ###### rearrange wide to long
  pivot_longer(matches("[_]n$|sum|Max|Mean|Median"), names_to = "parameter", values_to = "value") %>%
  
  ###### create variables indicating variable being summarized and how it is presented
  mutate(var = sub("^(.+)[_].+$", "\\1", parameter), 
         parameter = sub("^.+[_](.+)$", "\\1", parameter)) %>%
  
  ###### rearrange long to wide, wide by region
  pivot_wider(names_from = census.region.origin, values_from = value)




#### simplify huge table by dropping:
##### summaries of proportions
##### summaries by flow type (which are summarized differently next)
summary.table.flows.simple <- summary.table.flows.extensive %>%
  filter(indicator.state.flow=="All Flows" & parameter!="sum" & substr(var,1,1)!="p") %>%
  select(-indicator.state.flow) %>%
  
  ###### make observation counts pretty
  mutate(across(c(`All US`, Midwest, Northeast, South, West), ~ifelse(parameter=="n", paste0("N = ", .x), .x)),
         parameter = ifelse(parameter=="n", "", parameter))



#### summarize number of workers
##### in total and by flow type
##### start with huge table previously made

summary.table.commuting.extent <- summary.table.flows.extensive %>% 
  ###### include only the sums of workers (a total count)
  filter(parameter=="sum") %>% 
  
  ###### format
  mutate(across(c(`All US`, Midwest, Northeast, South, West), ~as.numeric(gsub(" ", "", .x)))) %>% 
  
  ###### drop unnecessary vars
  select(-parameter, -var) %>% 
  
  ###### rearrange wide to long, create region variable
  pivot_longer(c(`All US`, Midwest, Northeast, South, West), names_to = "region", values_to = "n") %>% 
  
  ###### rearrange long to wide by flow type
  pivot_wider(names_from = indicator.state.flow, values_from = n) %>%
  
  ###### copy total counts of workers
  mutate(total = `All Flows`) %>%
  
  ###### rearrange wide to long, recreate flow type var, now have total worker count as separate var
  pivot_longer(!c(period, region, total), names_to = "type", values_to = "value") %>%
  
  ###### summarize counts of workers by flow type
  mutate(`n (%)` = paste0(format(round(value,0), big.mark = " "), ifelse(type=="All Flows", "", paste0(" (", round(value/total*100, 1), ")")))) %>%
  
  ###### drop unnecessary vars
  select(-value, -total) %>%
  
  ###### rename
  select(period, region, var=type, value = `n (%)`) %>%
  
  ###### clean up, rename, relabel
  mutate(parameter = ifelse(var=="All Flows", "", "n (%)"),
         class = ifelse(var=="All Flows", "", var), 
         value = ifelse(var=="All Flows", paste0("N = ", value), value),
         var = ifelse(var=="All Flows", "Total Workers", "Extent of Workers' Commute")) %>%
  
  ###### rearrange long to wide by region
  pivot_wider(names_from = region, values_from = value) %>%
  
  ###### clean up
  mutate(across(everything(), ~trimws(.x)))




#### combine simple table and commuting extent table
##### clean up and make pretty
summary.table.flows <- bind_rows(summary.table.flows.simple, 
                                 summary.table.commuting.extent) %>%
  mutate(var = ifelse(var=="distance.km", "Distance (km)", var), 
         var = factor(var, levels = c("Total Observations", "Total Workers", "Extent of Workers' Commute", "Workers in Commuting Flow", "Distance (km)"), ordered = TRUE), 
         parameter = factor(parameter, levels = c("", "n (%)", "Mean (SD)", "Median [IQR]", "[Min, Max]"), ordered = TRUE), 
         class = factor(class, levels = c("", "Intracounty", "Intrastate", "Intraregion", "Interregion"), ordered = TRUE), 
         period = factor(period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE)) %>%
  select(Period=period, Variable=var, Class=class, Parameter=parameter, `All US`, Midwest, Northeast, South, West) %>%
  arrange(Period, Variable, Parameter, Class) %>%
  mutate(across(everything(), ~as.character(.x)))




## summarize population data

### combine population and region data
pop <- full_join(pop, 
                 regions, 
                 by = c("STNAME"="State")) %>%
  #### format
  mutate(POPESTIMATE=as.numeric(POPESTIMATE))


### create summaries
#### for time periods:
##### 2011-2020
##### 2011-2015
###### 2013 (which was midpoint and combined with commuting data for 2011-2015)
##### 2016-2020
###### 2018 (which was midpoint and combined with commuting data for 2016-2020)


summary.table.pop.extensive <- pop %>%
  #### create period variable
  mutate(Period = ifelse(YEAR%in%c(2011:2015), "2011-2015", "2016-2020")) %>%
  
  #### duplicate rows for aggregate time period and regions 
  bind_rows(mutate(., Region = "All US")) %>%
  bind_rows(mutate(., Period = "2011-2020")) %>%
  
  #### isolate 2013 and 2018 and add in duplicates
  bind_rows(., 
            pop %>% 
              filter(YEAR%in%c(2013,2018)) %>% 
              mutate(Period = ifelse(YEAR==2013, "2013", "2018")) %>%
              bind_rows(mutate(., Region = "All US"))) %>%
  
  
  #### summarize by period and region
  group_by(Period, Region) %>%
  
  summarise(`Total Counties_n` = as.character(format(n(), big.mark = " ")), 
            `Total Population_sum` = as.character(format(sum(POPESTIMATE), big.mark = " ")), 
            across(POPESTIMATE, 
                   list(`Mean (SD)` = ~(\(..x) paste0(format(round(mean(..x),1), big.mark = " "), " (", format(round(sd(..x),1), big.mark = " "), ")"))(.x), 
                        `Median [IQR]` = ~(\(..x) paste0(format(round(median(..x),2), big.mark = " "), " [", paste(trimws(format(round(quantile(..x, probs = c(0.25,0.75)),2), big.mark = " ")), collapse = ", "), "]"))(.x), 
                        `[Min, Max]` = ~(\(..x) paste0("[", paste(trimws(format(round(range(..x),3), big.mark = " ")), collapse = ", "), "]"))(.x)), 
                   .names = "{.col}_{.fn}")) %>%
  ungroup() %>%
  
  
  
  #### rearrange wide to long
  pivot_longer(matches("[_]n$|sum|Max|Mean|Median"), names_to = "parameter", values_to = "value") %>%
  
  #### create variables indicating variable being summarized and how it is presented
  mutate(var = sub("^(.+)[_].+$", "\\1", parameter), 
         parameter = sub("^.+[_](.+)$", "\\1", parameter)) %>%
  
  #### rearrange long to wide, wide by region
  pivot_wider(names_from = Region, values_from = value) %>%
  
  #### scrub junk values
  filter(!(Period%in%c("2011-2015", "2011-2020", "2016-2020") & parameter%in%c("n", "sum"))) %>%
  
  #### clean up and make pretty 
  mutate(across(c(`All US`, Midwest, Northeast, South, West), 
                ~ifelse(parameter%in%c("n", "sum"), paste0("N = ", .x), .x)), 
         parameter = ifelse(parameter%in%c("n", "sum"), "", parameter), 
         var = ifelse(var=="POPESTIMATE", "Population", var)) %>%
  mutate(Period = factor(Period, levels = c("2011-2020", "2011-2015", "2013", "2016-2020", "2018"), ordered = TRUE), 
         parameter = factor(parameter, levels = c("", "Mean (SD)", "Median [IQR]", "[Min, Max]"), ordered = TRUE)) %>%
  select(Period, Variable=var, Parameter = parameter, everything()) %>%
  arrange(Period, Parameter)
  

### simplify to include only 2013 and 2018
summary.table.pop <- summary.table.pop.extensive %>%
  filter(Period%in%c("2013", "2018"))


## combine summaries

summary.table <- bind_rows(summary.table.flows, 
                           summary.table.pop) %>%
  mutate(Period = factor(Period, levels = c("2011-2020", "2011-2015", "2013", "2016-2020", "2018"), ordered = TRUE)) %>%
  arrange(Period)




## save

save(summary.table, 
     summary.table.flows.extensive, 
     summary.table.flows, 
     summary.table.pop.extensive, 
     summary.table.pop, 
     file = "./03-Output/01-Tables/summary_tables.rdata")




## clean environment
rm(list = ls())
gc()










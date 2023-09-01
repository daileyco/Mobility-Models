# Interpolate Connecticut Planning Region Population Estimates


## Packages
library(readxl)
library(dplyr)
library(tidyr)


## Load data
pop1019.ctplan <- read_xlsx("./01-Data/00-Raw-Data/Population/Connecticut-Planning-Regions-2010-2019.xlsx")

### simpler names
names(pop1019.ctplan) <- c("planning.region", "2010", "2019")


### transform wide to long, format variable types
pop1019.ctplan <- pop1019.ctplan %>%
  pivot_longer(c("2010","2019"), names_to = "year", values_to = "pop.est") %>%
  mutate(year = as.numeric(year), 
         pop.est = as.numeric(pop.est))


## Linear model fit
fit.popct <- lm(pop.est~planning.region*year, data = pop1019.ctplan)


## Expand dataframe to include missing years
pop1019.ctplan.all <- expand.grid(planning.region = unique(pop1019.ctplan$planning.region), 
                                  year = 2010:2019)

pop1019.ctplan <- full_join(pop1019.ctplan, 
                            pop1019.ctplan.all, 
                            by = c("planning.region", "year"))


## Use linear model to predict (interpolate) population estimates for missing years
pop1019.ctplan$pop.est.pred <- predict(fit.popct, pop1019.ctplan)


## Arrange neatly
pop1019.ctplan <- pop1019.ctplan %>%
  arrange(planning.region, year)




## Save data
save(pop1019.ctplan, file = "./01-Data/01-Processed-Data/pop1019_ctplan.rds")


## Clean environment
rm(list=ls())
gc()




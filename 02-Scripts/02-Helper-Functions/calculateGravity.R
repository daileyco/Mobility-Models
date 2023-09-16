# Function to estimate human mobility using a variation of a gravity model

# model
## model equations have 10 parameters and 1 pseudo-parameter (?)
### piece-wise with respect to intra- or inter-region movement
#### two parameters are for intra-region movement
##### a0 + a1 log(Pi)

#### eight parameters and one pseudo-parameter are for inter-region movement
##### base model
###### b0 + b1 log(Pi) + b2 log(Pj) + b3 log(dij)
##### adjustments / interaction terms for long distance movements
###### b4 I(dij>dt) + b5 I(dij>dt) log(Pi) + b6 I(dij>dt) log(Pj) + b7 I(dij>dt) log(dij)

### dependent variable
#### the count of people moving (commuting) within origin or between origin and destination

### independent variables
#### Pi is the origin population size
#### Pj is the destination population size
#### dij is the distance between origin and destination

### parameters are
#### dt is a distance threshold, to control for long distance flows (the pseudo-parameter)
##### I(dij>dt) represents an indicator variable denoting long distance 

#### a0,b0,b4 are intercept/intercept adjustments
#### a1,b1,b5 are slopes/slope adjustments for origin population, Pi
#### b2,b6 are slope/slope adjustment for destination population, Pj
#### b3,b7 are slope/slope adjustment for flow distance




# function arguments
## params is a vector of parameter values
### params[1:2] = c(a0,a1)
### params[3:10] = c(b0:b7)
### params[11] = c(dt)

## oddf is a dataframe of observed movements between origins and destinations
### contains independent variables 
#### (hardcoded for this project)
#### log transformed


# function

calculateGravity <- function(observed.data, params){
  
  require(dplyr)
  
  
  observed.data %>%
    
    mutate(indicator.long.distance = ifelse(log.distance.km>params[11], 1, 0)) %>%
    
    mutate(gravity.flow.log = case_when(distance.km == 0 ~ 
                                          params[1] 
                                        + params[2]*log.POPESTIMATE.origin, 
                                        
                                        
                                        distance.km > 0 ~ 
                                          params[3] 
                                        + params[4]*log.POPESTIMATE.origin 
                                        + params[5]*log.POPESTIMATE.destination 
                                        + params[6]*log.distance.km
                                        
                                        + params[7]*indicator.long.distance 
                                        + params[8]*indicator.long.distance*log.POPESTIMATE.origin 
                                        + params[9]*indicator.long.distance*log.POPESTIMATE.destination 
                                        + params[10]*indicator.long.distance*log.distance.km, 
                                        
                                        
                                        TRUE ~ NA
                                        )) %>%
           
    mutate(gravity.flow = exp(gravity.flow.log)) %>%
    
    select(indicator.long.distance, 
           gravity.flow.log,
           gravity.flow) %>%
    
    return()
  
}















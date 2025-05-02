# Estimate gravity between two locations given gravity model parameters

## model is fit against logistic transformation of 
## proportion of origin population size participating in flow

## model is fit using equations with 16 parameters
### base model
#### b0 + b1 log(Pi) + b2 log(Pj) + b3 log(dij)
### adjustments for internal, zero distance, flows
#### b4 I(dij=0) + b5 I(dij=0) log(Pi)
### adjustments for long distance flows
#### b6 I(dij>=dt) + b7 I(dij>=dt) log(Pi) + b8 I(dij>=dt) log(Pj) + b9 I(dij>=dt) log(dij)
### adjustments for flows between two large populations
#### b10 I(Pi,Pj >= Pt) + b11 I(Pi,Pj >= Pt) log(Pi) + b12 I(Pi,Pj >= Pt) log(Pj) + b13 I(Pi,Pj >= Pt) log(dij)

### b0,b4,b6,b10 are intercept/intercept adjustments
### b1,b5,b7,b11 are slopes/slope adjustments for origin population, Pi
### b2,b8,b12 are slopes/slope adjustments for destination population, Pj
### b3,b9,b13 are slopes/slope adjustments for flow distance
### dt is a distance threshold, to control for long distance flows
### Pt is a population size threshold, to control for flows between two large populations
### I() represents indicator variables


## params is a vector of parameter values
### params[1:14] = c(b0:b13)
### params[15:16] = c(dt, Pt)


## data are adjusted to force internal flows to be estimated separately 
### for distance.km = 0
#### the log.distance.km is set to 0 & 
#### the log.POPESTIMATE.destination is set to 0

## so we're left with five groups
### internal flows are estimated as
#### (b0 + b4) + (b1 + b5) log(Pi)
### flows with distance below long distance threshold and not between two large pops are estimated as
#### b0 + b1 log(Pi) + b2 log(Pj) + b3 log(dij)
### flows with distances beyond long distance threshold and not between two large pops are estimated as
#### (b0 + b6) + (b1 + b7) log(Pi) + (b2 + b8) log(Pj) + (b3 + b9) log(dij)
### flows with distances below long distance threshold and between two large pops are estimated as
#### (b0 + b10) + (b1 + b11) log(Pi) + (b2 + b12) log(Pj) + (b3 + b13) log(dij)
### and flows with distances beyond lond distance threshold and between two large pops are estimated as
#### (b0 + b6 + b10) + (b1 + b7 + b11) log(Pi) + (b2 + b8 + b12) log(Pj) + (b3 + b9 + b13) log(dij)



calculateGravity <- function(params, oddf){
  
  require(dplyr)
  
  
  oddf %>%
    mutate(log.distance.km = ifelse(distance.km==0, 0, log.distance.km), 
           log.POPESTIMATE.destination = ifelse(distance.km==0, 0, log.POPESTIMATE.destination)) %>%
    
    mutate(indicator.zero.distance = ifelse(distance.km==0, 1, 0), 
           indicator.long.distance = ifelse(log.distance.km>=params[15], 1, 0), 
           indicator.large.pops = ifelse(log.POPESTIMATE.origin >= params[16] & log.POPESTIMATE.destination >= params[16], 1, 0)) %>%
    
    # mutate(gravity.logistic =
    #          params[1] + params[2]*log.POPESTIMATE.origin + params[3]*log.POPESTIMATE.destination + params[4]*log.distance.km
    #        + params[5]*indicator.zero.distance + params[6]*log.POPESTIMATE.origin
    #        + params[7]*indicator.long.distance + params[8]*indicator.long.distance*log.POPESTIMATE.origin + params[9]*indicator.long.distance*log.POPESTIMATE.destination + params[10]*indicator.long.distance*log.distance.km
    #        + params[11]*indicator.large.pops + params[12]*indicator.large.pops*log.POPESTIMATE.origin + params[13]*indicator.large.pops*log.POPESTIMATE.destination + params[14]*indicator.large.pops*log.distance.km) %>%
    
    # mutate(gravity.proportion = exp(gravity.logistic)/(1+exp(gravity.logistic))) %>%
    # 
    # mutate(gravity.flow = gravity.proportion*log.POPESTIMATE.origin)%>%
    
    mutate(gravity.flow.log =
           params[1] + params[2]*log.POPESTIMATE.origin + params[3]*log.POPESTIMATE.destination + params[4]*log.distance.km
         + params[5]*indicator.zero.distance + params[6]*log.POPESTIMATE.origin
         + params[7]*indicator.long.distance + params[8]*indicator.long.distance*log.POPESTIMATE.origin + params[9]*indicator.long.distance*log.POPESTIMATE.destination + params[10]*indicator.long.distance*log.distance.km
         + params[11]*indicator.large.pops + params[12]*indicator.large.pops*log.POPESTIMATE.origin + params[13]*indicator.large.pops*log.POPESTIMATE.destination + params[14]*indicator.large.pops*log.distance.km) %>%
  
    mutate(gravity.flow = exp(gravity.flow.log)) %>%
    
  
    select(indicator.zero.distance, 
           indicator.long.distance, 
           indicator.large.pops, 
           # gravity.logistic, 
           # gravity.proportion, 
           gravity.flow.log,
           gravity.flow
           ) %>%
    
    return()
  
}















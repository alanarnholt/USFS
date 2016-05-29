##Var 5 H
##Annual Domestic harvest (H)

#Var5= (Calculaton DM) *1000
#DM= DI + DO + DN

Var5 <- function(y){
  return((Calc_DI(y)+Calc_DO(y)
          + Calc_DN(y))*1000)
}
#########
#DI= industrial roundwood production as C flow
Calc_DI <- function(y){
  return(PRO17*usa_N(y))
}
usa_N <- function(year){
  if (year < 1950){
    return((h3(year, 38)*InceN5)*1000)
  }
  if (year < 1965){
    return((u4(year, 25)*InceN5)*1000)
  }
  if (year < 2021){
    return((h5(year, 20)*InceN5)*1000)
  }
  if (year < 2051){
    return((InceM13*InceN5)*1000)
  }
}

#########
#DO= fuelwood
Calc_DO <- function(year){
  PRO17*usa_G(year)
}
usa_G <- function(year){
  if (year < 1950){
    return(((u5(year, 28)*InceV5+ u6(year,28) *InceW5)) / ((u5(year, 28)+ (u6(year, 28) * h3(year, 39)) *1000))
  }
  if (year < 1965){
    
  }
  if (year < 2013){
    
  }
    
  }





##########
#DN= bark carbon 

#in progress


##conversions 
PRO17 <- 0.0000004535925 ##SWP, odt -> Tg/c
PRO18 <- 0.00000039008955 ##PAPer, odt -> Tg/C
PRP10 <- 2.53087281800454 ###half life of paper in yrs
InceB5 <- 0.455065529436239 ##sw plywood 1000ft 3/8in to od tons
InceC5 <- 0.525099768956102 ##osb/waferboard 1000ft 3/8in to odt tons
InceE5 <- 0.543478260869565##hw plywood/veneer 1000 bd ft to od tons
InceF5 <- 0.8319 ##SW lumber, 1000bdft -> od tons
InceG5 <- 1.327375 ##HW lumber, 1000bdft -> od tons
InceI5 <- 1.12702865157283 ##hw particle board prod.1000 ft to od tons
InceJ5 <- 0.271739130434783 ##hardboard prodcution 1000 ft, 1/8 in to od tons
InceK5 <- 1.24007936507936 ##pulp/paper, mdf prod.
InceL5 <- 0.970873786407767 ##Pulp/paper 1000tons -> od tons
InceN5 <- 14.89675 #### pulpwood (other industrial products) 1000ft^3 to od tons
InceO5 <- 0.425724637681159 ##insulating board 1000 ft, 1/2 inch
InceQ5 <- 0.900 ## tons insulating board to od tons
InceS5 <- 13.8649961379827 #softwood roundwood 1000 Cu ft to od tons
InceT5 <- 15.9283989668863#hardwood roundwood 1001 Cu ft to od tons
InceR5 <- 0.173913043478261 ##hw veneer, 1000ft^2 to od tons 
InceV5 <- 13.8649961379827 ##SW roundwood, 1000ft^3 -> od tons
InceW5 <- 15.9283989668863 ##HW roundwood 1000ft^3 -> od tons
PRI96 <- 1 #Factor to adjust base MSW not burned for paper and wood
PRP62 <- 1.00 ##Error in fraction of domestic products from imported wood/pulp
PRM45 <- .23 #WOOD DECAY LIMIT IN SWDS
PRM46 <- 0.56#paper decat limit in swds
PRM50 <- 0.0300063714528115 #Wood waste HL in SWDS, for landfills
PRM51 <- 0.0478032538317204#Paper waste HL in SWDS, landfills
PRM57 <- 1 #swtich to be included in the SWDS degradable carbon pool
PRM60 <- 0.0420089200339361 #Wood waste HL in SWDS, for dumps
PRM61 <- 0.0840178400678722#Paper waste HL in SWDS, dumps
PRJ96 <- 1 #Factor to adjust base MSW not burned for paper and wood
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
    return((i1(year,13)*InceN5)*1000)
  }
}

#########
#DO= fuelwood
Calc_DO <- function(year){
  PRO17*usa_G(year)
}
usa_G <- function(year){
  if (year < 1950){
    return(((u5(year, 28)*InceV5+ u6(year,28) *InceW5)) / ((u5(year, 28)+ u6(year, 28) * h3(year, 39)) *1000))
  }
  if (year < 1965){
    return(u5(year, 28) * InceV5 + u6(year, 28) * InceW5) *1000
  }
  if (year < 2014){
    return(h6(year, 25) * InceV5 + h7(year, 25)* InceW5) * 1000
  }
}





##########
#DN= bark carbon 
Calc_DN <- function(year){
  PRO17*((PRM19*usa_BO)+(PRM20*usa_BP))
}
usa_BO <- function(year){
  if (year < 1950){
    return(h3(year, 5)* InceV5) *1000
  }
  if (year < 1965){
    return(u5(year, 4) * InceV5) *1000
  }
  if (year < 2014){
    return(h6(year, 4) * InceV5) * 1000
  }
  if (year < 2051){
    return(i3(year, 11)* InceV5) *1000
  }
}

usa_BP <- function(year){
  (usa_C - usa_BO)
}
usa_C <- function(year){
  if (year < 1950){
    return(h3(year, 6)* InceV5 + h3(year, 7) * InceW5) *1000
  }
  if (year < 1965){
    return(u5(year, 5) * InceV5 + u6(year, 5) * InceW5) *1000
  }
  if (year < 2014){
    return(h6(year, 4) * InceV5 + h7(year, 4) * InceW5) * 1000
  }
  if (year < 2051){
    return((i3(year, 11)* InceV5) + i3(year, 10) * InceW5) *1000
  }
}



 ##Var #3 P_IM
# #Annual Imports of wood, and paper products + wood fuel, pulp, recovered paper, roundwood/ chips
var3 <- function(y){
  return((Calc_AX(y)+Calc_AY(y)
            + Calc_AZ(y)+Calc_BA(y) + Calc_BB(y))*1000)
}
############
Calc_AX <- function(y){
  return(PRO17*usa_O(y))
}
usa_O <- function(year){
  if (y < 1950){
    return((h3(year,7)*InceS5+h3(year,9)*InceT5)*1000)
  }
}
############
Calc_AY <- function(year){
  PRO17*usa_I(year)
}
h8(1915, 3)*InceF5*1000
h8(1915,3)
InceF5
usa_I <- function(year){
  if (year < 1918){
    return(h8(year, 4)*InceF5*1000)
  }
  if(year < 1950){
    return(((h8(year,5)+h8(year,7))*InceF5+(h8(year,6)*InceG5))*1000)
  }
  if (year < 1965){
    return(((u29(year, 5)*1000*InceF5)+(u29(year, 6)*1000*InceG5))*1000)
  }
  if (year < 2021){
    ((h28(year, 5)*1000*InceF5)+(h28(year, 6)*1000*InceG5))*1000
  }
}
usa_I(1959)

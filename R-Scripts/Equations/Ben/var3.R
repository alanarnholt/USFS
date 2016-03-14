 ##Var #3 P_IM
# #Annual Imports of wood, and paper products + wood fuel, pulp, recovered paper, roundwood/ chips
#mistake is excel for yrs 2019+2020
Var3 <- function(y){
  return((Calc_AX(y)+Calc_AY(y)
            + Calc_AZ(y)+Calc_BA(y) + Calc_BB(y))*1000)
}
############
Calc_AX <- function(y){
  return(PRO17*usa_O(y))
}
usa_O <- function(year){
  if (year < 1950){
    return((h3(year,7)*InceS5+h3(year,9)*InceT5)*1000)
  }
  if (year < 1965){
    return((u5(year,20)*InceS5+u6(year,25)*InceT5)*1000)
  }
  if (year < 1983){
    return((h6(year,20)*InceS5+h7(year,21)*InceT5)*1000)
  }
  if (year < 2021){
    return((h6(year,20)*InceS5+h7(year,20)*InceT5+h6(year,22)*InceS5+h7(year,22)*InceT5)*1000)
  }
}
############
Calc_AY <- function(year){
  PRO17*usa_I(year)
}
usa_I <- function(year){
  if (year < 1918){
    return(h8(year, 4)*InceF5*1000)
  }
  if (year < 1950){
    return(((h8(year,5)+h8(year,7))*InceF5+(h8(year,6)*InceG5))*1000)
  }
  if (year < 1965){
    return(((u29(year, 5)*1000*InceF5)+(u29(year, 6)*1000*InceG5))*1000)
  }
  if (year < 2021){
    ((h28(year, 5)*1000*InceF5)+(h28(year, 6)*1000*InceG5))*1000
  }
}
############
Calc_AZ <- function(year){
  PRO17 * usa_L(year)
}
usa_L <- function(year){
  if (year < 1927)
    return(0)
  if (year < 1935)
    return((h3t21(year,1)/1000)*InceR5*1000)
  if (year < 1950)
    return(((h3t20(year,3)*InceE5 +h3t21(year,1)*InceR5)/1000)*1000)
  if (year < 1954)
    return(((u36(year, 5)*InceB5)+(u36(year, 6)*InceE5))*1000)
  if (year < 1956)
    return(((u36(year, 6)*InceB5)+(u36(year,7)*InceE5)+(u54(year,2)*InceJ5)+(u53(year,2)*InceO5))*1000)
  if (year < 1963)
    if (year == 1956 || year == 1959)
      return(((u36(year, 7)*InceE5)+(u54(year, 2)*InceJ5)+(u53(year, 2)*InceO5))*1000)
    else 
      return(((u36(year, 6)*InceB5)+(u36(year, 7)*InceE5)+(u54(year, 2)*InceJ5)+(u53(year, 2)*InceO5))*1000)
  if (year < 1965)
    return(((u36(year, 5)*InceB5)+(u36(year, 6)*InceE5)+(u52(year, 4)*InceI5)+(u54(year, 2)*InceJ5)+(u53(year, 2)*InceO5))*1000)
  if (year < 1980)
    return(((h37(year,5)*InceB5)+(h37(year,6)*InceE5)+(h53(year,4)*InceI5)+(h56(year,2)*InceJ5)+h55(year,2)*InceQ5)*1000)###InceP23 doesnt make sense**, Q5 does.
  if(year < 2021)
    return(((h37(year,5)*InceB5)+(h38(year,6)*InceC5)+(h37(year, 6)*InceE5)+(h53(year,4)*InceI5)+(h56(year,2)*InceJ5)+h55(year, 2)*InceQ5)*1000)
}
######
Calc_BA <- function(year){
  PRO18*usa_T(year)*usa_AB(year)
}
usa_T <- function(year){
  getIncePap(year,2)*1000*InceL5
}
usa_AB <- function(year){
  ((usa_AM(year)+usa_AN(year)-usa_AO(year))-(usa_AI(year)+usa_AJ(year)-usa_AK(year)))/(usa_AM(year)+usa_AN(year)-usa_AO(year))
}
####
Calc_BB <- function(year){
  PRO18*usa_AX(year)
}
usa_AX <- function(year){
  usa_AE(year)+usa_AU(year)+usa_AQ(year)
}
usa_AU <- function(year){
  if (year < 1965)
    return(apiTotal(year,7))
  if (year < 2021)
    return(h47(year,5)*1000)
}
usa_AQ <- function(year){
  if (year < 1998)
    return(0)
  if (year < 2014)
    return(usa_CF(year)*.90718)
  if (year < 2021)
    return(usa_AQ(2007))
}
usa_CF <- function(year){
  recFibPulpUSA[year-1997, "usa_CF"]
}
errors <- testfunc(1990:2020, usa_AU);max(errors)
for(i in 1990:2020){
  print(Var3(i))
}

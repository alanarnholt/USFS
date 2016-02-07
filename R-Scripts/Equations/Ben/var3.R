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
  if (y > 1949 && y < 1965){
    return((u5(year,20)*InceS5+u6(year,25)*InceT5)*1000)
  }
  if (y > 1964 && y < 1983){
    return((h6(1970,20)*InceS5+h7(year,21)*InceT5)*1000)
  }
  if (y > 1982 && y < 2021){
    return((h6(1970,20)*InceS5+h7(year,20)*InceT5+h6(year,22)*InceS5+h7(year,22)*InceT5)*1000)
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
  if (year > 1917 && year < 1950){
    return(((h8(year,5)+h8(year,7))*InceF5+(h8(year,6)*InceG5))*1000)
  }
  if (year > 1949 && year < 1965){
    return(((u29(year, 5)*1000*InceF5)+(u29(year, 6)*1000*InceG5))*1000)
  }
  if (year > 1964 && year < 2021){
    ((h28(year, 5)*1000*InceF5)+(h28(year, 6)*1000*InceG5))*1000
  }
}
############
Calc_AZ <- function(year){
  PRO17 * usa_L(year)
}
usa_L <- function(year){
  if (year < 1927){
    return(0)
  }
  if (year > 1926 && year < 1935){
    return((h3t21(year,1)/1000)*InceR5*1000)
  }
  if (year > 1934 && year < 1950){
    return(((h3t20(year,3)*InceE5 +h3t21(year,1)*InceR5)/1000)*1000)
  }
  if (year > 1949 && year < 1954){
    return(((u36(year, 6)*InceB5)+(u36(year, 7)*InceE5))*1000)
  }
  if (year > 1953 && year < 1956){
    return(((u36(year, 6)*InceB5)+(u36(year,7)*InceE)+('Ulrich_Table 54_adj'!C46*'Ince_Table 4'!J$5)+('Ulrich_Table 53_adj'!C38*'Ince_Table 4'!O$5))*1000)
  }
  if (year < )
}


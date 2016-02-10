####################
#####Var 2PRODUCTION APPROACH, SOLID WOOD PRODUCTS CALCULATIONS
Var2_C_SWP_STOCKCHANGE <- function(y){
  
  return((Var2_totalC(y) - Var2_totalC(y-1))*PRO17)
}
#####totalC calculates total carbon left in yr from all end uses in million tonnes of carbon
Var2_totalC_SWPtable <- numeric(121)
for(y in 1900:2020){
  totalcarbon <- 0
  for (i in 1:16){
    if (i == 4 || i == 9 || i == 13){
      totalcarbon <- totalcarbon
    }
    else{
      totalcarbon <- totalcarbon + Var2_C_IU_J(y,i)
    }
  
  }
  ##pre1900() is result of calculation from linked site
  Var2_totalC_SWPtable[y-1899] <- totalcarbon + pre1900(y)
}
Var2_totalC <- function(y){
  return(Var2_totalC_SWPtable[y-1899])
}
######################################
###C_IU_J calculates total carbon left in year y for eu j in million tonnes of carbon
Var2_C_IU_J <- function(y,eu){
  total <- 0
  for(i in 1900:y){
    total <- total + (Var2_c_placed_IU(i,eu)*(exp(-log(2)/HL(i,eu)*((y-i)+1)))*(1-iuLoss(i,eu)))
  }
  return(total)
}
##################
#####c_placed_IU calculates carbon placed in use for a given end use in a year in million tonnes of carbon
Var2_c_placed_IU <- function(y,eu){
  if (eu == 16){
    return(qOther(y) * 1)
  }
  return((efinProdSawn(y) * fsw(y,eu)) +(ifinProdSP(y)*fsp(y,eu)) +(mfinProdNSP(y)*fnsp(y,eu))) 
}
################################################
efinProdSawn <- function(y) {
  efinaly <- (bSawn(y) - (1-a5) * dSawn(y)) * ((s_swp(y) + u_swp(y) * a5 - r_swp(y) * PRP62 )/ s_swp(y))
  return(efinaly)                                                                       
}
ifinProdSP <- function(y){
  ifin <- (fSP(y)-(1-a5)*hSP(y))*((s_swp(y)+u_swp(y)*a5-r_swp(y)*PRP62)/s_swp(y))
  return(ifin)
}
mfinProdNSP <- function(y){
  mfin <- (jNSP(y)-(1-a5)*lNSP(y))*((s_swp(y)+u_swp(y)*a5-r_swp(y)*PRP62)/s_swp(y))
  return(mfin)
}
qOther <- function(y){
  return(nOther(y)-(1-a5)*pOther(y))
}
nOther <- function(y){
  if (y > 1899 && y < 1950){
    return(h3(y,37)*InceN5*1000)
  }
  if (y > 1949 && y < 1965){
    return(u4(y,19)*InceN5*1000)
  }
  if (y > 1964 && y < 2021){
    return(h5(y,19)*InceN5*1000)
  }
}
pOther <- function(y){
  return(0)
}
bSawn <- function(y){
  cavg <- (h8(1904,2)-h8(1899,2))/5
  davg <- (h8(1904,3)-h8(1899,3))/5
  if(y < 1904){
    return((((h8(1899,2) +((y-1899)*(round(cavg,1))))* InceF5)+((h8(1899,3)+((y-1899)*(round(davg,1))))* InceG5))*1000)
  }
  if(y > 1903 && y < 1950){
    return(((h8(y,2)*InceF5)+(h8(y,3)*InceG5))*1000)
  }
  if(y > 1949 && y < 1965){
    return((u29(y,1)*1000*(((u29(y,2)/u29(y,1))*InceF5)+((u29(y,3)/u29(y,1))*InceG5)))*1000)
  }
  if(y > 1964 && y< 2021){
    return((h28(y,1)*1000*(((h28(y,2)/h28(y,1))*InceF5)+((h28(y,3)/h28(y,1))*InceG5)))*1000)
  }
}
a5 <- 1##Swithc
dSawn <- function(y){
  if(y < 1911){
    return(h8(y,13) * InceF5 * 1000)
  }
  if(y > 1910 && y < 1950){
    return((((h8(y,14) + h8(y,16))*InceF5) + (h8(y,15)*InceG5))*1000)
  }
  if(y > 1949 && y < 1965){
    return((((u29(y,8)*1000*InceF5)+(u29(y,9)*1000*InceG5)) * 1000))
  }
  if(y > 1964 && y< 2021){
    return( ((h28(y,8)*1000*InceF5)+(h28(y,9)*1000*InceG5))*1000)
  }
}
s_swp <- function(y){
  if(y < 1950){
    return((h3(y,28)+h3(y,31))*(InceV5*0.8+InceW5*0.2)*1000)
  }
  if(y > 1949 && y < 1965){
    return(1000*((u5(y,7)+u5(y,11))*InceV5+(u6(y,9)+u6(y,14))*InceW5))
  }
  if (y > 1964 && y < 2021){
    return(1000*((h6(y,7)+h6(y,11))*InceV5+(h7(y,7)+h7(y,11))*InceW5))
  }
}
u_swp <- function(y){
  if(y < 1965){
    return((h3(y,8)*InceV5+h3(y,10)*InceW5)*1000)
  }
  if(y<2021 && y > 1964){
    return( (h6(y,21)*InceV5+h7(y,21)*InceW5)*1000)
  }
}
r_swp <- function(y){
  if(y<1950){
    return(0)
  }
  if (y < 1965&&y>1949){
    return(1000*(u5(y,20)*InceV5+u6(y,25)*InceW5))
  }
  if(y > 1964 && y < 2021){
    return(1000*(h6(y,20)*InceV5+h7(y,20)*InceW5))
  }
}
fSP <- function(y){
  if(y < 1950){
    return(((inc1(y,1)*InceB5))*1000)
  }
  if(y >1949&&y < 1965){
    return(((u36(y,2)*InceB5))*1000)
  }
  if(y<1980&&y>1964){
    return(((h37(y,2)*InceB5))*1000)
  }
  if(y > 1979 && y < 2021){
    return(((h37(y,2)*InceB5)+(h38(y,3)*InceC5))*1000)
  }
}
hSP <- function(y){
  if(y < 1927){
    return(0)
  }
  if(y>1926 && y < 1950){
    return(((h3t20(y,7)/1000*InceB5))*1000)
  }
  if(y> 1949 && y < 1965){
    return(((u36(y,8)*InceB5))*1000)
  }
  if(y > 1964 && y < 1991){
    return(((h37(y,8)*InceB5))*1000)
  }
  if(y > 1990 && y < 2021){
    return(((h37(y,8)*InceB5)+(h38(y,9)*InceC5))*1000)
  }
}
jNSP <- function(y){
  if(y < 1950){
    return(((inc1(y,4)*InceE5)+(inc1(y,9)*InceJ5)+(inc1(y,13)*InceO5))*1000)
  }
  if(y < 1965 && y > 1949){
    return(((u36(y,3)*InceE5)+(u52(y,2)*InceI5)+(u54(y,1)*InceJ5)+(u53(y,1)*InceO5))*1000)
  }
  if(y < 2021 && y > 1964){
    return(((h37(y,3)*InceE5)+(h53(y,2)*InceI5)+(h56(y,1)*InceJ5)+(h53(y,3)*InceK5)+h55(y,1)*InceQ5)*1000)
  }
  
}
lNSP <- function(y){
  if (y<1916){
    return(0)
  }
  if(y < 1925 && y > 1915){
    return((u54(y,3)*InceJ5)*1000)
  }
  if ( y > 1924 && y < 1927){
    return(((u54(y,3)*InceJ5)+(u53(y,3)*InceO5))*1000)
  }
  if(y > 1926 && y < 1935){
    return((((h3t20(y,8)*InceE5+h3t21(y,4)*InceR5)/1000)+(u54(y,3)*InceJ5)+(u53(y,3)*InceO5))*1000)
  }
  if(y < 1950 && y > 1934){
    return((((h3t20(y,8)+h3t21(y,4))/1000*InceE5)+(u54(y,3)*InceJ5)+(u53(y,3)*InceO5))*1000)
  }
  if(y > 1949 && y < 1965){
    return(((u36(y,9)*InceE5)+(u54(y,3)*InceJ5)+(u53(y,3)*InceO5))*1000)
  }
  if(y > 1964 && y < 2021){
    return(((h37(y,9)*InceE5)+(h53(y,5)*InceI5)+(h56(y,3)*InceJ5)+h55(y,3)*InceQ5)*1000)
  }
}



perError <- function(correct,y){
  
  if( correct == 0 && y != 0){
    
    return(1000000)
  }
  if(correct == 0 && y == 0){
    return(0)
  }
  return(100*((y-correct)/correct))
}
####################
#####END-USES:
####SINGLE FAMILY HOUSING - 1
####MULTIFAMILY HOUSING - 2
####MOBILE HOME HOUSING - 3
#### TOTAL HOUSING - 4
####TOTAL RESIDENTIAL UPKEEP/IMPROVEMENT - 5
#### ALL NONRES CONSTRUCTION EX RAILROADS - 6
####RAILROAD TIES - 7
####RAILCAR REPAIR - 8
####TOTAL NONRES CONSTRUCTION - 9
####HOUSEHOLD FURNITURE - 10
####COMMERICAL FURNITURE - 11
####Manufacturing-other - 12
####manufacturing total - 13
#####SHIPPING TOTAL - 14
#####OTHER USES LUMBER/PANELS - 15
###USES FOR OTHER INDUSTRIAL PRODUCTS - 16
####EXPORTS - 17
##############
##################
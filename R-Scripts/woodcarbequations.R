##conversions 
PR017 <- 4.54e-7 ##SWP, odt -> Tg/c
PR018 <- 3.9e-7 ##PAPer, odt -> Tg/C
InceC5 <- 0.525 ##osb/waferboard 1000ft 3/8in to odt tons
InceE5 <- 0.543##hw plywood/veneer 1000 bd ft to od tons
InceJ5 <- .272 ##hardboard prodcution 1000 ft, 1/8 in to od tons
InceI5 <- 1.127 ##hw particle board prod.1000 ft to od tons
InceO5 <- 0.426 ##insulating board 1000 ft, 1/2 inch
InceF5 <- 0.832 ##SW lumber, 1000bdft -> od tons
InceK5 <- 1.24 ##pulp/paper, mdf prod.
InceB5 <- 0.455 ##sw plywood 1000ft 3/8in to od tons
InceG5 <- 1.327 ##HW lumber, 1000bdft -> od tons
InceR5 <- 0.174 ##hw veneer, 1000ft^2 to od tons 
InceV5<- 13.865 ##SW roundwood, 1000ft^3 -> od tons
InceL5 <- 0.971 ##Pulp/paper 1000tons -> od tons
InceW5 <- 15.928 ##HW roundwood 1000ft^3 -> od tons
InceQ5 <- 0.900 ## tons insulating board to od tons
PRP62 <- 1.00 ##Error in fraction of domestic products from imported wood/pulp

##########

swPcorrect <- read.xlsx("swPcorrect.xlsx",1,header=F)
rownames(swPcorrect) <- 1900:2020
getSWP <- function(y,c){
  return(swPcorrect[y-1899,c])
}
bSawn <- function(y){
  cavg <- (h8(1904,2)-h8(1899,2))/5
  davg <- (h8(1904,3)-h8(1899,3))/5
  if(y < 1904){
    return((((h8(1899,2) +((y-1899)*(cavg)))* InceF5)+((h8(1899,3)+((y-1899)*(davg)))* InceG5))*1000)
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
    return(1000*((u5(y,7)+u5(y,14))*InceV5+(u6(y,9)+u6(y,14))*InceW5))
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
    return(((inc1(y,LTC("e"))*InceE5)+(inc1(y,LTC("j"))*InceJ5)+(inc1(y,LTC("n"))*InceO5))*1000)
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
    return((((h3t20(y,LTC("i"))*InceE5+h3t21(y,LTC("e"))*InceR5)/1000)+(u54(y,3)*InceJ5)+(u53(y,3)*InceO5))*1000)
  }
  if(y < 1950 && y > 1934){
    return((((h3t20(y,LTC("i"))+h3t21(y,LTC("e")))/1000*InceE5)+(u54(y,3)*InceJ5)+(u53(y,3)*InceO5))*1000)
  }
  if(y > 1949 && y < 1965){
    return(((u36(y,9)*InceE5)+(u54(y,3)*InceJ5)+(u53(y,3)*InceO5))*1000)
  }
  if(y > 1964 && y < 2021){
    return(((h37(y,LTC("j"))*InceE5)+(h53(y,LTC("f"))*InceI5)+(h56(y,3)*InceJ5)+h55(y,3)*InceQ5)*1000)
  }
}

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
finProdCorrect <- read.xlsx("prodFin.xlsx",1,header=F)
rownames(finProdCorrect) <- 1900:2020
finCorr <- function(y,c){
  return(finProdCorrect[y-1899,c])
}
qOther <- function(y){
  return(nOther(y)-(1-a5)*pOther(y))
}
nOther <- function(y){
  if (y > 1899 && y < 1950){
    return(h3(y,37)*InceN5*1000)
  }
  if (y > 1949 && y < 1965){
    return(ulrich4(y,24)*InceN5*1000)
  }
  if (y > 1964 && y < 2021){
    return(howard5(y,19)*InceN5*1000)
  }
}
pOther <- function(y){
  return(0)
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
##for(i in 1900:2020){###checks bsawn
##  print(perDiff(bSawn(i),getSWP(i,1)))
##}####checks bSawn
# for(i in 1900:2020){
#   print(perDiff(dSawn(i),getSWP(i,3)))
# }###checks dSAwn
# for(i in 1900:2020){
#   print(perDiff(sSawn(i),getSWP(i,18)))
# }###checks sSAwn
# for(i in 1900:2020){
#   print(perDiff(uSawn(i),getSWP(i,20)))
# }####checks uSawn
# for(i in 1900:2020){
#   print(perDiff(rSawn(i),getSWP(i,17)))
# }####broken checks rSawn
# for(i in 1900:2020){##checks fSP, .017% max error 
#   print(perDiff(fSP(i),getSWP(i,5)))
# }
# for(i in 1900:2020){
#   print(perDiff(hSP(i),getSWP(i,7)))
# ###checks hSP
# ###for(i in 1900:2020){##efin
#   total[(i-1899)] <- perError(efinProdSawn(i),getSWP(i,4))
#   
# }##efin
# ###for(i in 1900:2020){
#   total[((i-1899)+121)] <- perError(ifinProdSP(i),getSWP(i,8)) 
#   
# }##ifin check, error is higher than e, go back and chec
# ###for(i in 1916:2020){##mfin check
#   total[(i-1915) + 242] <- perError(mfinProdNSP(i),getSWP(i,12))
#   
# }##mfin
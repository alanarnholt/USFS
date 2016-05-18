#### cgko
#'VAR 1b - not working 
#'Annual Change in stock of HWP in SWDS produced from domestic harvest 
#'Output in Gg C/yr
Var1b_C_STOCKCHANGE_TOTAL <- function(y){
  return(1000*(C_SWP_StockChange_LFDumps(y) + C_PAPER_StockChange_LFDumps(y)))
}
#'VAR #1a - working 
#'Annual Change in stock of HWP in use produced from domestic harvest 
#'Output is in Gg C/yr
###
Var1a_STOCKCHANGE_TOTAL <- function(y){
  return((Var1_C_SWP_STOCKCHANGE(y) + Var1_C_PAPER_STOCKCHANGE(y))*1000)
}

finalcheck <- read.xlsx("finaLIpcctable.xlsx",1,header=FALSE)
# for(i in 1990:2020){
#   print(perError(finalcheck[i-1989,1], VAR1_STOCKCHANGE_TOTAL(i)))
# }
# perError(finalcheck[2000-1989,1], VAR1_STOCKCHANGE_TOTAL(2000))
Var1_C_SWP_STOCKCHANGE <- function(y){
  
  return((Var1_totalC(y) - Var1_totalC(y-1))*PRO17)
}
Var1_totalC <- function(y){
  total <- 0
  for (i in 1900:y){
    for (eu in 1:16){
      if (eu == 4 || eu  == 9 || eu ==13){
        total <- total
      }
      else{
        total <- total + (Var1_c_placed_IU(i, eu)*(exp(-log(2)/HL(i,eu)*((y-i)+1)))*(1-iuLoss(i,eu)))
      }
    }
  }
  return(total + pre1900(y))
}
#####check for var1
# tesvector <- numeric(121)
# testcdata <- read.xlsx("totalCSWP.xlsx",1,header=FALSE)
# for(i in 1900:2020){
#   tesvector[i-1899] <- perError(testcdata[i-1899,2], var1_stockchnge_totalC(i))
# }
##################
#####c_placed_IU calculates carbon placed in use for a given end use in a year in million tonnes of carbon
Var1_c_placed_IU <- function(y,eu){
  if (eu == 16){
    return(qOther(y) * 1)
  }
  return((Var1_eSawn(y) * fsw(y,eu)) +(Var1_iSP(y)*fsp(y,eu)) +(Var1_mNSP(y)*fnsp(y,eu))) 
}


##e column in SWCALC
Var1_eSawn<-function(y){
  return(bSawn(y)+cSawn(y)-dSawn(y))
}
##I column in SWCALC
Var1_iSP<-function(y){
  return(fSP(y)+gSP(y)-hSP(y))
}
##m column in SWCALC
Var1_mNSP<-function(y){
  return(jNSP(y)+kNSP(y)-lNSP(y))
}

######################
##b column in swcalc
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
##d column in swcalc
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
##f column in swcalc
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
##h column in swcalc
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
##j column in swcalc
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
##l column in swcalc
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

##c column in swcalc
cSawn<-function(y){
  if(y > 1899 && y < 1918){
    return(h8(y,4)*InceF5*1000)
  }
  if(y>1917 && y < 1950){
    return((((h8(y,5)+h8(y,7))*InceF5)+(h8(y,6)*InceG5))*1000)
  }
  if(y > 1949 && y < 1965){
   return(((u29(y,5)*1000*InceF5)+(u29(y,6)*InceG5*1000))*1000)
  }
  if(y>1964 && y< 2021){
    return(((h28(y,5)*1000*InceF5)+(h28(y,6)*InceG5*1000))*1000)
  }
  if(y>2019 && y< 2051){
    return(((i1(y,4)*InceF5)+(i1(y,5)*InceG5))*1000)
  }
}

# swcalctest <- read.xlsx("swcalc.xlsx", 1, header = FALSE)
# tesvector <- numeric(121)
# perError(swcalctest[1949-1899,2], cSawn(1940))
# for (i in 1900:2021){
#   print(perError(swcalctest[i-1899,2], cSawn(i)))
# }
##g column in swcalc
gSP<-function(y){
  if (y>1889 &&y<1950){
    return(0)
  }
  if(y>1949 && y<1965){
    return((u36(y,5)*InceB5)*1000)
  }
  if (y>1964 && y<1980){
    return((h37(y,5)*InceB5)*1000)
  }
  if (y>1979 && y<2021){
    return(((h37(y,5)*InceB5)+(h38(y,6)*InceC5))*1000)
  }
  if(y>2019 && y<2051){
    return(((inc1(y,1)*InceB5)+(inc1(y,2)*InceC5))*1000)
  }
}
##k column in sw calc
kNSP<-function(y){
  if(y>1889 && y<1927){
    return(0)
  }
  if(y>1926 && y<1935){
    return(((h3t21(y,1)/1000)*InceR5)*1000)
  }
  if(y>1934 && y<1950){
    return((((h3t20(y,3)*InceE5)+(h3t21(y,1)*InceR5))/1000)*1000)
  }
  if(y>1949 && y<1954){
    return(u36(y,6)*InceE5*1000)
  }
  if(y>1953 && y<1963){
    return(((u36(y,6)*InceE5)+(u54(y,2)*InceJ5)+(u53(y,2)*InceO5))*1000) 
  } 
  if(y>1962 && y< 1965){
    return(((u36(y,6)*InceE5)+(u52(y,4)*InceI5)+(u54(y,2)*InceJ5)+(u53(y,2)*InceO5))*1000)
  }
  if(y>1964&& y<2021){
    return(((h37(y,6)*InceE5)+(h53(y,4)*InceI5)+(h56(y,2)*InceJ5)+(h55(y,2)*InceQ5))*1000)
  } 
}






































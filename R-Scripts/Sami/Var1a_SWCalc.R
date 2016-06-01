



## Calculates SW Calc J
Stock_Change<- function(y){
  return(Var1_totalC(y)-Var1_totalC(y-1))
}

## Calculates SW Calc H
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
########################################################
Var1_c_placed_IU <- function(y,eu){
  if (eu == 16){
    #return(0)
    return(qOther(y) * 1)
  }
  return((Var1_eSawn(y) * fsw(y,eu)) +(Var1_iSP(y)*fsp(y,eu)) +(Var1_mNSP(y)*fnsp(y,eu))) 
} 

qOther<-function(y){
  return(0)
}
########################################################
Var1_iSP<-function(y){
  return(fSP(y)+gSP(y)-hSP(y))
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

Var1_mNSP<-function(y){
  return(jNSP(y)+kNSP(y)-lNSP(y))
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

Var1_eSawn<-function(y){
  return(bSawn(y)+cSawn(y)-dSawn(y))
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


Var1_totalC_PAPER <- function(y){
  if (y == 1900){
    return(exp(-log(2)/PRP10)*Calc_D(y))
  }
  else{
    return(exp(-log(2)/PRP10)*(Calc_D(y)+Var1_totalC_PAPER(y-1)))
  }
  
}

Calc_D<-function(y){
  return((PRO18*usa_AB(y)*usa_V(y)))
}

usa_AB<-function(y){
  return(((usa_AM(y)+usa_AN(y)-usa_AO(y))-(usa_AI(y)+usa_AJ(y)-usa_AK(y)))/(usa_AM(y)+usa_AN(y)-usa_AO(y)))
}

usa_AM <- function(y){
  if (y < 1965){
    return(apiTotal(y,1)+apiTotalWP_L(y))
  }
  if (y >1964 && y < 2021){
    return((h49(y,1)+h46(y,5)*(h46(y,1)/h46(y,2)))*1000)
  }
  if (y > 2020){
    return(usa_AM(2002))
  }
}
usa_AN <- function(y){
  if (y < 1965){
    return(apiTotal(y,2)+apiTotalWP_L(y)*(apiTotal(y,2)/apiTotal(y,1)))
  }
  if (y >1964 && y < 2021){
    return((h49(y,2)+h46(y,5)*(h49(y,3)/100))*1000)
  }
  if (y > 2020){
    return(usa_AN(2002))
  }
}
usa_AO <- function(y){
  if (y < 1965){
    return(apiTotal(y,3)+apiTotalWP_L(y)*(apiTotal(y,3)/apiTotal(y,1)))
  }
  if (y > 1964 && y < 2021){
    return((h49(y,4)+h46(y,5)*(h46(y,1)/h46(y,2))*(h49(y,5)/100))*1000)
  }
  if (y > 2020){
    return(usa_AO(2002))
  }
}

usa_AI <- function(y){
  if (y < 1965){
    return(apiTotalWP_L(y))
  }
  if (y > 1964 && y < 2014){
    return(h46(y,5)*(h46(y,1)/h46(y,2))*1000)
  }
  if (y > 2013 && y < 2021){
    return(usa_AI(2007))
  }
  if (y > 2020){
    return(usa_AI(2002))
  }
}
usa_AJ <- function(y){
  if(y < 1965){
    return(apiTotalWP_L(y)*(apiTotal(y,2)/apiTotal(y,1)))
  }
  if (y > 1964 && y < 2014){
    return(h46(y,5)*(h49(y,3)/100)*1000)
  }
  if (y > 2013 && y < 2021){
    return(usa_AJ(2007))
  }
  if (y > 2020){
    return(usa_AJ(2002))
  }
}
usa_AK <- function(y){
  if(y < 1965){
    return(apiTotalWP_L(y)*(apiTotal(y,3)/apiTotal(y,1)))
  }
  if (y > 1964 && y < 2014){
    return(h46(y,5)*(h46(y,1)/h46(y,2))*(h49(y,5)/100)*1000)
  }
  if (y > 2013 && y < 2021){
    return(usa_AK(2007))
  }
  if (y > 2020){
    return(usa_AK(2002))
  }
}

usa_V<-function(y){
  return(usa_S(y)+usa_T(y)-usa_U(y))
}

usa_S <- function(y){
  return(getIncePap(y,1)*1000*InceL5)
}

usa_U <- function(y){
  return(getIncePap(y,3)*1000*InceL5)
}

usa_T<-function(y){
  return(getIncePap(y,2)*1000*InceL5)
}


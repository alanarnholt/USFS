#### Toclaulate inflow of production

##calculates sw calc b
prod_sawn <- function(y){
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
#### Calculates sw calc f
prod_SP <- function(y){
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
#### Calculates sw calc j
prod_NSP <- function(y){
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

df = data.frame(prod_sawn, prod_NSP, prod_SP)

######################################################################
## Claculates inflow exports

##d column in swcalc
Export_Sawn <- function(y){
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

##h column in swcalc
Export_SP <- function(y){
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

##l column in swcalc
Export_NSP <- function(y){
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

#######################################################################
####Calculating inflow for imports
##c column in swcalc
Import_Sawn<-function(y){
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

##g column in swcalc
Import_SP<-function(y){
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
Import_NSP<-function(y){
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


inflow <- c( swpcalcdata[["Sawnwood Prod Special"]])
              #swpcalcdata[["SP Prod Special"]], 
              #swpcalcdata[["NSP Prod Special"]])

#######################################
library(DT)
## Gives the columns for SW calc

minyr <- 1900
maxyr <- 2020
yrs <- minyr:maxyr
swcalcdata <- data.frame(Years = yrs)
yrs <- swcalcdata$Years


swcalcdata$`Structureal panel Consumption`<-sapply(yrs,function(y){
  return(fSP(y)+gSP(y)-hSP(y))
})
swcalcdata$`Structural Panel Production`<- sapply(yrs,function(y){
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
})
##h column in swcalc
swcalcdata$`Structural Panel Exports` <- sapply(yrs,function(y){
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
})
swcalcdata$`Structural Panel Imports`<-sapply(yrs,function(y){
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
}) 


swcalcdata$`Nonstructural Panels consumption`<-sapply(yrs,function(y){
  return(jNSP(y)+kNSP(y)-lNSP(y))
})

swcalcdata$`Nonstructural Panels Production` <- sapply(yrs,function(y){
  if(y < 1950){
    return(((inc1(y,4)*InceE5)+(inc1(y,9)*InceJ5)+(inc1(y,13)*InceO5))*1000)
  }
  if(y < 1965 && y > 1949){
    return(((u36(y,3)*InceE5)+(u52(y,2)*InceI5)+(u54(y,1)*InceJ5)+(u53(y,1)*InceO5))*1000)
  }
  if(y < 2021 && y > 1964){
    return(((h37(y,3)*InceE5)+(h53(y,2)*InceI5)+(h56(y,1)*InceJ5)+(h53(y,3)*InceK5)+h55(y,1)*InceQ5)*1000)
  }
  
})
##l column in swcalc
swcalcdata$`Nonstructural Panels Exports` <- sapply(yrs,function(y){
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
})

swcalcdata$`Nonstructural Panels Imports`<- sapply(yrs,function(y){
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
})

swcalcdata$`Sawn Wood consumption`<-sapply(yrs,function(y){
  return(bSawn(y)+cSawn(y)-dSawn(y))
}) 
swcalcdata$`Sawn Wood Production` <- sapply(yrs,function(y){
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
})
##d column in swcalc
swcalcdata$`Sawn Wood Exports` <- sapply(yrs,function(y){
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
})

swcalcdata$`Sawn Wood Imports`<-sapply(yrs,function(y){
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
})
##########
##Above this works!

#essentially the format of the matrices
# inflow.year<- subset(swcalcdata, Years=="1900")



yearsneeded<-1900:2020


Importdatalooped<-inflow.year[,c(-2,-3,-4, -6, -7, -8, -10,-11,-12)];Importdatalooped

for (i in 2:length(yearsneeded)){
  oneyear<-subset(swcalcdata, Years == yearsneeded[[i]])
  oneyear<-oneyear[,c(-2,-3,-4, -6, -7, -8, -10,-11,-12)]
  Importdatalooped<-rbind(Importdatalooped, oneyear)
}

Importdatalooped<-t(Importdatalooped)
# rownames(Importdatalooped)<-NULL  #if you do not want the rownames listed, use this line
View(Importdatalooped)







Exportdatalooped<-inflow.year[,c(-2,-3,-5,-6,-7,-9,-10,-11,-13)];Exportdatalooped

for (i in 2:length(yearsneeded)){
  oneyear<-subset(swcalcdata, Years == yearsneeded[[i]])
  oneyear<-oneyear[,c(-2,-3,-5,-6,-7,-9,-10,-11,-13)]
  Exportdatalooped<-rbind(Exportdatalooped, oneyear)
}

Exportdatalooped<-t(Exportdatalooped)
# rownames(Exportdatalooped)<-NULL
View(Exportdatalooped)







Productiondatalooped<-inflow.year[,c(-2,-4,-5,-6,-8,-9,-10,-12,-13)];Productiondatalooped

for (i in 2:length(yearsneeded)){
  oneyear<-subset(swcalcdata, Years == yearsneeded[[i]])
  oneyear<-oneyear[,c(-2,-4,-5,-6,-8,-9,-10,-12,-13)]
  Productiondatalooped<-rbind(Productiondatalooped, oneyear)
}

Productiondatalooped<-t(Productiondatalooped)
# rownames(Productiondatalooped)<-NULL
View(Productiondatalooped)




############################################



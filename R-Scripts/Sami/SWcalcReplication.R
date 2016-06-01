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
##########
rndcon <- function(dfs,...){
  #columns to look for in dfs 
  
  #look for columns and save values 
  
  #if not unique, choose one 
  
  #do calculation 
  #from bens code
}
swcarbontotal <- function(Yrs = 1990:2015, distribution = c("exp", "gamma"), THETA, K){
  type <- match.arg(distribution) 
  
  minyr <- 1900
  yrrange <- minyr:2020
  
  Var1_totalC_SW <- data.frame(Years = yrrange)
  skipEU <- c(4,9,13)
  
  for(year in 1900:maxyr){
    yearrange <- 1:(year - minyr + 1)
    for (eu in 1:16) {
      
      if (type == "exp") {
        decays <- exp(-log(2)/halfLives[yearrange,eu]*rev(yearrange))
      }
      
      if (type == "gamma") {
        p<-0 #not exactly sure what this is 
        
        decays <- numeric(length(yearrange))
        for (i in yearrange){
          if (missing(K)){
            K <- findKorTHETAforGamma(halflife = halfLives[i - 1899, eu], theta = THETA)
          }
          
          if(missing(THETA)) {
            THETA <- findKorTHETAforGamma(halflife = halfLives[i - 1899, eu], k = K)
          }
          
          p[i+1]<-p[i]+1 ##this part still confused about, why not just use i instead of setting first val to 0. Signifies no years passed?
          g <- function(x){
            ((x^(THETA - 1)) * (exp(-x/K))) / (gamma(THETA) * (K^THETA))
          }
          decays[i]<- 1 - integrate(g, lower=0, upper=p[i])$value
        }
      }
      
      Var1_totalC_SW[(year-(minyr-1)),paste("EU",eu,sep="")] <- ifelse(eu %in% skipEU, 0, 
                                                                        sum(placeIU[yearrange,(eu+1)]*decays*(1-lossIU[yearrange,eu])))
      
    }
  }
  Var1_totalC_SW[,"LumberPre1900"] <- lumberpre1900[yearrange,]
  Var1_totalC_SW[,"Total Carbon"] <- rowSums(Var1_totalC_SW[,-1])
  return(Var1_totalC_SW)
  
}

placeIU <- data.frame(Years = yrs)
for(i in 1:15){ ##use testthat to check these values with spreadsheet. 
  placeIU[,i+1]  <- swcalcdata[["Sawnwood Prod Special"]] * fracsawnwood[1:121,i] +
  swcalcdata[["SP Prod Special"]] * fracstrpanels[1:121,i] + 
  swcalcdata[["NSP Prod Special"]] * fracnonstrpanels[1:121,i]
}


testdatexp <- swcarbontotal(Yrs = 1900:2020, distribution = "exp")
testdatgamme <- swcarbontotal(Yrs = 1900:2020, distribution = "gamma",
                               THETA = 1)

testdatexp$id <- "Exponential"
testdatgamme$id <- "Gamma with Theta = 1"
df4 <- rbind(testdatexp, testdatgamme)
df4$id <- as.factor(df4$id)
library(ggvis)
df4 %>%
  ggvis(~Years, ~`Total Carbon`, stroke = ~id, strokeWidth := 3) %>%
  layer_lines() %>%
  add_legend('stroke', orient="center", title = "Distributions") %>%
  add_axis("x", format = "####")

Var1_C_SW_STOCKCHANGE <- function(year){
  return((swpcarbontotal(year)$`Total Carbon` - swpcarbontotal(year-1)$`Total Carbon`) * PRO17)
}
######################################

###Var2_C_IU_J calculates total carbon left in year y for eu j in million tonnes of carbon
Var1_C_IU_J <- function(y,eu){
  total <- 0
  minyr <- 1900 
  decays <- exp(-log(2)/halfLives[1:(y-minyr+1),1]*((y-minyr+1):1))
  
  total <- sum(placeIU[1:(y-minyr+1),(eu+1)]*decays*(1-lossIU[1:(y-minyr+1),eu]))
  
  return(total)
}


findKorTHETAforGamma <- function(halflife = 100, theta, k){
  g <- function(x){
    ((x^(theta - 1)) * (exp(-x/k))) / (gamma(theta) * (k^theta))
  }
  
  if(missing(k)){
    k <- 1
    decayval <- 1
    while(abs(decayval - 0.5) > 1e-14){
      l <- decayval / 0.5 
      k <- k * l
      
      decayval<-integrate(g, lower=0, upper=halflife)$value
    }
    return(k)
  }
  if(missing(theta)){
    theta <- 1.2
    decayval <- 1
    while(abs(decayval - 0.5) > 1e-14){
      l <- decayval / 0.5 
      theta <- theta * l
      
      decayval<-integrate(g, lower=0, upper=halflife)$value
    }
    theta
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




























































####################
library(DT)
a5 <- 1##Switch to include exports, for production or use of wood 
#####Var 2PRODUCTION APPROACH, SOLID WOOD PRODUCTS CALCULATIONS

###############
minyr <- 1900
maxyr <- 2020
yrs <- minyr:maxyr
swpcalcdata <- data.frame(Years = yrs)
yrs <- swpcalcdata$Years

swpcalcdata$`Other Products Production` <- sapply(yrs, function(y){
  if (y < 1950){
    return(h3(y,37)*InceN5*1000)
  }
  if (y < 1965){
    return(u4(y,19)*InceN5*1000)
  }
  if (y < 2021){
    return(h5(y,19)*InceN5*1000)
  }
})
pOther <- function(y){#other products exports, swpcalcP
  return(0)
}
swpcalcdata$`Other Products Exports` <- 0
swpcalcdata$`Sawnwood Production` <- sapply(yrs, function(y){
  ###CHECK THIS, why are errors x*10^-8? shouldnt error be 10^16 
  cavg <- (h8(1904,2)-h8(1899,2))/5
  davg <- (h8(1904,3)-h8(1899,3))/5
  if(y < 1904){
    return((((h8(1899,2) +((y-1899)*cavg))* InceF5)+(h8(1899,3)+(y-1899)*davg)* InceG5)*1000)
  }
  if(y < 1950){
    return((h8(y,2)*InceF5+h8(y,3)*InceG5)*1000)
  }
  if(y < 1965){
    return((u29(y,1)*1000*(((u29(y,2)/u29(y,1))*InceF5)+((u29(y,3)/u29(y,1))*InceG5)))*1000)
  }
  if(y< 2021){
    return((h28(y,1)*1000*(((h28(y,2)/h28(y,1))*InceF5)+((h28(y,3)/h28(y,1))*InceG5)))*1000)
  }
})
swpcalcdata$`Sawnwood Exports` <- sapply(yrs, function(y){
  if(y < 1911){
    return(h8(y,13) * InceF5 * 1000)
  }
  if(y < 1950){
    return((((h8(y,14) + h8(y,16))*InceF5) + (h8(y,15)*InceG5))*1000)
  }
  if(y < 1965){
    return((((u29(y,8)*1000*InceF5)+(u29(y,9)*1000*InceG5)) * 1000))
  }
  if(y< 2021){
    return( ((h28(y,8)*1000*InceF5)+(h28(y,9)*1000*InceG5))*1000)
  }
}) 
swpcalcdata$`Roundwood consumed for lumber and panels` <- sapply(yrs, function(y){
  if(y < 1950){
    return((h3(y,28)+h3(y,31))*(InceV5*0.8+InceW5*0.2)*1000)
  } #sawlog domestic prod + veneer logs domestic production 
 
  if(y < 1965){
    #u5$j, u5$o...u6$j, u6$o
    return(1000*((u5(y,7)+u5(y,11))*InceV5+(u6(y,8)+u6(y,12))*InceW5))
  } #J,O.. Lumber Production + plywood/veneer production, for HW and SW 
  if (y < 2021){
    return(1000*((h6(y,7)+h6(y,11))*InceV5+(h7(y,7)+h7(y,11))*InceW5))
  }
})
##if unique values in dfs 
rndcon <- function(dfs,...){
  #columns to look for in dfs 
  
  #look for columns and save values 
  
  #if not unique, choose one 
  
  #do calculation 
}

swpcalcdata$`Log Exports` <- sapply(yrs, function(y){
  if(y < 1965){
    return((h3(y,8)*InceV5+h3(y,10)*InceW5)*1000)
  }
  if(y < 2021){
    return((h6(y,21)*InceV5+h7(y,21)*InceW5)*1000)
  }
})
swpcalcdata$`Imported logs for lumber and panels` <- sapply(yrs, function(y){
  if(y<1950){
    return(0)
  }
  if (y < 1965){
    #u5$Y, u6$z
    return(1000*(u5(y,20)*InceV5+u6(y,21)*InceW5))
  }
  if(y < 2021){
    return(1000*(h6(y,20)*InceV5+h7(y,20)*InceW5))
  }
})
swpcalcdata$SP.Production <- sapply(yrs, function(y){
  if(y < 1950){
    #softwood plywood million ft^2 
    return(((inc1(y,1)*InceB5))*1000)
  }
  if(y < 1965){
    
    return(((u36(y,2)*InceB5))*1000)
  }
  if(y<1980){
    return(((h37(y,2)*InceB5))*1000)
  }
  if(y < 2021){
    return(((h37(y,2)*InceB5)+(h38(y,3)*InceC5))*1000)
  }
})
swpcalcdata$SP.Exports <- sapply(yrs, function(y){
  if(y < 1927){
    return(0)
  }
  if(y < 1950){
    return(((h3t20(y,7)/1000*InceB5))*1000)
  }
  if(y < 1965){
    return(((u36(y,8)*InceB5))*1000)
  }
  if(y < 1991){
    return(((h37(y,8)*InceB5))*1000)
  }
  if(y < 2021){
    return(((h37(y,8)*InceB5)+(h38(y,9)*InceC5))*1000)
  }
})
swpcalcdata$NSP.Production <- sapply(yrs, function(y){
  if(y < 1950){
    return(((inc1(y,4)*InceE5)+(inc1(y,9)*InceJ5)+(inc1(y,13)*InceO5))*1000)
  }
  if(y < 1965){
    return(((u36(y,3)*InceE5)+(u52(y,2)*InceI5)+(u54(y,1)*InceJ5)+(u53(y,1)*InceO5))*1000)
  }
  if(y < 2021){
    return(((h37(y,3)*InceE5)+(h53(y,2)*InceI5)+(h56(y,1)*InceJ5)+(h53(y,3)*InceK5)+h55(y,1)*InceQ5)*1000)
  }
})
swpcalcdata$NSP.Exports <- sapply(yrs, function(y){##lNSP
  if (y<1916){
    return(0)
  }
  if(y < 1925){
    return((u54(y,3)*InceJ5)*1000)
  }
  if(y < 1927){
    return(((u54(y,3)*InceJ5)+(u53(y,3)*InceO5))*1000)
  }
  if(y < 1935){
    return((((h3t20(y,8)*InceE5+h3t21(y,4)*InceR5)/1000)+(u54(y,3)*InceJ5)+(u53(y,3)*InceO5))*1000)
  }
  if(y < 1950){
    return((((h3t20(y,8)+h3t21(y,4))/1000*InceE5)+(u54(y,3)*InceJ5)+(u53(y,3)*InceO5))*1000)
  }
  if(y < 1965){
    return(((u36(y,9)*InceE5)+(u54(y,3)*InceJ5)+(u53(y,3)*InceO5))*1000)
  }
  if(y < 2021){
    return(((h37(y,9)*InceE5)+(h53(y,5)*InceI5)+(h56(y,3)*InceJ5)+h55(y,3)*InceQ5)*1000)
  }
})
######################
swpcalcdata$`Other Products Production Special` <- swpcalcdata$`Other Products Production` - (1-a5)*swpcalcdata$`Other Products Exports`
swpcalcdata$`Sawnwood Prod Special` <- (swpcalcdata$`Sawnwood Production` - (1-a5) * swpcalcdata$`Sawnwood Exports`) * ((swpcalcdata$`Roundwood consumed for lumber and panels`+swpcalcdata$`Log Exports`*a5-swpcalcdata$`Imported logs for lumber and panels`*PRP62)/swpcalcdata$`Roundwood consumed for lumber and panels`)
swpcalcdata$`SP Prod Special` <- (swpcalcdata$SP.Production-(1-a5)*swpcalcdata$SP.Exports)*((swpcalcdata$`Roundwood consumed for lumber and panels`+a5*swpcalcdata$`Log Exports`-swpcalcdata$`Imported logs for lumber and panels`*PRP62)/swpcalcdata$`Roundwood consumed for lumber and panels`)
swpcalcdata$`NSP Prod Special` <- (swpcalcdata$NSP.Production-(1-a5)*swpcalcdata$NSP.Exports)*((swpcalcdata$`Roundwood consumed for lumber and panels`+a5*swpcalcdata$`Log Exports`-swpcalcdata$`Imported logs for lumber and panels`*PRP62)/swpcalcdata$`Roundwood consumed for lumber and panels`)   


placeIU <- data.frame(Years = yrs)
for(i in 1:15){ ##use testthat to check these values with spreadsheet. 
  placeIU[,i+1]  <- swpcalcdata[["Sawnwood Prod Special"]] * fracsawnwood[1:121,i] + swpcalcdata[["SP Prod Special"]] * fracstrpanels[1:121,i] + swpcalcdata[["NSP Prod Special"]] * fracnonstrpanels[1:121,i]
}
placeIU$V17 <- swpcalcdata$`Other Products Production Special`
write.csv2(swpcalcdata, "swpcalcdata.csv")

swpcarbontotal <- function(Yrs = 1990:2015, distribution = c("exp", "gamma"), THETA, K){
  type <- match.arg(distribution) 
  
  minyr <- 1900
  yrrange <- minyr:2020
  
  Var2_totalC_SWP <- data.frame(Years = yrrange)
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
      
      Var2_totalC_SWP[(year-(minyr-1)),paste("EU",eu,sep="")] <- ifelse(eu %in% skipEU, 0, 
                                                                        sum(placeIU[yearrange,(eu+1)]*decays*(1-lossIU[yearrange,eu])))
      
    }
  }
  Var2_totalC_SWP[,"LumberPre1900"] <- lumberpre1900[yearrange,]
  Var2_totalC_SWP[,"Total Carbon"] <- rowSums(Var2_totalC_SWP[,-1])
  return(Var2_totalC_SWP)
  
}

testdatexp <- swpcarbontotal(Yrs = 1900:2020, distribution = "exp")
testdatgamme <- swpcarbontotal(Yrs = 1900:2020, distribution = "gamma",
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

Var2_C_SWP_STOCKCHANGE <- function(year){
  return((swpcarbontotal(year)$`Total Carbon` - swpcarbontotal(year-1)$`Total Carbon`) * PRO17)
}
######################################

###Var2_C_IU_J calculates total carbon left in year y for eu j in million tonnes of carbon
Var2_C_IU_J <- function(y,eu){
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
##################table for total carbon values
checkswp <- read.csv("./Data/checkswp.csv", header=FALSE)

###CHECK FOR PLACE IU 
checkplacediu <- read.csv("./Data/checkplacedIU.csv", header=TRUE)
drops <- c("Years", "V5","V10", "V14")
placeiu2 <- placeIU[,!(names(placeIU) %in% drops)]
Errordf <- data.frame(Years = yrs)
for(i in 1:13){
  for(j in 1:121){
    Errordf[j,paste(i,"h",sep="")] <- checkplacediu[j,i]- placeiu2[j,i]
  }
}
lapply(Errordf, max )
identical(swpcalcdata$`Sawnwood Production`, 
          checkswp[1:121,2])
swpcalcdata$`Sawnwood Production` - checkswp[1:121,2]



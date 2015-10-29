##WOODCARB Excel caluclations
# b n
getwd()
library(gdata)
help(WOODCARB.xls)

library(xlsx)

getwd()
setwd("/Users/benjones/Documents/woodcarb")

readin <- function(){
#######DATA FILES
LTC <- function(s){
  
  switch(s, "b"=1,"c"=2,"d"=3,"e"=4,
         "f"=5,"g"=6,"h"=7,"i"=8,"j"=9,
         "k"=10,"l"=11,"m"=12,"n"=13,"o"=14,
         "p"=15,"q"=16,"r"=17,"s"=18,"t"=19,
         "u"=20,"v"=21,"w"=22,"x"=23,"y"=24,"z"=25,
         "aa"=26,"ab"=27,"ac"=28,"ad"=29,"ae"=30,
         "af"=31)
}
###HAIR 1958 TAB 14 TIMBER DATA
# FROM 1899-1956
hair1958 = read.xlsx("hair1958.xlsx",1,
                        header = F,
                     )
rownames(hair1958) <- 1899:1956
h8 <- function(y,c){
  r <- (y-1898)
  return(hair1958[r,c])
}
#######################
#####HAIR 1963 TABLE 2 TIMBER PRODUCT DATA
hair1963 <- read.xlsx("hair1963tab2.xlsx", 1, header=F,
                      rowIndex = 6:90, colIndex = 2:39)
rownames(hair1963) <- 1900:1984
h3 <- function(y,c){
  return(hair1963[y-1899,c])
}
h3(1950,2)
#####HAIR1963 table 20 US import/export plywood 1927-1962
hair1963t20 <- read.xlsx("hair1963t20.xlsx",1,header=F)
rownames(hair1963t20) <- 1927:1962
h3t20 <- function(y,c){
  return(hair1963t20[y-1926,c])
}
#########HAIR1963 TABLE21
hair1963t21 <- read.xlsx("hair1963t21.xlsx",1,header=F)
rownames(hair1963t21) <- 1927:1962
h3t21 <- function(y,c){
  return(hair1963t21[y-1926,c])
}
#######################
####### ULRICH TABLE 5 softwood timber products, by major product, 1950-87
ulrich5 <- read.xlsx("ulrich5.xlsx", 1, header = F)
rownames(ulrich5) <- 1950:1987
u5 <- function(y,c){
  return(ulrich5[y-1949,c])
}
########################
###### ULRICH TABLE 6
ulrich6 <- read.xlsx("UlrichTable6.xlsx", 1, header=F,
                     rowIndex = 11:48, colIndex = 2:29)
rownames(ulrich6) <- 1950:1987
u6 <- function(y,c){
  return(ulrich6[y-1949,c])
}
###################
#####ULRRICH TABLE 29 lumber production 1950-1987
ulrich29 <- read.xlsx("ulrich29.xlsx",1,header=F)
rownames(ulrich29) <- 1950:1987
u29 <- function(y,c){
  return(ulrich29[y-1949,c])
}
#####ulrich 36 plywood production 1950-1987
ulrich36 <- read.xlsx("ulrich36.xlsx",1,header=F)
rownames(ulrich36) <- 1950:1987
u36 <- function(y,c){
  return(ulrich36[y-1949,c])
}
####ulrich 52 1950-1987
ulrich52 <- read.xlsx("ulrich52.xlsx",1,header=F)
rownames(ulrich52) <- 1950:1987
u52 <- function(y,c){
  return(ulrich52[y-1949,c])
}
#####ulrich 53 1950-1987
ulrich53 <- read.xlsx("ulrich53.xlsx",1,header=F)
rownames(ulrich53) <- 1925:1987
u53 <- function(y,c){
  return(ulrich53[y-1924,c])
}
#####ulrich 54 1950-1987
ulrich54 <- read.xlsx("ulrich54.xlsx",1,header=F)
rownames(ulrich54) <- 1916:1987
u54 <- function(y,c){
  return(ulrich54[y-1915,c])
}
#########################
####HOWARD 6A SW TIMBER 1965-2020
howard6 <- read.xlsx("howard6a.xlsx", 1, header=F,
                     rowIndex = 10:65, colIndex = 2:25)
rownames(howard6) <- 1965:2020
h6 <- function(y,c){
  return(howard6[(y-1964),c])
}
##########################
##### HOWARD 7A hw TIMBER 1965-2020
howard7 <- read.xlsx("howard7a.xlsx", 1, header=F,
                     rowIndex = 10:65, colIndex = 2:25)
rownames(howard7) <- 1965:2020
h7 <- function(y,c){
  return(howard7[y-1964,c])
}
########################
######HOWARD 28 lumber production 1965-2020
howard28 <- read.xlsx("howard28.xlsx",1,header=F)
rownames(howard28) <- 1965:2020
h28 <- function(y,c){
  return(howard28[y-1964,c])
}
#######HOWARD 37 plywood production 1965-2020
howard37 <- read.xlsx("howard37.xlsx",1,header=F)
rownames(howard37) <- 1965:2020
h37 <- function(y,c){
  return(howard37[y-1964,c])
}
#####HOWARD 38 production structural panels 1980-2020
howard38 <- read.xlsx("howard38.xlsx",1,header=F)
rownames(howard38) <- 1980:2020
h38 <- function(y,c){
  return(howard38[y-1979,c])
}
####HOWARD 53
howard53 <- read.xlsx("howard53.xlsx",1,header=F)
rownames(howard53) <- 1965:2020
h53 <- function(y,c){
  return(howard53[y-1964,c])
}
#####HOWARD 55
howard55 <- read.xlsx("howard55.xlsx",1,header=F)
rownames(howard55) <- 1965:2020
h55 <- function(y,c){
  return(howard55[y-1964,c])
}
######HOWARD 56
howard56 <- read.xlsx("howard56.xlsx",1,header=F)
rownames(howard55) <- 1965:2020
h56 <- function(y,c){
  return(howard56[y-1964,c])
}
####INce table1
ince1 <- read.xlsx("ince1.xlsx",1,header=F)
rownames(ince1) <- 1900:2050
inc1 <- function(y,c){
  return(ince1[y-1899,c])
}
######fraction of sawnwood used in various end uses 1900-2050
fracsawnwood <- read.xlsx("fracsawnwood.xlsx", 1, header = F,
                          colIndex = 2:19)
rownames(fracsawnwood) <- 1900:2050
fsw <- function(y,c){
  return(fracsawnwood[y-1899,c])
}
#######################
######fraction of structural panel used in various end uses 1900-2050
fracstrpanels <- read.xlsx("fracstrpanels.xlsx",
                           1,
                           header = F, colIndex = 2:20)
rownames(fracstrpanels) <- 1900:2050
fsp <- function(y,c){
  return(fracstrpanels[y-1899,c])
}
######fraction of nonstructural panel used in various end uses 1900-2050
fracnonstrpanels <- read.xlsx("fracnonstrpanels.xlsx", 1,
                              header = F, colIndex = 2:20)
rownames(fracnonstrpanels) <- 1900:2050
fnsp <- function(y,c){
  return(fracnonstrpanels[y-1899,c])
}
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
}
##########
swPcorrect <- read.xlsx("swPcorrect.xlsx",1,header=F)
rownames <- 1900:2020
getSWP <- function(y,c){
  return(swPcorrect[y-1899,c])
}
perDiff <- function(x,y){
  if(((x+y)/2) == 0){
    return(0)
  }
  return(abs(x-y)/((x+y)/2))
}
##for(i in 1900:2020){###checks bsawn
  print(perDiff(bSawn(i),getSWP(i,1)))
}####checks dSawn
##for(i in 1900:2020){
  print(perDiff(dSawn(i),getSWP(i,3)))
}###checks dSAwn
##for(i in 1900:2020){
  print(perDiff(sSawn(i),getSWP(i,18)))
}###checks sSAwn
##for(i in 1900:2020){
  print(perDiff(uSawn(i),getSWP(i,20)))
}####checks uSawn
##for(i in 1900:2020){
  print(perDiff(rSawn(i),getSWP(i,17)))
}####broken checks rSawn
loadSwCalcVars <- function(){
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
  sSawn <- function(y){
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
  uSawn <- function(y){
    if(y < 1965){
      return((h3(y,8)*InceV5+h3(y,10)*InceW5)*1000)
    }
    if(y<2021 && y > 1964){
      return( (h6(y,21)*InceV5+h7(y,21)*InceW5)*1000)
    }
  }
  rSawn <- function(y){
    if(y<1950){
      return(0)
    }
    if (y < 1965&&y>1949){
      return(1000*(u5(y,20)*InceV5+u6(y,25)*InceW5))
    }
    if(y > 1964 && y < 2021){
      return(1000*(h6(y,20)*InceV5+h7(y,25)*InceW5))
    }
  }
  
}

efinProdSawn <- function(year) {
efinaly <- (bSawn(year) - (1-a5) * dSawn(year)) * ((sSawn(year) + uSawn(year) * a5 - rSawn(year) * PRP62 )/ sSawn(year))
return(efinaly)                                                                       
}
########
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
sSP <- sSawn
uSP <- uSawn
rSP <- rSawn
ifinProdSP <- function(y){
  ifin <- (fSP(y)-(1-a5)*hSP(y))*((sSP(y)+uSP(y)*a5-rSP(y)*PRP62)/sSP(y))
  return(ifin)
}
########
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
  if (y<1915){
    return(0)
  }
  if(y < 1925 && y > 1915){
    return((u53(y,3)*InceJ5)*1000)
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
sNSP <- sSawn
uNSP <- uSawn
rNSP <- rSawn
mfinProdNSP <- function(y){
  mfin <- (jNSP(y)-(1-a5)*lNSP(y))*((sNSP(y)+uNSP(y)*a5-rNSP(y)*PRP62)/sNSP(y))
}
##for(i in 1900:2020){
  print(((efinProdSawn(i) - finCorr(i,1))/((efinProdSawn(i) + finCorr(i,1))/2))*100)
###efin breaks at 1950-2020
}
finProdCorrect <- read.xlsx("prodFin.xlsx",1,header=F)
rownames(finProdCorrect) <- 1900:2020
finCorr <- function(y,c){
  return(finProdCorrect[y-1899,c])
}
##for(i in 1900:2020){
  print(((ifinProdSP(i) - finCorr(i,2))/((ifinProdSP(i) + finCorr(i,2))/2))*100)
  ##breaks at 1950-2020
}
 
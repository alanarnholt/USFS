library(xlsx)
setwd("/Users/benjones/Documents/woodcarb")
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
#######ULRICH TABLE 4 - PRODUCTION, IMPPORTS/EXPORTS AND CONSUMPTION OF TIMBER BY MAJOR PRODUCT
ulrich4 <- read.xlsx("ulrich4.xlsx",1,header=F)
rownames(ulrich4) <- 1950:1987
u4 <- function(y,c){
  return(ulrich4[y-1949,c])
}
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
#####
###HOWARD 5A
howard5 <- read.xlsx("howard5.xlsx", 1, header=F)
rownames(howard5) <- 1965:2013
h5 <- function(y,c){
  return(howard5[y-1964,c])
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
fracsawnwood <- read.xlsx("fracsawnwood.xlsx", 1, header = F)
rownames(fracsawnwood) <- 1900:2050
fsw <- function(y,c){
  return(round(fracsawnwood[y-1899,c],3))
}
#######################
######fraction of structural panel used in various end uses 1900-2050
fracstrpanels <- read.xlsx("fracstrpanels.xlsx",1, header = F)
rownames(fracstrpanels) <- 1900:2050
fsp <- function(y,c){
  return(round(fracstrpanels[y-1899,c],3))
}
######fraction of nonstructural panel used in various end uses 1900-2050
fracnonstrpanels <- read.xlsx("fracnonstrpanels.xlsx", 1,
                              header = F, colIndex = 2:20)
rownames(fracnonstrpanels) <- 1900:2050
fnsp <- function(y,c){
  return(round(fracnonstrpanels[y-1899,c],3))
}
########halflives for various end uses for yrs 1900-2050
halfLives <- read.xlsx("halfLives.xlsx", 1,header=F)
rownames(halfLives) <- 1900:2050
HL <- function(y,eu){
  return(round(halfLives[y-1899,eu],1))
}
######LOSS when placed IU based on EU and yr
lossIU <- read.xlsx("lossWhenPlacedIU.xlsx",1,header=F)
rownames(lossIU) <- 1900:2050
iuLoss <- function(y,eu){
  return(round(lossIU[y-1899,eu],1))
}
##########
####single family carbon placed in use
sFcarbon <- read.xlsx("sfHomeCheck.xlsx",1,header=F)
rownames(sFcarbon) <- 1900:2050
sfcarb <- function(y,c){
  return(sFcarbon[y-1899,c])
}

library(xlsx)
library(rJava)
#######DATA FILES
###HAIR 1958 TAB 14 TIMBER DATA
# FROM 1899-1956
hair1958 <- read.xlsx("./Data/hair1958.xlsx",1,header = FALSE)
rownames(hair1958) <- 1899:1956
h8 <- function(y,c){
  return(hair1958[y-1898,c])
}
#######################
#####HAIR 1963 TABLE 2 TIMBER PRODUCT DATA
##this table includes numbers results of calcs, needs fix
hair1963 <- read.xlsx("./Data/hair1963tab2.xlsx", 1, header=FALSE)
rownames(hair1963) <- 1900:1984
h3 <- function(y,c){
    return(hair1963[y-1899,c])
}
#####HAIR1963 table 20 US import/export plywood 1927-1962
hair1963t20 <- read.xlsx("./Data/hair1963t20.xlsx",1,header=FALSE)
rownames(hair1963t20) <- 1927:1962
h3t20 <- function(y,c){
  return(hair1963t20[y-1926,c])
}
#########HAIR1963 TABLE21
hair1963t21 <- read.xlsx("./Data/hair1963t21.xlsx",1,header=FALSE)
rownames(hair1963t21) <- 1927:1962
h3t21 <- function(y,c){
  return(hair1963t21[y-1926,c])
}
#######################
#######ULRICH TABLE 4 - PRODUCTION, IMPPORTS/EXPORTS AND CONSUMPTION OF TIMBER BY MAJOR PRODUCT
ulrich4 <- read.xlsx("./Data/ulrich4.xlsx",1,header=FALSE)
rownames(ulrich4) <- 1950:1987
u4 <- function(y,c){

    return(ulrich4[y-1949,c])
 
}
####### ULRICH TABLE 5 softwood timber products, by major product, 1950-87
ulrich5 <- read.xlsx("./Data/ulrich5.xlsx", 1, header = FALSE)
rownames(ulrich5) <- 1950:1987
u5 <- function(y,c){

    return(ulrich5[y-1949,c])
  
}
########################
###### ULRICH TABLE 6
ulrich6 <- read.xlsx("./Data/UlrichTable6.xlsx", 1, header=FALSE)
rownames(ulrich6) <- 1950:1987
u6 <- function(y,c){
  
    return(ulrich6[y-1949,c])
  
}
###################
#####ULRRICH TABLE 29 lumber production 1950-1987
ulrich29 <- read.xlsx("./Data/ulrich29.xlsx",1,header=FALSE)
rownames(ulrich29) <- 1950:1987
u29 <- function(y,c){
  return(ulrich29[y-1949,c])
}
#####ulrich 36 plywood production 1950-1987
ulrich36 <- read.xlsx("./Data/ulrich36.xlsx",1,header=FALSE)
rownames(ulrich36) <- 1950:1987
u36 <- function(y,c){
  return(ulrich36[y-1949,c])
}
####ulrich 52 1950-1987
ulrich52 <- read.xlsx("./Data/ulrich52.xlsx",1,header=FALSE)
rownames(ulrich52) <- 1950:1987
u52 <- function(y,c){
  return(ulrich52[y-1949,c])
}
#####ulrich 53 1950-1987
ulrich53 <- read.xlsx("./Data/ulrich53.xlsx",1,header=FALSE)
rownames(ulrich53) <- 1925:1987
u53 <- function(y,c){
  return(ulrich53[y-1924,c])
}
#####ulrich 54 1950-1987
ulrich54 <- read.xlsx("./Data/ulrich54.xlsx",1,header=FALSE)
rownames(ulrich54) <- 1916:1987
u54 <- function(y,c){
  return(ulrich54[y-1915,c])
}
#####
###HOWARD 5A
howard5 <- read.xlsx("./Data/howard5.xlsx", 1, header=FALSE)
rownames(howard5) <- 1965:2020
h5 <- function(y,c){
  return(howard5[y-1964,c])
}
#########################
####HOWARD 6A SW TIMBER 1965-2020
howard6 <- read.xlsx("./Data/howard6a.xlsx", 1, header=FALSE)
rownames(howard6) <- 1965:2020
h6 <- function(y,c){
  return(howard6[(y-1964),c])
}
##########################
##### HOWARD 7A hw TIMBER 1965-2020
howard7 <- read.xlsx("./Data/howard7a.xlsx", 1, header=FALSE)
rownames(howard7) <- 1965:2020
h7 <- function(y,c){
  return(howard7[y-1964,c])
}
########################
######HOWARD 28 lumber production 1965-2020
howard28 <- read.xlsx("./Data/howard28.xlsx",1,header=FALSE)
rownames(howard28) <- 1965:2020
h28 <- function(y,c){
  
    return(howard28[y-1964,c])
  
}
#######HOWARD 37 plywood production 1965-2020
howard37 <- read.xlsx("./Data/howard37.xlsx",1,header=FALSE)
rownames(howard37) <- 1965:2020
h37 <- function(y,c){
      return(howard37[y-1964,c])
}
#####HOWARD 38 production structural panels 1980-2020
howard38 <- read.xlsx("./Data/howard38.xlsx",1,header=FALSE)
rownames(howard38) <- 1980:2020
h38 <- function(y,c){
    return(howard38[y-1979,c]) 
}
#########
####HOWARD 46
howard46 <- read.xlsx("./Data/howard46.xlsx",1,header=FALSE)
rownames(howard46) <- 1965:2020
h46 <- function(y,c){
  return(howard46[y-1964,c])
}
######
###Howard 47
howard47 <- read.xlsx("./Data/howard47.xlsx",1,header=FALSE)
rownames(howard47) <- 1965:2020
h47 <- function(y,c){
  return(howard47[y-1964,c])
}
##HOWard 49
howard49 <- read.xlsx("./Data/howard49.xlsx",1,header=FALSE)
rownames(howard49) <- 1965:2020
h49 <- function(y,c){
  return(howard49[y-1964,c])
}
####HOWARD 53
howard53 <- read.xlsx("./Data/howard53.xlsx",1,header=FALSE)
rownames(howard53) <- 1965:2020
h53 <- function(y,c){
    return(howard53[y-1964,c])
}
#####HOWARD 55
howard55 <- read.xlsx("./Data/howard55.xlsx",1,header=FALSE)
rownames(howard55) <- 1965:2020
h55 <- function(y,c){
    return(howard55[y-1964,c])
}
######HOWARD 56
howard56 <- read.xlsx("./Data/howard56.xlsx",1,header=FALSE)
rownames(howard55) <- 1965:2020
h56 <- function(y,c){
  return(howard56[y-1964,c])
}
####INce table1
ince1 <- read.xlsx("./Data/ince1.xlsx",1,header=FALSE)
rownames(ince1) <- 1900:2050
inc1 <- function(y,c){
    return(ince1[y-1899,c])
}
######fraction of sawnwood used in various end uses 1900-2050
fracsawnwood <- read.xlsx("./Data/fracsawnwood.xlsx", 1, header = FALSE)
rownames(fracsawnwood) <- 1900:2050
fsw <- function(y,c){
  return(fracsawnwood[y-1899,c])
}
#######################
######fraction of structural panel used in various end uses 1900-2050
fracstrpanels <- read.xlsx("./Data/fracstrpanels.xlsx",1, header = FALSE)
rownames(fracstrpanels) <- 1900:2050
fsp <- function(y,c){
  return(fracstrpanels[y-1899,c])
}
######fraction of nonstructural panel used in various end uses 1900-2050
fracnonstrpanels <- read.xlsx("./Data/fracnonstrpanels.xlsx", 1,
                              header = FALSE)
rownames(fracnonstrpanels) <- 1900:2050
fnsp <- function(y,c){
  return(fracnonstrpanels[y-1899,c])
}
########halflives for various end uses for yrs 1900-2050
halfLives <- read.xlsx("./Data/halfLives.xlsx", 1,header=FALSE)
rownames(halfLives) <- 1900:2050
HL <- function(y,eu){
  return(halfLives[y-1899,eu])
}
######LOSS when placed IU based on EU and yr
lossIU <- read.xlsx("./Data/lossWhenPlacedIU.xlsx",1,header=FALSE)
rownames(lossIU) <- 1900:2050
iuLoss <- function(y,eu){
  return(lossIU[y-1899,eu])
}
##########

##########LUMBER WOOD IN SF HOUSES 1800-1900
lumberpre1900 <- read.xlsx("./Data/lumberpre1900.xlsx",1,header=FALSE)
rownames(lumberpre1900) <- 1900:2050
pre1900 <- function(y){
  return(lumberpre1900[y-1899,1])
}
##################
########PAPER
IncePaper <- read.xlsx("./Data/Ince_Paper.xlsx",1,header=FALSE)
rownames(IncePaper) <- 1899:2050
getIncePap <- function(y,c){
  return(IncePaper[y-1898,c])
}
apiFiberpulp <- read.xlsx("./Data/api1975Fiberpulp.xlsx",1,header=FALSE)
rownames(apiFiberpulp) <- 1900:1974
apiFiber <- function(y,c){
  return(apiFiberpulp[y-1899,c])
}

apiTotalWoodPulp <- read.xlsx("./Data/apiTotalWoodPulp.xlsx",1,header=FALSE)
rownames(apiTotalWoodPulp) <- c(1869,1879,1889,1899:1972)
apiTotal <- function(y,c){
  return(apiTotalWoodPulp[y-1894,c])
}
usaFiberPulp <- read.xlsx("./Data/usaFiberPulpCG.xlsx",1,header=FALSE)
rownames(usaFiberPulp) <- 1998:2013
#####
# usaCheck <- read.xlsx("usaCheck.xlsx",1,header=F)
# usCheck <- function(y,c){
#   return(usaCheck[y-1899,c])
# } 
# CalcBUcheck <- read.xlsx("CalcBUcheck.xlsx",1,header=F)
# BUcheck <- function(y,c){
#   return(CalcBUcheck[y-1899,1])
# }
woodToLandFills <- read.xlsx("./Data/woodToLandFills.xlsx", 1, header=F)
rownames(woodToLandFills) <- 1900:2050
woodToLF <- function(y){
  return(woodToLandFills[y-1899,1])
}
########
woodToDumps <- read.xlsx("./Data/woodToDumps.xlsx", 1, header=F)
rownames(woodToDumps) <- 1900:2050
woodDumps <- function(y){
  return(woodToDumps[y-1899,1])
}
paperToLandFills <- read.xlsx("./Data/paperToLandFIlls.xlsx",1,header=F)
rownames(paperToLandFills) <- 1900:2050
paperToLF <- function(y){
  return(paperToLandFills[y-1899,1])
}
########
imports1file<-read.xlsx("./Data/imports1.xlsx",1,header=FALSE)
rownames(imports1file)<-1965:2050
imports1<-function(y,c){
  return(imports1[y-1964,c])
}
#########
recFibPulpUSA <- read.xlsx("./Data/recFibPulpusa.xlsx", 1, header=TRUE)








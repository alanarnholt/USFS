library(xlsx)

#from 1900-2050
#
swcalc=read.xlsx("swcalc.xlsx",1,header=F)
rownames(swcalc)<-1900:2050
sw<-function(y,c){
  return(swcalc[y-1899,c])#need to fix
}

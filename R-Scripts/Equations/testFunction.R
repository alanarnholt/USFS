#test function for reproduced columns in woodcarb, not working yet
testWoodcarb <- read.xlsx("testWoodcarb.xlsx",1,header=TRUE)
testWood <- function(y,c){
  return(testWoodcarb[y-1899,c])
  
}
colnames(testWoodcarb) ##gives all functions that can be tested. 
testfunc <- function(rnge, usfsfunc)
{
  testvector <- numeric(length(rnge)-1)
  minyr <- min(rnge)
  maxyr <- max(rnge)
  funcstring <- deparse(substitute(usfsfunc))
  for (i in minyr:maxyr){
    testvector[i - (minyr-1)] <- perError(testWood(i, funcstring), usfsfunc(i))
  }
  return(testvector)
}
  
tst<-function(x,y,func){
  for(i in x:y){
    
    print(paste("Hello", deparse(substitute(func)), sep=" "))
  }
  # mean(c(x,y))
}

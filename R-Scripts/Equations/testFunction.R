#test function for reproduced columns in woodcarb, not working yet
testWoodcarb <- read.xlsx("testWoodcarb.xlsx",1,header=TRUE)
testWood <- function(y,c){
  return(testWoodcarb[y-1899,c])
  
}
testfunc <- function(usfsfunc, years = NULL)
{
  correctCol <- testWood(years,deparse(substitute(x)))
  return(perError(correctCol, usfsfunc(years)))
}
  
testfunc(usa_S,1905:1910)

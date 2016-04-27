par(mfrow=(c(1,2)))
times <- numeric(91)
years <- 1900:1990
for(i in 1900:1990)
  times[i-1899] <- max(system.time(testvar2C(i), gcFirst = TRUE))

df <- data.frame(years,times)

ggplot(aes(x = years, y = times), data = df) + 
  geom_line() + 
  ggtitle("Time used to calculate total carbon with for loops")+ 
  xlab("Years")+ 
  ylab("Times (s)")

test3 <- function(k){
  
  totalcarbon <- 0
  for (i in 1:16){
    if (i == 4 || i == 9 || i == 13){
      totalcarbon <- totalcarbon
    }
    else{
      totalcarbon <- totalcarbon + Var2_C_IU_J(k,i)
    }
  }
  ##pre1900() is result of calculation from linked site
  return(totalcarbon + pre1900(k))
  
}
timesjustyr <- numeric(91)
for(k in 1900:2020)
  timesjustyr[k-1899] <- max(system.time(test3(k),gcFirst = TRUE))
df <- data.frame(years, timesjustyr)
ggplot(aes(x = years, y = timesjustyr), data = df) + 
  geom_line() + 
  ggtitle("Time used to calculate total carbon with for loops(just yr)")+ 
  xlab("Years")+ 
  ylab("Times (s)")

testvar2C <- function(y){
  for(k in 1900:y){
    totalcarbon <- 0
    for (i in 1:16){
      if (i == 4 || i == 9 || i == 13){
        totalcarbon <- totalcarbon
      }
      else{
        totalcarbon <- totalcarbon + Var2_C_IU_J(k,i)
      }
    }
    ##pre1900() is result of calculation from linked site
    Var2_totalC_SWPtable[k-1899] <- totalcarbon + pre1900(k)
  }
}
test5 <- function(){
  Var2_totalC_SWPtable <- numeric(121)
  for(y in 1900:2020){
    totalcarbon <- 0
    for (i in 1:16){
      if (i == 4 || i == 9 || i == 13){
        totalcarbon <- totalcarbon
      }
      else{
        totalcarbon <- totalcarbon + Var2_C_IU_J(y,i)
      }
    }
    ##pre1900() is result of calculation from linked site
    Var2_totalC_SWPtable[y-1899] <- totalcarbon + pre1900(y)
  }###}
library(WOODCARB3R)
# dimnames(halfLives)
halflivesmatters <- halfLives

halflivesmatters[sample(1:length(rownames(halflivesmatters)), 1, replace = TRUE), sample(1:length(colnames(halflivesmatters)), 1, replace = TRUE)]

stepping_stones<- seq(-.2,.2,.2);stepping_stones

FinalCountDown<-finalCarbonContribution()
#

#Marriott Wardman Park in DC (in maryland, not DC) stuff about carbon March27-30?
# 
# ptm <- proc.time()
# 
# change<-list()
# stonesize<-list()
# 
# plot(FinalCountDown)
# 
# for (i in 1:100){
#   sample_pebble<-sample(1:13,1,replace = TRUE)
#   stones<-sample(stepping_stones,1,replace = TRUE)
#   halflivesmatters <- halfLives
#   halflivesmatters[,sample_pebble] <- (1+ stones)*halflivesmatters[,sample_pebble]
#   lines(finalCarbonContribution(halflives = halflivesmatters))
#   change[[i]] <- ((finalCarbonContribution(halflives = halflivesmatters) - FinalCountDown)/FinalCountDown)*100
#   stonesize[[i]]<-stones
# }
# lines(FinalCountDown, col = "red")
# proc.time() - ptm
# change
# t(stonesize)

#put in loop
# (new - control)/control * 100 <- percent change

#perfcent to each product class, halflife, 
#halflife <- go by year instead of percent
#one, use what i got
#change perecentage <- amount of change? more papaer less, blah budgeting
#in 2 weeks, maybe paper reporting senesitivity analysis of wood stuff

# -hlaf-life class +- 20%  results in % change in total carbon table
# - +- 1 year results in % change in total carbon OR total change in carbon (acutal change)
# 
# - product distributino +- 2% results in change in total carbon 
#  maybe approach this by (automatic compensation?) ie) +2% paper - ?% everything else  total must stay at 100%



#percent change applied to all halflives according to year
set.seed(90)
ptm <- proc.time()

change<-list()
stonesize<-list()


for (i in 1:length(rownames(halflivesmatters))){
  stones<-sample(stepping_stones,1,replace = TRUE)
  halflivesmatters <- halfLives
  halflivesmatters[i,] <- (1+ stones)*halflivesmatters[i,]
  change[[i]] <- ((finalCarbonContribution(halflives = halflivesmatters) - FinalCountDown)/FinalCountDown)*100
  stonesize[[i]]<-stones
}
proc.time() - ptm
change
hhh<-as.data.frame(change)
hh<-as.matrix(change)
rownames(hh) <- t(stonesize)


#halflivesmatters[1,]+1 to add one year to one year's data of halflives
ptm <- proc.time()

changeincrease<-list()
changedecrease<-list()

for (i in 1:length(rownames(halflivesmatters))){
  halflivesmatters <- halfLives
  halflivesmatters[i,] <- halflivesmatters[i,]+ 1
  changeincrease[[i]] <- ((finalCarbonContribution(halflives = halflivesmatters) - FinalCountDown)/FinalCountDown)*100
  halflivesmatters <- halfLives
  halflivesmatters[i,] <- halflivesmatters[i,] - 1
  changedecrease[[i]] <- ((finalCarbonContribution(halflives = halflivesmatters) - FinalCountDown)/FinalCountDown)*100
}
proc.time() - ptm
# changeincrease
gamechanger <- as.data.frame(changeincrease)
anotherone <- as.data.frame(changedecrease)
#columns have one year's worth of data increased by one year 
dimnames(gamechanger)


#adding one year to one class of half-lives
ptm <- proc.time()

changeincrease1<-list()
changedecrease1<-list()

for (i in 1:length(colnames(halflivesmatters))){
  halflivesmatters <- halfLives
  halflivesmatters[i,] <- halflivesmatters[,i]+ 1
  changeincrease1[[i]] <- ((finalCarbonContribution(halflives = halflivesmatters) - FinalCountDown)/FinalCountDown)*100
  halflivesmatters <- halfLives
  halflivesmatters[i,] <- halflivesmatters[,i] - 1
  changedecrease1[[i]] <- ((finalCarbonContribution(halflives = halflivesmatters) - FinalCountDown)/FinalCountDown)*100
}
proc.time() - ptm

dimnames(changeincrease1)
dimnames(changedecrease1)




ptm <- proc.time()
sigchange<-list()

for(i in 1:length(changedecrease1)){
  if (abs(as.data.frame(changedecrease1[i])[1,1]) > 0.01){
    cc<-colnames(halfLives)[i]
    sigchange[[i]]<-as.character(cc)
  }
  
}
proc.time() - ptm
sigchange
#end of paper, (explaining the whole project) they did sensitivity, start with their stuff.  (recreate theirs?)
#


#3NOV 8 2018
#1. which half life matters the most?
#2. change each by % and by +- "1" year
#if all of them changed randomly, what resulting change is expected?
# - halflifes (all) + U(-1,1) what is the range of output variable? try with N(0,1)
#both 1 and 2, look at plots (2) plot 1000 times and should be getting a cone (narrow to wide, left to right) 

#hlaflife +- U(-.1,.1) -> result on average changes___?
# final number (2015) looking at mean, sD, median




#another thing
#proportion into classes
#change proportion of product that goes into each class
#sensitive <- halflife or the fraction that goes into halflife category (may be the fraction)
#do this with the halflife loops somehow (be creative)

#are there other quantities/parameters that have uncertainties?

#set.seeds, mainly reproducipiloby 


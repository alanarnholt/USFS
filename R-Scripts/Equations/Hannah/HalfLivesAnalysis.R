library(WOODCARB3R)
# View(halfLives)

halflivesmatters <- halfLives
# dimnames(halflivesmatters)

halflivesmatters[sample(1:151, 1, replace = TRUE), sample(1:16, 1, replace = TRUE)]

stepping_stones<- seq(-.2,.2,.01);stepping_stones

#Marriott Wardman Park in DC (in maryland, not DC) stuff about carbon March27-30?

FinalCountDown<-finalCarbonContribution()

ptm <- proc.time()

change<-list()
stonesize<-list()

plot(FinalCountDown)

for (i in 1:100){
  sample_pebble<-sample(1:13,1,replace = TRUE)
  stones<-sample(stepping_stones,1,replace = TRUE)
  halflivesmatters <- halfLives
  halflivesmatters[,sample_pebble] <- (1+ stones)*halflivesmatters[,sample_pebble]
  lines(finalCarbonContribution(halflives = halflivesmatters))
  change[[i]] <- ((finalCarbonContribution(halflives = halflivesmatters) - FinalCountDown)/FinalCountDown)*100
  stonesize[[i]]<-stones
}
lines(FinalCountDown, col = "red")
proc.time() - ptm
change
t(stonesize)

#put in loop
# (new - control)/control * 100 <- percent change



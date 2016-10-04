library(WOODCARB3R)
# View(halfLives)

halflivesmatters <- halfLives
# dimnames(halflivesmatters)
halflivesmatters[,1]<- (1+.9)*halflivesmatters[,1]
halflivesmatters[,10]<- (1+.9)*halflivesmatters[,10]

finalCarbonContribution(Years = 1990) - finalCarbonContribution(Years = 1990,halflives = halflivesmatters)

halflivesmatters[sample(1:151, 1, replace = TRUE), sample(1:16, 1, replace = TRUE)]

stepping_stones<-c(-.90,-.80,-.70,-.60,-.50,-.40,-.30,-.20,-.10, 0,.10,.20,.30,.40,.50,.60,.70,.80,.90)



ptm <- proc.time()

change<-list()

for (i in 1:100){
  sample_pebble<-sample(c(1:3,5:8,10:12,14:16),1,replace = TRUE)
  halflivesmatters <- halfLives
  halflivesmatters[,sample_pebble] <- (1+ sample(stepping_stones, 1, replace = TRUE))*halflivesmatters[,sample_pebble]
  change[i] <- finalCarbonContribution(Years = sample_pebble + 1989) - finalCarbonContribution(Years = sample_pebble + 1989, halflives = halflivesmatters)
  
}
proc.time() - ptm
change

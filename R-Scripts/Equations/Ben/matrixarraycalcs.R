
inflow <- array(0, dim = c(1,13,151))
inflow[1,1,1:121] <- swpcalcdata$`Sawnwood Prod Special` 
inflow[1,2,1:121] <- swpcalcdata$`SP Prod Special`
inflow[1,3,1:121] <- swpcalcdata$`NSP Prod Special` 
inflow[1,4,1:121] <- swpcalcdata$`Other Products Production Special`


Fs <- array(0, dim = c(13,13,151))
Fs[1,,1:151] <- t(fracsawnwood[,-totalEUs])
Fs[2,,1:151] <- t(fracstrpanels[,-totalEUs])
Fs[3,,1:151] <- t(fracnonstrpanels[,-totalEUs])

placediu <- array( 0,dim=c(1,13,151))
  
for(j in 1:121){
  for(i in 1:dim(inflow)[2]){
    placediu[,i,j] <- sum(Fs[,i,j] * inflow[,,j])
  }
}
placediu[,13,1:121] <- swpcalcdata$`Other Products Production Special`
#placediu <- apply(placediu, c(2,3), sum)

decay1 <- matrix( , nrow = 121, ncol = 13)       

for (eu in 1:13) { ##decay matrix for year 2050 
  decays<- exp(-log(2)/halflives[1:121,eu]*121:1)
  decay1[,eu] <- decays 
}

totalcarbon <- array(0,dim=c(4,1,13,151)) #total carbon for one year. for all years need all decays  

for(k in 1:4){
  for(i in 1:121){
    for(j in 1:13){
      totalcarbon[k,,j,i] <- sum(placediu[,j,1:i] * decay_array[k,j,1:i,i] * (1-lossIU[1:i,j]))
    } 
  }
}
  
testdf <- data.frame(Decay1 = totalcarbon[1,1,1,],
                     Decay2 = totalcarbon[2,1,1,],
                     Decay3 = totalcarbon[3,1,1,],
                     Decay4 = totalcarbon[4,1,1,])
ggplot() + 
  geom_line(data = testdf, aes(x = 1900:2050, y = testdf$Decay1, color = "black"))+ 
  geom_line(data = testdf, aes(x = 1900:2050, y = testdf$Decay2, color = "blue"))+
  geom_line(data = testdf, aes(x = 1900:2050, y = testdf$Decay3, color = "red")) + 
  geom_line(data = testdf, aes(x = 1900:2050, y = testdf$Decay4, color = "green"))


for(i in 1:13){
  totalcarbon[,i,121] <- sum(placediu[,i,1:121] * decay_array[1,i,1:121,121] * (1 - lossIU[1:121, i])) ##can incorporate loss when placed IU into decay or seperate. 
}


totalcarbon[,,121] <- sum(placediu[,,1:121] * t(decay1) * (1 - lossIU[1:121,])) ##can incorporate loss when placed IU into decay or seperate. 


sum(totalcarbon[,,121]) + pre1900(2020)





placeIU <- placeIU[,-c(1,5,10,14)]
for(i in 1:13){
  print(max(placeIU[,i + 1] - placediu[1,i,1:121]))
}




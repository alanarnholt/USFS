library(tensorA)


m1 <- matrix(1:6, 2, 3, byrow = TRUE) 
m2 <- matrix(7:12, 3, 2, byrow = TRUE)

m1 %*% m2

crossprod(m1, m2)


a <- to.tensor(1:6, c(a=1, b=3, c=2))
b <- a
a %*% b



inflow <- array(1:13, dim = c(1,13,151))
Fs <- array(1:13**2, dim = c(13,13,151))
placediu <- array( 0,dim=c(13,13,151))
  
for(j in 1:151){
  for(i in 1:dim(inflow)[2]){
    placediu[,i,j] <- Fs[i,,j] * inflow[,i,j]
  }
}

placediu <- apply(placediu, c(2,3), sum)

decay1 <- matrix( , nrow = 151, ncol = 13)       

for (eu in 1:13) { ##decay matrix for year 2050 
  decays<- exp(-log(2)/halfLives[1:151,eu]*151:1)
  decay1[,eu] <- decays 
}

totalcarbon <- array(0,dim=c(1,13)) #total carbon for one year. for all years need all decays  

for(i in 1:13){
  totalcarbon[,i] <- sum(placediu[i] * decay1[i]) ##can incorporate loss when placed IU into decay or seperate. 
}




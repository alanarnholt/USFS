inflows <- c( swpcalcdata[["Sawnwood Prod Special"]], 
              swpcalcdata[["SP Prod Special"]], 
              swpcalcdata[["NSP Prod Special"]])

##want 13 x 13 matrix 



Fs <- rep(list(matrix(0,nrow=13, ncol = 13)),121) #one matrix for each year, can easily append for new years 

#lapply(X = mats, FUN = )

for(i in 1:length(mats)){
  mats[[i]][1,] <- as.numeric(fracsawnwood[i,c(1:3, 5:8, 10:12, 14:16)]) #row for sawnwood products 
  mats[[i]][2,] <- as.numeric(fracstrpanels[i,c(1:3, 5:8, 10:12, 14:16)]) #structural panels 
  mats[[i]][3,] <- as.numeric(fracnonstrpanels[i,c(1:3, 5:8, 10:12, 14:16)]) #non-structural panels 
}


placeiumats <- mats

for(i in 1:length(placeiumats)){
  placeiumats[[i]][1,] <- mats[[1]][1,] * inflows[i]
  placeiumats[[i]][2,] <- mats[[1]][1,] * inflows[121 + i]
  placeiumats[[i]][3,] <- mats[[1]][1,] * inflows[121 + 121 +i]
}


####decay matrices 


decaylist <- list(decay1, decay2)  

#decay1 <- rep(list(matrix(0,nrow=121, ncol = 13)),121) 
decay1 <- list() 
for(i in 1:121){
      decay1[[i]] <- matrix(, nrow = i, ncol = 16)       
      yearrange <- 1:nrow(decay1[[i]])  #number of years from 1900 to year 
      for (eu in 1:16) { #for each column 
        decays <- exp(-log(2)/halfLives[yearrange,eu]*rev(yearrange))
        decay1[[i]][,eu] <- decays 
      }
}

decay2 <- list() ## k = 2
g <- function(x){ ##gamma function 
  ((x^(THETA - 1)) * (exp(-x/K))) / (gamma(THETA) * (K^THETA))
}
for(i in 1:121){  
  decay2[[i]] <- matrix(, nrow = i, ncol = 16)         
  yearrange <- 1:nrow(decay2[[i]])  #number of years from 1900 to year 
  for (eu in 1:16) { #for each column 
     
    
    decays <- numeric(length(yearrange))
    K <- 2
    THETA <- findKorTHETAforGamma(halflife = halfLives[i, eu], k = K)
    for (j in yearrange){
      p<-0 #not exactly sure what this is
      p[j+1]<-p[j]+1 ##this part still confused about, why not just use i instead of setting first val to 0. Signifies no years passed?
      decays[j]<- 1 - integrate(g, lower=0, upper=p[j])$value
    }
    decay2[[i]][,eu] <- decays
  }
}
  
  
mt <- matrix(, nrow = 10, ncol = 10)  

for(i in 1:121){
  decay1[[i]] <- matrix(, nrow = i, ncol = 16)   
}
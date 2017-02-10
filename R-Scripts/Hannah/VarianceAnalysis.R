library(WOODCARB3R)

############################
#In these series of for loops we are looking to produce random error into one class of halflife and come its variance with other halflife classes.
#within each for loop, a normally distributed error is applied to a specified halflife class
#this modified halflife class is then used with the untouched halflife classes to calculate the final Carbon Contribution
#each column in the data frame stores each simulation of applying the random error with the exception of the very first column which is removed after the simulation is complete
#the variance is then taken from 


# fixes to make
##multiple by randomError only since its centered around 1
##lower number of simulations to 1000 from 2000 because laptop cannot handle it

#Some halflife classes have larger variance range than others, large difference(1,4) and the smallest difference is with 7(note that this is a much smaller range than the others ie 4,000 instead of 3,000,000), using randomError+1
#each loop takes approximately 30 minutes
#running all 13 loops takes roughly 6hours


#fixes made, results are still teh same as above, it took roughly 4 hours to complete all simulations for the 13 classes

set.seed(1)
Histdf<-data.frame(nrow=21)

ptmAll<-proc.time()
ptm<-proc.time()
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,1]<- halflivesmatters[,1]*(randomError)
  Histdf<-cbind(Histdf,as.data.frame(finalCarbonContribution(Years=c(1990, 2000, 2010),halflives = halflivesmatters)))
}
Histdf<-Histdf[,-1]


#we need to create this other loop because not doing so would result in a data frame that has each calculated variance with rows of one variance filling in "missing" values.
#By doing this loop, we are only obtaining the variances of each simulation without the filler data.
#a histogram is then used to see the distribution of the variance

Vardf<-data.frame(nrow=1)
for(i in 1:length(Histdf)){
  Vardf<-cbind(Vardf,var(Histdf[i]))
}

Vardf<-Vardf[,-1]

hist(as.numeric(Vardf))


OneTrial<-proc.time() - ptm








set.seed(1)
Histdf2<-data.frame(nrow=21)

ptm<-proc.time()
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,2]<- halflivesmatters[,2]*(randomError)
  Histdf2<-cbind(Histdf2,as.data.frame(finalCarbonContribution(Years=c(1990, 2000, 2010),halflives = halflivesmatters)))
}
Histdf2<-Histdf2[,-1]


Vardf2<-data.frame(nrow=1)
for(i in 1:length(Histdf2)){
  Vardf2<-cbind(Vardf2,var(Histdf2[i]))
}

Vardf2<-Vardf2[,-1]

hist(as.numeric(Vardf2))









set.seed(1)
Histdf3<-data.frame(nrow=21)

ptm<-proc.time()
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,3]<- halflivesmatters[,3]*(randomError)
  Histdf3<-cbind(Histdf3,as.data.frame(finalCarbonContribution(Years=c(1990, 2000, 2010),halflives = halflivesmatters)))
}
Histdf3<-Histdf3[,-1]


Vardf3<-data.frame(nrow=1)
for(i in 1:length(Histdf3)){
  Vardf3<-cbind(Vardf3,var(Histdf3[i]))
}

Vardf3<-Vardf3[,-1]

hist(as.numeric(Vardf3))









set.seed(1)
Histdf4<-data.frame(nrow=21)

ptm<-proc.time()
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,4]<- halflivesmatters[,4]*(randomError)
  Histdf4<-cbind(Histdf4,as.data.frame(finalCarbonContribution(Years=c(1990, 2000, 2010),halflives = halflivesmatters)))
}
Histdf4<-Histdf4[,-1]


Vardf4<-data.frame(nrow=1)
for(i in 1:length(Histdf4)){
  Vardf4<-cbind(Vardf4,var(Histdf4[i]))
}

Vardf4<-Vardf4[,-1]

hist(as.numeric(Vardf4))









set.seed(1)
Histdf5<-data.frame(nrow=21)

ptm<-proc.time()
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,5]<- halflivesmatters[,5]*(randomError)
  Histdf5<-cbind(Histdf5,as.data.frame(finalCarbonContribution(Years=c(1990, 2000, 2010),halflives = halflivesmatters)))
}
Histdf5<-Histdf5[,-1]


Vardf5<-data.frame(nrow=1)
for(i in 1:length(Histdf5)){
  Vardf5<-cbind(Vardf5,var(Histdf5[i]))
}

Vardf5<-Vardf5[,-1]

hist(as.numeric(Vardf5))









set.seed(1)
Histdf6<-data.frame(nrow=21)

ptm<-proc.time()
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,6]<- halflivesmatters[,6]*(randomError)
  Histdf6<-cbind(Histdf6,as.data.frame(finalCarbonContribution(Years=c(1990, 2000, 2010),halflives = halflivesmatters)))
}
Histdf6<-Histdf6[,-1]


Vardf6<-data.frame(nrow=1)
for(i in 1:length(Histdf6)){
  Vardf6<-cbind(Vardf6,var(Histdf6[i]))
}

Vardf6<-Vardf6[,-1]

hist(as.numeric(Vardf6))









set.seed(1)
Histdf7<-data.frame(nrow=21)

ptm<-proc.time()
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,7]<- halflivesmatters[,7]*(randomError)
  Histdf7<-cbind(Histdf7,as.data.frame(finalCarbonContribution(Years=c(1990, 2000, 2010),halflives = halflivesmatters)))
}
Histdf7<-Histdf7[,-1]


Vardf7<-data.frame(nrow=1)
for(i in 1:length(Histdf7)){
  Vardf7<-cbind(Vardf7,var(Histdf7[i]))
}

Vardf7<-Vardf7[,-1]

hist(as.numeric(Vardf7))









set.seed(1)
Histdf8<-data.frame(nrow=21)

ptm<-proc.time()
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,8]<- halflivesmatters[,8]*(randomError)
  Histdf8<-cbind(Histdf8,as.data.frame(finalCarbonContribution(Years=c(1990, 2000, 2010),halflives = halflivesmatters)))
}
Histdf8<-Histdf8[,-1]


Vardf8<-data.frame(nrow=1)
for(i in 1:length(Histdf8)){
  Vardf8<-cbind(Vardf8,var(Histdf8[i]))
}

Vardf8<-Vardf8[,-1]

hist(as.numeric(Vardf8))









set.seed(1)
Histdf9<-data.frame(nrow=21)

ptm<-proc.time()
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,9]<- halflivesmatters[,9]*(randomError)
  Histdf9<-cbind(Histdf9,as.data.frame(finalCarbonContribution(Years=c(1990, 2000, 2010),halflives = halflivesmatters)))
}
Histdf9<-Histdf9[,-1]


Vardf9<-data.frame(nrow=1)
for(i in 1:length(Histdf9)){
  Vardf9<-cbind(Vardf9,var(Histdf9[i]))
}

Vardf9<-Vardf9[,-1]

hist(as.numeric(Vardf9))









set.seed(1)
Histdf10<-data.frame(nrow=21)

ptm<-proc.time()
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,10]<- halflivesmatters[,10]*(randomError)
  Histdf10<-cbind(Histdf10,as.data.frame(finalCarbonContribution(Years=c(1990, 2000, 2010),halflives = halflivesmatters)))
}
Histdf10<-Histdf10[,-1]


Vardf10<-data.frame(nrow=1)
for(i in 1:length(Histdf10)){
  Vardf10<-cbind(Vardf10,var(Histdf10[i]))
}

Vardf10<-Vardf10[,-1]

hist(as.numeric(Vardf10))









set.seed(1)
Histdf11<-data.frame(nrow=21)

ptm<-proc.time()
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,11]<- halflivesmatters[,11]*(randomError)
  Histdf11<-cbind(Histdf11,as.data.frame(finalCarbonContribution(Years=c(1990, 2000, 2010),halflives = halflivesmatters)))
}
Histdf11<-Histdf11[,-1]


Vardf11<-data.frame(nrow=1)
for(i in 1:length(Histdf11)){
  Vardf11<-cbind(Vardf11,var(Histdf11[i]))
}

Vardf11<-Vardf11[,-1]

hist(as.numeric(Vardf11))









set.seed(1)
Histdf12<-data.frame(nrow=21)

ptm<-proc.time()
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,12]<- halflivesmatters[,12]*(randomError)
  Histdf12<-cbind(Histdf12,as.data.frame(finalCarbonContribution(Years=c(1990, 2000, 2010),halflives = halflivesmatters)))
}
Histdf12<-Histdf12[,-1]


Vardf12<-data.frame(nrow=1)
for(i in 1:length(Histdf12)){
  Vardf12<-cbind(Vardf12,var(Histdf12[i]))
}

Vardf12<-Vardf12[,-1]

hist(as.numeric(Vardf12))









set.seed(1)
Histdf13<-data.frame(nrow=21)

ptm<-proc.time()
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,13]<- halflivesmatters[,13]*(randomError)
  Histdf13<-cbind(Histdf13,as.data.frame(finalCarbonContribution(Years=c(1990, 2000, 2010),halflives = halflivesmatters)))
}
Histdf13<-Histdf13[,-1]


Vardf13<-data.frame(nrow=1)
for(i in 1:length(Histdf13)){
  Vardf13<-cbind(Vardf13,var(Histdf13[i]))
}

Vardf13<-Vardf13[,-1]

hist(as.numeric(Vardf13))

#difference in the max and min variances of simulations, this way we can tell how big of a range the variance of each halflife is going.
as.numeric(max(Vardf))-as.numeric(min(Vardf))
as.numeric(max(Vardf2))-as.numeric(min(Vardf2))
as.numeric(max(Vardf3))-as.numeric(min(Vardf3))
as.numeric(max(Vardf4))-as.numeric(min(Vardf4))
as.numeric(max(Vardf5))-as.numeric(min(Vardf5))
as.numeric(max(Vardf6))-as.numeric(min(Vardf6))
as.numeric(max(Vardf7))-as.numeric(min(Vardf7))
as.numeric(max(Vardf8))-as.numeric(min(Vardf8))
as.numeric(max(Vardf9))-as.numeric(min(Vardf9))
as.numeric(max(Vardf10))-as.numeric(min(Vardf10))
as.numeric(max(Vardf11))-as.numeric(min(Vardf11))
as.numeric(max(Vardf12))-as.numeric(min(Vardf12))
as.numeric(max(Vardf13))-as.numeric(min(Vardf13))


#max and min of the variances produced in each simulation
max(Vardf);min(Vardf)
max(Vardf2);min(Vardf2)
max(Vardf3);min(Vardf3)
max(Vardf4);min(Vardf4)
max(Vardf5);min(Vardf5)
max(Vardf6);min(Vardf6)
max(Vardf7);min(Vardf7)
max(Vardf8);min(Vardf8)
max(Vardf9);min(Vardf9)
max(Vardf10);min(Vardf10)
max(Vardf11);min(Vardf11)
max(Vardf12);min(Vardf12)
max(Vardf13);min(Vardf13)


AllOfThem<-proc.time() - ptmAll


OneTrial;AllOfThem

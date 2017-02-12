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


#fixes made, results are still the same as above, it took roughly 4 hours to complete all simulations for the 13 classes



##as of february 10, 2017
# change years to just individual years :1990,2000,2010


#table of variances: cols are years and rows are the halflives variances
#each histogram will have the max and min labelled on the bottom AND SAME SCALE


# show histograms of the best and worst of one year (2000) (most and least sensitive basically)

#in description: normal distributed error, ...
#find ben, which variables in halflive sheet should i be lookig at.  dont match up with paper's stated halflives (there are 7 not 13)



#The loops for each of the variables to calculate the amount of variance after 1000 simulations incorporating normally distributed error.
ptmAll<-proc.time()
ptm<-proc.time()

##Variable 1
set.seed(1)
Histdfa<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,1]<- halflivesmatters[,1]*(randomError)
  Histdfa<-cbind(Histdfa,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdfa<-Histdfa[,-1]
OneTrial<-proc.time() - ptm




set.seed(1)
Histdfb<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,1]<- halflivesmatters[,1]*(randomError)
  Histdfb<-cbind(Histdfb,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdfb<-Histdfb[,-1]




set.seed(1)
Histdfc<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,1]<- halflivesmatters[,1]*(randomError)
  Histdfc<-cbind(Histdfc,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdfc<-Histdfc[,-1]






##Variable 2
set.seed(1)
Histdf2a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,1]<- halflivesmatters[,1]*(randomError)
  Histdf2a<-cbind(Histdf2a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf2a<-Histdf2a[,-1]




set.seed(1)
Histdf2b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,2]<- halflivesmatters[,2]*(randomError)
  Histdf2b<-cbind(Histdf2b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf2b<-Histdf2b[,-1]




set.seed(1)
Histdf2c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,2]<- halflivesmatters[,2]*(randomError)
  Histdf2c<-cbind(Histdf2c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf2c<-Histdf2c[,-1]






##Variable 3
set.seed(1)
Histdf3a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,3]<- halflivesmatters[,3]*(randomError)
  Histdf3a<-cbind(Histdf3a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf3a<-Histdf3a[,-1]




set.seed(1)
Histdf3b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,3]<- halflivesmatters[,3]*(randomError)
  Histdf3b<-cbind(Histdf3b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf3b<-Histdf3b[,-1]




set.seed(1)
Histdf3c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,3]<- halflivesmatters[,3]*(randomError)
  Histdf3c<-cbind(Histdf3c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf3c<-Histdf3c[,-1]






##Variable 4
set.seed(1)
Histdf4a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,4]<- halflivesmatters[,4]*(randomError)
  Histdf4a<-cbind(Histdf4a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf4a<-Histdf4a[,-1]




set.seed(1)
Histdf4b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,4]<- halflivesmatters[,4]*(randomError)
  Histdf4b<-cbind(Histdf4b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf4b<-Histdf4b[,-1]




set.seed(1)
Histdf4c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,4]<- halflivesmatters[,4]*(randomError)
  Histdf4c<-cbind(Histdf4c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf4c<-Histdf4c[,-1]






##Variable 5
set.seed(1)
Histdf5a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,5]<- halflivesmatters[,5]*(randomError)
  Histdf5a<-cbind(Histdf5a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf5a<-Histdf5a[,-1]




set.seed(1)
Histdf5b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,5]<- halflivesmatters[,5]*(randomError)
  Histdf5b<-cbind(Histdf5b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf5b<-Histdf5b[,-1]




set.seed(1)
Histdf5c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,5]<- halflivesmatters[,5]*(randomError)
  Histdf5c<-cbind(Histdf5c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf5c<-Histdf5c[,-1]






##Variable 6
set.seed(1)
Histdf6a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,6]<- halflivesmatters[,6]*(randomError)
  Histdf6a<-cbind(Histdf6a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf6a<-Histdf6a[,-1]




set.seed(1)
Histdf6b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,6]<- halflivesmatters[,6]*(randomError)
  Histdf6b<-cbind(Histdf6b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf6b<-Histdf6b[,-1]




set.seed(1)
Histdf6c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,6]<- halflivesmatters[,6]*(randomError)
  Histdf6c<-cbind(Histdf6c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf6c<-Histdf6c[,-1]






##Variable 7
set.seed(1)
Histdf7a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,7]<- halflivesmatters[,7]*(randomError)
  Histdf7a<-cbind(Histdf7a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf7a<-Histdf7a[,-1]




set.seed(1)
Histdf7b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,7]<- halflivesmatters[,7]*(randomError)
  Histdf7b<-cbind(Histdf7b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf7b<-Histdf7b[,-1]




set.seed(1)
Histdf7c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,7]<- halflivesmatters[,7]*(randomError)
  Histdf7c<-cbind(Histdf7c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf7c<-Histdf7c[,-1]






##Variable 8
set.seed(1)
Histdf8a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,8]<- halflivesmatters[,8]*(randomError)
  Histdf8a<-cbind(Histdf8a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf8a<-Histdf8a[,-1]




set.seed(1)
Histdf8b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,8]<- halflivesmatters[,8]*(randomError)
  Histdf8b<-cbind(Histdf8b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf8b<-Histdf8b[,-1]




set.seed(1)
Histdf8c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,8]<- halflivesmatters[,8]*(randomError)
  Histdf8c<-cbind(Histdf8c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf8c<-Histdf8c[,-1]






##Variable 9
set.seed(1)
Histdf9a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,9]<- halflivesmatters[,9]*(randomError)
  Histdf9a<-cbind(Histdf9a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf9a<-Histdf9a[,-1]




set.seed(1)
Histdf9b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,9]<- halflivesmatters[,9]*(randomError)
  Histdf9b<-cbind(Histdf9b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf9b<-Histdf9b[,-1]




set.seed(1)
Histdf9c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,9]<- halflivesmatters[,9]*(randomError)
  Histdf9c<-cbind(Histdf9c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf9c<-Histdf9c[,-1]






##Variable 10
set.seed(1)
Histdf10a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,10]<- halflivesmatters[,10]*(randomError)
  Histdf10a<-cbind(Histdf10a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf10a<-Histdf10a[,-1]




set.seed(1)
Histdf10b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,10]<- halflivesmatters[,10]*(randomError)
  Histdf10b<-cbind(Histdf10b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf10b<-Histdf10b[,-1]




set.seed(1)
Histdf10c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,10]<- halflivesmatters[,10]*(randomError)
  Histdf10c<-cbind(Histdf10c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf10c<-Histdf10c[,-1]






##Variable 11
set.seed(1)
Histdf11a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,11]<- halflivesmatters[,11]*(randomError)
  Histdf11a<-cbind(Histdf11a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf11a<-Histdf11a[,-1]




set.seed(1)
Histdf11b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,11]<- halflivesmatters[,11]*(randomError)
  Histdf11b<-cbind(Histdf11b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf11b<-Histdf11b[,-1]




set.seed(1)
Histdf11c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,11]<- halflivesmatters[,11]*(randomError)
  Histdf11c<-cbind(Histdf11c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf11c<-Histdf11c[,-1]






##Variable 12
set.seed(1)
Histdf12a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,12]<- halflivesmatters[,12]*(randomError)
  Histdf12a<-cbind(Histdf12a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf12a<-Histdf12a[,-1]




set.seed(1)
Histdf12b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,12]<- halflivesmatters[,12]*(randomError)
  Histdf12b<-cbind(Histdf12b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf12b<-Histdf12b[,-1]




set.seed(1)
Histdf12c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,12]<- halflivesmatters[,12]*(randomError)
  Histdf12c<-cbind(Histdf12c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf12c<-Histdf12c[,-1]






##Variable 13
set.seed(1)
Histdf13a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,13]<- halflivesmatters[,13]*(randomError)
  Histdf13a<-cbind(Histdf13,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf13a<-Histdf13a[,-1]




set.seed(1)
Histdf13b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,13]<- halflivesmatters[,13]*(randomError)
  Histdf13b<-cbind(Histdf13b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf13b<-Histdf13b[,-1]




set.seed(1)
Histdf13c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,13]<- halflivesmatters[,13]*(randomError)
  Histdf13c<-cbind(Histdf13c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf13c<-Histdf13c[,-1]

#

#timing to see how long it takes to run all 13 variables for each of the three years (1990, 2000, 2010)
AllOfThem<-proc.time() - ptmAll

OneTrial;AllOfThem



#variance of each variable for each year put into a matrix.  Columns are the years, Rows are the variables
VarianceTable<-t(as.matrix(c(var(t(Histdfa)),var(t(Histdfb)),var(t(Histdfc)))))
rbind(VarianceTable,t(as.matrix(c(var(t(Histdf2a)),var(t(Histdf2b)),var(t(Histdf2c))))))
rbind(VarianceTable,t(as.matrix(c(var(t(Histdf3a)),var(t(Histdf3b)),var(t(Histdf3c))))))
rbind(VarianceTable,t(as.matrix(c(var(t(Histdf4a)),var(t(Histdf4b)),var(t(Histdf4c))))))
rbind(VarianceTable,t(as.matrix(c(var(t(Histdf5a)),var(t(Histdf5b)),var(t(Histdf5c))))))
rbind(VarianceTable,t(as.matrix(c(var(t(Histdf6a)),var(t(Histdf6b)),var(t(Histdf6c))))))
rbind(VarianceTable,t(as.matrix(c(var(t(Histdf7a)),var(t(Histdf7b)),var(t(Histdf7c))))))
rbind(VarianceTable,t(as.matrix(c(var(t(Histdf8a)),var(t(Histdf8b)),var(t(Histdf8c))))))
rbind(VarianceTable,t(as.matrix(c(var(t(Histdf9a)),var(t(Histdf9b)),var(t(Histdf9c))))))
rbind(VarianceTable,t(as.matrix(c(var(t(Histdf10a)),var(t(Histd10fb)),var(t(Histd10fc))))))
rbind(VarianceTable,t(as.matrix(c(var(t(Histdf11a)),var(t(Histdf11b)),var(t(Histdf11c))))))
rbind(VarianceTable,t(as.matrix(c(var(t(Histdf12a)),var(t(Histdf12b)),var(t(Histdf12c))))))
rbind(VarianceTable,t(as.matrix(c(var(t(Histdf13a)),var(t(Histdf13b)),var(t(Histdf13c))))))

colnames(VarianceTable)<-c(1990,2000,2010)
rownames(VarianceTable)<-c("Variable 1","Variable 2","Variable 3","Variable 4","Variable 5",
                           "Variable 6","Variable 7","Variable 8","Variable 9","Variable 10",
                           "Variable 11","Variable 12","Variable 13")



#finding the minimum amount of variance for year 1990
min(var(t(Histdfa)),
    var(t(Histdf2a)),
    var(t(Histdf3a)),
    var(t(Histdf4a)),
    var(t(Histdf5a)),
    var(t(Histdf6a)),
    var(t(Histdf7a)),
    var(t(Histdf8a)),
    var(t(Histdf9a)),
    var(t(Histdf10a)),
    var(t(Histdf11a)),
    var(t(Histdf12a)),
    var(t(Histdf13a))
    )

#finding the minimum amount of variance for year 2000
min(var(t(Histdfb)),
    var(t(Histdf2b)),
    var(t(Histdf3b)),
    var(t(Histdf4b)),
    var(t(Histdf5b)),
    var(t(Histdf6b)),
    var(t(Histdf7b)),
    var(t(Histdf8b)),
    var(t(Histdf9b)),
    var(t(Histdf10b)),
    var(t(Histdf11b)),
    var(t(Histdf12b)),
    var(t(Histdf13b))
    )

#finding the minimum amount of variance for year 2010
min(var(t(Histdfc)),
    var(t(Histdf2c)),
    var(t(Histdf3c)),
    var(t(Histdf4c)),
    var(t(Histdf5c)),
    var(t(Histdf6c)),
    var(t(Histdf7c)),
    var(t(Histdf8c)),
    var(t(Histdf9c)),
    var(t(Histdf10c)),
    var(t(Histdf11c)),
    var(t(Histdf12c)),
    var(t(Histdf13c))
    )
#




#finding the maximum amount of variance for year 1990
max(var(t(Histdfa)),
    var(t(Histdf2a)),
    var(t(Histdf3a)),
    var(t(Histdf4a)),
    var(t(Histdf5a)),
    var(t(Histdf6a)),
    var(t(Histdf7a)),
    var(t(Histdf8a)),
    var(t(Histdf9a)),
    var(t(Histdf10a)),
    var(t(Histdf11a)),
    var(t(Histdf12a)),
    var(t(Histdf13a))
)

#finding the maximum amount of variance for year 2000
max(var(t(Histdfb)),
    var(t(Histdf2b)),
    var(t(Histdf3b)),
    var(t(Histdf4b)),
    var(t(Histdf5b)),
    var(t(Histdf6b)),
    var(t(Histdf7b)),
    var(t(Histdf8b)),
    var(t(Histdf9b)),
    var(t(Histdf10b)),
    var(t(Histdf11b)),
    var(t(Histdf12b)),
    var(t(Histdf13b))
)

#finding the maximum amount of variance for year 2010
max(var(t(Histdfc)),
    var(t(Histdf2c)),
    var(t(Histdf3c)),
    var(t(Histdf4c)),
    var(t(Histdf5c)),
    var(t(Histdf6c)),
    var(t(Histdf7c)),
    var(t(Histdf8c)),
    var(t(Histdf9c)),
    var(t(Histdf10c)),
    var(t(Histdf11c)),
    var(t(Histdf12c)),
    var(t(Histdf13c))
)
#

#histograms of each variable for each of the three years.  WILL HAVE ALL OF THEM SCALED THE SAME
hist(as.numeric(Histdfa), main="Variable1 Year 1990", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdfb), main="Variable1 Year 2000", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdfc), main="Variable1 Year 2010", xlab = "Final Carbon Contribution")

hist(as.numeric(Histdf2a), main="Variable1 Year 1990", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf2b), main="Variable1 Year 2000", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf2c), main="Variable1 Year 2010", xlab = "Final Carbon Contribution")

hist(as.numeric(Histdf3a), main="Variable1 Year 1990", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf3b), main="Variable1 Year 2000", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf3c), main="Variable1 Year 2010", xlab = "Final Carbon Contribution")

hist(as.numeric(Histdf4a), main="Variable1 Year 1990", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf4b), main="Variable1 Year 2000", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf4c), main="Variable1 Year 2010", xlab = "Final Carbon Contribution")

hist(as.numeric(Histdf5a), main="Variable1 Year 1990", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf5b), main="Variable1 Year 2000", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf5c), main="Variable1 Year 2010", xlab = "Final Carbon Contribution")

hist(as.numeric(Histdf6a), main="Variable1 Year 1990", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf6b), main="Variable1 Year 2000", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf6c), main="Variable1 Year 2010", xlab = "Final Carbon Contribution")

hist(as.numeric(Histdf7a), main="Variable1 Year 1990", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf7b), main="Variable1 Year 2000", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf7c), main="Variable1 Year 2010", xlab = "Final Carbon Contribution")

hist(as.numeric(Histdf8a), main="Variable1 Year 1990", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf8b), main="Variable1 Year 2000", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf8c), main="Variable1 Year 2010", xlab = "Final Carbon Contribution")

hist(as.numeric(Histdf9a), main="Variable1 Year 1990", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf9b), main="Variable1 Year 2000", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf9c), main="Variable1 Year 2010", xlab = "Final Carbon Contribution")

hist(as.numeric(Histdf10a), main="Variable1 Year 1990", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf10b), main="Variable1 Year 2000", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf10c), main="Variable1 Year 2010", xlab = "Final Carbon Contribution")

hist(as.numeric(Histdf11a), main="Variable1 Year 1990", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf11b), main="Variable1 Year 2000", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf11c), main="Variable1 Year 2010", xlab = "Final Carbon Contribution")

hist(as.numeric(Histdf12a), main="Variable1 Year 1990", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf12b), main="Variable1 Year 2000", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf12c), main="Variable1 Year 2010", xlab = "Final Carbon Contribution")

hist(as.numeric(Histdf13a), main="Variable1 Year 1990", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf13b), main="Variable1 Year 2000", xlab = "Final Carbon Contribution")
hist(as.numeric(Histdf13c), main="Variable1 Year 2010", xlab = "Final Carbon Contribution")
#








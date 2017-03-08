library(triangle) #loading triangle package, may need to install

mu <- 4.535925e-07 #the expected value, as indicated in WOODCARB
pcterror <- .1 #percentage error in 90% confidence interval

a <- 2.642672084 * pcterror * mu #lower limit of distribution, for derivation see Maple file
b <- mu + (mu - a) #upper limit of distribution
c <- mu #mode of distribution

errorsamples <- 5000 #amount of times to repeat sample
samples <- numeric(errorsamples)
samples <- rtriangle(errorsamples, a, b, c)
hist(samples, freq = FALSE)



w2cerror <- rtriangle(600, (2.642672084 * .1 * c), 4.535925e-07 + (4.535925e-07 - (2.642672084 * .1 * 4.535925e-07)), 4.535925e-07)
rtriangle(600, (2.642672084 * .1 * 4.535925e-07), 4.535925e-07 + (4.535925e-07 - (2.642672084 * .1 * 4.535925e-07)), 4.535925e-07)
hist(w2cerror, freq=TRUE)

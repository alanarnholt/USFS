library(triangle) #loading triangle package, may need to install

mu <- 400 #the expected value, as indicated in WOODCARB
pcterror <- .15 #percentage error in 90% confidence interval

a <- 2.642672084 * pcterror * mu #lower limit of distribution, for derivation see Maple file
b <- mu + (mu - a) #upper limit of distribution
c <- mu #mode of distribution

errorsamples <- 5000 #amount of times to repeat sample
samples <- numeric(errorsamples)
samples <- rtriangle(errorsamples, a, b, c)
hist(samples, freq = FALSE)
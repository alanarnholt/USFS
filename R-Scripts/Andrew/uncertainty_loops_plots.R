## woodcarb package install and load
devtools::install_github('benjones2/WOODCARB3R', build_vignettes = TRUE)
library(WOODCARB3R)



## ERROR IN ONE VARIABLE, WITH HISTOGRAM FOR ONE YEAR (IN THIS EXAMPLE, FRACTION SAWNWOOD TO DIFFEREND END USES)

repetitions <- 100 #number of times to repeat sample
year <- 1990 #year to plot on histogram
years <- year - 1900
final <- finalCarbonContribution(Years = year) #actual carbon contribution from package data
error <- numeric(repetitions)
for (i in 1:repetitions)
{
  errorfsawn <- rnorm(13*years,1,.2) #error distribution, based on percentage
  errorfsawnmultiply <- aperm(array(dim = c(13, years), data = errorfsawn)) #array of error percentages to multiply by package array
  error[i] <- finalCarbonContribution(Years = year, 
                                      fsawn = fracsawnwood * errorfsawnmultiply) #filling sample vector
  print(i)
}
hist(error, 
     col = "cyan", 
     main = "Uncertainty in 1990 Final Carbon Contribution with Sawnwood Product Distribution Error", 
     ylab = "Frequency", 
     xlab = "Carbon Contribution (Thousand Metric Tons CO2 Sequestered)")
abline(v = final, col = "red", lwd = 2)
summary(error)



## ERROR IN ONE VARIABLE, WITH ERROR LINES FOR ALL YEARS (IN THIS EXAMPLE, FRACTION SAWNWOOD TO DIFFERENT END USES)

repetitions <- 100 #number of times to repeat sample
year <- c(1901:2010) #range of years to calculate and plot
years <- length(year)
final <- finalCarbonContribution(Years = year) #actual carbon contribution from package data
errorarray <- array(0, dim = c(repetitions, years))
for (i in 1:repetitions)
{
  errorfsawn <- rnorm(13*years,1,.2) #error distribution, based on percentage
  errorfsawnmultiply <- aperm(array(dim = c(13, years), data = errorfsawn)) #array of error percentages to multiply by package array
  error <- finalCarbonContribution(Years = year, 
                                   fsawn = fracsawnwood * errorfsawnmultiply)
  for (j in 1:years)
  {
    errorarray[i, j] <- error[j] #filling sample array
  }
  print(i)
}

# Alan adjusting par
par(mar = c(4, 7, 4, 0) + 0.1)
#

plot(errorarray[1,], 
     type = "l", 
     col = "cyan", 
     ylim = c(min(errorarray), max(errorarray)), 
     main = "Uncertainty Final Carbon Contribution with Sawnwood Product Distribution Error", 
     xlab = "Years (Since 1900)", 
     ylab = "Carbon Contribution\n (Thousand Metric Tons\n CO2 Sequestered)")
for (i in 2:repetitions)
{
  points(errorarray[i,], type = "l", col = "cyan")
}
points(final, type = "l", col = "red", lwd = 2)

par(mar = c(5, 4, 4, 2) + 0.1)


## ERROR IN MULTIPLE VARIABLES, WITH ERROR LINES FOR ALL YEARS

repetitions <- 100 #number of times to repeat sample
year <- c(1901:2010) #range of years to calculate and plot
years <- length(year)
final <- finalCarbonContribution(Years = year) #actual carbon contribution from package data
errorarray <- array(0, dim = c(repetitions, years))
for (i in 1:repetitions)
{
  errorfsawn <- rnorm(13*years,1,.2) #error distribution, based on percentage
  errorfsawnmultiply <- aperm(array(dim = c(13, years), data = errorfsawn)) #array of error percentages to multiply by package array
  errorhalflives <- rnorm(13*years,1,.1) #error distribution, based on percentage
  errorhalflivesmultiply <- aperm(array(dim = c(13,years), data = errorhalflives)) #array of error percentages to multiply by package array
  errorfsp <- rnorm(13*years,1,.3) #error distribution, based on percentage
  errorfspmultiply <- aperm(array(dim = c(13,years), data = errorfsp)) #array of error percentages to multiply by package array
  errorfnsp <- rnorm(13*years,1,.4) #error distribution, based on percentage
  errorfnspmultiply <- aperm(array(dim = c(13,years), data = errorfnsp)) #array of error percentages to multiply by package array
  error <- finalCarbonContribution(Years = year, 
                                   fsawn = fracsawnwood * errorfsawnmultiply, 
                                   halflives = halfLives * errorhalflivesmultiply, 
                                   fsp = fracstrpanels * errorfspmultiply, 
                                   fnsp = fracnonstrpanels * errorfnspmultiply)
  for (j in 1:years)
  {
    errorarray[i, j] <- error[j] #filling sample array
  }
  print(i)
}
plot(errorarray[1,], 
     type = "l", 
     col = "cyan", 
     ylim = c(min(errorarray), max(errorarray)), 
     main = "Uncertainty in Final Carbon Contribution", 
     xlab = "Years (Since 1900)", 
     ylab = "Carbon Contribution (Thousand Metric Tons CO2)")
for (i in 2:repetitions)
{
  points(errorarray[i,], type = "l", col = "cyan")
}
points(final, type = "l", col = "red", lwd = 2)



## MARKING PERCENTILES ON PREVIOUS PLOT

incr <- 10 #year increment for quartile placement
percentiles <- 2 #number of percentiles you wish to plot
quantarray <- array(dim = c(percentiles, years))
for (i in seq(1,years,by=incr)) #getting percentiles for selected years
{
  quantarray[1,i] <- quantile(errorarray[,i],.25) #25th percentile
  quantarray[2,i] <- quantile(errorarray[,i],.75) #75th percentile
}
plot(errorarray[1,], 
     type = "l", 
     col = "cyan", 
     ylim = c(min(errorarray), max(errorarray)), 
     main = "Uncertainty Final Carbon Contribution with Product Distribution and Half Lives Error", 
     xlab = "Years (Since 1900)", 
     ylab = "Carbon Contribution (Thousand Metric Tons CO2 Sequestered)")
for (i in 2:repetitions)
{
  points(errorarray[i,], type = "l", col = "cyan")
}
for (i in seq(1,years,by = incr))
{
  points(quantarray[1,], pch = 16, col = "purple")
  points(quantarray[2,], pch = 16, col = "purple")
}
points(final, type = "l", col = "red", lwd = 2)



## UNCERTAINTY USING TRIANGULAR DISTRIBUTION
## triangular package install and load
install.packages("triangle")
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


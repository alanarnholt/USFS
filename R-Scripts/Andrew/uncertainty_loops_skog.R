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
     main = "Uncertainty Final Carbon Contribution with Product Distribution and Half Lives Error", 
     xlab = "Years (Since 1900)", 
     ylab = "Carbon Contribution (Thousand Metric Tons CO2 Sequestered)")
for (i in 2:repetitions)
{
  points(errorarray[i,], type = "l", col = "cyan")
}
points(final, type = "l", col = "red", lwd = 2)
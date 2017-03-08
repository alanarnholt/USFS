## This script performs a total carbon contribution calculation using the WOODCARB3R package with error introduced based on error sources and distributions defined in the paper "Sequestration of carbon in harvested wood products for the United States" (Skog 2008). Derivation of triangular distribution parameters is explained below script. Each total contribution with error is plotted, forming a range around the base value indicated in the package. Quantiles and other measures can be calculated using the errorarray and sufficient number of samples.

## package install and load
devtools::install_github('benjones2/WOODCARB3R', build_vignettes = TRUE)
library(WOODCARB3R)
library(triangle)

## uncertainty samples
repetitions <- 2000 #number of times to repeat sample
year <- c(1901:2012) #range of years to calculate and plot
years <- length(year)
final <- finalCarbonContribution(Years = year) #actual carbon contribution from package data
finalplot <- numeric()
finalplot[year] <- final
errorarray <- array(NA, dim = c(repetitions, 2012))
for (i in 1:repetitions)
{
  #error in solidwood and product production and trade time series, +/-20% on 90% confidence interval triangular distribution
  prod_trade <- returnData(DataSheet = "SwCalc")
  for (j in 1:years)
  {
    mu <- prod_trade$`Other Products Production`[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$`Other Products Production`[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$`Other Products Exports`[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$`Other Products Exports`[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$`Sawnwood Production`[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$`Sawnwood Production`[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$`Sawnwood Imports`[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$`Sawnwood Imports`[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$`Sawnwood Exports`[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$`Sawnwood Exports`[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$`Log Exports (tons)`[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$`Log Exports (tons)`[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$`Imported logs for lumber and panels (1000 tons)`[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$`Imported logs for lumber and panels (1000 tons)`[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$SP.Production[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$SP.Production[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$SP.Imports[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$SP.Imports[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$SP.Exports[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$SP.Exports[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$NSP.Production[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$NSP.Production[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$NSP.Imports[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$NSP.Imports[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$NSP.Exports[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$NSP.Exports[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$`Other Products Production Special`[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$`Other Products Production Special`[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$`Sawnwood Prod Special`[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$`Sawnwood Prod Special`[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$`SP Prod Special`[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$`SP Prod Special`[j] <- rtriangle(1, a, b, c)
    
    mu <- prod_trade$`NSP Prod Special`[j]
    pcterror <- .2
    a <- mu - (2.642672084 * pcterror * mu)
    b <- mu + (mu - a)
    c <- mu
    prod_trade$`NSP Prod Special`[j] <- rtriangle(1, a, b, c)
  }
  
  #error in factor to convert wood to carbon, +/-10% on 90% confidence interval triangular distribution
  mu <- 4.535925e-07
  pcterror <- .1
  a <- mu - (2.642672084 * pcterror * mu)
  b <- mu + (mu - a)
  c <- mu
  errorwoodtocarbon <- rtriangle(1, a, b, c)
  #error in factor to convert wood to carbon, +/-10% on 90% confidence interval
  mu <- 3.9008955e-07
  pcterror <- .1
  a <- mu - (2.642672084 * pcterror * mu)
  b <- mu + (mu - a)
  c <- mu
  errorpapertocarbon <- rtriangle(1, a, b, c) 
  
  #error in fractions of solidwood products not subject to decay in landfills, uniform distribution from .684 to .987
  errorswpswds <- runif(1, .684, .987) 
  #error in fractions of paper not subject to decay in landfills, uniform distribution from .329 to .552
  errorpaperswds <- runif(1, .329, .552) 
  
  #error in decay rate of solid-wood products in dumps as a half-life in years, +/-30% on 90% confidence interval
  mu <- 0.0420089200339361
  pcterror <- .3
  a <- mu - (2.642672084 * pcterror * mu)
  b <- mu + (mu - a)
  c <- mu
  errorswpdumpdecay <- rtriangle(1, a, b, c)
  #error in decay rate of paper products in dumps as a half-life in years, +/-30% on 90% confidence interval
  mu <- 0.0840178400678722
  pcterror <- .3
  aa <- mu - (2.642672084 * pcterror * mu)
  b <- mu + (mu - a)
  c <- mu
  errorpaperdumpdecay <- rtriangle(1, a, b, c)
  #error in decay rate of solid-wood products in landfills as a half-life in years, +/-30% on 90% confidence interval
  mu <- 0.0300063714528115
  pcterror <- .3
  a <- mu - (2.642672084 * pcterror * mu)
  b <- mu + (mu - a)
  c <- mu
  errorswplanddecay <- rtriangle(1, a, b, c)
  #error in decay rate of paper products in landfills as a half-life in years, +/-30% on 90% confidence interval
  mu <- 0.0478032538317204
  pcterror <- .3
  a <- mu - (2.642672084 * pcterror * mu)
  b <- mu + (mu - a)
  c <- mu
  errorpaperlanddecay <- rtriangle(1, a, b, c)
  
  error <- finalCarbonContribution(Years = year, 
                                   swpdata = prod_trade,
                                   woodToCarbon = errorwoodtocarbon,
                                   paperToCarbon = errorpapertocarbon,
                                   swpSwdsNondegradable = errorswpswds,
                                   paperSwdsNondegradable = errorpaperswds,
                                   swpDumpDecay = errorswpdumpdecay,
                                   paperDumpDecay = errorpaperdumpdecay,
                                   swpLandfillDecay = errorswplanddecay,
                                   paperLandfillDecay = errorpaperlanddecay) #final calculation for each sample
  for (k in 1:years)
  {
    if (error[k] > 0)
    {
      errorarray[i, 1900 + k] <- 0
    }
    else
    {
      errorarray[i, 1900 + k] <- error[k] #filling sample array
    }
  }
  print(i)
}
plot(errorarray[1,], 
     type = "l", 
     col = "cyan", 
     ylim = c(min(errorarray, final, na.rm=TRUE), max(errorarray, final, na.rm=TRUE)),
     xlim = c(1900,2012),
     main = "Uncertainty in Final Carbon Contribution", 
     xlab = "Year", 
     ylab = "Carbon Contribution \n(Thousand Metric Tons CO2)")
for (i in 2:repetitions)
{
  points(errorarray[i,], type = "l", col = "cyan")
}
points(finalplot, type = "l", col = "red", lwd = 3)

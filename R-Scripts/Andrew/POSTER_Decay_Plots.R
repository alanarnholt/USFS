## This script creates an overall plot of carbon contribution using the WOODCARB3R package and its differing decay functions in hard wood products. Additionally, there is script to create a plot of decay of a single hard wood product as an example of the individual shapes of the decay functions.

## woodcarb package install and load
devtools::install_github('benjones2/WOODCARB3R', build_vignettes = TRUE)
library(WOODCARB3R)

## overall contribution plot
years <- c(1901:2012)
exp <- numeric()
exp[years] <- finalCarbonContribution(Years = years, 
                                      approach = "Production", 
                                      decay = "Exponential",
                                      plot = FALSE)
k2 <- numeric()
k2[years] <- finalCarbonContribution(Years = years, 
                                     approach = "Production", 
                                     decay = "K=2",
                                     plot = FALSE)
k10 <- numeric()
k10[years] <- finalCarbonContribution(Years = years, 
                                      approach = "Production", 
                                      decay = "K=10",
                                      plot = FALSE)

## plots
par(mar=c(4,6,4,1)+0.1)
plot(exp,
     type = "l",
     col = "red",
     lwd = 3,
     xlim = c(1900,2012),
     ylim = c(min(exp,k2,k10,na.rm=TRUE), max(exp,k2,k10,na.rm=TRUE)),
     main = "Effect of Decay Function on Carbon Sequestration",
     ylab = "Carbon Contribution\n (Thousand Metric Tons CO2)",
     xlab = "Year")
lines(k2,
      type = "l",
      col = "blue",
      lwd = 3)
lines(k10,
      type = "l",
      col = "green",
      lwd = 3)
par(mar=c(4,5,4,4)+0.1)



## decay example plot
## year and half-life data
years <- c(1:300)
halflife <- 49

## function setup
g <- function(x)
{
  ((x^(k-1))*(exp(-x/h)))/(gamma(k)*(h^k))
}

G2 <- function(h) 
{
  g <- function(x)
  {
    ((x^(k-1))*(exp(-x/h)))/(gamma(k)*(h^k))
  }
  return(g)
}

gh <- function (h) 
{ 
  integrate(G2(h),  lower=0, upper=halflife)$value - .5 #different upper limit in functiuon definition
}

## EXPONENTIAL
k <- 1
exp <- numeric()
h <- halflife / log(2)
for (i in 1:length(years))
{
  decay <- integrate(g, lower=0, upper=i)$value
  exp[1949 + i] <- 1 - decay
}

## K = 2
k <- 2
k2 <- numeric()
h <- uniroot(gh, lower = .1, upper = halflife, tol = 1e-14)$root
for (i in 1:length(years))
{
  decay <- integrate(g, lower=0, upper=i)$value
  k2[1949+i] <- 1 - decay
}

## K = 10
k <- 10
k10 <- numeric()
h <- uniroot(gh, lower = .1, upper = halflife, tol = 1e-14)$root
for (i in 1:length(years))
{
  decay <- integrate(g, lower=0, upper=i)$value
  k10[1949+i] <- 1 - decay
}

## plot
product <- 1
decayexp <- product * exp
decayk2 <- product * k2
decayk10 <- product * k10
plot(decayexp,
     type = "l",
     lwd = 3,
     col = "red",
     main = "Decay of Hardwood in Multifamily Housing Built in 1950",
     ylab = "Percentage Remaining",
     xlab = "Year",
     ylim = c(min(decayexp,decayk2,decayk10,na.rm=TRUE), max(decayexp,decayk2,decayk10,na.rm=TRUE)),
     xlim = c(1950,1950+length(years)))
lines(decayk2,
      type = "l",
      lwd = 3,
      col = "blue")
lines(decayk10,
      type = "l",
      lwd = 3,
      col = "green")

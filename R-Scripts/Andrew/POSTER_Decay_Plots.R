## woodcarb package install and load
devtools::install_github('benjones2/WOODCARB3R', build_vignettes = TRUE)
library(WOODCARB3R)

## OVERALL CONTRIBUTION PLOTS
exp <- finalCarbonContribution(Years = c(1901:2012), 
                               approach = "Production", 
                               decay = "Exponential",
                               plot = FALSE)
k2 <- finalCarbonContribution(Years = c(1901:2012), 
                              approach = "Production", 
                              decay = "K=2",
                              plot = FALSE)
k10 <- finalCarbonContribution(Years = c(1901:2012), 
                               approach = "Production", 
                               decay = "K=10",
                               plot = FALSE)

## plots
plot(exp,
     type = "l",
     col = "red",
     lwd = 3,
     ylim = c(min(exp,k2,k10), 100000),
     main = "Effect of Decay Function on Carbon Sequestration",
     ylab = "Carbon Contribution (Thousand Metric Tons CO2)",
     xlab = "Years (Since 1900)")
lines(k2,
      type = "l",
      col = "blue",
      lwd = 3)
lines(k10,
      type = "l",
      col = "green",
      lwd = 3)
legend("topright",
       c("K=1", "K=2", "K=10"),
       col = c("red", "blue", "green"),
       lwd = 3)

## LOADING DATA
years <- c(1:300)
halflife <- 49

## FUNCTION SETUP NOTE - DIFFERENT THAN LOOKUP TABLES
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
exp <- numeric(length(years))
h <- halflife / log(2)
for (i in 1:length(years))
{
  decay <- integrate(g, lower=0, upper=i)$value
  exp[i] <- 1 - decay
}

## K = 2
k <- 2
k2 <- numeric(length(years))
h <- uniroot(gh, lower = .1, upper = halflife, tol = 1e-14)$root
for (i in 1:length(years))
{
  decay <- integrate(g, lower=0, upper=i)$value
  k2[i] <- 1 - decay
}

## K = 10
k <- 10
k10 <- numeric(length(years))
h <- uniroot(gh, lower = .1, upper = halflife, tol = 1e-14)$root
for (i in 1:length(years))
{
  decay <- integrate(g, lower=0, upper=i)$value
  k10[i] <- 1 - decay
}

## EXAMPLE
## decay end use 2, produced in 1960
product <- 1
decayexp <- product * exp
decayk2 <- product * k2
decayk10 <- product * k10
plot(decayexp,
     type = "l",
     lwd = 3,
     col = "red",
     main = "Decay of Hardwood in Multifamily Housing Built in 1960",
     ylab = "Percentage Remaining",
     xlab = "Years (Since 1960)",
     ylim = c(min(decayexp,decayk2,decayk10), 1))
lines(decayk2,
      type = "l",
      lwd = 3,
      col = "blue")
lines(decayk10,
      type = "l",
      lwd = 3,
      col = "green")
legend("topright",
       c("K=1","K=2","K=10"),
       col = c("red","blue","green"),
       lwd = 3)

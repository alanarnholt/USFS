## woodcarb package install and load
devtools::install_github('benjones2/WOODCARB3R', build_vignettes = TRUE)
library(WOODCARB3R)

## OVERALL CONTRIBUTION PLOTS
expprod <- finalCarbonContribution(Years = c(1901:2010), 
                                   approach = "Production", 
                                   decay = "Exponential",
                                   plot = FALSE)
expstock <- finalCarbonContribution(Years = c(1901:2010),
                                    approach = "Stock Change",
                                    decay = "Exponential",
                                    plot = FALSE)
k2prod <- finalCarbonContribution(Years = c(1901:2010), 
                                  approach = "Production", 
                                  decay = "K=2",
                                  plot = FALSE)
k2stock <- finalCarbonContribution(Years = c(1901:2010), 
                                   approach = "Stock Change", 
                                   decay = "K=2",
                                   plot = FALSE)
k10prod <- finalCarbonContribution(Years = c(1901:2010), 
                                   approach = "Production", 
                                   decay = "K=10",
                                   plot = FALSE)
k10stock <- finalCarbonContribution(Years = c(1901:2010), 
                                    approach = "Stock Change", 
                                    decay = "K=10",
                                    plot = FALSE)

## plots
plot(expprod,
     type = "l",
     col = "red",
     lwd = 3,
     ylim = c(min(expprod,k2prod,k10prod) - 100000, 0),
     main = "Effect of Decay Function on Carbon Sequestration",
     ylab = "Carbon Contribution (Thousand Metric Tons CO2)",
     xlab = "Years (Since 1900)")
lines(k2prod,
      type = "l",
      col = "blue",
      lwd = 3)
lines(k10prod,
      type = "l",
      col = "green",
      lwd = 3)
legend("bottomleft",
       c("exp", "K=2", "K=10"),
       col = c("red", "blue", "green"),
       lwd = 3)

plot(expstock,
     type = "l",
     col = "red",
     lwd = 2,
     ylim = c(min(expprod,k2prod,k10prod) - 100000, 0),
     main = "Stock Change Approach",
     ylab = "Carbon Contribution (Thousand Metric Tons CO2 Sequestered)",
     xlab = "Years (Since 1900)")
lines(k2stock,
      type = "l",
      col = "blue",
      lwd = 2)
lines(k10stock,
      type = "l",
      col = "green",
      lwd = 2)
legend("bottomleft",
       c("exp", "K=2", "K=10"),
       col = c("red", "blue", "green"),
       lwd = 2)



## INDIVIDUAL DECAY EXAMPLE PLOTS

## LOADING DATA
hl <- read.csv("./halflivescsv.csv") #halflives data load will need speific location and formatting

## BUILDING DECAY ARRAY BASED ON HALFLIVES DATA
decays <- 3 #decay type, 1 = exponential, 2 = k=2, 3 = k=10
enduses <- 13 
years <- 151 
decay_array <- array(0,dim=c(decays, enduses, years, years))

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
  integrate(G2(h),  lower=0, upper=hl[i,j])$value - .5 #different upper limit in functiuon definition
}

## EXPONENTIAL
k <- 1
for (i in 1:years)
{
  for (j in 1:enduses)
  {
    h <- hl[i,j] / log(2)
    for (l in 1:(years - i + 1))
    {
      decay <- integrate(g, lower=0, upper=l)$value
      decay_array[1, j, i, (i + l - 1)] <- 1 - decay
    }
  }
  print(i)
}

## K = 2
k <- 2
for (i in 1:years)
{
  for (j in 1:enduses)
  {
    h <- uniroot(gh, lower = .1, upper = hl[i,j], tol = 1e-14)$root
    for (l in 1:(years - i + 1))
    {
      decay <- integrate(g, lower=0, upper=l)$value
      decay_array[2, j, i, (i + l - 1)] <- 1 - decay
    }
  }
  print(i)
}

## K = 10
k <- 10
for (i in 1:years)
{
  for (j in 1:enduses)
  {
    h <- uniroot(gh, lower = .1, upper = hl[i,j], tol = 1e-14)$root
    for (l in 1:(years - i + 1))
    {
      decay <- integrate(g, lower=0, upper=l)$value
      decay_array[3, j, i, (i + l - 1)] <- 1 - decay
    }
  }
  print(i)
}


## EXAMPLE
## decay end use 2, produced in 1960
product <- 1
decayexp <- product * decay_array[1,1,20,c(20:115)]
decayk2 <- product * decay_array[2,1,20,c(20:115)]
decayk10 <- product * decay_array[3,1,20,c(20:115)]
plot(decayexp,
     type = "l",
     lwd = 3,
     col = "red",
     main = "Decay of Hardwood in Multifamily Housing Built in 1920",
     ylab = "Percentage Remaining",
     xlab = "Years (Since 1920)",
     ylim = c(min(decayexp,decayk2,decayk10), 1))
lines(decayk2,
      type = "l",
      lwd = 3,
      col = "blue")
lines(decayk10,
      type = "l",
      lwd = 3,
      col = "green")
legend("bottomleft",
       c("exp","K=2","K=10"),
       col = c("red","blue","green"),
       lwd = 3)

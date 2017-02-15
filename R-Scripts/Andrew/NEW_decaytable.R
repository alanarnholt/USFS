## TABLE SETUP
years <- seq(1:150)
halflives <- seq(1:300)

## FUNCTION SETUP
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
  integrate(G2(h),  lower=0, upper=i)$value - .5
}

## K = 2
k <- 2
k2_array <- array(0,dim=c(length(halflives), length(years)))

for (i in 1:length(halflives))
{
  h <- uniroot(gh, lower = .1, upper = i, tol = 1e-14)$root
  for (j in 1:length(years))
  {
    decay <- integrate(g, lower=0, upper=j)$value
    k2_array[i, j] <- 1 - decay
    print(i)
  }
}

## K = 10
k <- 10
k10_array <- array(0,dim=c(length(halflives), length(years)))

for (i in 1:length(halflives))
{
  h <- uniroot(gh, lower = .1, upper = i, tol = 1e-14)$root
  for (j in 1:length(years))
  {
    decay <- integrate(g, lower=0, upper=j)$value
    k10_array[i, j] <- 1 - decay
    print(i)
  }
}

## LOOKUP IMPLEMENT EXAMPLE
hl <- read.csv("./halflivescsv.csv") #halflives data load will need speific location and formatting
enduse <- 3
year <- 75
lookup <- round(hl[year,enduse])
product <- 100
decayk2 <- product * k2_array[lookup,]
decayk10 <- product * k10_array[lookup,]
plot(decayk2)
plot(decayk10)
##reading in half lives data
library(xlsx)
halflives <- read.xlsx("./Data/halfLives.xlsx", 1, header = FALSE)

##removing "Total" columns from half lives data
halflives <- halflives[-c(4, 9, 13)]

##creating blank four-dimensional array to fill
##look-up array:
##1. Decay Type (1 = exponential, 2 = k=2)
##2. End Use (refer to spreadsheet - will specify later)
##3. Year Put in Use (1 = 1900)
##4. Year (1 = 1900)
##Example: decay_array[1, 3, 50, 100] will produce percentage carbon left after exponential decay of a mobile home built in 1950 at year 2000
##Example: decay_array[1, 3, 50, ] will produce a vector of percentage carbon left after exponential decay of a mobile home built in 1950 for years 1950 - 2050
##Example: decay_array[1, 3, , 50] will produce a vector of percentage carbon left after exponential decay of mobile homes built in 1900 - 1950 in the year 1950
decays <- 4 
enduses <- 13 
year <- 151 
decay_array <- array(0,dim=c(decays, enduses, year, year))

##filling with decay percentages
##exponential (k=1)
for (i in 1:year)
{
  for (j in 1:enduses)
  {
    for (l in 1:i)
    {  
      n <- i
      k <- 1
      h <- halflives[l,j] / log(2)
      g <- function(x) {((x^(k-1))*(exp(-x/h)))/(gamma(k)*(h^k))}
      decay <- integrate(g, lower=l-1, upper=n)$value
      decay_array[1, j, l, i] <- 1-decay
    }
  }
}

##k=2
for (i in 1:year)
{
  for (j in 1:enduses)
  {
    for (l in 1:i)
    {  
      n <- i
      k <- 2
      h <- 
      g <- function(x) {((x^(k-1))*(exp(1)^(-x/h)))/(gamma(k)*(h^k))}
      decay <- integrate(g, lower=l-1, upper=n)$value
      decay_array[2, j, l, i] <- 1-decay
    }
  }
}

#testarray <- array(0,dim=c(5,5,5,5))

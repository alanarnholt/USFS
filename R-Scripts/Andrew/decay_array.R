##reading in half lives data
library(xlsx)
halflives <- read.xlsx("./Data/halfLives.xlsx", 1, header = FALSE)

##removing "Total" columns from half lives data
halflives <- halflives[-c(4, 9, 13)]

##creating blank four-dimensional array to fill
##look-up array:
##1. Decay Type (1 = exponential, 2 = k=2, 3 = chi-squared)
##2. End Use (refer to spreadsheet - will specify later)
##3. Year Put in Use (1 = 1900)
##4. Year (1 = 1900)
##Example: decay_array[1, 3, 51, 101] will produce percentage carbon left after exponential decay of mobile homes built in 1950 at year 2000
##Example: decay_array[1, 3, 51, ] will produce a vector of percentage carbon left after exponential decay of mobile homes built in 1950 for years 1950 - end year
##Example: decay_array[1, 3, , 101] will produce a vector of percentage carbon left after exponential decay of mobile homes built from 1900 to 2000 in the year 2000
decays <- 4 
enduses <- 13 
years <- 151 
decay_array <- array(0,dim=c(decays, enduses, years, years))

##filling with decay percentages
##exponential (k=1)
for (i in 1:years)
{
  g <- function(x) {((x^(k-1))*(exp(-x/h)))/(gamma(k)*(h^k))}
  k <- 1
  for (j in 1:enduses)
  {
    h <- halflives[i,j] / log(2)
    for (l in 1:(years - i + 1))
    {
      decay <- integrate(g, lower=0, upper=l)$value
      decay_array[1, j, i, (i + l - 1)] <- 1 - decay
    }
  }
}

##k=2
for (i in 1:years)
{
  g <- function(x) {((x^(k-1))*(exp(1)^(-x/h)))/(gamma(k)*(h^k))}
  k <- 2
  for (j in 1:enduses)
  {
    h <- 1
    decayval <- 1
    while(abs(decayval - 0.5) > 1e-14) #finding h given each half life
    {
      m <- decayval / 0.5
      h <- h * m
      decayval <- integrate(g, lower=0, upper=halflives[i,j])$value
    }
    for (l in 1:(years - i + 1))
    {  
      decay <- integrate(g, lower=0, upper=l)$value
      decay_array[2, j, i, (i + l - 1)] <- 1 - decay
    }
  }
}

##chi-squared
for (i in 1:years)
{
  g <- function(x) {((x^(k-1))*(exp(1)^(-x/h)))/(gamma(k)*(h^k))}
  h <- 2
  for (j in 1:enduses)
  {
    k <- 1.2
    decayval <- 1
    while(abs(decayval - 0.5) > 1e-1) #finding k given each half life
                                      #needs greater precision?
                                      #divergent integral?
    {
      m <- decayval / 0.5
      k <- k * m
      decayval <- integrate(g, lower=0, upper=halflives[i,j])$value
    }
    for (l in 1:(years - i + 1))
    {  
      decay <- integrate(g, lower=0, upper=l)$value
      decay_array[3, j, i, (i + l - 1)] <- 1 - decay
    }
  }
}

##example and plots
##creating test vector of product
decay_test <- rep(100, times = enduses)

##multiplying by exponential decay values for year 2016
exponential_test2016 <- decay_test * decay_array[1, c(1:enduses), 117, c(117:years)]

##multiplying by k=2 decay values for year 2016
k2_test2016 <- decay_test * decay_array[2, c(1:enduses), 117, c(117:years)]

##plots for exponential vs k=2 amount stored in mobile homes built in 2016 - end year
plot(x = c(117:years), y = exponential_test2016[3,],
     type = "l",
     col = "red",
     lwd = 3,
     main = "Decay of Mobile Homes Built in 2016",
     xlab = "Years (Since 1900)",
     ylab = "Amount of Carbon Stored")
lines(x = c(117:years), y = k2_test2016[3,],
      col = "blue",
      lwd = 3)
legend("bottomleft", c("exp", "k=2"),
       col = c("red", "blue"),
       lwd = 3)




##finding k in chi-squared work
#h <- 2
#k <- 1.2
#decayval <- 1
#while(abs(decayval - 0.5) > 0.1) #finding k for each half life
#{
#  m <- decayval / 0.5
#  k <- k * m
#  decayval <- integrate(g, lower=0, upper=halflives[i,j])$value
#}
#print(k)



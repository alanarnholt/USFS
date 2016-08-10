##following pulled from decay_array and loss_in_use scripts
library(xlsx)
halflives <- read.xlsx("./Data/halfLives.xlsx", 1, header = FALSE)
halflives <- halflives[-c(4, 9, 13)]

decays <- 4 
enduses <- 13 
years <- 151 
decay_array <- array(0,dim=c(decays, enduses, years, years))

g <- function(x) {((x^(k-1))*(exp(-x/h)))/(gamma(k)*(h^k))}
k <- 1
for (i in 1:years)
{
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

g <- function(x) {((x^(k-1))*(exp(1)^(-x/h)))/(gamma(k)*(h^k))}
k <- 2
for (i in 1:years)
{
  for (j in 1:enduses)
  {
    h <- 1
    decayval <- 1      
    while(abs(decayval - 0.5) > 1e-14) #finding h given each half life
    {
      m <- decayval * 2
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

for (i in 1:years)
{
  g <- function(x) {((x^(k-1))*(exp(1)^(-x/h)))/(gamma(k)*(h^k))}
  h <- 2
  for (j in 1:enduses)
  {
    k <- 1
    decayval <- 1
    if (halflives[i,j] >= 100) #finding k given each half life
    {  
      while(abs(decayval - 0.5) > 0.001) 
      {
        m <- decayval * 1.6
        k <- k * m
        decayval <- integrate(g, lower=0, upper=halflives[i,j])$value
      }
    }
    else if (halflives[i,j] >= 72)
    {  
      while(abs(decayval - 0.5) > 0.001) 
      {
        m <- decayval * 1.65
        k <- k * m
        decayval <- integrate(g, lower=0, upper=halflives[i,j])$value
      }
    }
    else if (halflives[i,j] >= 50)
    {  
      while(abs(decayval - 0.5) > 0.001) 
      {
        m <- decayval * 1.555
        k <- k * m
        decayval <- integrate(g, lower=0, upper=halflives[i,j])$value
      }
    }
    else if (halflives[i,j] >= 24)
    {  
      while(abs(decayval - 0.5) > 0.001) 
      {
        m <- decayval * 2.05
        k <- k * m
        decayval <- integrate(g, lower=0, upper=halflives[i,j])$value
      }
    }
    else 
    {
      while(abs(decayval - 0.5) > 0.001) 
      {
        m <- decayval * 2.5
        k <- k * m
        decayval <- integrate(g, lower=0, upper=halflives[i,j])$value
      }
    }
    for (l in 1:(years - i + 1))
    {  
      decay <- integrate(g, lower=0, upper=l)$value
      decay_array[3, j, i, (i + l - 1)] <- 1 - decay
    }
  }
}

loss_array <- array(0, dim = years)
base_value <- .09
for (i in 1:years)
{
  loss_array[i] <- 1 - base_value
  base_value <- base_value - .0001
}

##decay plots based on input values given in spreadsheet
##single family homes built in 1900
single_family1900 <- 10639742
exp_single_family1900 <- single_family1900 * decay_array[1,1,1,] * loss_array[1]
k2_single_family1900 <- single_family1900 * decay_array[2,1,1,] * loss_array[1]
chi_single_family1900 <- single_family1900 * decay_array[3,1,1,] * loss_array[1]
plot(x = c(1:years), y = exp_single_family1900,
     type = "l",
     col = "red",
     lwd = 3,
     main = "Decay of Carbon in Single Family Homes Built in 1900",
     xlab = "Years (Since 1900)",
     ylab = "Amount of Carbon in Use (Million Tonnes)",
     ylim = c(0, single_family1900))
lines(x = c(1:years), y = k2_single_family1900,
      col = "blue",
      lwd = 3)
lines(x = c(1:years), y = chi_single_family1900,
      col = "green",
      lwd = 3)
legend("bottomleft", c("exp", "k=2", "chi^2"),
       col = c("red", "blue", "green"),
       lwd = 3)

##single family homes built in 2000
single_family2000 <- 22265555
exp_single_family2000 <- single_family2000 * decay_array[1,1,101,c(101:years)] * loss_array[101]
k2_single_family2000 <- single_family2000 * decay_array[2,1,101,c(101:years)] * loss_array[101]
chi_single_family2000 <- single_family2000 * decay_array[3,1,101,c(101:years)] * loss_array[101]
plot(x = c(101:years), y = exp_single_family2000,
     type = "l",
     col = "red",
     lwd = 3,
     main = "Decay of Carbon in Single Family Homes Built in 2000",
     xlab = "Years (Since 1900)",
     ylab = "Amount of Carbon in Use (Million Tonnes)",
     ylim = c(0, single_family2000))
lines(x = c(101:years), y = k2_single_family2000,
      col = "blue",
      lwd = 3)
lines(x = c(101:years), y = chi_single_family2000,
      col = "green",
      lwd = 3)
legend("bottomleft", c("exp", "k=2", "chi^2"),
       col = c("red", "blue", "green"),
       lwd = 3)

##railcar repair in 1900
railcar_repair1900 <- 89035
exp_railcar_repair1900 <- railcar_repair1900 * decay_array[1,5,1,] * loss_array[1]
k2_railcar_repair1900 <- railcar_repair1900 * decay_array[2,5,1,] * loss_array[1]
chi_railcar_repair1900 <- railcar_repair1900 * decay_array[3,5,1,] * loss_array[1]
plot(x = c(1:years), y = exp_railcar_repair1900,
     type = "l",
     col = "red",
     lwd = 3,
     main = "Decay of Carbon in Wood Used in Railcar Repair in 1900",
     xlab = "Years (Since 1900)",
     ylab = "Amount of Carbon in Use (Million Tonnes)",
     ylim = c(0, railcar_repair1900))
lines(x = c(1:years), y = k2_railcar_repair1900,
      col = "blue",
      lwd = 3)
lines(x = c(1:years), y = chi_railcar_repair1900,
      col = "green",
      lwd = 3)
legend("bottomleft", c("exp", "k=2", "chi^2"),
       col = c("red", "blue", "green"),
       lwd = 3)

##railcar repair in 2000
railcar_repair2000 <- 91367
exp_railcar_repair2000 <- railcar_repair2000 * decay_array[1,5,101,c(101:years)] * loss_array[101]
k2_railcar_repair2000 <- railcar_repair2000 * decay_array[2,5,101,c(101:years)] * loss_array[101]
chi_railcar_repair2000 <- railcar_repair2000 * decay_array[3,5,101,c(101:years)] * loss_array[101]
plot(x = c(101:years), y = exp_railcar_repair2000,
     type = "l",
     col = "red",
     lwd = 3,
     main = "Decay of Carbon in Wood Used in Railcar Repair in 2000",
     xlab = "Years (Since 1900)",
     ylab = "Amount of Carbon in Use (Million Tonnes)",
     ylim = c(0, railcar_repair2000))
lines(x = c(101:years), y = k2_railcar_repair2000,
      col = "blue",
      lwd = 3)
lines(x = c(101:years), y = chi_railcar_repair2000,
      col = "green",
      lwd = 3)
legend("bottomleft", c("exp", "k=2", "chi^2"),
       col = c("red", "blue", "green"),
       lwd = 3)

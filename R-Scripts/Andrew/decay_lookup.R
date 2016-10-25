step = .1
halflives <- seq(from = 1, to = 300, by = step)
years <- c(1:300)
g <- function(x) {((x^(k-1))*(exp(-x/h)))/(gamma(k)*(h^k))}

##exponential
exp_array <- array(0,dim=c(length(halflives), length(years)))
k <- 1

for (i in 1:length(halflives))
{
  for (j in 1:length(years))
  {
    h <- halflives[i] / log(2)
    decay <- integrate(g, lower=0, upper=j)$value
    exp_array[i, j] <- 1 - decay
    print(i)
  }
}

write.csv(exp_array, file = "exp_lookup.csv")

##chi-squared
chi_array <- array(0,dim=c(length(halflives), length(years)))
#chi <- function(x) {((x^((n/2)-1))*(exp(-x/h)))/(gamma(n/2)*(h^(n/2)))}
h <- 2

for (i in 1:length(halflives))
{
  for (j in 1:length(years))
  {
    k <- 1
    decayval <- 1      
    while(abs(decayval - 0.5) > 1e-14) #GETS STUCK - LOWER TOLERANCE LEVEL
    {
      m <- decayval + .5
      k <- k * m
      decayval <- integrate(g, lower=0, upper=halflives[i])$value
      print(i)
      print(decayval)
      print(abs(decayval - 0.5))
    }
    decay <- integrate(g, lower=0, upper=j)$value
    chi_array[i, j] <- 1 - decay
  }
}

write.csv(chi_array, file = "chi_lookup.csv")

##k=2
k2_array <- array(0,dim=c(length(halflives), length(years)))
k <- 2

for (i in 1:length(halflives))
{
  for (j in 1:length(years))
  {
    h <- 1
    decayval <- 1      
    while(abs(decayval - 0.5) > 1e-14) 
    {
      m <- decayval * 2
      h <- h * m
      decayval <- integrate(g, lower=0, upper=halflives[i])$value
      print(i)
      print(decayval)
      print(abs(decayval - 0.5))
    }
    decay <- integrate(g, lower=0, upper=j)$value
    k2_array[i, j] <- 1 - decay
  }
}

write.csv(k2_array, file = "k2_lookup.csv")

##standard gamma
gamma_array <- array(0,dim=c(length(halflives), length(years)))
h <- 1

for (i in 1:length(halflives))
{
  for (j in 1:length(years))
  {
    k <- 1
    decayval <- 1      
    while(abs(decayval - 0.5) > 1e-14) 
    {
      m <- decayval + .5
      k <- k * m
      decayval <- integrate(g, lower=0, upper=halflives[i])$value
      print(i)
      print(decayval)
      print(abs(decayval - 0.5))
    }
    decay <- integrate(g, lower=0, upper=j)$value
    gamma_array[i, j] <- 1 - decay
  }
}

write.csv(gamma_array, file = "gamma_lookup.csv")
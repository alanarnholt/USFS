## SETUP
step = .1
halflives <- seq(from = 1, to = 150, by = step)
years <- c(1:1000)

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

## K
k <- 10
k_array <- array(0,dim=c(length(halflives), length(years)))

for (i in 1:length(halflives))
{
  h <- uniroot(gh, lower = .1, upper = i, tol = 1e-14)$root
  for (j in 1:length(years))
  {
    decay <- integrate(g, lower=0, upper=j)$value
    k_array[i, j] <- 1 - decay
    print(i)
  }
}

## exponential

exp_array <- array(0,dim=c(length(halflives), length(years)))
k <- 1

for (i in 1:length(halflives))
{
  h <- halflives[i] / log(2)
  for (j in 1:length(years))
  {
    decay <- integrate(g, lower=0, upper=j)$value
    exp_array[i, j] <- 1 - decay
    print(i)
  }
}

## plots
plot(exp_array[10,],
     type = "l",
     main = "10",
     col = "red",
     ylim = c(0,1))
lines(k_array[10,],
      type = "l",
      col = "blue")

plot(exp_array[30,],
     type = "l",
     main = "30",
     col = "red",
     ylim = c(0,1))
lines(k_array[30,],
      type = "l",
      col = "blue")

plot(exp_array[75,],
     type = "l",
     main = "75",
     col = "red",
     ylim = c(0,1))
lines(k_array[75,],
      type = "l",
      col = "blue")

plot(exp_array[120,],
     type = "l",
     main = "120",
     col = "red",
     ylim = c(0,1))
lines(k_array[120,],
      type = "l",
      col = "blue")

## tests
diff50 <- k_array[50,] - exp_array[50,]
sum(diff50)
diff25 <- k_array[25,] - exp_array[25,]
sum(diff25)
diff150 <- k_array[150,] - exp_array[150,]
sum(diff150)
#diff250 <- k_array[250,] - exp_array[250,]
#sum(diff250)

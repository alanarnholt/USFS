#testing the integration by defining gamma with brute force
n=100
k=
h=50##144.25
g<-function(x) {((x^(k-1))*(exp(1)^(-x/h)))/(gamma(k)*(h^k))}
decay<-integrate(g, lower=0, upper=n)$value;decay
#the above works!
n=100
k=.5
h=144.2695 ##144.25
g<-function(x) {((x^(k-1))*(exp(1)^(-x/h)))/(gamma(k)*(h^k))}
decay<-integrate(g, lower=0, upper=n)


#' Finds missing parameter for a Gamma Distribution. 
#'
#' HL is required. Theta and k are each optional but one must be specified.  
#'
#' @param HL halflife for gamma distribution. This value must be specified 
#' @param theta paramater for gamma distribution 
#' @param k paramater for gamma distribution  
#'
#' @return correct value for missing parameter. If theta is passed then k will be 
#' returned and vice versa. 
#' @export
#'
#' @examples
#' findKorTHETAforGamma(HL = 100, theta = 1)
#' findKorTHETAforGamma(HL = 100, k = 144.2695) 
#' system.time(findKorTHETAforGamma(HL = 100, theta = 1))
findKorTHETAforGamma <- function(halflife = 100, theta, k){
  g <- function(x){
    ((x^(theta - 1)) * (exp(-x/k))) / (gamma(theta) * (k^theta))
  }
  decayval <- 1
  if(missing(k)){
    k <- 1
    
    while(abs(decayval - 0.5) > 1e-14){
      l <- decayval / 0.5 
      k <- k * l
      
      decayval<-integrate(g, lower=0, upper=halflife)$value
    }
    return(k)
  }
  if(missing(theta)){
    theta <- 1.2
    while(abs(decayval - 0.5) > 1e-14){
      l <- decayval / 0.5 
      theta <- theta * l
      
      decayval<- integrate(g, lower=0, upper=halflife)$value
    }
    return(theta)
  }
}

#Calculates the 100 year average for a distribution, giving k and theta values, assuming x(0)=1
p<-0
for (i in 1:101){
  k=70.333
  h=1
  p[i+1]<-p[i]+1
  g<-function(x) {((x^(k-1))*(exp(1)^(-x/h)))/(gamma(k)*(h^k))}
  decay[i]<-integrate(g, lower=0, upper=p[i])$value
  fr[i]<-(1-decay[i])
}
fr
mean(fr)

#Calculating the 100 year average for each wood product class based on distributions. Again, assuming x(0)=1

#Matrix of wood product class proportions
#Each colomn represents a wood product type, and the proportion ofhalf life value within it. 
#rows go in increasing order of 1/2 lifes [6,12,30,67,70,100]
WP= matrix(
  c(0.045, 0.364, 0.033, 0.001, 0.006,
    0.201, 0.159, 0.128, 0.122, 0.271, 
    0.280, 0.322, 0.339, 0.172, 0.468, 
    0.079, 0.028, 0.090, 0.071, 0.053,
    0.031, 0.004, 0.033, 0.047, 0.019,
    0.332, 0.039, 0.334, 0.578, 0.130),
  nrow= 6,
  ncol=5,
  byrow=TRUE)
#1/2 life 100 year storage averages [6,12,30,67,70,100] for exponential distribution
e<-c(0.090738097, 0.1767409, 0.391461522, 0.623576,0.6351771,0.721604205)
Storagefactors<-c(e%*%WP)
Storagefactors

chi<-c(0.070821051, 0.1306931, 0.30855443, 0.6746007,0.7039981,0.944897391)
chi.storagefactors<-chi%*%WP
chi.storagefactors

g<-c(0.067623766,0.1272277,0.305272264,0.6715991,0.7012986,0.959620214)
gamma.storagefactors<-g%*%WP
gamma.storagefactors

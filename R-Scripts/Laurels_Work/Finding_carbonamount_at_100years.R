#testing the integration by defining gamma with bruteforce
n=100

k=2
h=41.7
g<-function(x) {((x^(k-1))*(exp(1)^(-x/h)))/(gamma(k)*(h^k))}
decay<-integrate(g, lower=0, upper=n)
#the above works!

p<-0
decay<-0
fr<-0
for (i in 1:101){
  k=2
  h=7.149
  p[1]<-0
  p[i+1]<-p[i]+1
  g<-function(x) {((x^(k-1))*(exp(1)^(-x/h)))/(gamma(k)*(h^k))}
  decay[i]<-integrate(g, lower=0, upper=p[i])$value
  fr[i]<-(1-decay[i])
}
fr

#Matrix of wood product class proportions
#Each colomn represents a wood product type, and the proportion ofhalf life value within it. 
#rows go in increasing order of 1/2 lifes [6,12,30,67,70,100]
WP= matrix(
  c(0.045, 0.364, 0.033, 0.001, 0.006,
    0.234, 0.242, 0.171, 0.131, 0.325, 
    0.280, 0.322, 0.339, 0.172, 0.468, 
    0.079, 0.028, 0.090, 0.071, 0.053,
    0.031, 0.004, 0.033, 0.047, 0.019,
    0.332, 0.039, 0.334, 0.578, 0.130),
  nrow= 6,
  ncol=5,
  byrow=TRUE)
#1/2 life 100 year storage averages [6,12,30,67,70,100] for exponential distribution.  
#Result is product class storage factors. 
e<-c(0, 0.003, 0.0992 , 0.3553,0.3714,0.5)
Storagefactors<-c(e%*%WP)
Storagefactors

chi<-c(0, 0, 0,0.00647,0.012381,0.5)
chi.storagefactors<-chi%*%WP
chi.storagefactors

g<-c(0,0,0,0.0002,0.00075,0.5)
gamma.storagefactors<-g%*%WP
gamma.storagefactors

k2<-c(0,0, 0.0245, 0.28627, 0.3088590, 0.5)
k2.storagefactors<-k2%*%WP
k2.storagefactors

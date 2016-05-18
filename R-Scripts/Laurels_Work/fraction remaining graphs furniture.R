#Fraction Remaining graphs for furniture

#exponential
p<-0
decay<-0
fre<-0
for (i in 1:101){
  k=1
  h=43.28
  p[1]<-0
  p[i+1]<-p[i]+1
  g<-function(x) {((x^(k-1))*(exp(1)^(-x/h)))/(gamma(k)*(h^k))}
  decay[i]<-integrate(g, lower=0, upper=p[i])$value
  fre[i]<-(1-decay[i])
}
fre
mean(fre)

#k2
p<-0
decay<-0
frk<-0
for (i in 1:101){
  k=2
  h=17.8747
  p[1]<-0
  p[i+1]<-p[i]+1
  g<-function(x) {((x^(k-1))*(exp(1)^(-x/h)))/(gamma(k)*(h^k))}
  decay[i]<-integrate(g, lower=0, upper=p[i])$value
  frk[i]<-(1-decay[i])
}
frk
mean(frk)

#chi squared
p<-0
decay<-0
frc<-0
for (i in 1:101){
  k=15.332
  h=2
  p[1]<-0
  p[i+1]<-p[i]+1
  g<-function(x) {((x^(k-1))*(exp(1)^(-x/h)))/(gamma(k)*(h^k))}
  decay[i]<-integrate(g, lower=0, upper=p[i])$value
  frc[i]<-(1-decay[i])
}
frc
mean(frc)

#standard gamma
p<-0
decay<-0
frs<-0
for (i in 1:101){
  k=30.3325
  h=1
  p[1]<-0
  p[i+1]<-p[i]+1
  g<-function(x) {((x^(k-1))*(exp(1)^(-x/h)))/(gamma(k)*(h^k))}
  decay[i]<-integrate(g, lower=0, upper=p[i])$value
  frs[i]<-(1-decay[i])
}
frs
mean(fr)


#plots
plot(fre, type='l', col=3, xlab="Years",ylab="Fraction Remaining of Original Product",main="Fraction remaining in each year for products with 30 year halflives")
lines(frk, type='l', col=30)
lines(frc, type='l', col=50)
lines(frs, type='l', col=100)
legend(65, 0.9, 
       c("Exponential Distribution", "K=2 distribution", "Chi-Squared distribution", "Standard Gamma Distribution"),
       col=c(3, 30, 50, 100), pch='-'
       )


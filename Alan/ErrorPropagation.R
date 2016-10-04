### Error propagation 5/23/2016
library(propagate)
EXPR1 <- expression(x ^ y)
x <- c(5, 1)
y <- c(2, 1)
DF1 <- cbind(x, y)
RES1 <- propagate(expr = EXPR1, data = DF1, type = "stat",
                  do.sim = TRUE, verbose = TRUE, 
                  alpha = 0.10, nsims = 100000)
RES1
RES1$sim

#####
# set.seed(23)
x1 <- rnorm(100000, 5, 1)
y1 <- rnorm(100000, 2, 1)
AN <- x1^y1
c(MEAN = mean(AN), SD = sd(AN), quantile(AN, probs = c(0.05, 0.95)))
RES1$sim
#####

#################
#### Inputs
M1 <- matrix(1:6, nrow = 2, byrow = TRUE)
M2 <- matrix(8:11, nrow = 2, byrow = TRUE)
M1
M2
M1[1,] * M2[1, 1]
M1[1,] * M2[1, 2]
M1[2,] * M2[2, 1]
M1[2,] * M2[2, 2]

##########################################

I <- dim(M1)[2]
J <- dim(M1)[1]
K <- dim(M2)[2]
L <- dim(M2)[1]
c(I, J, K, L)


BM <- array(0, c(L, I, K))
BM
for(l in 1:L){
  for(i in 1:I){
    for(k in 1:K){
      BM[l, i, k] <- M1[l, i]*M2[l, k]
    }
  }
}
BM
dim(BM) <- c(l, i*k)
BM

##########################################

FIJ <- function(m1, m2){
  I <- dim(m1)[2]
  J <- dim(m1)[1]
  K <- dim(m2)[2]
  L <- dim(m2)[1]
  BM <- array(0, c(L, I, K))
  for(l in 1:L){
    for(i in 1:I){
      for(k in 1:K){
        BM[l, i, k] <- M1[l, i]*M2[l, k]
      }
    }
  }
  dim(BM) <- c(l, i*k)
  BM
}


set.seed(12)
neu <- 16
nep <- 5
M1 <- t(rmultinom(21, neu, prob = rep(0.2, neu))/neu)
M1
M2 <- t(rmultinom(21, nep, prob = rep(0.5, nep))/nep)
M2

round(FIJ(M1, M2), 2)

apply(FIJ(M1, M2), 1, sum)  # Check that all are 1





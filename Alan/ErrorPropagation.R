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

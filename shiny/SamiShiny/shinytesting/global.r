swpcalcdata <- read.csv("swpcalcdata.csv")

IPCC <- read.csv("IPCCfinal.csv", sep=",")



names(IPCC) <- c("1a", "1b", "2a", "2b", "3", "4", "5", "6", "7")
IPCC <- cbind(Years = 1991:2020, IPCC)

paste("Var", names(IPCC))
names(IPCC)[1] <- "Years"

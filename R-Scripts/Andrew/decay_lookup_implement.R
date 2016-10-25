##parameters for lookup array
step <- .1
hl <- seq(from = 1, to = 300, by = step)
years <- c(1:300)

##loading and renaming lookup csv
exp_lookup <- read.csv("./R-Scripts/Andrew/exp_lookup.csv")
rownames(exp_lookup) <- hl
colnames(exp_lookup) <- years

##reading in half lives data
library(xlsx)
halflives <- read.xlsx("./Data/halfLives.xlsx", 1, header = FALSE)

##removing "Total" columns from half lives data
##halflives array format - halflives[year,enduse]
##rounding halflives
halflives <- halflives[-c(4, 9, 13)]
halflives <- round(halflives, digits = 1)

##EXAMPLE: looking up exponential decay of a half-life for END USE = 3, YEAR BUILT = 7
enduse <- 3
yearbuilt <- 7
example_decay <- exp_lookup[halflives[3,7],]

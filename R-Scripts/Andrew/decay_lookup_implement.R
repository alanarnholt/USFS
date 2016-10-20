halflives <- seq(from = 1, to = 300, by = .1)
years <- c(1:300)

exp_lookup <- read.csv("exp_lookup.csv")
rownames(exp_lookup) <- halflives
colnames(exp_lookup) <- years
##copying spreadsheet formula for "loss when placed in use"
years <- 151
base_year <- 101
base_value <- .08
increase <- -.01
year_array <- array(c(1:151), dim = years)
loss_array <- array(0, dim = years)
for (i in 1:years)
{
  loss_array[i] <- base_value + (year_array[i] - base_year) * increase
}
##gives different values than spreadsheet?

##assuming linear decrease in "loss when placed in use", starting with .09 (9.0%) at year 1900, as implied by spreadsheet
loss_array <- array(0, dim = years)
base_value <- .09
for (i in 1:years)
{
  loss_array[i] <- 1 - base_value
  base_value <- base_value - .0001
}

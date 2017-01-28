dfyears = data.frame(yr1990,yr2000,yr2010)
for (i in 1:2000)
{
  errors = rnorm(13,1,.1)
  dfyears[i,]=finalCarbon(c(1990,2000,2010),fsawn=mapply("*",fracsawnwood,errors))
}
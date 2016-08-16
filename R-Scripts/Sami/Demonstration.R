############  Plot demonstration ###################
########### Graphing variable 2A the production approach

### This can be done once the variables are coded and the data is added. 
### Intermediate steps in the process can also be plotted

############
### These are the packages that are used to create the plots
###########
library(tidyr)
library(ggplot2)
library(grid)

##################
## The year range we are working with
years<-1901:2020
###########
HWPinuseStockChange <- numeric(120)
##### Numeric creates a vector of 120 
for (i in years){
  HWPinuseStockChange[i-1900] <- (Var2_C_SWP_STOCKCHANGE(i) + Var2_C_PAPER_STOCKCHANGE(i))
}
###########
HWPinswdsStockChange <- numeric(120)
for(i in years){
  HWPinswdsStockChange[i-1900] <- C_SWP_StockChange_LFDumps(i) + C_PAPER_StockChange_LFDumps(i)
}
############
totalchangeStockChange <- numeric(120)
for (i in years){
  totalchangeStockChange[i-1900] <- HWPinswdsStockChange[i-1900] + HWPinuseStockChange[i-1900]
}
############
hwpproddata <- data.frame(cbind(years,HWPinuseStockChange,HWPinswdsStockChange,totalchangeStockChange), row.names = NULL)
###### cbind combines the columns and they are made into a data frame
hwpproddata_long <- gather(hwpproddata, stockchanges, carbon, HWPinuseStockChange:totalchangeStockChange)
###### gather takes columns, used then there aren't variables   


HWPstockChangeGraph <- ggplot(hwpproddata_long, aes(x=years, y=carbon)) +
  theme_gray() +
  geom_line(aes(colour=stockchanges, group = stockchanges)) +
  xlab("Years") + ylab("Tg C/yr") +
  
  ggtitle("Stock Changes in US Carbon, Production Approach") +
  scale_colour_manual(values=c("#CC79A7", "#E69F00", "#56B4E9"),
                      breaks=c("HWPinuseStockChange", "HWPinswdsStockChange", "totalchangeStockChange"),
                      labels=c("In Use", "SWDS", "Total")
  ) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.position = c(0.14, 0.83),
        legend.title=element_blank())


HWPstockChangeGraph





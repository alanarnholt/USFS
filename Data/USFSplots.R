
library(tidyr)
library(ggplot2)
library(grid)
years<-1901:2020
###########
HWPinuseStockChange <- numeric(120)
for (i in 1901:2020){
  HWPinuseStockChange[i-1900] <- (C_SWP_STOCKCHANGE(i) + C_PAPER_STOCKCHANGE(i))
}
###########
HWPinswdsStockChange <- numeric(120)
for(i in 1901:2020){
  HWPinswdsStockChange[i-1900] <- C_SWP_StockChange_LFDumps(i) + C_PAPER_StockChange_LFDumps(i)
}
############
totalchangeStockChange <- numeric(120)
for (i in 1901:2020){
  totalchangeStockChange[i-1900] <- HWPinswdsStockChange[i-1900] + HWPinuseStockChange[i-1900]
}
############
hwpproddata <- data.frame(cbind(years,HWPinuseStockChange,HWPinswdsStockChange,totalchangeStockChange), row.names = NULL)
hwpproddata_long <- gather(hwpproddata, stockchanges, carbon, HWPinuseStockChange:totalchangeStockChange)

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
 


HWPinuseStock <- numeric(120)
for (i in 1901:2020){
  HWPinuseStock[i-1900] <- (totalC_SWP(i) * PRO17 + Calc_CA(i))
}
###########
HWPinswdsStock <- numeric(120)
for(i in 1901:2020){
  HWPinswdsStock[i-1900] <- Calc_CL(i) + Calc_CT(i) + Calc_CN(i) + Calc_CV(i) + Dumps_O(i) + Dumps_T(i)
}
############
totalStock <- numeric(120)
for (i in 1901:2020){
  totalStock[i-1900] <- HWPinswdsStock[i-1900] + HWPinuseStock[i-1900]
}
############
hwpproddataStock <- data.frame(cbind(years,HWPinuseStock,HWPinswdsStock,totalStock), row.names = NULL)
library(tidyr)
hwpproddataStock_long <- gather(hwpproddataStock, stock, carbon, HWPinuseStock:totalStock)
hwpproddataStock_long
HWPstockGraph <- ggplot(hwpproddataStock_long, aes(x=years, y=carbon)) +
  theme_gray() +
  geom_line(aes(colour=stock, group = stock)) +
  xlab("Years") + ylab("Tg C") +
  ggtitle("US Carbon Stock, Production Approach") +
  scale_colour_manual(values=c("#CC79A7", "#E69F00", "#56B4E9"),
                      breaks=c("HWPinuseStock", "HWPinswdsStock", "totalStock"),
                      labels=c("In Use", "SWDS", "Total")
  ) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.position = c(0.14, 0.83),
        legend.title=element_blank()) 
  HWPstockGraph +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=-.5))


swcalclmn <- read.xlsx("swcalcLMN.xlsx", 1,header=FALSE)
colnames(swcalclmn) <- c("sf","mf","mobile")
swcalclmn_long <- gather(swcalclmn, enduses, wood, sf:mobile)
years2050 <- 1900:2050
swcalclmn_long <- cbind(years2050,swcalclmn_long) 
swcalclmn_long[,3] <- swcalclmn_long[,3] / 10e5

woodHomeGraph <- ggplot(swcalclmn_long, aes(x=years2050, y=wood)) +
  theme_gray() +
  ##geom_line(aes(colour=enduses, group=enduses)) +
  
  xlab("Years") + ylab("million oven-dry tons ") +
  ggtitle("Wood in US Homes") +
  geom_area(aes(fill=enduses))+
    scale_colour_manual(values=c("#CC79A7", "#E69F00", "#56B4E9")
                        
    ) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.position = c(0.14, 0.83),
        legend.title=element_blank()) 

multiplot(HWPstockChangeGraph, HWPstockGraph, woodHomeGraph, cols=3)
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#essentially the format of the matrices
# inflow.year<- subset(swcalcdata, Years=="1900")



yearsneeded<-1900:2020


Importdatalooped<-inflow.year[,c(-2,-3,-4, -6, -7, -8, -10,-11,-12)];Importdatalooped

for (i in 2:length(yearsneeded)){
  oneyear<-subset(swcalcdata, Years == yearsneeded[[i]])
  oneyear<-oneyear[,c(-2,-3,-4, -6, -7, -8, -10,-11,-12)]
  Importdatalooped<-rbind(Importdatalooped, oneyear)
}

Importdatalooped<-t(Importdatalooped)
# rownames(Importdatalooped)<-NULL  #if you do not want the rownames listed, use this line
View(Importdatalooped)







Exportdatalooped<-inflow.year[,c(-2,-3,-5,-6,-7,-9,-10,-11,-13)];Exportdatalooped

for (i in 2:length(yearsneeded)){
  oneyear<-subset(swcalcdata, Years == yearsneeded[[i]])
  oneyear<-oneyear[,c(-2,-3,-5,-6,-7,-9,-10,-11,-13)]
  Exportdatalooped<-rbind(Exportdatalooped, oneyear)
}

Exportdatalooped<-t(Exportdatalooped)
# rownames(Exportdatalooped)<-NULL
View(Exportdatalooped)







Productiondatalooped<-inflow.year[,c(-2,-4,-5,-6,-8,-9,-10,-12,-13)];Productiondatalooped

for (i in 2:length(yearsneeded)){
  oneyear<-subset(swcalcdata, Years == yearsneeded[[i]])
  oneyear<-oneyear[,c(-2,-4,-5,-6,-8,-9,-10,-12,-13)]
  Productiondatalooped<-rbind(Productiondatalooped, oneyear)
}

Productiondatalooped<-t(Productiondatalooped)
# rownames(Productiondatalooped)<-NULL
View(Productiondatalooped)




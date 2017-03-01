setwd("~/USFS/R-Scripts/Hannah")

#Reading in all CSV files
Histdfa<-read.csv("Histdfa.csv")
Histdfb<-read.csv("Histdfb.csv")
Histdfc<-read.csv("Histdfc.csv")

Histdf2a<-read.csv("Histdf2a.csv")
Histdf2b<-read.csv("Histdf2b.csv")
Histdf2c<-read.csv("Histdf2c.csv")

Histdf3a<-read.csv("Histdf3a.csv")
Histdf3b<-read.csv("Histdf3b.csv")
Histdf3c<-read.csv("Histdf3c.csv")

Histdf4a<-read.csv("Histdf4a.csv")
Histdf4b<-read.csv("Histdf4b.csv")
Histdf4c<-read.csv("Histdf4c.csv")

Histdf5a<-read.csv("Histdf5a.csv")
Histdf5b<-read.csv("Histdf5b.csv")
Histdf5c<-read.csv("Histdf5c.csv")

Histdf6a<-read.csv("Histdf6a.csv")
Histdf6b<-read.csv("Histdf6b.csv")
Histdf6c<-read.csv("Histdf6c.csv")

Histdf7a<-read.csv("Histdf7a.csv")
Histdf7b<-read.csv("Histdf7b.csv")
Histdf7c<-read.csv("Histdf7c.csv")

Histdf8a<-read.csv("Histdf8a.csv")
Histdf8b<-read.csv("Histdf8b.csv")
Histdf8c<-read.csv("Histdf8c.csv")

Histdf8a<-read.csv("Histdf8a.csv")
Histdf8b<-read.csv("Histdf8b.csv")
Histdf8c<-read.csv("Histdf8c.csv")

Histdf9a<-read.csv("Histdf9a.csv")
Histdf9b<-read.csv("Histdf9b.csv")
Histdf9c<-read.csv("Histdf9c.csv")

Histdf10a<-read.csv("Histdf10a.csv")
Histdf10b<-read.csv("Histdf10b.csv")
Histdf10c<-read.csv("Histdf10c.csv")

Histdf11a<-read.csv("Histdf11a.csv")
Histdf11b<-read.csv("Histdf11b.csv")
Histdf11c<-read.csv("Histdf11c.csv")

Histdf12a<-read.csv("Histdf12a.csv")
Histdf12b<-read.csv("Histdf12b.csv")
Histdf12c<-read.csv("Histdf12c.csv")

Histdf13a<-read.csv("Histdf13a.csv")
Histdf13b<-read.csv("Histdf13b.csv")
Histdf13c<-read.csv("Histdf13c.csv")

Histdf14a<-read.csv("Histdf14a.csv")
Histdf14b<-read.csv("Histdf14b.csv")
Histdf14c<-read.csv("Histdf14c.csv")


plot(density(as.numeric(Histdf6b)), ylim=c(0,.029), xlim=c(-113500,-112500), main = "Densities of Halflives", xlab = "Carbon Contribution")

lines(density(as.numeric(Histdfb)), col="black")
lines(density(as.numeric(Histdf2b)), col="red")
lines(density(as.numeric(Histdf3b)), col="orange")
lines(density(as.numeric(Histdf4b)), col="yellow")
lines(density(as.numeric(Histdf5b)), col="green")
lines(density(as.numeric(Histdf6b)), col="blue")
lines(density(as.numeric(Histdf7b)), col="purple")
lines(density(as.numeric(Histdf8b)), col="pink")
lines(density(as.numeric(Histdf9b)), col="brown")
lines(density(as.numeric(Histdf10b)), col="chocolate3")
lines(density(as.numeric(Histdf11b)), col="skyblue")
lines(density(as.numeric(Histdf12b)), col="tan2")
lines(density(as.numeric(Histdf13b)), col="seagreen")
lines(density(as.numeric(Histdf14b)), col="rosybrown")
legend("topright", legend=c("Variable 1","Variable 2","Variable 3","Variable 4","Variable 5",
                            "Variable 6","Variable 7","Variable 8","Variable 9","Variable 10",
                            "Variable 11","Variable 12","Variable 13", "Variable 14"), 
       col = c("black","red", "orange", "yellow", "green", "blue", "purple", "pink",
               "brown", "chocolate3", "skyblue", "tan2", "seagreen", "rosybrown"),
       cex=.8, pch=8, pt.cex = 1)


#Variance Table
VarianceTable<-t(as.matrix(c(var(t(Histdfa)),var(t(Histdfb)),var(t(Histdfc)))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf2a)),var(t(Histdf2b)),var(t(Histdf2c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf3a)),var(t(Histdf3b)),var(t(Histdf3c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf4a)),var(t(Histdf4b)),var(t(Histdf4c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf5a)),var(t(Histdf5b)),var(t(Histdf5c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf6a)),var(t(Histdf6b)),var(t(Histdf6c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf7a)),var(t(Histdf7b)),var(t(Histdf7c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf8a)),var(t(Histdf8b)),var(t(Histdf8c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf9a)),var(t(Histdf9b)),var(t(Histdf9c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf10a)),var(t(Histdf10b)),var(t(Histdf10c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf11a)),var(t(Histdf11b)),var(t(Histdf11c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf12a)),var(t(Histdf12b)),var(t(Histdf12c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf13a)),var(t(Histdf13b)),var(t(Histdf13c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf14a)),var(t(Histdf14b)),var(t(Histdf14c))))))

colnames(VarianceTable)<-c(1990,2000,2010)
rownames(VarianceTable)<-c("Variable 1","Variable 2","Variable 3","Variable 4","Variable 5",
                           "Variable 6","Variable 7","Variable 8","Variable 9","Variable 10",
                           "Variable 11","Variable 12","Variable 13","Variable 14")



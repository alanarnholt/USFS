
#essentially the format of the matrices
inflow.year<- subset(swcalcdata, Years=="1900")



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





#--Predictions for years 2003 to2050--#
plot(Productiondatalooped["Years",], Productiondatalooped["Structural Panel Production",])
plot(Productiondatalooped["Years",], Productiondatalooped["Nonstructural Panels Production",])
plot(Productiondatalooped["Years",], Productiondatalooped["Sawn Wood Production",])

plot(Importdatalooped["Years",], Importdatalooped["Structural Panel Imports",])
plot(Importdatalooped["Years",], Importdatalooped["Nonstructural Panels Imports",])
plot(Importdatalooped["Years",], Importdatalooped["Sawn Wood Imports",])

plot(Exportdatalooped["Years",], Exportdatalooped["Structural Panel Exports",])
plot(Exportdatalooped["Years",], Exportdatalooped["Nonstructural Panels Exports",])
plot(Exportdatalooped["Years",], Exportdatalooped["Sawn Wood Exports",])


Imported<-as.data.frame(t(Importdatalooped))
Produced<-as.data.frame(t(Productiondatalooped))
Exported<-as.data.frame(t(Exportdatalooped))

colnames(Imported)<-gsub(" ","_",colnames(Imported))
colnames(Produced)<-gsub(" ","_",colnames(Produced))
colnames(Exported)<-gsub(" ","_",colnames(Exported))
# View(Imported)



library(ISLR)

#Linear
lmimports.fit1 <- lm(Structural_Panel_Imports ~ Years, data = Imported);summary(lmimports.fit1)
lmimports.fit2 <- lm(Nonstructural_Panels_Imports ~ Years, data = Imported);summary(lmimports.fit2)
lmimports.fit3 <- lm(Sawn_Wood_Imports ~ Years, data = Imported);summary(lmimports.fit3)

lmprod.fit1 <- lm(Structural_Panel_Production ~ Years, data = Produced);summary(lmprod.fit1)
lmprod.fit2 <- lm(Nonstructural_Panels_Production ~ Years, data = Produced);summary(lmprod.fit2)
lmprod.fit3 <- lm(Sawn_Wood_Production ~ Years, data = Produced);summary(lmprod.fit3)

lmexports.fit1 <- lm(Structural_Panel_Exports ~ Years, data = Exported);summary(lmexports.fit1)
lmexports.fit2 <- lm(Nonstructural_Panels_Exports ~ Years, data = Exported);summary(lmexports.fit2)
lmexports.fit3 <- lm(Sawn_Wood_Exports ~ Years, data = Exported);summary(lmexports.fit3)



#Exponential
# expimports.fit1 <- lm(log(Structural_Panel Imports) ~ log(Years), data = Imported);summary(expimports.fit1)
# expimports.fit2 <- lm(log(Nonstructural_Panels_Imports) ~ log(Years), data = Imported);summary(expimports.fit2)
expimports.fit3 <- lm(log(Sawn_Wood_Imports) ~ log(Years), data = Imported);summary(expimports.fit3)

# expprod.fit1 <- lm(log(Structural_Panel_Production) ~ log(Years), data = Produced);summary(expprod.fit1)
expprod.fit2 <- lm(log(Nonstructural_Panels_Production) ~ log(Years), data = Produced);summary(expprod.fit2)
expprod.fit3 <- lm(log(Sawn_Wood_Production) ~ log(Years), data = Produced);summary(expprod.fit3)

# expexports.fit1 <- lm(log(Structural_Panel_Exports) ~ log(Years), data = Exported);summary(expexports.fit1)
# expexports.fit2 <- lm(log(Nonstructural_Panels_Exports) ~ log(Years), data = Exported);summary(expexports.fit2)
expexports.fit3 <- lm(log(Sawn_Wood_Exports) ~ log(Years), data = Exported);summary(expexports.fit3)


#Linear Model
plot(Structural_Panel_Imports ~ Years, data = Imported)
abline(lmimports.fit1)
plot(Nonstructural_Panels_Imports ~ Years, data = Imported)
abline(lmimports.fit2)
plot(Sawn_Wood_Imports ~ Years, data = Imported)
abline(lmimports.fit3)



plot(Structural_Panel_Exports ~ Years, data = Exported)
abline(lmexports.fit1)
plot(Nonstructural_Panels_Exports ~ Years, data = Exported)
abline(lmexports.fit2)
plot(Sawn_Wood_Exports ~ Years, data = Exported)
abline(lmexports.fit3)



plot(Structural_Panel_Production ~ Years, data = Produced)
abline(lmprod.fit1)
plot(Nonstructural_Panels_Production ~ Years, data = Produced)
abline(lmprod.fit2)
plot(Sawn_Wood_Production ~ Years, data = Produced)
abline(lmprod.fit3)




#Exponential Model
# plot(log(Structural_Panel_Imports) ~ log(Years), data = Imported)
# abline(expimports.fit1)
# plot(log(Nonstructural_Panels_Imports) ~ log(Years), data = Imported)
# abline(expimports.fit2)
plot(log(Sawn_Wood_Imports) ~ log(Years), data = Imported)
abline(expimports.fit3)



# plot(log(Structural_Panel_Production) ~ log(Years), data = Produced)
# abline(expprod.fit1)
plot(log(Nonstructural_Panels_Production) ~ log(Years), data = Produced)
abline(expprod.fit2)
plot(log(Sawn_Wood_Production) ~ log(Years), data = Produced)
abline(expprod.fit3)



# plot(log(Structural_Panel_Exports) ~ log(Years), data = Exported)
# abline(expexports.fit1)
# plot(log(Nonstructural_Panels_Exports) ~ log(Years), data = Exported)
# abline(expexports.fit2)
plot(log(Sawn_Wood_Exports) ~ log(Years), data = Exported)
abline(expexports.fit3)



importsss <- lm(Structural_Panel_Imports ~ Years, data = Imported);importsss #correct way to setup model

predict(importsss, newdata = data.frame(Years=2050), interval = "pred") #correct way to ask for prediction



#open sami's Var1a_SWCalc.R
#open ben's equationsProdSWP.R


fpred<-function(y,m,item){
  lmimports.fit1 <- lm(Structural_Panel_Imports ~ Years, data = Imported);summary(lmimports.fit1)
  lmimports.fit2 <- lm(Nonstructural_Panels_Imports ~ Years, data = Imported);summary(lmimports.fit2)
  lmimports.fit3 <- lm(Sawn_Wood_Imports ~ Years, data = Imported);summary(lmimports.fit3)
  
  lmprod.fit1 <- lm(Structural_Panel_Production ~ Years, data = Produced);summary(lmprod.fit1)
  lmprod.fit2 <- lm(Nonstructural_Panels_Production ~ Years, data = Produced);summary(lmprod.fit2)
  lmprod.fit3 <- lm(Sawn_Wood_Production ~ Years, data = Produced);summary(lmprod.fit3)
  
  lmexports.fit1 <- lm(Structural_Panel_Exports ~ Years, data = Exported);summary(lmexports.fit1)
  lmexports.fit2 <- lm(Nonstructural_Panels_Exports ~ Years, data = Exported);summary(lmexports.fit2)
  lmexports.fit3 <- lm(Sawn_Wood_Exports ~ Years, data = Exported);summary(lmexports.fit3)
  
  
  expimports.fit3 <- lm(log(Sawn_Wood_Imports) ~ log(Years), data = Imported);summary(expimports.fit3)
  
  expprod.fit2 <- lm(log(Nonstructural_Panels_Production) ~ log(Years), data = Produced);summary(expprod.fit2)
  expprod.fit3 <- lm(log(Sawn_Wood_Production) ~ log(Years), data = Produced);summary(expprod.fit3)
  
  expexports.fit3 <- lm(log(Sawn_Wood_Exports) ~ log(Years), data = Exported);summary(expexports.fit3)
  
  if(m == "exponential" && item == "import"){
    
    print("Sawn_Wood_Imports");print(predict(expimports.fit3, newdata = data.frame(Years = y), interval = "pred"))
  }
  
  
  
  if(m == "exponential" && item == "prod"){
   
    print("Nonstructural_Panels_Production");print((predict(expprod.fit2, newdata = data.frame(Years = 2000), interval = "pred")));
    print("Sawn_Wood_Production");print((predict(expprod.fit3, newdata = data.frame(Years = 2000), interval = "pred")))
  }
  
  
  
  if(m == "exponential" && item == "export"){
  
    print("Sawn_Wood_Imports");print(predict(expexports.fit3, newdata = data.frame(Years = y), interval = "pred"))
  }
  
  
  
  if(m == "linear" && item == "import"){
    
    print("Structural_Panel_Imports");print(predict(lmimports.fit1, newdata = data.frame(Years = y), interval = "pred"))
    print("Nonstructural_Panels_Imports");print(predict(lmimports.fit2, newdata = data.frame(Years = y), interval = "pred"))
    print("Sawn_Wood_Imports");print(predict(lmimports.fit3, newdata = data.frame(Years = y), interval = "pred"))
  }
 

   
  if(m == "linear" && item == "prod"){
    
    print("Structural_Panel_Imports");print(predict(lmprod.fit1, newdata = data.frame(Years = y), interval = "pred"));
    print("Nonstructural_Panels_Imports");print(predict(lmprod.fit2, newdata = data.frame(Years = y), interval = "pred"));
    print("Sawn_Wood_Imports");print(predict(lmprod.fit3, newdata = data.frame(Years = y), interval = "pred"))
  }
  
  
  
  if(m == "linear" && item == "export"){

    print("Structural_Panel_Imports");print(predict(lmexports.fit1, newdata = data.frame(Years = y), interval = "pred"));
    print("Nonstructural_Panels_Imports");print(predict(lmexports.fit2, newdata = data.frame(Years = y), interval = "pred"));
    print("Sawn_Wood_Imports");print(predict(lmexports.fit3, newdata = data.frame(Years = y), interval = "pred"))
  }
}

fpred(2030,"exponential","prod")
fpred(2050,"exponential","import")
fpred(2050,"linear","import")









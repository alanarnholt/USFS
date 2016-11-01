library(WOODCARB3R)

library(magrittr)    # load the %>% operator
library(simputation)

#first imputation will be on the total carbon contribution
#Production approach is the same as carbon contribution 
#no script for atmospheric approach?
#making an overall table for variables
production<-data.table(finalCarbonContribution(Years=1990:2015, approach="Production"))
production

stockchange<- data.table(finalCarbonContribution(Years=1990:2015, approach="Stock Change"))
stockchange

#the first two combined
data.frame(c(production, stockchange))

#tables for first four final variables
#these are used to calculate stockchange and production
Var1A<- data.table(finalVariables(Years = 1990:2015, Variable ="Var1A"))
Var1A
Var1B<- data.table(finalVariables(Years = 1990:2015, Variable ="Var1B"))
Var1B
Var2A<- data.table(finalVariables(Years = 1990:2015, Variable ="Var2A"))
Var2A
Var2B<- data.table(finalVariables(Years = 1990:2015, Variable ="Var2B"))
Var2B

#below is the table to begin with. these values will be maniuplated
starttable<-data.frame(c(production, stockchange,Var1A,Var1B,Var2A,Var2B))
starttable
#V1=HWP Contribution to AFOLU emissions/ removals (Production Approach)
#V1.1=Stockchange approach
#V1.2=Var1A....Annual Change in stock of HWP in use from consumption           
#V1.3=Var1B...Annual Change in stock of HWP in SWDS from consumption           
#V1.4=Var2A...Annual Change in stock of HWP in use produced from domestic harvest                      
#V1.5=Var2B...Annual Change in stock of HWP in SWDS produced from domestic harvest                      

#empty some fields
#will do 2006-2009 for production and 1B and 2A
#choose 2, then regress on the other 2
starttable[17:20,1]<-starttable[17:20,4]<-starttable[17:20,5] <- NA 
prodimp<-head(starttable,26)
prodimp

imputed <- prodimp %>%  #Using the %>% operator from the popular magrittr allows for a very compact specification
  impute_lm(V1.3 + V1.4 ~ V1.2 + V1.5 ) %>% #first Var1B and Var2A by regression on Var1A and Var2B.
  impute_cart(V1 ~ .) #imputes V1(production) using a decision tree model (CART) using every other variable as a predictor (including the ones just imputed).
head(imputed,26)

#make observations
#do other methods
#change variables around



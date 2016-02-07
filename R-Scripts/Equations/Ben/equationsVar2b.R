####
###return stock change from paper in dumps
C_PAPER_StockChange_LFDumps <- function(y){
  return(Calc_CS(y)+Calc_CX(y)+Dumps_V(y))
}

##########
Calc_CS <- function(y){
  return((1-PRM46)*Calc_CI(y))
}
Calc_CI <- function(y){
  return(ParamResults_P(y)*Calc_CB(y))
}
Calc_CB <- function(y){
  if (y == 1900){
    return(Calc_BU(y) - Calc_CA(y))
  }
  else{
    return(Calc_CA(y-1)+Calc_BU(y)-Calc_CA(y))
  }
}
Calc_CA <- function(y){
  if (y == 1900){
    return(exp(-log(2)/PRP10)*(Calc_BU(y)))
  }
  else{
    return(exp(-log(2)/PRP10)*(Calc_BU(y)+Calc_CA(y-1)))
  }
}
ParamResults_P <- function(y){
  return(ParamResults_V(y) * PRI96)
}
ParamResults_V <- function(y){
  return(paperToLF(y))
}
##############
Calc_CX <- function(y){
  if(y == 1900){
    return(0)
  }
  else{
    return(Calc_CV(y) - Calc_CV(y-1))
  }
}

###########
###########
Calc_CU <- function(y){
  return(PRM46*Calc_CI(y))
}
CVtable <- numeric(121)
for(i in 1900:2020){
  if (i==1900)
    CVtable[1] <-  (1/(1+PRM51))*Calc_CU(1900)
  else
    CVtable[i-1899] <- (1/(1+PRM51))*(CVtable[i-1900]+Calc_CU(i))
}
Calc_CV <- function(y){
  return(CVtable[y-1899])
}

###########
Dumps_V <- function(y){
  if(y == 1900){
    return(Dumps_T(y))
  }
  else{
    return(Dumps_T(y)-Dumps_T(y-1))
  }
}
#########
#########
###P+R 'O200' column
ParamResults_O <- function(y){
  ######woodTolandfills is data linked to another site, not sure how it's calculated. 
  return(woodToLF(y) * PRJ96)
}
####
ParamResults_Q <- function(y){
  return(woodDumps(y))######Calculated from a linked site.
}
###########
Dumps_S <- function(y){
  return(PRM57*Calc_CB(y)*ParamResults_Q(y))
}
dumpTtable <- numeric(121)
for(i in 1900:2020){
  if (i==1900)
    dumpTtable[1] <-  (1/(1+PRM61))*Dumps_S(i)
  else
    dumpTtable[i-1899] <- (1/(1+PRM61))*(dumpTtable[i-1900]+Dumps_S(i))
}
Dumps_T <- function(y){
  return(dumpTtable[y-1899])
}
#########


 ########################################
########################################
########################################
#####################################
#####################################
C_SWP_StockChange_LFDumps <- function(y){
  if (y == 1900){
    return((1-PRM45)*ParamResults_O(y)*totalC_Output(y) * PRO17 + (Calc_CM(y)) + Dumps_O(y))
  }
  else{
    return((1-PRM45)*ParamResults_O(y)*totalC_Output(y) * PRO17 + (Calc_CN(y) - Calc_CN(y-1)) + (Dumps_O(y) - Dumps_O(y-1)))
  }
}
##########

############
#########returns total carbon stored 
carbonStored <- function(y){
  return(Var2_totalC_SWP(y) - pre1900(y))
}
##############################
#######returns the carbon stored in a year, from that year. 
carbonStoredSameYear <- function(y){
  totalcarbon <- 0
  for (i in 1:16){
    if (i == 4 || i == 9 || i == 13){
      totalcarbon <- totalcarbon+0
    }
    else{
      totalcarbon <- totalcarbon + (Var2_c_placed_IU(y,i)*(exp(-log(2)/HL(y,i)*((0)+1)))*(1-iuLoss(y,i)))
  }
  return(totalcarbon)
}
############################
###returns the carbon placed in use for a given year for all end uses
C_placed_Iu_total <- function(y){
  totalcarbon <- 0
  for (i in 1:16){
    if (i == 4 || i == 9 || i == 13){
      totalcarbon <- totalcarbon+0
    }
    else{
      totalcarbon <- totalcarbon + Var2_c_placed_IU(y,i)
    }
    
  }
  return(totalcarbon)
}
###Calculates total carbon outputted for year y in od tons
totalC_Output <- function(y){
  if (y == 1900){
    return(C_placed_Iu_total(y) - carbonStored(1900))
  }
  else{
    return(carbonStored(y-1)-(carbonStored(y)-carbonStoredSameYear(y)) + (C_placed_Iu_total(y)-carbonStoredSameYear(y))+(pre1900(y-1)-pre1900(y)))
  }
}
##########
Calc_CM <- function(y){
  return(PRM45 * ParamResults_O(y) * totalC_Output(y) * PRO17)
}
################
CNtable <- numeric(121)
for(i in 1900:2020){
  if (i==1900)
    CNtable[1] <-  (1/(1+PRM50))*Calc_CM(i)
  else
    CNtable[i-1899] <- (1/(1+PRM50))*(CNtable[i-1900]+Calc_CM(i))
}
Calc_CN <- function(y){
  return(CNtable[y-1899])
  
}
###########
###########
Dumps_N <- function(y){
  return(PRM57*totalC_Output(y) * PRO17*ParamResults_Q(y))
}

############
DumpsOtable <- numeric(121)
for(i in 1900:2020){
  if (i==1900)
    DumpsOtable[1] <-  (1/(1+PRM60))*Dumps_N(i)
  else
    DumpsOtable[i-1899] <- (1/(1+PRM60))*(DumpsOtable[i-1900]+Dumps_N(i))
}
Dumps_O <- function(y){
  return(DumpsOtable[y-1899])
}
#############

Calc_BX <- function(year){
  return(totalC_Output(year)*PRO17)
}
Calc_CH <- function(year){
  return(ParamResults_O(year)*Calc_BX(year))
}
Calc_CK <- function(year){
  return((1-PRM45)*Calc_CH(year))
}

CalcCLtable <- numeric(121)
for (i in 1900:2020){
  if(i ==1900)
    CalcCLtable[1] <- Calc_CK(i)
  else
    CalcCLtable[i-1899] <- CalcCLtable[i-1900] + Calc_CK(i)
}
Calc_CL <- function(year){
  return(CalcCLtable[year-1899])
}
###############

CalcCTtable <- numeric(121   )
for(i in 1900:2020){
  if(i == 1900)
    CalcCTtable[1] <- Calc_CS(i)
  else
    CalcCTtable[i-1899] <- CalcCTtable[i-1900] + Calc_CS(i)
}
Calc_CT <- function(year){
  return(CalcCTtable[year-1899])
}

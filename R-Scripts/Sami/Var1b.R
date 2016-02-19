

Total_paper_stock_change_LD<- function(y){
  return(Calc_AG(y)+Calc_AL(y)+Dumps_K(y))
}

Calc_AL<-function(y){
  if (y<1901){
    return(Calc_AJ(y))
  }
  if (y>1900){
    return(Calc_AJ(y)-Calc_AJ(y-1))
  }
}

Calc_AJ<-function(y){
  return(PRM46*Calc_R(y))
}

Dumps_K<-function(y){
  if(y<1901){
    return(Dumps_I(y))
  }
  if(y>1900){
    return(Dumps_I(y)-Dumps_I(y-1))
  }
}

Calc_AG<-function(y){
  return((1-PRM46)*Calc_R(y))
}

Calc_R<-function(y){
  return(Calc_K(y)*paperToLF(y)*PRI96)
}


Dumps_I<-function(y){
  return((1/(1+PRM61))*Dumps_H(y))
}

Dumps_H<-function(y){
  return(PRM57*Calc_K(y)*woodDumps(y))
}

Calc_K<-function(y){
  if (y<1901){
    return(Calc_D(y)-Calc_J(y))
  }
  if(y>1900){
    return(Calc_J(y-1)+Calc_D(y)-Calc_J(y))
  }
}

Calc_J<- function(y){
  if (y<1901){
    return((exp(-log(2)/PRP10))*(Calc_D(y)))
  }
  if (y>1900){
    return((exp(-log(2)/PRP10))*(Calc_D(y)+Calc_J(y-1)))
  }
}

Calc_D<-function(y){
  return((PRO18*usa_AB(y)*usa_V(y)))
}


Total_stock_change_LD<-function(y){
  return(CalcAA(y)+CalcV(y)+Dumps_F(y))
}

CalcAA<-function(y){
  return(CalcY(y)-CalcY(y-1))
}


CalcV<- function(y){
  return((1-PRM45)*CalcQ(y))
}


CalcY<- function(y){
  if(y<1901){
    return(0.0)
  }
  if(y>1900){
    return((1/(1+PRM50))*(CalcY(y-1)+CalcX(y)))
  }
}


CalcX<-function(y){
  return(PRM45*CalcQ(y))
}

CalcQ<-function(y){
  return(paperToLF(y)*PRJ96*Var1_outputs(y))
}



Dumps_F <- function(y){
  return(dumpDtable[y-1899]-dumpDtable[y-1900])
}


dumpDtable <- numeric(121)
for(i in 1900:2020){
  if (i==1900)
    dumpDtable[1] <-  (1/(1+PRM60))*Dumps_C(i)
  else
    dumpDtable[i-1899] <- (1/(1+PRM60))*(dumpDtable[i-1900]+Dumps_C(i))
}
#for(i in 1900:1910){
# if (i==1900)
#  print((1/(1+PRM60))*Dumps_C(i))
#else
# print((1/(1+PRM60))*(dumpDtable[i-1900]+Dumps_C(i)))
#}
Dumps_C <- function(y){
  return(PRM57*Var1_outputs(y) * ParamResults_Q(y))
}


Var1_outputs<-function(y){
  return(Var1_totalC_Output(y)*PRO17)
}

Var1_totalC_Output <- function(y){
  return(Var1_totalC_outputtable[y-1899])
}

Var1_totalC_outputtable <- numeric(121)
for (y in 1900:2020){
  if (y == 1900){
    Var1_totalC_outputtable[1] <- Var1_C_placed_Iu_total(y) - Var1_carbonStored(1900)
  }
  else{
    Var1_totalC_outputtable[y-1899] <- Var1_carbonStored(y-1)-
      (Var1_carbonStored(y)-Var1_carbonStoredSameYear(y)) + 
      (Var1_C_placed_Iu_total(y)-Var1_carbonStoredSameYear(y))+(pre1900(y-1)-pre1900(y))
  }
}


Var1_C_SWP_StockChange_LFDumps <- function(y){
  if (y == 1900){
    return((1-PRM45)*ParamResults_O(y)*Var1_totalC_Output(y) * PRO17 + (Calc_CM(y)) + Dumps_O(y))
  }
  else{
    return((1-PRM45)*ParamResults_O(y)*Var1_totalC_Output(y) * PRO17 + 
             (Calc_CN(y) - Calc_CN(y-1)) + (Dumps_O(y) - Dumps_O(y-1)))
  }
}
##########

############
#########returns total carbon stored 
Var1_carbonStored <- function(y){
  return(Var1_totalC(y) - pre1900(y))
}

##############################
#######returns the carbon stored in a year, from that year. 
Var1_carbonStoredSameYear <- function(y){
  totalcarbon <- 0
  for (i in 1:16){
    if (i == 4 || i == 9 || i == 13){
      totalcarbon <- totalcarbon+0
    }
    else{
      totalcarbon <- totalcarbon + 
        (Var1_c_placed_IU(y,i)*(exp(-log(2)/HL(y,i)*((0)+1)))*(1-iuLoss(y,i)))
    }
  }
  return(totalcarbon)
}

Var1_c_placed_IU <- function(y,eu){
  if (eu == 16){
    return(qOther(y) * 1)
  }
  return((Var1_eSawn(y) * fsw(y,eu)) +(Var1_iSP(y)*fsp(y,eu)) +(Var1_mNSP(y)*fnsp(y,eu))) 
}


Var1b_totalC <- function(y){
  total <- 0
  for (i in 1900:y){
    for (eu in 1:16){
      if (eu == 4 || eu  == 9 || eu ==13){
        total <- total
      }
      else{
        total <- total + (Var1_c_placed_IU(i, eu)*(exp(-log(2)/HL(i,eu)*((y-i)+1)))*(1-iuLoss(i,eu)))
      }
    }
  }
  return(total + pre1900(y))
}












############################
###returns the carbon placed in use for a given year for all end uses
###Valuses found in SWPCalc
Var1_C_placed_Iu_total <- function(y){
  totalcarbon <- 0
  for (i in 1:16){
    if (i == 4 || i == 9 || i == 13){
      totalcarbon <- totalcarbon+0
    }
    else{
      totalcarbon <- totalcarbon + Var1_c_placed_IU(y,i)
    }
    
  }
  return(totalcarbon)
}



















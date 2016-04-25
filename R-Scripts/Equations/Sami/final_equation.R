###Final Variable (1a) Stock HWP Stock Change###
# List of constants:
#PRO17, InceF5, InceG5, PRP10, PRO18  



HWP_Stock_Change_1a<- function(y){
  return(((((total <- 0
             for (i in 1900:y){
               for (eu in 1:16){
                 if (eu == 4 || eu  == 9 || eu ==13){
                   total <- total
                 }
                 else{
                   total <- total + ((if (eu == 16){
                     return(qOther(y) * 1)
                   }
                   return((Var1_eSawn(y) * fsw(y,eu)) +(Var1_iSP(y)*fsp(y,eu)) +(Var1_mNSP(y)*fnsp(y,eu))) 
                 )*(exp(-log(2)/HL(i,eu)*((y-i)+1)))*(1-iuLoss(i,eu)))
                 }
               }
             }
             return(total + pre1900(y))) - (total <- 0
                                            for (i in 1900:y){
                                              for (eu in 1:16){
                                                if (eu == 4 || eu  == 9 || eu ==13){
                                                  total <- total
                                                }
                                                else{
                                                  total <- total + ((if (eu == 16){
                                                    return(qOther(y) * 1)
                                                  }
                                                  return((Var1_eSawn(y) * fsw(y,eu)) +(Var1_iSP(y)*fsp(y,eu)) +(Var1_mNSP(y)*fnsp(y,eu))))*(exp(-log(2)/HL(i,eu)*((y-i)+1)))*(1-iuLoss(i,eu)))
                                                }
                                              }
                                            }
                                            return(total + pre1900(y))-1))*PRO17)) 
  +(if (y == 1900){
    return(0)
  }
  else{
    return((Var1_totalC_PAPER(y) - Var1_totalC_PAPER(y-1))*1000)
  }))


}


Var1_c_placed_IU <- function(y,eu){
  if (eu == 16){
    return(qOther(y) * 1)
  }
  return((Var1_eSawn(y) * fsw(y,eu)) +(Var1_iSP(y)*fsp(y,eu)) +(Var1_mNSP(y)*fnsp(y,eu))) 
}






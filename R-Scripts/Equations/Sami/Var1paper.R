

Var1_C_PAPER_STOCKCHANGE <- function(y){
  if (y == 1900){
    return(0)
  }
  else{
    return(Var1_totalC_PAPER(y) - Var1_totalC_PAPER(y-1))
  }
  
}
##########################
#Total carbon in paper for year y in Tg C/yr
Var1_totalC_PAPER <- function(y){
  if (y == 1900){
    return(exp(-log(2)/PRP10)*Calc_D(y))
  }
  else{
    return(exp(-log(2)/PRP10)*(Calc_D(y)+Var1_totalC_PAPER(y-1)))
  }
  
}

Calc_D<-function(y){
  return(PRO18*usa_V(y)*usa_AB(y))
}

usa_V<-function(y){
  return(usa_S(y)+usa_T(y)-usa_U(y))
}

usa_T<-function(y){
  return(getIncePap(y,2)*1000*InceL5)
}


usa_AB<-function(y){
  return(((usa_AM(y)+usa_AN(y)-usa_AO(y))-(usa_AI(y)+usa_AJ(y)-usa_AK(y)))/(usa_AM(y)+usa_AN(y)-usa_AO(y)))
}



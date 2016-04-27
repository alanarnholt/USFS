Calc_BU <- function(y){
  return(PRO18*((usa_S(y)+usa_U(y)*(a5-1))*(1-usa_BC(y)*PRP62)*(1-usa_BD(y)*PRP62)*((usa_BF(y)-usa_BE(y)+usa_BL(y)/1000)/usa_BF(y))+(usa_AR(y)+usa_AV(y)+usa_AO(y))*a5))
}
####Total paper carbon 
var2papertab <- numeric(length(yrs))
for(i in 1900:maxyr){
  if(i == 1900){
    var2papertab[i-1899] <- exp(-log(2)/PRP10)*Calc_BU(i)
  }
  else{
    var2papertab[i-1899] <- exp(-log(2)/PRP10)*(Calc_BU(i)+var2papertab[i-1900])
  }
}
Var2_totalC_PAPER <- function(y){
  var2papertab[y-1899]
}
Var2_C_PAPER_STOCKCHANGE <- function(y){
  if (y == 1900){
    return(0)
  }
  else{
    return(Var2_totalC_PAPER(y) - Var2_totalC_PAPER(y-1))
  }
}



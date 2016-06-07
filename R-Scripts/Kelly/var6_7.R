###Variables 6 and 7
#Variable 6= Annual release of carbon to the atmosphere from HWP consumption (from fuelwood & products in use and products in SWDS)
Var6<- function(year){
  return(Var5(year)+Var3(year)-Var4(year)-Var1b_C_STOCKCHANGE_TOTAL(year)-Var1a_STOCKCHANGE_TOTAL)
}

Var7<- function(year){
  return(Var4(year)-Var2_C_PAPER_STOCKCHANGE (year)-Var2b_C_STOCKCHANGE_TOTAL_2B)
}



###completed what is possible, waiting for variable 4

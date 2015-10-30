####Var 2B
##total c stock change for wood products in swds
####
var2a <- function(y){
  return(Calc_CQ(y) + Calc_CY(y))
}
Calc_CQ <- function(y){
  return(Calc_CK(y) + Calc_CP(y) + Dumps_Q(y))
}
Calc_CK <- function(y){
  return((1-PRM45)*Calc_CH(y))
}
Calc_CH <- function(y){
  return(P_R_O(y)*Calc_BX(y))
}
Calc_BX <- function(y){
  
}
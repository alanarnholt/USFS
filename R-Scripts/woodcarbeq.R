##WOODCARB Excel caluclations
####################
####################
#####END-USES:
####SINGLE FAMILY HOUSING - 1
####MULTIFAMILY HOUSING - 2
####MOBILE HOME HOUSING - 3
#### TOTAL HOUSING - 4
####TOTAL RESIDENTIAL UPKEEP/IMPROVEMENT - 5
#### ALL NONRES CONSTRUCTION EX RAILROADS - 6
####RAILROAD TIES - 7
####RAILCAR REPAIR - 8
####TOTAL NONRES CONSTRUCTION - 9
####HOUSEHOLD FURNITURE - 10
####COMMERICAL FURNITURE - 11
####Manufacturing-other - 12
####manufacturing total - 13
#####SHIPPING TOTAL - 14
#####OTHER USES LUMBER/PANELS - 15
###USES FOR OTHER INDUSTRIAL PRODUCTS - 16
####EXPORTS - 17
##############
##################
###TOTAL CARBON STOCK CHANGE
C_STOCKCHANGE_TOTAL_2A <- function(y){
  return((C_SWP_STOCKCHANGE(y) + C_PAPER_STOCKCHANGE(y))*1000)
}

##########'CALC'CY###SOLID WOOD PRODUCTS STOCK CHANGE
##########'
C_SWP_STOCKCHANGE <- function(y){
  
  return((totalC_SWP(y) - totalC_SWP(y-1))*PRO17)
}
######
####CArbon STTOCK CHANGE PAPER PRODUCTS
C_PAPER_STOCKCHANGE <- function(y){
  if (y == 1900){
    return(0)
  }
  else{
    return(totalC_PAPER(y) - totalC_PAPER(y-1))
  }
  
}

##########################
totalC_PAPER <- function(y){
  if (y == 1900){
    return(exp(-log(2)/PRP10)*CalcBU(y))
  }
  else{
    return(exp(-log(2)/PRP10)*(CalcBU(y)+totalC_PAPER(y-1)))
  }
  
}

CalcBU <- function(y){
  return(PRO18*((usa_S(y)+usa_U(y)*(a5-1))*(1-usa_BC(y)*PRP62)*(1-usa_BD(y)*PRP62)*((usa_BF(y)-usa_BE(y)+usa_BL(y)/1000)/usa_BF(y))+(usa_AR(y)+usa_AV(y)+usa_AO(y))*a5))
}
#####c_placed_IU calculates carbon placed in use for a given end use in a year
c_placed_IU <- function(y,eu){
  if (eu == 16){
    return(qOther(y) * 1)
  }
  return((efinProdSawn(y) * fsw(y,eu)) +(ifinProdSP(y)*fsp(y,eu)) +(mfinProdNSP(y)*fnsp(y,eu))) 
}
######
###C_IU_J calculates total carbon left in year y for eu j
C_IU_J <- function(y,eu){
  total <- 0
  for(i in 1900:y){
     total <- total + (c_placed_IU(i,eu)*(exp(-log(2)/HL(i,eu)*((y-i)+1)))*(1-iuLoss(i,eu)))
  }
  return(total)
}
########
#####totalC calculates total carbon left in yr from all end uses
totalC_SWP <- function(y){
  totalcarbon <- 0
  for (i in 1:16){
    if (i == 4 || i == 9 || i == 13){
      totalcarbon <- totalcarbon+0
    }
    else{
      totalcarbon <- totalcarbon + C_IU_J(y,i)
    }
    
  }
  ######total carbon in solid wood products, with carbon in lumber from SF housing 1800-1900
  return(totalcarbon + pre1900(y))
}




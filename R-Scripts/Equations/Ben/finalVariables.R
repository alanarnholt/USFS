###############################
#############FINAL VARIABLES
#############woodcarb 'IPCC 06'
##############
#'VAR #2a
#'Annual Change in stock of HWP in use produced from domestic harvest 
#'Output is in Gg C/yr
###
C_STOCKCHANGE_TOTAL_2A <- function(y){
  return((C_SWP_STOCKCHANGE(y) + C_PAPER_STOCKCHANGE(y))*1000)
}
####
#'VAR #2b
#'Annual Change in stock of HWP in SWDS produced from domestic harvest 
#'Output in Gg C/yr
C_STOCKCHANGE_TOTAL_2B <- function(y){
  return(1000*(C_SWP_StockChange_LFDumps(y) + C_PAPER_StockChange_LFDumps(y)))
}
###
HWP_Contribution_AFOLU_Prod_Approach <- function(y){
  if (y < 1900 || y > 2050){
    return(0)
  }
  else{
    return((-1*C_STOCKCHANGE_TOTAL_2A(y)-C_STOCKCHANGE_TOTAL_2B(y))*(44/12))# + "Gg C/yr in emmissions/removals")
  }
  
}

# HWP_Contribution_AFOLU_Prod_Approach(2005)
# C_STOCKCHANGE_TOTAL_2A(2010)
# warnings()
# HWPFINALCHECK <- read.xlsx("HWP_FINAL_CHECK.xlsx",1,header=F)
# hwpcheck <- numeric(31)
# for (i in 1990:2020){
#   print(round(HWPFINALCHECK[i-1989,1],0) - round(HWP_Contribution_AFOLU_Prod_Approach(i)))
# }
# 
# for (i in 1990:2020){
#   total <- total + system.time(HWP_Contribution_AFOLU_Prod_Approach(i))
# }
# total
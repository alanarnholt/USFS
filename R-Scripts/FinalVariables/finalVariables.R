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

##change tables to fit with other rscripts, then THIS variable is complete
##need to find/create Export tables from WOODCARB.xlsx



Var4<- function(x){return(1000*((PRO17*usa_E(year))*(PRO17*usa_J(year))*
                                (PRO17*usa_M(year))*(PRO18*usa_U(year))*
                                (PRO18*usa_Y(year))))}





#Parts of Main Equation

usa_E <- function(year){
  if(year = 1900 && year = 1920 && year = 1930 && eyar = 1940){
    return(1000*((h3(year, 8)*InceS5) + (h3(year, 10)*InceT5)))
  }
  
  if(year = 1950 && year > 1959 && year < 1965){
    return(1000 * ((u5(year, 26) * InceS5) + (u5(year, 26) * InceT5)))}
  
  
  
  if(year >1964 && year < 1971 && year = 1980){
    return(1000 * ((h6(year, 21)*InceS5) + (h7(year, 13) * InceT5)
                   + ((h5(year, 23)*InceS5)*(154/(154+117))) 
                   + ((h5(year, 23)*InceT5)*(117/(154+117)))))}
  
  if(year > 1989 && year < 2021){
    return(1000 * ((h6(year, 21)*InceS5) + (h7(year, 13) * InceT5)
                   + ((h5(year, 23)*InceS5)) 
                   + ((h5(year, 23)*InceT5))))}
  
  
  if(year > 2020 && year < 2051){
    return(1000*((Exportt3HardRoundWoodEquivofLogAndChipExports * InceT5) 
                 + (452 * InceS5)))}
  }



#452 is the number for Exportt3SoftRoundWoodEquivLogAndChipExports years 2021 - 2050



usa_J <- function(year){
  if (year = 1900){
    return(1000*(h8(year, 13) * InceF5))}
  
  if(year > 1919 && year < 1965){
    return(1000*((u29(year, 10) * 1000 * InceF5) + (u29(year, 11) * 1000 * InceG5)))}
  
  if(year > 1964 && year < 2021){
    return(1000*((h28(year, 8) * 1000 * InceF5) + (h28(year, 9) * 1000 * InceG5)))}
  
  if(year > 2020 && year < 2051){
    return(1000*((1500 * InceF5) + (1500 * InceG5)))}
  }


#1500 is the number for Exportt1SoftWoodLumberExport and Exportt1HardWoodLumberExport years 2021 - 2050



usa_M <- function(year){
  if(year = 1920){
    return(1000*u54(year, 3)*InceJ5)
  }
  
  if(year = 1930 && year = 1940){
    return(1000*(((h3t20(year, 7)*InceB5)) + (((h3t20(year, 8)*InceE5) + (h3t21(year, 4)*InceR5))/1000)
    + (u54(year, 3)*InceJ5) + (u53(year, 3)*InceO5)))
  }
  
  if(year > 1949 && year < 1965){
    return(1000*((u36(year, 10)*InceB5) + (u36(year, 11)*InceE5) + (u54(year, 3)*InceJ5) + (u53(year, 3)*InceO5)))
  }
  
  if(year > 1964 && year < 1991){
    return(1000*((h37(year, 8)*InceB5) + (h37(year, 9)*E5) + (h53(year, 5)*InceI5) + (h56(year, 3)*InceJ5) + (h55(year, 3)*InceO5)))
  }
  
  if(year > 1990 && year < 2021){
    return(1000*((h37(year, 8)*InceB5) + (h38(year, 9)*InceC5) + (h37(year, 9)*InceE5) + (h53(year, 5)*InceI5) + (h56(year, 3)*InceJ5) + (h55(year, 3)*InceO5)))
  }
  
  if(year > 2020 && year < 2051){
   return(1000*((1000*InceB5) + (0*InceC5) + (Exportt1Hardwoodplywood*InceE5)
                + (Exportt1Particleboard*InceI5) + (Exportt1Hardboard*InceJ5) + (Exportt1Insulatingboard*InceO5))) 
  }
}


#1000 for Exportt1Softwoodplywood years 2021 - 2050
#0 for Exportt1OSBWaferboard years 2021 - 2050




usa_U <- function(year){
  if(year > 1900 && year < 2050){return(1000*((InceL5*IncePaper(year, 3))))}
  }





USA_AK <- function(year){
  if(year >1899 && year <1965){
    return(apiTotalWoodPulp(year,9))
  }
  
  if(year > 1964 && year < 2014){
    return(1000*h46(year,13))
  }
  
  if(year > 2013 && year < 2021){
    return(1000*h46(2007,13))
  }
  
  if(year > 2019 && year < 2051){
    return(1000*h46(2002,13))
  }
}






USA_AO <- function(year){
  if(year > 1899 && year < 1965){
    return(apiTotalWoodPulp(year,3) + apiTotalWoodPulp(year,13))
  }
  
  if(year > 1964 && year < 2021){
    return(1000*(h49(year,4) + h46(year,13)))
  }
  
  if(year > 2020 && year < 2051){
    return(1000*(h49(2002,4) + h46(2002 ,13)))
  }
}




usa_AF <- function(year){
  return(USA_AK(year) - USA_AO(year))
}




usa_AV <- function(year){
  if(year > 1899 && year < 1965){
    return(apiTotalWoodPulp(year,10))
  }
  
  if(year > 1964 && year < 2021){
    return(1000*(h47(year,4)))
  }
  
  if(year > 2020 && year < 2051){
    return(1000*(h47(2002,4)))
  }
}




usa_AR <- function(year){
  if(year > 1997 && year < 2014){
    return(0.90718 * recFibPulpUSA(year, 3))
  }
  
  if(year > 2013 && year < 2021){
    return(0.90718 * recFibPulpUSA(2007, 3))
  }
}


usa_Y <- function(year){return(usa_AF + usa_AV + usa_AR)}














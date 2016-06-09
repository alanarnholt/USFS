##change tables to fit with other rscripts, then THIS variable is complete




Var4<- function(x){return(1000*((PRO17*RoundwoodExports(year))*(PRO17*SawnwoodExports(year))*
                                (PRO17*WoodBasedPanelExports(year))*(PRO18*PaperAndPaperboardExports(year))*
                                (PRO18*TotalFibreFurnishExports(year))))}

#Parts of Main Equation

RoundwoodExports <- function(year){
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
                 + (Exportt3SoftRoundWoodEquivLogAndChipExports * InceS5)))}
  }




SawnwoodExports <- function(year){
  if (year = 1900){
    return(1000*(h8(year, 13) * InceF5))}
  
  if(year > 1919 && year < 1965){
    return(1000*((u29(year, 10) * 1000 * InceF5) + (u29(year, 11) * 1000 * InceG5)))}
  
  if(year > 1964 && year < 2021){
    return(1000*((h28(year, 8) * 1000 * InceF5) + (h28(year, 9) * 1000 * InceG5)))}
  
  if(year > 2020 && year < 2051){
    return(1000*((Exportt1SoftWoodLumberExport * InceF5) + (Exportt1HardWoodLumberExport * InceG5)))}
  }



WoodBasedPanelExports <- function(year){
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
   return(1000*((Exportt1Softwood*InceB5) + (Exportt1OSBWaferboard*InceC5) + (Exportt1Hardwood*InceE5)
                + (Exportt1Particleboard*InceI5) + (Exportt1Hardwood*InceJ5) + (Exportt1Insulatingboard*InceO5))) 
  }
}



PaperAndPaperboardExports <- function(year){
  if(year > 1900 && year < 2050){return(1000*((InceL5*IncePaper(year, 3))))}
  }



TotalFibreFurnishExports <- function(year){return(USAWoodPulpForPaperExports + RecoveredPaperExports + RecoveredFibrePulpExports)}














##change tables to fit with other rscripts, then THIS variable is complete




Var4<- function(x){return(1000*((PRO17*RoundwoodExports(year))*(PRO17*SawnwoodExports(year))*
                                (PRO17*WoodBasedPanelExports(year))*(PRO18*PaperAndPaperboardExports(year))*
                                (PRO18*TotalFibreFurnishExports(year))))}

#Parts of Main Equation

RoundwoodExports <- function(year){
  if(year = 1900 && year = 1920 && year = 1930 && eyar = 1940){
    return(1000*((hair1963t2SoftwoodChipexport*InceS5) + (hair1963t2HardlogChipexport*InceT5)))
  }
  
  if(year = 1950 && year > 1959 && year < 1965){
    return(1000 * ((U5logexports * InceS5) + (U5logexports * InceT5)))}
  
  
  
  if(year >1964 && year < 1971 && year = 1980){
    return(1000 * ((H6aLogExports*InceS5) + (H7aPlywoodAndVeneerExports * InceT5)
                   + ((H5aPulpwoodChipExports*InceS5)*(154/(154+117))) 
                   + ((H5aPulpwoodChipsExport*InceT5)*(117/(154+117)))))}
  
  if(year > 1989 && year < 2021){
    return(1000 * ((H6aLogExports*InceS5) + (H7aPlywoodAndVeneerExports * InceT5)
                   + ((H5aPulpwoodChipExports*InceS5)) 
                   + ((H5aPulpwoodChipsExport*InceT5))))}
  
  
  if(year > 2020 && year < 2051){
    return(1000*((Exportt3HardRoundWoodEquivofLogAndChipExports * InceT5) 
                 + (Exportt3SoftRoundWoodEquivLogAndChipExports * InceS5)))}
  }




SawnwoodExports <- function(year){
  if (year = 1900){
    return(1000*(hair1958t14TotalExports * InceF5))}
  
  if(year > 1919 && year < 1965){
    return(1000*((U29SoftWoodExports * 1000 * InceF5) + (U29HardWoodExports * 1000 * InceG5)))}
  
  if(year > 1964 && year < 2021){
    return(1000*((H28SoftWoodExport * 1000 * InceF5) + (H28HardWoodExport * 1000 * InceG5)))}
  
  if(year > 2020 && year < 2051){
    return(1000*((Exportt1SoftWoodLumberExport * InceF5) + (Exportt1HardWoodLumberExport * InceG5)))}
  }



WoodBasedPanelExports <- function(year){
  if(year = 1920){
    return(1000*Ulrich54*InceJ5)
  }
  
  if(year = 1930 && year = 1940){
    return(1000*(((Hair1963t20*InceB5)) + (((Hair1963t20*InceE5) + (Hair1963t21*InceR5))/1000)
    + (Ulrich54*InceJ5) + (Ulrich53*InceO5)))
  }
  
  if(year > 1949 && year < 1965){
    return(1000*((Ulrich36*InceB5) + (Ulrich36*InceE5) + (Ulrich54*InceJ5) + (Ulrich53*InceO5)))
  }
  
  if(year > 1964 && year < 1991){
    return(1000*((H37*InceB5) + (H37*E5) + (H53*InceI5) + (H56*InceJ5) + (H55*InceO5)))
  }
  
  if(year > 1990 && year < 2021){
    return(1000*((H37*InceB5) + (H38*InceC5) + (H37*InceE5) + (H53*InceI5) + (H56*InceJ5) + (H55*InceO5)))
  }
  
  if(year > 2020 && year < 2051){
   return(1000*((Exportt1Softwood*InceB5) + (Exportt1OSBWaferboard*InceC5) + (Exportt1Hardwood*InceE5)
                + (Exportt1Particleboard*InceI5) + (Exportt1Hardwood*InceJ5) + (Exportt1Insulatingboard*InceO5))) 
  }
}



PaperAndPaperboardExports <- function(year){
  if(year > 1900 && year < 2050){return(1000*((InceL5*PaperAndBoardExports)))}
  }



TotalFibreFurnishExports <- function(year){return(USAWoodPulpForPaperExports + RecoveredPaperExports + RecoveredFibrePulpExports)}














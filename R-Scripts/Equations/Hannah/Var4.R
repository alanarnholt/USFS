


Var4<- function(y){return(1000*((PRO17*usa_E(y))+(PRO17*usa_J(y))+
                                (PRO17*usa_M(y))+(PRO18*usa_U(y))+
                                (PRO18*usa_Y(y))))}








#Parts of Main Equation

usa_E <- function(year){
  if(year >1899 && year < 1950){
    return(1000 *((h3(year, 8) * InceS5) + (h3(year, 10) * InceT5)))
  }
  
  if(year > 1949 && year < 1965){
    return(1000 * ((u5(year, 21) * InceS5) + (u6(year, 22) * InceT5)))}
  
  
  
  if(year >1964 && year < 1990){
    return(1000 * ((h6(year, 21)*InceS5) + (h7(year, 13) * InceT5)
                   + ((h5(year, 23)*InceS5)*(154/(154+117))) 
                   + ((h5(year, 23)*InceT5)*(117/(154+117)))))}
  
  if(year > 1989 && year < 2021){
    return(1000 * ((h6(year, 21)*InceS5) + (h7(year, 13) * InceT5)
                   + ((h6(year, 23)*InceS5)) 
                   + ((h7(year, 23)*InceT5))))}
  }






usa_J <- function(year){
  if (year == 1900){
    return(1000*(h8(year, 13) * InceF5))}
  
  if(year >1900 && year < 1950){
    return(1000*(((h8(year, 14) + (h8(year,16))) * InceF5) + (h8(year,15) * InceG5)))
  }
  
  if(year > 1949 && year < 1965){
    return(1000*((u29(year, 8) * 1000 * InceF5) + (u29(year, 9) * 1000 * InceG5)))}
  
  if(year > 1964 && year < 2021){
    return(1000*((h28(year, 8) * 1000 * InceF5) + (h28(year, 9) * 1000 * InceG5)))}

  }






usa_M <- function(year){
  if(year < 1921){
    return(1000*u54(year, 3)*InceJ5)
  }

  if(year > 1929 && year < 1940){

    return(1000*((h3t20(year, 7)/1000*InceB5) + (((h3t20(year,8)*InceE5) + (h3t21(year,4)*InceR5))/1000) + (u54(year,3)*InceJ5) + (u53(year,3)*InceO5)))
  }

  if(year > 1939 && year < 1950){
    return(1000*(((h3t20(year, 7)/1000)*InceB5) + (((h3t20(year,8) + (h3t21(year,4)))/1000)*InceE5) + (u54(year, 3)*InceJ5) + (u53(year,3)*InceO5)))
  }

  if(year > 1949 && year < 1965){
    return(1000*((u36(year, 8)*InceB5) + (u36(year, 9)*InceE5) + (u54(year, 3)*InceJ5) + (u53(year, 3)*InceO5)))
  }

  if(year > 1964 && year < 1991){
    return(1000*((h37(year, 8)*InceB5) + (h37(year, 9)*InceE5) + (h53(year, 5)*InceI5) + (h56(year, 3)*InceJ5) + (h55(year, 3)*InceQ5)))
  }

  if(year > 1990 && year < 2019){
    return(1000*((h37(year, 8)*InceB5) + (h38(year, 9)*InceC5) + (h37(year, 9)*InceE5) + (h53(year, 5)*InceI5) + (h56(year, 3)*InceJ5) + (h55(year, 3)*InceQ5)))
  }
  
  if(year>2018 && year<2021){
    return(1000*((h37(year, 8)*InceB5) + (h38(year, 9)*InceC5) + (h37(year, 9)*InceE5)))
  }

}








usa_U <- function(year){
  if(year > 1900 && year < 2021){return(1000*((InceL5*getIncePap(year, 3))))}
  }





USA_AK <- function(year){
  if(year >1899 && year <1965){
    return(apiTotal(year-1,11))
  }
  
  if(year > 1964 && year < 2014){
    return(1000*h46(year,13))
  }
  
  if(year > 2013 && year < 2021){
    return(1000*h46(2007,13))
  }
  
}








USA_AO <- function(year){
  if(year > 1899 && year < 1965){
    return(apiTotal(year-1,3) + apiTotal(year-1,11))
  }
  
  if(year > 1964 && year < 2021){
    return(1000*(h49(year,4) + h46(year,13)))
  }
  
}








usa_AF <- function(year){
  return(USA_AO(year) - USA_AK(year))
}






usa_AV <- function(year){
  if(year > 1899 && year < 1965){
    return(apiTotal(year-1,8))
  }
  
  if(year > 1964 && year < 2021){
    return(1000*(h47(year,4)))
  }

}







usa_AR <- function(year){
  if(year < 1997){return(0)}
  
  if(year > 1997 && year < 2014){
    return(0.90718 * recFibPulp_USA(year, 3))
  }
  
  if(year > 2013 && year < 2021){
    return(0.90718 * recFibPulp_USA(2007, 3))
  }
}









usa_Y <- function(year){
  return(
    usa_AF(year) + usa_AV(year) + usa_AR(year))
  }






#######################################
#########PRODUCTION APPROACH, PAPER PRODUCTS
###USA_X: X column in USA sheet in woodcarb.xlsx 
usa_S <- function(y){
  return(getIncePap(y,1)*1000*InceL5)
}
usa_U <- function(y){
  return(getIncePap(y,3)*1000*InceL5)
}
usa_AD <- function(y){
  return(usa_AM(y)-usa_AI(y))
}
usa_AE <- function(y){
  return(usa_AN(y)-usa_AJ(y))
}
usa_AF <- function(y){
  return(usa_AO(y)-usa_AK(y))
}
usa_AI <- function(y){
  if (y < 1965){
    return(apiTotalWP_L(y))
  }
  if (y > 1964 && y < 2014){
    return(h46(y,5)*(h46(y,1)/h46(y,2))*1000)
  }
  if (y > 2013 && y < 2021){
    return(usa_AI(2007))
  }
  if (y > 2020){
    return(usa_AI(2002))
  }
}
usa_AJ <- function(y){
  if(y < 1965){
    return(apiTotalWP_L(y)*(apiTotal(y,2)/apiTotal(y,1)))
  }
  if (y > 1964 && y < 2014){
    return(h46(y,5)*(h49(y,3)/100)*1000)
  }
  if (y > 2013 && y < 2021){
    return(usa_AJ(2007))
  }
  if (y > 2020){
    return(usa_AJ(2002))
  }
}
usa_AK <- function(y){
  if(y < 1965){
    return(apiTotalWP_L(y)*(apiTotal(y,3)/apiTotal(y,1)))
  }
  if (y > 1964 && y < 2014){
    return(h46(y,5)*(h46(y,1)/h46(y,2))*(h49(y,5)/100)*1000)
  }
  if (y > 2013 && y < 2021){
    return(usa_AK(2007))
  }
  if (y > 2020){
    return(usa_AK(2002))
  }
}
usa_AM <- function(y){
  if (y < 1965){
    return(apiTotal(y,1)+apiTotalWP_L(y))
  }
  if (y >1964 && y < 2021){
    return((h49(y,1)+h46(y,5)*(h46(y,1)/h46(y,2)))*1000)
  }
  if (y > 2020){
    return(usa_AM(2002))
  }
}
usa_AN <- function(y){
  if (y < 1965){
    return(apiTotal(y,2)+apiTotalWP_L(y)*(apiTotal(y,2)/apiTotal(y,1)))
  }
  if (y >1964 && y < 2021){
    return((h49(y,2)+h46(y,5)*(h49(y,3)/100))*1000)
  }
  if (y > 2020){
    return(usa_AN(2002))
  }
}
usa_AO <- function(y){
  if (y < 1965){
    return(apiTotal(y,3)+apiTotalWP_L(y)*(apiTotal(y,3)/apiTotal(y,1)))
  }
  if (y > 1964 && y < 2021){
    return((h49(y,4)+h46(y,5)*(h46(y,1)/h46(y,2))*(h49(y,5)/100))*1000)
  }
  if (y > 2020){
    return(usa_AO(2002))
  }
}
usa_AR <- function(y){
  if (y <1998){
    return(0)
  }
  if (y > 1997 && y < 2014){
    return(usa_CG(y)*0.90718)
  }
  if (y > 2013 && y < 2021){
    usa_AR(2007)
  }
}
usa_AV <- function(y){
  if (y < 1965){
    return(apiTotal(y,8))##calculation 
  }
  if (y > 1964 && y < 2021){
    return(h47(y,4)*1000)
  }
}
usa_BC <- function(y){
  return((usa_AI(y)+usa_AJ(y)-usa_AK(y))/(usa_AM(y)+usa_AN(y)-usa_AO(y)))
}
usa_AJ(1900)
usa_BD <- function(y){
  return(usa_AE(y)/(usa_AD(y)+usa_AE(y)-usa_AF(y)))
}
usa_BF <- function(y){
  if (y < 1950){
    return(h3(y,34)*(usa_BF(1950)/(u5(1950,15)+u6(1950,19))))
  }
  if (y > 1949 && y < 1965){
    return(u5(y,16)*InceV5+u6(y,19)*InceW5)
  }
  if (y > 1964 && y < 2021){
    return(h6(y,15)*InceV5+h7(y,15)*InceW5)
  }
}
usa_BE <- function(y){
  if (y > 1988 && y < 2021){
    return(h6(y,22)*InceV5+h7(y,22)*InceW5)
  }
  if (y < 1989 && y > 1964){
    return(h5(y,22)*(usa_BE(1989)/(h5(1989,22))))
  }
  else{
    return(0)
  }
}
usa_BL <- function(y){
  if (y < 1965){
    return(0)
  }
  if (y > 1964 && y < 2021){
    return((h6(y,23)*InceV5+h7(y,23)*InceW5)*1000)
  }
  if (y > 2020){
    return(usa_BL(1999))
  }
}
usa_CG <- function(y){
  if (y > 1997 && y < 2014){
    return(usaFiberPulp[y-1997,3])
  }
}
######
##api includes calculations from estimates/averages
apiTotalWP_L <- function(y){
  if (y < 1957){
    return((apiFiber(y,3)+apiFiber(y,5))*(apiTotal(y,1)/apiTotal(y,5)))
  }
  if (y >1956){
    return(apiFiber(y,4)*(apiTotal(y,1)/apiTotal(y,5)))
  }
}

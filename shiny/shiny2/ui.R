library(shiny)
library(ggvis)

shinyUI(fluidPage(
  
  title = "Wood Predictions",
  h2("Wood Predictions"),
  
  fluidRow(
    column(4, 
           h3("Choose Variables"),
           selectInput('xInp',"X Variable", choices=names(swpcalcdata),
                       selected=names(swpcalcdata)[[1]]),
           selectInput('yInp', "Y Variable", choice = names(swpcalcdata),
                       selected  = names(swpcalcdata)[[2]]),
           h3("Years for Predictions"),
           sliderInput('ypred', "Number of Years To Use For Prediction",
                        value = 10, min = 1, max = 100),
           sliderInput('pred', "Number of Years To Predict", 
                        value = 5, min = 1, max = 30),
           sliderInput("range", 
                       label = "Range of interest:",
                       min = 1900, max = 2020, 
                       value = c(2000,2020),
                       sep=""),
           sliderInput("span", "Span for LOESS Fit",
                       min = 0.1, max = 2,
                       value = .5),
           selectInput("fitm", "Type of Fit", choices = c("lm", "loess"),
                       selected = "lm")
           ##inputs to get variable, years to use to predict, years ahead to predict with actionbuttion or submitbutton 
          ),
    column(8, ##plot graph thats made in newserver.r,,could add interactivity here or to ggvis plot 
           ggvisOutput("cgraph"),
           uiOutput("craph_ui")
          
  
  )
)
  
)
)
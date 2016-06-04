
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library()

shinyServer(function(input, output) {
  library(ggplot2)
  library(ggvis)
  #swpcalcdata$pred <- as.factor(ifelse(swpcalcdata$Years > 2013, "USFS Pred"))
  output$distPlot <- renderPlot({
    string <- paste( input$var2, "in the United States (million tonnes C)")
    range <- input$range
    plot(swpcalcdata$Years, swpcalcdata[[input$var2]] / 10, type="l", col= "blue",
         xlim = c(min(range), max(range)), pch=19, main = string,
         xlab = "Years", ylab=input$var2)
  })
  
  output$ipcctable <- renderDataTable({
    if(input$radbut == "All")
      round(IPCC)
    else
      round(cbind(Years =  IPCC$Years, IPCC[input$radbut]))
      #round(IPCC[c(IPCC$Years, input$radbut)])
  })
 
})

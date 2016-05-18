library(shiny)
library(ggvis)

shinyServer(function(input, output,session) {
  #last year of actual reported data 
  lastcurrentyear <- 2013
  
  swpdat <- reactive({
    yearrange <- min(input$range):lastcurrentyear 
    x <- swpcalcdata[swpcalcdata$Years %in% yearrange, c(input$xInp, input$yInp)]
    names(x) <- c("x", "y")
    x
  })
  
  preds1 <- reactive({
    y <- swpcalcdata[swpcalcdata$Years >= lastcurrentyear, c(input$xInp, input$yInp)]
    names(y) <- c("x2", "y2")
    y
  })
  
  preds <- reactive({
   
    predrange <- (lastcurrentyear - input$ypred):lastcurrentyear 
    predictedrange <- lastcurrentyear:(lastcurrentyear + input$pred)
      
    form <- as.formula(paste(input$yInp, input$xInp, sep = "~"))
    
    p <- swpcalcdata[swpcalcdata$Years %in% predrange,] %>% 
        compute_model_prediction(form, domain = c(min(predictedrange) - input$ypred, tail(predictedrange, 1)),
                                 model = "lm") %>%
      layer_smooths(x = ~pred_, y = ~resp_, stroke := "purple", layer_smooths(x = ~pred_, y = ~resp_, stroke := "purple", 
                                                                               data = preds),
                    data = preds)
    p
  })
  

  swpdat %>% ggvis(x=~x, y=~y) %>%
    layer_lines(stroke:="black") %>%
    layer_paths(x = ~pred_, y = ~resp_, stroke := "purple", 
                  data = preds) %>%
    layer_paths(x = ~x2, y = ~y2, data = preds1, stroke := "blue") %>%
    add_axis("x", format = '####') %>% 
    bind_shiny("cgraph", controls_id="cgraph_ui")
  
})
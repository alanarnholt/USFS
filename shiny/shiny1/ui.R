shinyUI(fluidPage(
  titlePanel("UShinyFS"),
  sidebarLayout(
    sidebarPanel(
      helpText("United States Forestry Statistics"),
    
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 1900, max = 2020, 
                  value = c(1990,2020),
                  sep=""),
      br(),
      selectInput("var2", 
                  label = "Choose a wood products category",
                  choices = names(swpcalcdata),
                  selected=names(swpcalcdata[2])),
     
      br(),
      radioButtons("radbut",
                   label="Choose Variable to Calculate AFOLU Contrib.",
                   choices = c(names(IPCC)[-1], "All"),
                   selected=names(IPCC)[2])
      ),

    mainPanel(
      tabsetPanel(
        tabPanel(h4("Graphing Forestry Statistics"), 
              plotOutput("distPlot")),
        
        tabPanel(h4("Annual C HWP Contribution to AFOLU (Gg C/Yr)",
                 "Final Vars."), 
                 dataTableOutput("ipcctable"))
        # tabPanel(h4("Sensitivity Analysis"), 
        #          selectInput("select", label = h3("Paramaters to Adjust"),
        #             choices = list("Param 1" = a, "Param 2" = b)),
        #          sliderInput("slider", label = h3("Changing Values of Parameters by Percent"),
        #                      min = -200, max = 200, value = 0)
                 
        
    )
  )
)))
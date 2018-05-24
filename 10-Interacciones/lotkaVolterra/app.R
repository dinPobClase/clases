#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("LV-helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Configurar modelo"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
         helpText("Escoja la cantidad de años que desea proyectar."),
         sliderInput("time",
                     label = "Años por simular:",
                     min = 1,
                     max = 100,
                     value = 30),
      
     hr(),
     helpText("Escoja los parámetros de N1"),
     numericInput(
       "K1",
       label = "K1",
       value = 1000,
       min = 100,
       max = 1200,
       step = 100
     ),
     sliderInput(
       "N1",
       label = "N1, porcentaje de K",
       value = 0.1,
       min = 0,
       max = 0.2,
       step = 0.02
     ),
     numericInput(
       "alpha",
       label = "alpha",
       value = 0.15,
       min = 0,
       max = 1,
       step = 0.01
     ),     
     numericInput(
       "r1",
       label = "r1",
       value = .49,
       min = 0,
       max = 2,
       step = 0.01
     ),
     
     hr(),
     helpText("Escoja los parámetros de N2"),
     numericInput(
       "K2",
       label = "K2",
       value = 10,
       min = 1,
       max = 30,
       step = 1
     ),
     sliderInput(
       "N2",
       label = "N2, porcentaje de K",
       value = 0.5,
       min = 0,
       max = 1,
       step = 0.05
     ),
     numericInput(
       "beta",
       label = "beta",
       value = -60,
       min = -1000,
       max = 1000
     ),     
     numericInput(
       "r2",
       label = "r2",
       value = .14,
       min = 0,
       max = 2,
       step = 0.01
     )
     #,submitButton("Submit")
      
      ),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("lotkaVolterra")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$lotkaVolterra <- renderPlot({
    plotLV(N1 = input$N1*input$K1,
           N2 = input$N1*input$K1,
           alpha = input$alpha,
           beta = input$beta,
           r1 = input$r1,
           r2 = input$r2,
           K1 = input$K1,
           K2 = input$K2,
           t = input$time,
           dt = 1 /12)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


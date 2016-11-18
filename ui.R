
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Confidence Interval Simulation"),
  p(paste("This simulation 'runs' a large number of studies over time by simulating ",
          "a random sample from a normal distribution. The confidence intervals that ",
          "result from this are shown in the top plot. Red intervals do not include ",
          "the true population mean. Additionally, a cumulative histogram of the ",
          "proportion of times a value was inside the confidence interval is shown ",
          "on the bottom. This value will tend towards the confidence level for the ",
          "population mean.")),
  # Sidebar with input
  sidebarLayout(
    sidebarPanel(
      fluidRow(column(6, numericInput(inputId = "nsamp",
                                      label=strong("Sample Size"),
                                      value=100, min=5, max=10000)),
               column(6, numericInput(inputId = "mean",
                                      label = strong("Mean"),
                                      value = 5))),
      fluidRow(column(6, numericInput(inputId = "stdev",
                                      label = strong("Standard Deviation"),
                                      value = 2, min=.01, max=100)),
               column(6, numericInput(inputId = "conf.level",
                                      label=strong("Confidence Level"),
                                      value=0.95, min=0.1, max=0.99, step=0.04))),
      sliderInput(inputId = "speed",
                    label=strong("Animation Speed"),
                    value=1.0, min=0.1, max=10, step=0.1),
      checkboxInput(inputId = "running", label='Run Simulation', value = FALSE),
      actionButton(inputId = 'resetButton', label="Reset Animation"),
      actionButton(inputId = 'stepButton', label = 'Generate Sample'),
      hr(),
      # Sample properties and histogram
      verbatimTextOutput('sample'),
      plotOutput('samp.hist', height='200px')
    ),
    
    mainPanel(plotOutput('conf.plot', height="300px"),
              plotOutput('hist.plot', height="400px"))
  )
  
))

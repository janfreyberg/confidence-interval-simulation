
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Confidence Interval Simulation"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "nsamp"
                   ,label=strong("Sample Size")
                   ,value=100
                   ,min=5
                   ,max=10000)
      ,numericInput(inputId = "mean"
                    ,label = strong("Mean")
                    ,value = 5)
      ,numericInput(inputId = "stdev"
                    ,label = strong("Standard Deviation")
                    ,value = 2
                    ,min=.01
                    ,max=100)
      
      ,numericInput(inputId = "conf.level"
                    ,label=strong("Confidence Level")
                    ,value=0.95
                    ,min=0.1
                    ,max=0.99
                    ,step=0.04)
      ,sliderInput(inputId = "speed",
                    label=strong("Animation Speed"),
                    value=5.0, min=0.1, max=10, step=0.1)
      ,checkboxInput(inputId = "running",
                     label='Run Simulation',
                     value = FALSE)
      ,actionButton(inputId = 'resetButton',
                    label="Reset")
      ,actionButton(inputId = 'stepButton',
                    label = 'Step')
      # Sample properties and histogram
      ,verbatimTextOutput('sample')
      ,plotOutput('samp.hist', height='200px')
    ),

    # Show a plot of the generated distribution
    mainPanel(plotOutput('conf.plot', height="300px"),
              plotOutput('hist.plot', height="400px"))
  )
  
))

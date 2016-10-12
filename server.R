
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

intervals <- rep(list(c(0, 0)), 100)
barheight <- rep(0, 500)
cumulatives <- 1
speed <- 1.0
confidence <- 0.95

shinyServer(function(input, output, session) {
  
  
  observeEvent(input$speed,
                {
                  speed <<- input$speed
                })

  
  output$conf.plot<-renderPlot({
    # Make sure this plot updates regularly
    # autoRefresh()
    
    if(input$running){
      xstring = 'Running'
    } else {
      xstring = 'Paused'
    }
    
    plot(NULL
         ,xlim = c(input$mean-input$stdev,input$mean+input$stdev)
         ,ylim = c(0,40)
         ,yaxt = 'n'
         ,xaxt = 'n'
         ,ylab = ('Studies run over time')
         ,main="CIs of lots of studies over time (red CIs do not include the mean)"
         ,cex.main=1.5
         ,cex.lab=1.5
         ,xlab = xstring
    )
    
    axis(1,
         at = c(input$mean-input$stdev, input$mean-0.5*input$stdev,
                input$mean,
                input$mean+0.5*input$stdev, input$mean+input$stdev),
         labels = c(expression(paste('-', sigma)),
                    expression(paste('-', sigma, '/2')),
                    expression(mu),
                    expression(paste(sigma, '/2')),
                    expression(paste(sigma))),
         cex.axis=2,
         yaxt='n')
    
    # Create a new interval
    x <<- rnorm(input$nsamp, mean = input$mean, sd = input$stdev)
    test <- t.test(x,conf.level=input$conf.level)
    
    intervals <<- c(list(test$conf.int), intervals) # feed at bottom
    intervals[[101]] <<- NULL # cut off top
    
    # update histogram
    barheight <<- barheight + ((seq(input$mean-input$stdev,
                                   input$mean+input$stdev,
                                   length.out=500) > intervals[[1]][1]) &
                               (seq(input$mean-input$stdev,
                                    input$mean+input$stdev,
                                    length.out=500) < intervals[[1]][2]))
    # normalise the histogram
    cumulatives <<- cumulatives + 1
    barnorm <<- barheight / cumulatives

    # Plot the intervals
    for (i in 1:40){
      interval = intervals[[i]]
      if(input$mean>interval[1] & input$mean<interval[2]){
        lines(c(interval[1],interval[2]),c(i,i), lwd=2,col='black')
      }
      else{
        lines(c(interval[1],interval[2]),c(i,i), lwd=2,col='red' )
      }
    }
    
    if(input$running){
      invalidateLater(500 / speed)
    }

  })
  
  output$hist.plot <- renderPlot({
    # This is a cumulative histogram of all the confidence intervals so far
    barplot(barnorm, ylim=c(0, 1),
            col=c('darkgray'), border=c('darkgray'),
            ylab="Proportion of CIs that include value",
            main="How often is a value inside the CI?",
            cex.lab=1.5, cex.main=1.5)
    
    # Add a reference line at 95%
    lines(c(150, 400),c(confidence,confidence),
          lwd=2,col='red' )
    text(400, confidence, labels=paste(c(confidence*100, "%"), collapse=''),
         pos=4, col=c('red'))
    axis(1,
         at = 50 + c(0, 125, 250, 375, 500),
         labels = c(expression(paste('-', sigma)),
                    expression(paste(sigma, '/2')),
                    expression(mu),
                    expression(paste(sigma, '/2')),
                    expression(paste(sigma))),
         cex.axis=2)
    if(input$running){
      invalidateLater(500 / speed)
    }
  })
  
  # Text of sample properties for left hand side:
  output$sample <- renderText({
    if(input$running){
      invalidateLater(500 / speed)
    }
    return(
      paste(" Population Mean: ", input$mean, "\n",
      "Sample Mean: ", round(mean(x), digits=2), "\n",
      "Confidence Interval: \n",
      round(intervals[[1]][1], digits=2), " - ",
      round(intervals[[1]][2], digits=2))
    )
  })
  # Small histogram of sample for left hand side:
  output$samp.hist <- renderPlot({
    if(input$running){
      invalidateLater(500 / speed)
    }
    hist(x,
         xlim = c(input$mean-2*input$stdev, input$mean+2*input$stdev),
         breaks=100,
         main="Histogram of Sample",
         yaxt='n',
         xaxt='n',
         axes=FALSE)
    axis(1,
         at = c(input$mean-input$stdev, input$mean-0.5*input$stdev,
                input$mean,
                input$mean+0.5*input$stdev, input$mean+input$stdev),
         labels = c(expression(paste('-', sigma)),
                    expression(paste('-', sigma, '/2')),
                    expression(mu),
                    expression(paste(sigma, '/2')),
                    expression(paste(sigma))),
         cex.axis=1,
         yaxt='n')
  })
  
  # If the input changes, reset intervals
  observeEvent(input$mean | input$stdev | input$nsamp | input$conf.level | input$resetButton, {
    confidence <<- input$conf.level
    intervals <<- rep(list(c(0, 0)), 100)
    barheight <<- rep(0, 500)
    cumulatives <<- 1
  })
  


})

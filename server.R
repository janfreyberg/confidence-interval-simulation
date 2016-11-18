
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

intervals <- rep(list(c(0, 0)), 40)
barheight <- rep(0, 500)
barnorm <- barheight
x <- barheight
cumulatives <- 1
speed <- 1.0
confidence <- 0.95
stepping <- FALSE

shinyServer(function(input, output, session) {
  
  observeEvent(input$stepButton, {
    if(!input$running){
    updateCheckboxInput(session,
                        inputId = "running",
                        label='Run Simulation',
                        value = TRUE)
    stepping <<- TRUE
    }
  })
  
  # If the input changes, reset intervals
  observeEvent(input$mean | input$stdev | input$nsamp | input$conf.level | input$resetButton, {
    confidence <<- input$conf.level
    intervals <<- rep(list(c(0, 0)), 100)
    barheight <<- rep(0, 500)
    cumulatives <<- 1
  })
  
  
  output$conf.plot<-renderPlot({

    plot(NULL
         ,xlim = c(input$mean-input$stdev,input$mean+input$stdev)
         ,ylim = c(0,40)
         ,yaxt = 'n'
         ,xaxt = 'n'
         ,ylab = ('Studies run over time')
         ,main="CIs of lots of studies over time (red CIs do not include the mean)"
         ,cex.main=1.5
         ,cex.lab=1.5
         ,xlab = NULL)
    
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
    
    # Plot the intervals
    for (i in 1:40){
      interval = intervals[[i]]
      if(input$mean>interval[1] & input$mean<interval[2]){
        lineColor <- 'black' }
      else {
        lineColor <- 'red' }
      lines(c(interval[1],interval[2]),c(i,i), lwd=2,col=lineColor)
    }
    
    if(input$running){
      # Create a new Interval
      
      x <<- rnorm(input$nsamp, mean = input$mean, sd = input$stdev)
      
      test <- t.test(x,conf.level=input$conf.level)
      intervals <<- c(list(test$conf.int), intervals) # feed at bottom
      intervals[[41]] <<- NULL # cut off top

      # Update histogram
      barheight <<- barheight + ((seq(input$mean-input$stdev,
                                      input$mean+input$stdev,
                                      length.out=500) > intervals[[1]][1]) &
                                   (seq(input$mean-input$stdev,
                                        input$mean+input$stdev,
                                        length.out=500) < intervals[[1]][2]))
      # Normalise the histogram
      cumulatives <<- cumulatives + 1
      barnorm <<- barheight / cumulatives
      
      # Auto-Update the Plot
      if(stepping){
        updateCheckboxInput(session,
                            inputId = "running",
                            label='Run Simulation',
                            value = FALSE)
        stepping <<- FALSE
      } else {
        invalidateLater(500 / input$speed)
      }
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
      if(stepping){
        updateCheckboxInput(session,
                            inputId = "running",
                            label='Run Simulation',
                            value = FALSE)
        stepping <<- FALSE
      } else {
        invalidateLater(500 / input$speed)
      }
    }
  })
  
  # Text of sample properties for left hand side:
  output$sample <- renderText({
    if(input$running){
      if(stepping){
        updateCheckboxInput(session,
                            inputId = "running",
                            label='Run Simulation',
                            value = FALSE)
        stepping <<- FALSE
      } else {
        invalidateLater(500 / input$speed)
      }
    }
    return(
      paste(
      "Population Mean: ", format(round(input$mean, digits=1), nsmall=1), " | ",
      "Sample Mean: ", format(round(mean(x), digits=1), nsmall=1), "\n",
      "Sample St. Dev:  ", format(round(sd(x), digits=1), nsmall=1), " | ",
      "Conf. Interval: ",
      format(round(intervals[[1]][1], digits=1), nsmall=1), "-",
      format(round(intervals[[1]][2], digits=1), nsmall=1),
      sep="")
    )
  })
  
  # Small histogram of sample for left hand side:
  output$samp.hist <- renderPlot({
    if(input$running){
      invalidateLater(500 / input$speed)
      if(stepping){
        updateCheckboxInput(session,
                            inputId = "running",
                            label='Run Simulation',
                            value = FALSE)
        stepping <<- FALSE
      }
    }
    # Plot histogram of sample
    hist(x,
         xlim = c(input$mean-2*input$stdev, input$mean+2*input$stdev),
         ylim = c(0, 0.4),
         breaks=50,
         freq=FALSE,
         main="Histogram of Sample and Distribution of Population",
         yaxt='n',
         xaxt='n',
         axes=FALSE)
    # Scale anbd label the axes
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
  
  


})

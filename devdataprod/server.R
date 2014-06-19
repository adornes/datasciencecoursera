# Developed by Daniel Adornes as partial requirement 
# for the Developing Data Products course, 
# by Johns Hopkins University through Coursera

library(shiny)
library(plyr)
library(ggplot2)


# Define server logic for the application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the data
  # for the selected crime type
  table.data <- reactive({
    State <- row.names(USArrests)
    Rate <- USArrests[,c(input$crime)]
    
    data.frame(State, Rate)
  })
  
  # Generate a plot of the data.
  output$plot <- renderPlot({
    if(input$ascDesc == "A"){
      plot <- ggplot(table.data(), aes(x=reorder(State, -Rate), y=Rate))
    }else{
      plot <- ggplot(table.data(), aes(x=reorder(State, Rate), y=Rate))
    }
    plot + geom_bar(stat="identity") + coord_flip() + xlab("State") + 
           ylab(paste(input$crime,"Rate", sep=" "))
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(table.data()$Rate)
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    if(input$ascDesc == "A"){
      arrange(table.data(),
              Rate,
              State)
    }else{
      arrange(table.data(),
              desc(Rate),
              State)
    }
  })
  
})

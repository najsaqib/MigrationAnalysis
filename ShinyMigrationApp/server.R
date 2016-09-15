library(shiny)
library(DT)

function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- CombinedStates
    if (input$source != "All") {
      data <- data[data$source == input$source,]
    }
    if (input$EngagementScore != "All") {
      data <- data[data$EngagementScore == input$EngagementScore,]
   
    }
    data
  }))
  
}


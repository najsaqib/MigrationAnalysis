library(DT)
library(shiny)
library(dplyr)

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Engagement State"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("Organization", "Organization ID", 
                  unique(as.character(CombinedStates1$ORGID15))),
      hr(),
      helpText("Data from Workforce Engagement Survey 2013 & 2015"),
      hr(),
      downloadButton('downloadFile','Download Report', class="dlButton")
    ),
    
    # Generating the main panel
    mainPanel(
      h2(textOutput("orgName")), # Header with the full name of the organization
      DT::dataTableOutput("EngagementTable")  # The main datatable
    )
    
  )
)

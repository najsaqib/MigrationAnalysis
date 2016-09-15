library(shiny)
library(DT)

fluidPage(
  titlePanel("Engagement Scores"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("source",
                       "source",
                       c("All",
                         unique(as.character(CombinedStates$source))))
    ),
    column(4,
           selectInput("EngagementScore",
                       "EngagementScore",
                       c("All",
                         unique(as.character(CombinedStates$EngagementScore))))
    )
  ),
  # Create a new row for the table.
  fluidRow(
    DT::dataTableOutput("table")
  )
)


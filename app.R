# Load packages
library("tidyverse")
library("shiny")

# Create UI
ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)

# Create Server
server <- function(input, output, session) {

}

# Run Application
shinyApp(ui, server)
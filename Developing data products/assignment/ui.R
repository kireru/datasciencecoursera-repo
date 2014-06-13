library(shiny)
#reading data from its raw form and processing the data
data <- readRDS("data/healthexp.Rds")
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Life Expectancy"),
  
  # Sidebar with controls to select the variable to plot against
  # Life.Expectancy and to specify whether outliers should be included
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Variable:",
                  c("Year" = "Year",
                    "Region" = "Region")),
      
      checkboxInput("outliers", "Show outliers", FALSE),
   
    hr(),
    helpText("Data from World bank on Life Expectancy by Andrew Data science specialization.")
  ),
    # Show the caption and plot of the requested variable against
    # Life.Expectancy
    mainPanel(
      h3(textOutput("caption")),
      
      plotOutput("Plot")
    )
  )
))

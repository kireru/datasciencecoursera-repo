library(shiny)

  #reading data from its raw form and processing the data
data <- readRDS("data/healthexp.Rds")
shinyServer(function(input, output) {
  
  # Compute the forumla text in a reactive expression since it is
  # shared by the output$caption and output$Plot functions
  formulaText <- reactive({
    paste("Life.Expectancy ~", input$variable)
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against Life Expectancy and 
  #only include outliers if requested
  output$Plot <- renderPlot({
    boxplot(as.formula(formulaText()), 
            data = data,
            outline = input$outliers)
  })
})
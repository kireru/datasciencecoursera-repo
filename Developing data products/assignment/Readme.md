This project uses the data taken from the world bank health indicator that shows the life expectancy. 

This  demonstrates a core feature of Shiny: **assignment**. In `server.R`, a reactive called `output$Plot` is declared. 

Notice that the reactive expression depends on the input expression `Output("Plot")`, and that it's used by both the output expression `output$Plot` and `input$variable`. 

**Notice** also that the reactive expression doesn't just update whenever anything changes--only the inputs it depends on will trigger an update. 

**Reactive expressions**: formulaText is a reactive expression. Note how it re-evaluates when the Variable field is changed, but not when the Show Outliers box is ticked.

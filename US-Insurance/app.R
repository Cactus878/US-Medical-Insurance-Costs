library(shiny)
library(bslib)
library(ggplot2)

ui <- page_sidebar(
  title = "US Medical Insurance",
  sidebar = sidebar(
    
    selectInput(inputId = 'x',
                label = 'X-axis',
                choices = c('age', 'bmi', 'charges')
                ),
    
    selectInput(inputId = 'y',
                label = 'Y-axis',
                choices = c('age', 'bmi', 'charges')
                )

  ),
  card(
    plotOutput(outputId = "scatterplot")
  )
)

server <- function(input, output) {
  output$scatterplot <- renderPlot({
    ggplot(data = insurance, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
}

shinyApp(ui = ui, server = server)

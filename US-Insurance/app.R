library(shiny)
library(bslib)
library(ggplot2)
library(caret)
library(readr)

insurance <- read_csv("~/Projects/US-Medical-Insurance-Costs/insurance.csv")

set.seed(100)

training <- createDataPartition(insurance$charges, p=0.8, list=FALSE)
trainingSet <- insurance[training,]
testingSet <- insurance[-training,]

model <- train(charges ~ ., data = trainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale", "center"),
               trControl = trainControl(method="none"),
               tuneGrid = data.frame(degree=1, scale=1, C=1)
)

model.cv <- train(charges ~ ., data = trainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale", "center"),
               trControl = trainControl(method="cv", number=10),
               tuneGrid = data.frame(degree=1, scale=1, C=1)
)

# Apply model for prediction
model.training <- predict(model, trainingSet)
model.testing <- predict(model, testingSet) 
model.cv.pred <- predict(model.cv, trainingSet)

# Model performance:(RMSE, MAE, R²)
model.training.metrics <- postResample(model.training, trainingSet$charges)
model.testing.metrics <- postResample(model.testing, testingSet$charges)
model.cv.metrics <- postResample(model.cv.pred, trainingSet$charges)

# Print performance metrics
print(model.training.metrics)
print(model.testing.metrics)
print(model.cv.metrics)
                                            
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

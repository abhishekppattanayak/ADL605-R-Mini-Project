library(shiny)
library(randomForest)
library(caret)
library(ggplot2)

# Load dataset
df <- read.csv("Auto.csv")

# Split data into training and testing sets
set.seed(1)
sample_indices <- sample(1:nrow(df), 0.8 * nrow(df))  # 80% for training, 20% for testing
train_data <- df[sample_indices, ]
test_data <- df[-sample_indices, ]

# Create the Random Forest model using training data
rf_model <- randomForest(mpg ~ cylinders + displacement +	horsepower + weight + acceleration, data = train_data)

# UI
ui <- fluidPage(
  titlePanel("Miles per gallon Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("cylinders", "Cylinders", value = 8, min = 2, max = 10),
      numericInput("displacement", "Displacement", value = 400, min = 50, max = 500),
      numericInput("horsepower", "Horsepower", value = 170, min = 30, max = 150),
      numericInput("weight", "Weight", value = 4668, min = 1000, max = 5000),
      numericInput("acceleration", "Acceleration", value = 11.5, min = 5.0, max = 20.0),
      actionButton("predict_button", "Predict")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Prediction", 
                 h4("Predicted Miles per gallon:"),
                 verbatimTextOutput("prediction")
        ),
        tabPanel("Exploratory Data Analysis",
                 plotOutput("histogram"),
                 plotOutput("scatterplot"),
                 plotOutput("boxplot"),
                 plotOutput("bar_chart_cylinders"),
        )
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Prediction
  observeEvent(input$predict_button, {
    
    new_data <- data.frame(
      cylinders = input$cylinders,
      displacement = input$displacement,
      horsepower = input$horsepower,
      weight = input$weight,
      acceleration = input$acceleration,
      stringsAsFactors = FALSE
    )
    
    prediction <- predict(rf_model, newdata = new_data)
    print(prediction)  # Debugging statement to check prediction result

    output$prediction <- renderText({
      paste("Miles per Gallon:", prediction)
    })
  })
  

  
  # Exploratory Data Analysis
  output$histogram <- renderPlot({
    ggplot(df, aes(x = horsepower)) +
      geom_histogram(fill = "skyblue", color = "black") +
      labs(title = "Histogram of Horsepower")
  })
  
  output$scatterplot <- renderPlot({
    ggplot(df, aes(x = horsepower, y = acceleration)) +
      geom_point() +
      labs(title = "horsepower vs. acceleration", x = "horsepower", y = "acceleration")
  })
  
  
  output$boxplot <- renderPlot({
    ggplot(df, aes(x = as.factor(cylinders), y = mpg)) +
      geom_boxplot(fill = "skyblue", color = "black") +
      labs(title = "Boxplot of MPG by Cylinder Count", x = "Cylinders", y = "MPG")
  })
  
  output$bar_chart_cylinders <- renderPlot({
    ggplot(df, aes(x = as.factor(cylinders))) +
      geom_bar(fill = "skyblue", color = "black") +
      labs(title = "Bar Chart of Cylinder Counts", x = "Cylinders", y = "Frequency")
  })
}


# Run the application
shinyApp(ui = ui, server = server)

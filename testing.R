library(shiny)
library(randomForest)
library(caret)
library(ggplot2)

# Load dataset
df <- read.csv("Mobile_Price_prediction.csv")
df$price_range <- factor(df$price_range)

# Split data into training and testing sets
set.seed(1)
sample_indices <- sample(1:nrow(df), 0.8 * nrow(df))  # 80% for training, 20% for testing
train_data <- df[sample_indices, ]
test_data <- df[-sample_indices, ]

# Create the Random Forest model using training data
rf_model <- randomForest(price_range ~ battery_power + dual_sim + wifi + four_g + px_height + px_width + ram, data = train_data)

# Define custom price range labels in rupees
price_labels <- c("Low (0 - 5000)", "Medium (5001 - 15000)", "High (15001 - 30000)", "Very High (30001 - 50000)")

# UI
ui <- fluidPage(
  titlePanel("Mobile Price Range Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("battery_power", "Battery Power", value = 1500, min = 500, max = 2000),
      selectInput("dual_sim", "Dual SIM", choices = c("No" = 0, "Yes" = 1), selected = 0),
      selectInput("wifi", "WiFi", choices = c("No" = 0, "Yes" = 1), selected = 0),
      selectInput("four_g", "4G", choices = c("No" = 0, "Yes" = 1), selected = 0),
      numericInput("px_height", "Pixel Height", value = 500, min = 0, max = 2000),
      numericInput("px_width", "Pixel Width", value = 800, min = 0, max = 2000),
      numericInput("ram", "RAM", value = 256, min = 256, max = 6000, step = 256),
      actionButton("predict_button", "Predict")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Prediction", 
                 h4("Predicted Price Range:"),
                 verbatimTextOutput("prediction")
        ),
        tabPanel("Exploratory Data Analysis",
                 plotOutput("histogram"),
                 plotOutput("scatterplot")
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
      battery_power = input$battery_power,
      dual_sim = input$dual_sim,
      wifi = input$wifi,
      four_g = input$four_g,
      px_height = input$px_height,
      px_width = input$px_width,
      ram = input$ram
    )
    prediction <- predict(rf_model, newdata = new_data)
    price_label <- price_labels[as.numeric(prediction)]
    output$prediction <- renderText({
      paste("Price Range:", price_label)
    })
  })
  
  # Exploratory Data Analysis
  output$histogram <- renderPlot({
    ggplot(df, aes(x = battery_power)) +
      geom_histogram(fill = "skyblue", color = "black") +
      labs(title = "Histogram of Battery Power")
  })
  
  output$scatterplot <- renderPlot({
    ggplot(df, aes(x = px_height, y = px_width, color = price_range)) +
      geom_point() +
      labs(title = "Pixel Height vs. Pixel Width", x = "Pixel Height", y = "Pixel Width")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
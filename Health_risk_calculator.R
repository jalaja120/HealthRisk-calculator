# Load necessary libraries
library(shiny)
library(ggplot2)

# Define the UI
ui <- fluidPage(
  titlePanel("Health Risk Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Age (years):", value = 30, min = 0, max = 120),
      numericInput("weight", "Weight (kg):", value = 70, min = 0),
      selectInput("smoking", "Smoking Habit:",
                  choices = c("Non-Smoker", "Occasional Smoker", "Regular Smoker")),
      numericInput("cholesterol", "Cholesterol Level (mg/dL):", value = 200, min = 0),
      numericInput("systolic_bp", "Systolic Blood Pressure (mmHg):", value = 120, min = 0),
      actionButton("calculate", "Calculate Risk")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Results", 
                 verbatimTextOutput("risk_output"),
                 plotOutput("risk_plot")),
        tabPanel("Recommendations", verbatimTextOutput("recommendations"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Updated coefficients for a balanced logistic regression model
  calculate_risk <- reactive({
    req(input$calculate)
    
    coef <- list(
      intercept = -7.0,  # Lower baseline risk
      age = 0.015,       # Moderate age impact
      weight = 0.002,    # Reduced weight impact
      smoking = c("Non-Smoker" = 0, "Occasional Smoker" = 0.5, "Regular Smoker" = 1.2),  # Significant smoking impact
      cholesterol = 0.005,  # Balanced cholesterol contribution
      systolic_bp = 0.01    # Balanced systolic BP contribution
    )
    
    # Handle smoking coefficient safely
    smoking_coef <- coef$smoking[[input$smoking]]
    
    # Calculate risk score
    score <- coef$intercept +
      coef$age * input$age +
      coef$weight * input$weight +
      smoking_coef +
      coef$cholesterol * input$cholesterol +
      coef$systolic_bp * input$systolic_bp
    
    # Convert to probability using logistic function
    risk <- 1 / (1 + exp(-score))
    return(risk)
  })
  
  # Output the risk calculation
  output$risk_output <- renderPrint({
    req(input$calculate)
    risk <- calculate_risk()
    paste("Estimated Risk of Heart Disease:", round(risk * 100, 2), "%")
  })
  
  # Output a visualization of risk
  output$risk_plot <- renderPlot({
    req(input$calculate)
    risk <- calculate_risk()
    
    ggplot(data.frame(x = c("Risk", "Remaining"), y = c(risk, 1 - risk)), aes(x = x, y = y, fill = x)) +
      geom_bar(stat = "identity") +
      labs(title = "Risk Distribution", y = "Proportion") +
      scale_fill_manual(values = c("Risk" = "red", "Remaining" = "green")) +
      theme_minimal()
  })
  
  # Provide recommendations based on the risk
  output$recommendations <- renderPrint({
    req(input$calculate)
    risk <- calculate_risk()
    
    if (risk > 0.7) {
      "High risk detected. Consult a healthcare provider for immediate action. Focus on lifestyle changes like a healthy diet, regular exercise, and quitting smoking."
    } else if (risk > 0.3) {
      "Moderate risk detected. Monitor your health and adopt a healthier lifestyle. Consider consulting a healthcare provider for advice."
    } else {
      "Low risk detected. Maintain your current lifestyle and consider regular health check-ups."
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

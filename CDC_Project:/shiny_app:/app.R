library(shiny)
library(ggplot2)
library(dplyr)

# -----------------------
# Simulated data
# -----------------------
data <- data.frame(
  math = rnorm(100, 70, 10),
  reading = rnorm(100, 75, 8),
  writing = rnorm(100, 72, 9)
)

# -----------------------
# UI
# -----------------------
ui <- fluidPage(
  
  titlePanel("Student Score Explorer (A+ Version)"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "subject",
        "Choose a subject:",
        choices = c("math", "reading", "writing")
      )
    ),
    
    mainPanel(
      plotOutput("histPlot"),
      plotOutput("scatterPlot"),
      textOutput("meanText"),
      textOutput("corText")
    )
  )
)

# -----------------------
# Server
# -----------------------
server <- function(input, output) {
  
  selected_data <- reactive({
    data[[input$subject]]
  })
  
  output$histPlot <- renderPlot({
    ggplot(data.frame(x = selected_data()), aes(x = x)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 20) +
      theme_minimal() +
      labs(title = paste("Distribution of", input$subject),
           x = input$subject,
           y = "Count")
  })
  
  output$meanText <- renderText({
    paste("Mean:", round(mean(selected_data()), 2))
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(data, aes_string(x = "math", y = input$subject)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_smooth(method = "lm", color = "red") +
      theme_minimal() +
      labs(title = paste("Math vs", input$subject))
  })
  
  output$corText <- renderText({
    cor_value <- cor(data$math, selected_data())
    paste("Correlation with Math:", round(cor_value, 2))
  })
  
}

# -----------------------
# Run app
# -----------------------
shinyApp(ui, server)
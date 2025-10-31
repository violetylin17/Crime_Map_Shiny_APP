# name: Jason,YuChien(Violet), Becky

library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(shinyWidgets)

# Load data
df <- read_csv("crime_vs_socioeconomic_factors.csv")

# UI
ui <- fluidPage(
  setBackgroundColor(
    color = c("#FFFCFF", "blue"),
    gradient = "linear",
    direction = "bottom"
  ),
  titlePanel("Crime Rate Vs. Socioeconomic Factors (Jason, Violet, Becky)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var1", "X-axis variable", choices = names(df)),
      selectInput("var2", "Y-axis variable", choices = names(df)),
      sliderInput("point_size", "Point Size", min = 1, max = 5, value = 2),
      sliderInput("alpha", "Transparency", min = 0.1, max = 1, value = 0.7),
      checkboxInput("show_lm", "Add Regression Line", value = FALSE),
      br(),
      h4("Summary Statistics"),
      verbatimTextOutput("summary"),
      br(),
      h4("Correlation"),
      textOutput("correlation")
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$plot <- renderPlot({
    gg <- ggplot(df, aes(x = .data[[input$var1]], y = .data[[input$var2]])) +
      geom_point(color = "blue", size = input$point_size, alpha = input$alpha) +
      ggtitle("Scatter Plot") +
      theme_minimal()
    
    if (input$show_lm) {
      gg <- gg + geom_smooth(method = "lm", se = FALSE, color = "red")
    }
    
    gg
  })
  
  output$summary <- renderPrint({
    summary(df[, c(input$var1, input$var2)])
  })
  
  output$correlation <- renderText({
    corr <- cor(df[[input$var1]], df[[input$var2]], use = "complete.obs")
    paste("Correlation:", round(corr, 3))
  })
}

# Run app
shinyApp(ui = ui, server = server)

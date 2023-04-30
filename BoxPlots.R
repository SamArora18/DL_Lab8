#Violen/Box Plots

library(shiny)
library(ggplot2)
library(dplyr)

# Load iris dataset
data(iris)

# Define UI
ui <- fluidPage(
  titlePanel("Violin Plot of Iris Dataset"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose variable", choices = names(iris)[-5], selected = "Sepal.Length"),
      br(),
      checkboxInput("show_boxplot", "Show boxplot", value = TRUE),
      br(),
      sliderInput("bandwidth", "Bandwidth", min = 0.01, max = 1, value = 0.2, step = 0.01)
    ),
    mainPanel(
      plotlyOutput("violinplot", height = "500px")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Create violin plot
  output$violinplot <- renderPlotly({
    
    iris_subset <- iris %>% filter(Species %in% c("setosa", "versicolor", "virginica"))
    
    plot <- iris_subset %>%
      ggplot(aes(x = Species, y = !!sym(input$variable), fill = Species)) +
      geom_violin(kernel = "cosine", adjust = input$bandwidth, trim = TRUE) +
      geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA, alpha = 0.7) +
      labs(title = paste("Violin Plot of", input$variable),
           x = "Species", y = input$variable) +
      theme_bw()
    
    if (input$show_boxplot) {
      plot <- plot + geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA, alpha = 0.7)
    }
    
    ggplotly(plot)
  })
  
}

# Run app
shinyApp(ui, server)


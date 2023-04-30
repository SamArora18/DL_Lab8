#ScatterPlots

library(shiny)
library(ggplot2)
library(dplyr)

# Load iris dataset
data(iris)

# Define UI
ui <- fluidPage(
  titlePanel("Scatter Plot of Iris Dataset"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x", "Choose x-axis variable", choices = names(iris)[-5], selected = "Sepal.Length"),
      selectInput("y", "Choose y-axis variable", choices = names(iris)[-5], selected = "Sepal.Width"),
      selectInput("color", "Choose species variable", choices = c("Species")),
      br(),
      checkboxInput("jitter", "Add jitter", value = FALSE),
      br(),
      textInput("setosa_color", "Setosa color:", value = "red"),
      textInput("versicolor_color", "Versicolor color:", value = "green"),
      textInput("virginica_color", "Virginica color:", value = "blue"),
      br(),
      numericInput("setosa_size", "Setosa point size:", value = 3, min = 1, max = 10),
      numericInput("versicolor_size", "Versicolor point size:", value = 3, min = 1, max = 10),
      numericInput("virginica_size", "Virginica point size:", value = 3, min = 1, max = 10),
      br(),
      selectInput("setosa_shape", "Setosa point shape:", choices = c("circle", "square", "triangle-up"), selected = "circle"),
      selectInput("versicolor_shape", "Versicolor point shape:", choices = c("circle", "square", "triangle-up"), selected = "circle"),
      selectInput("virginica_shape", "Virginica point shape:", choices = c("circle", "square", "triangle-up"), selected = "circle"),
      br(),
      checkboxInput("show_all_species", "Show all species", value = TRUE),
      checkboxGroupInput("species", "Select species to display", choices = unique(iris$Species), selected = unique(iris$Species)),
      br(),
      actionButton("reset", "Reset Zoom")
    ),
    mainPanel(
      plotOutput("scatterplot", height = "500px")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Create scatter plot
  output$scatterplot <- renderPlot({
    iris_subset <- iris
    
    if (!input$show_all_species) {
      iris_subset <- iris_subset %>% filter(Species %in% input$species)
    }
    
    iris_subset %>%
      ggplot(aes_string(x = input$x, y = input$y, color = input$color, shape = "Species")) +
      geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
                 size = case_when(
                   iris_subset$Species == "setosa" ~ input$setosa_size,
                   iris_subset$Species == "versicolor" ~ input$versicolor_size,
                   iris_subset$Species == "virginica" ~ input$virginica_size
                 ),
                 alpha = 0.8) +
      scale_color_manual(values = c(setosa = input$setosa_color,
                                    versicolor = input$versicolor_color,
                                    virginica = input$virginica_color)) +
      scale_shape_manual(values = c(setosa = input$setosa_shape,
                                    versicolor = input$versicolor_shape,
                                    virginica = input$virginica_shape)) +
      theme_bw()
  })
  
  # Reset zoom
  observeEvent(input$reset, {
    session$sendCustomMessage(type = "plotly_relayout", list(xaxis = list(range = NULL), yaxis = list(range = NULL)))
  })
  
}

# Run app
shinyApp(ui, server)
                                                                                              

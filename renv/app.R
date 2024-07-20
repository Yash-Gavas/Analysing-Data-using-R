library(shiny)
library(plotly)
library(tidyverse)
library(lubridate)
library(DT)
library(ggplot2)
library(maps)
library(ggmap)

# Function to load the new weather data from a CSV file
load_weather_data <- function() {
  file_path <- "weather_classification_data.csv"  # Adjust the file path to where the CSV is stored
  if (file.exists(file_path)) {
    weather_data <- read_csv(file_path)
    print(head(weather_data))  # Print the first few rows of the data for inspection
    return(weather_data)
  } else {
    stop(paste("File '", file_path, "' does not exist. Please check the file path.", sep = ""))
  }
}

# Define UI
ui <- fluidPage(
  titlePanel("Real-time Weather Analytics Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Select Season:", choices = NULL),  # choices will be updated dynamically
      selectInput("weather_type", "Select Weather Type:", choices = NULL)  # choices will be updated dynamically
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Graph", plotlyOutput("bar_plot")),
        tabPanel("Box Plot", plotlyOutput("box_plot")),
        tabPanel("3D Scatter Plot", plotlyOutput("scatter3d_plot")),
        tabPanel("Heat Map", plotlyOutput("heatmap_plot")),
        tabPanel("Map Visualization", plotOutput("map_plot")),
        tabPanel("Weather Data", DTOutput("weather_table")),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load weather data
  weather_data <- reactiveVal(NULL)
  
  observe({
    weather_data(load_weather_data())
  })
  
  # Update the season and weather type selection choices
  observe({
    req(weather_data())
    season_choices <- unique(weather_data()$Season)
    updateSelectInput(session, "season", choices = season_choices, selected = season_choices[1])
    
    weather_type_choices <- unique(weather_data()$`Weather Type`)
    updateSelectInput(session, "weather_type", choices = weather_type_choices, selected = weather_type_choices[1])
  })
  
  output$bar_plot <- renderPlotly({
    req(input$season, input$weather_type, weather_data())
    filtered_data <- weather_data() %>% filter(Season == input$season, `Weather Type` == input$weather_type)
    plot_ly(filtered_data, x = ~Temperature, type = 'bar', marker = list(color = 'yellow')) %>%
      layout(title = paste("Temperature Bar Graph for", input$weather_type, "in", input$season), xaxis = list(title = "Temperature"), yaxis = list(title = "Count"))
  })
  
  output$box_plot <- renderPlotly({
    req(input$season, input$weather_type, weather_data())
    filtered_data <- weather_data() %>% filter(Season == input$season, `Weather Type` == input$weather_type)
    plot_ly(filtered_data, y = ~Humidity, type = 'box', name = "Humidity") %>%
      add_trace(y = ~`Wind Speed`, type = 'box', name = "Wind Speed") %>%
      layout(title = paste("Box Plot for", input$weather_type, "in", input$season), yaxis = list(title = "Value"))
  })
  
  output$scatter3d_plot <- renderPlotly({
    req(input$season, input$weather_type, weather_data())
    filtered_data <- weather_data() %>% filter(Season == input$season, `Weather Type` == input$weather_type)
    plot_ly(filtered_data, x = ~Temperature, y = ~Humidity, z = ~`Wind Speed`, type = 'scatter3d', mode = 'markers') %>%
      layout(title = paste("3D Scatter Plot for", input$weather_type, "in", input$season), scene = list(
        xaxis = list(title = "Temperature"),
        yaxis = list(title = "Humidity"),
        zaxis = list(title = "Wind Speed")
      ))
  })
  
  output$heatmap_plot <- renderPlotly({
    req(input$season, input$weather_type, weather_data())
    filtered_data <- weather_data() %>% filter(Season == input$season, `Weather Type` == input$weather_type)
    plot_ly(filtered_data, x = ~Temperature, y = ~Humidity, z = ~`Precipitation (%)`, type = 'heatmap') %>%
      layout(title = paste("Heat Map for", input$weather_type, "in", input$season), xaxis = list(title = "Temperature"), yaxis = list(title = "Humidity"))
  })
  
  output$map_plot <- renderPlot({
    req(input$season, input$weather_type, weather_data())
    filtered_data <- weather_data() %>% filter(Season == input$season, `Weather Type` == input$weather_type)
    
    # Using ggplot2 and maps to create a map visualization
    world_map <- map_data("world")
    ggplot() +
      geom_map(data = world_map, map = world_map,
               aes(x = long, y = lat, map_id = region),
               fill = "white", color = "black") +
      geom_point(data = filtered_data, aes(x = runif(n(), -180, 180), y = runif(n(), -90, 90)),  # Replace with actual coordinates
                 color = "red", size = 3) +
      labs(title = paste("Map Visualization for", input$weather_type, "in", input$season), x = "Longitude", y = "Latitude")
  })
  
  output$weather_table <- renderDT({
    req(weather_data())
    datatable(weather_data())
  })
  
  output$summary <- renderPrint({
    req(input$season, input$weather_type, weather_data())
    filtered_data <- weather_data() %>% filter(Season == input$season, `Weather Type` == input$weather_type)
    summary_data <- filtered_data %>% summarize(
      Total_Observations = n(),
      Average_Temperature = mean(Temperature),
      Average_Humidity = mean(Humidity),
      Average_Wind_Speed = mean(`Wind Speed`),
      Average_Precipitation = mean(`Precipitation (%)`),
      Average_Cloud_Cover = mean(`Cloud Cover`),
      Average_Atmospheric_Pressure = mean(`Atmospheric Pressure`),
      Average_UV_Index = mean(`UV Index`),
      Average_Visibility = mean(`Visibility (km)`)
    )
    print(summary_data)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

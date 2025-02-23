# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(readr)
library(leaflet)

# Read the crime data
crime_data <- read_csv("D:/Soham_new/Mad Hacks Data/updated_crime_data.csv")

# Convert Date column to POSIXct for easier manipulation
crime_data$Date <- as.POSIXct(crime_data$Date, format = "%d-%m-%Y %H:%M")

# Ensure Latitude and Longitude are numeric (important for mapping)
crime_data <- crime_data %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

# Define UI ----
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),  
  titlePanel("Chicago Crime Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("crimeCategory", "Select Crime Category:", 
                  choices = unique(crime_data$Crime_Category), 
                  selected = unique(crime_data$Crime_Category)[1], multiple = TRUE),
      selectInput("timeUnit", "Select Time Unit:", 
                  choices = c("Day", "Month"), 
                  selected = "Day"),
      checkboxInput("showCumulative", "Show Cumulative Crimes", FALSE),  
      style = "padding: 20px;"
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Crime Trends", 
                 plotOutput("barPlot", height = "500px"), 
                 conditionalPanel(
                   condition = "input.showCumulative == true", 
                   plotOutput("cumulativePlot", height = "500px")
                 )),
        
        tabPanel("Crime Map", leafletOutput("crimeMap", height = "600px"))  
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # Reactive expression to filter and summarize crime data
  bar_data <- reactive({
    if (input$timeUnit == "Day") {
      crime_data %>%
        filter(Crime_Category == input$crimeCategory) %>%
        group_by(Date = as.Date(Date)) %>%
        summarise(Count = n(), .groups = "drop")
    } else {
      crime_data %>%
        filter(Crime_Category == input$crimeCategory) %>%
        mutate(Month = format(Date, "%Y-%m")) %>%
        group_by(Month) %>%
        summarise(Count = n(), .groups = "drop")
    }
  })
  
  # Render the bar plot
  output$barPlot <- renderPlot({
    ggplot(data = bar_data(), aes(x = if (input$timeUnit == "Day") Date else Month, y = Count)) +
      geom_bar(stat = "identity", fill = "#C71585", color = "white") +  
      labs(title = paste("Crime Density for", input$crimeCategory),
           x = input$timeUnit, y = "Number of Crimes") +
      theme_minimal(base_size = 15) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(face = "bold.italic"),
        axis.title.y = element_text(face = "bold.italic"),
        panel.grid.major = element_line(color = "lightgray"),
        panel.grid.minor = element_blank()
      )
  })
  
  # Cumulative Crime Data (for all categories)
  cumulative_data <- reactive({
    crime_data %>%
      mutate(Month = format(Date, "%Y-%m")) %>%
      group_by(Month, Crime_Category) %>%
      summarise(Count = n(), .groups = "drop") %>%
      arrange(Month)
  })
  
  # Render the cumulative crime plot
  output$cumulativePlot <- renderPlot({
    ggplot(data = cumulative_data(), aes(x = Month, y = Count, fill = Crime_Category)) +
      geom_area(position = "stack", alpha = 0.6) +  
      labs(title = "Cumulative Crimes Over Time",
           x = "Month", y = "Total Crimes") +
      theme_minimal(base_size = 15) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(face = "bold.italic"),
        axis.title.y = element_text(face = "bold.italic"),
        legend.position = "right"
      )
  })
  
  # Reactive expression for crime map data
  map_data <- reactive({
    filtered_data <- crime_data %>%
      filter(Crime_Category == input$crimeCategory) %>%
      filter(!is.na(Latitude) & !is.na(Longitude))  
    
    if (nrow(filtered_data) == 0) {
      return(NULL)  
    }
    return(filtered_data)
  })
  
  # Render the Leaflet crime map
  output$crimeMap <- renderLeaflet({
    data <- map_data()
    
    if (is.null(data)) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = -87.6298, lat = 41.8781, zoom = 10) %>%
        addPopups(-87.6298, 41.8781, "No data available for selected category")
    } else {
      leaflet(data) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addMarkers(
          lng = ~Longitude, lat = ~Latitude,
          clusterOptions = markerClusterOptions(),  # Fixed clustering function
          popup = ~paste("Crime:", Crime_Category, "<br>", "Date:", Date)
        )
    }
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

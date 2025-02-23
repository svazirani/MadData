# LIBRARIES ----
library(shiny)
library(leaflet)
library(dplyr)

# LOAD DATA ----
crime_data <- read.csv("updated_crime_data.csv")

# Ensure dataset contains necessary columns
if (!all(c("Latitude", "Longitude", "Primary.Type") %in% colnames(crime_data))) {
    stop("Dataset must contain 'Latitude', 'Longitude', and 'Primary.Type' columns.")
}

# UI ----
ui <- fluidPage(
    titlePanel("Chicago Crime Dynamic Map"),

    sidebarLayout(
        sidebarPanel(
            selectInput("crime_type", "Select Crime Type:",
                        choices = c("ALL", unique(crime_data$Primary.Type)),
                        selected = "ALL",
                        multiple = TRUE),

            radioButtons("map_type", "Map Type:",
                         choices = c("Heatmap" = "heat", "Markers" = "markers"),
                         selected = "markers"),

            sliderInput("safe_radius", "Safe Walking Distance (meters):",
                        min = 100, max = 2000, value = 500, step = 100),

            actionButton("update_map", "Update Map", class = "btn btn-primary")
        ),

        mainPanel(
            leafletOutput("crime_map", height = "700px")
        )
    )
)

# SERVER ----
server <- function(input, output, session) {

    # Filter data reactively
    filtered_data <- reactive({
        req(crime_data)
        if (!("ALL" %in% input$crime_type)) {
            crime_data %>% filter(Primary.Type %in% input$crime_type)
        } else {
            crime_data
        }
    })

    # Initial Map Setup
    output$crime_map <- renderLeaflet({
        leaflet() %>%
            setView(lng = -87.6298, lat = 41.8781, zoom = 11) %>%  # Center on Chicago
            addProviderTiles(providers$CartoDB.Positron)  # Light-themed basemap
    })

    # Update Map Dynamically
    observeEvent(input$update_map, {
        data <- filtered_data()

        leafletProxy("crime_map") %>%
            clearMarkers() %>%
            clearShapes() %>%
            clearHeatmap() %>%
            {
                if (input$map_type == "markers") {
                    addCircleMarkers(
                        data = data,
                        lng = ~Longitude, lat = ~Latitude,
                        color = ~as.factor(Primary.Type),
                        radius = 4, stroke = FALSE, fillOpacity = 0.7,
                        popup = ~paste("<strong>Crime Type:</strong>", Primary.Type,
                                       "<br><strong>Location:</strong>", Location.Description)
                    )
                } else {
                    addHeatmap(
                        data = data,
                        lng = ~Longitude, lat = ~Latitude,
                        intensity = 1, blur = 20, max = 0.05
                    )
                }
            } %>%
            addCircles(
                lng = -87.6298, lat = 41.8781, radius = input$safe_radius,
                color = "blue", fillColor = "blue", fillOpacity = 0.2
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)

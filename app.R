#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(DT)
library(plotly)
library(tidyverse)
library(DataExplorer)
library(leaflet)

crime_data <- read.csv("cleaned_crime_data.csv")

data_list <- list(
    "Chicago Crime Data" = crime_data
)

# Ensure the dataset contains latitude & longitude
if (!all(c("Latitude", "Longitude", "Primary.Type") %in% colnames(crime_data))) {
    stop("Dataset must contain 'Latitude', 'Longitude', and 'Primary.Type' columns.")
}


# Define UI for application that draws a histogram
# 1.0 USER INTERFACE ----
ui <- navbarPage(

    title = "Chicago Crime Explorer",

    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),

    tabPanel(
        title = "Explore",

        sidebarLayout(

            sidebarPanel(
                width = 3,
                h1("Explore Crime Data"),

                selectInput(
                    inputId = "crime_type",
                    label   = "Select Crime Type:",
                    choices = unique(crime_data$Primary.Type),
                    selected = "ALL",
                    multiple = TRUE
                ),

                sliderInput(
                    inputId = "safe_radius",
                    label   = "Safe Walking Distance (meters):",
                    min = 100, max = 2000, value = 500, step = 100
                ),

                actionButton("update_map", "Update Map", class = "btn btn-primary"),

                hr(),
                h3("Chicago Crime Analysis"),
                p("Explore crime patterns and trends using interactive tools.")
            ),

            mainPanel(
                tabsetPanel(
                    tabPanel("Crime Map", leafletOutput("crime_map", height = 700)),
                    tabPanel("Data Table", DT::dataTableOutput("data_table"))
                )
            )
        )
    )
)

# Define server logic required to draw a histogram




# Run the application
shinyApp(ui = ui, server = function(input, output) {})

# shinyApp(ui = ui, server = server)


library(shiny)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(tibble)
library(lubridate)
library(sp)
library(sf)
library(rgdal)
library(leaflet)

# Load data
all_months <- read_csv('jan_dec.csv')

# Read taxi zone shapefiles
taxi_zones <- readOGR("taxi_zones/taxi_zones.shp")

# Transform polygon
proj <- spTransform(taxi_zones, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

# Creating a separate column for hour of the day
all_months$hour <- hour(all_months$tpep_pickup_datetime)

# Creating a separate column for weekdays
all_months$day <- wday(all_months$tpep_pickup_datetime, label = TRUE, week_start = 1)

# Creating a separate column for months
all_months$month <- month(all_months$tpep_pickup_datetime, label = TRUE)

# Creating a separate column for the date
all_months$date <- date(all_months$tpep_pickup_datetime)

# Replace Inf values with 0
all_months <- all_months %>%
    mutate(avg_mph = ifelse(avg_mph == Inf, 0, avg_mph))

#add column containing names
zone_df <- as.data.frame(taxi_zones) %>%
    select("LocationID", "zone")

all_months$PUzone <- zone_df$zone[match(all_months$PULocationID, zone_df$LocationID)]
# Exclude values where duration is negative
all_months <- all_months %>%
    filter (duration > 0)

ui <- fluidPage(
    titlePanel("Traffic Volume Analysis"),
    mainPanel(
        leafletOutput("map"),
        br(), br()),
    sidebarPanel(
        ### User chooses the species to map
        selectInput("location", "Starting Area Location",
                    unique(all_months$PUzone))
    ))

server <- function(input, output, session) {
    output$map <- renderLeaflet({
        filtered_df <- 
            all_months %>%
            filter(PUzone == input$location) %>%
            group_by(DOLocationID) %>%
            tally()
        
        if(nrow(filtered_df) < 5){stop("Not enough unique dropoff locations from this origin point. Please select another starting point.")}
        
        #create popup contents
        content_do <- paste("Neighborhood:", taxi_zones$zone, "<br/>",
                            "Number of Dropoffs:", filtered_df$n, "<br/>")
        #filter second projection
        proj2 <- subset(proj, proj$zone %in% input$location)
        
        #create pal
        pal <- colorNumeric(
            palette = "Blues",
            domain = filtered_df$n)
        
        #create map
        leaflet(filtered_df) %>%
            addTiles() %>%
            setView(lng = -73.98928, lat = 40.75042, zoom = 10.2) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addPolygons(
                data = proj,
                popup = content_do,
                weight = 1,
                fillColor = ~pal(filtered_df$n),
                fillOpacity = 1,
                highlightOptions = highlightOptions(
                    color='#000000',
                    weight = 3,
                    bringToFront = TRUE,
                    sendToBack = TRUE),
                label = taxi_zones$zone) %>%
            addPolygons(data = proj2,
                        weight = 1,
                        color = "Red",
                        fill = "Red") %>%
            addLegend("topright",
                      pal = pal,
                      values = filtered_df$n,
                      title = "Drop Off Volume",
                      opacity = 1)
    })
}

shinyApp(ui, server)

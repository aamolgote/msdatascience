#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(maps)
library(scales)
library(sf)

king <- read_csv("data/KING COUNTY House Data.csv")

# Define server logic required to draw 3 graphs for assignment 3
shinyServer(function(input, output, session) {
  
    # Get County data
    counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
    counties_wa <-counties %>%
      filter(str_detect(ID, 'washington,')) # Filter Washington state counties
    counties_wa_king <- counties_wa %>%
      filter(str_detect(ID, "king")) #Filter king county data
    sites <- data.frame(longitude = c(-122.3321), latitude = c(47.6062))
    
    
    average_price_data <- reactive({
      floorsFilter <- input$floors # Floors Filter
      waterfrontView <- input$waterfront #Water front filter
      livingSqFeet <- input$livingSqFeet #Living area sq feet filter
      gradeMin <- input$gradeRange[1] #Minimum grade from slider input
      gradeMax <- input$gradeRange[2] #Maximum grade from slider input
      
      #Set appropriate water front filter vector, based on the drop down input from ui
      waterfrontFilter <- c(0,1)
      if (waterfrontView == 1){
        waterfrontFilter <- c(1)
      }
      else if (waterfrontView == 0){
        waterfrontFilter <- c(0)
      }
      else if (waterfrontView == -1){
        waterfrontFilter <- c(0,1)  
      }
      # Number of bedrooms filter 
      numberOfBedRooms <- input$numberOfBedRooms
      
      #Build the data for the plot averagePriceEachYear
      averagePriceEachYear <- king %>%
        filter(condition %in% input$condition & waterfront %in% waterfrontFilter) %>%
        filter(waterfront %in% waterfrontFilter) %>%
        filter(floors %in% floorsFilter) %>%
        filter(bedrooms >= numberOfBedRooms) %>%
        filter(sqft_living >= livingSqFeet) %>%
        filter (grade > gradeMin & grade < gradeMax) %>%
        filter (price >= input$priceRange[1] & price <= input$priceRange[2]) %>%
        group_by(yr_built) %>%
        summarise(averagePrice = mean(price))
    })

    #Plot 1: Average price Year on Year
    output$averagePricePlot <- renderPlot({
        ggplot(data = average_price_data()) +
        geom_point(aes(x = yr_built, y = averagePrice)) +
        geom_smooth(aes(yr_built, averagePrice)) +
        scale_y_continuous(labels = scales::dollar) +
        labs(x = "Year", y = "Average Price") +
        theme_minimal()
    })
    
    houses_data <- reactive({
      waterfrontView <- input$waterfront #get the waterfront filter
      floorsFilter <- input$floors #get floors filter
      
      #Set appropriate water front filter vector, based on the drop down input from ui
      waterfrontFilter <- c(0,1)
      if (waterfrontView == 1){
        waterfrontFilter <- c(1)
      }
      else if (waterfrontView == 0){
        waterfrontFilter <- c(0)
      }
      else if (waterfrontView == -1){
        waterfrontFilter <- c(0,1)  
      }
      numberOfBedRooms <- input$numberOfBedRooms #Number of bedrooms filter
      livingSqFeet <- input$livingSqFeet #Living Area Sq feet filter
      gradeMin <- input$gradeRange[1] #Minimum grade from slider input
      gradeMax <- input$gradeRange[2] #Maximum grade from slider input
      
      # Apply filter to houses data for king county
      king  %>% 
        filter(condition %in% input$condition) %>%
        filter(waterfront %in% waterfrontFilter) %>%
        filter(floors %in% floorsFilter) %>%
        filter(bedrooms >= numberOfBedRooms) %>%
        filter(sqft_living >= livingSqFeet) %>%
        filter (grade > gradeMin & grade < gradeMax) %>%
        filter (price >= input$priceRange[1] & price <= input$priceRange[2])
    })
    
    # Plot 2: Price by Geography
    output$geopraphicPlot <- renderPlot({
    counties_wa_king %>%
      ggplot() +
      geom_sf() +
      geom_point(data = houses_data(), aes(x = long, y = lat, color = price), alpha= .05) +
      geom_point(data = sites, aes(x = longitude, y = latitude), size = 4,
                 shape = 23, fill = "red") +
      geom_text(data = sites, aes(x = longitude, y = latitude), label = 'Seattle', position =
                  position_dodge(width = 0.8), size = 3, vjust = -1.0) +
      scale_colour_viridis_c("Price", limits = c(input$range[1], input$range[2]), labels = scales::dollar) +
      theme_minimal() +
      labs(x = "Longitude",
           y = "Latitude")
    })
    
    # Plot 3: Price over time and geographic space
    output$geopraphicPlotByTime <- renderPlot({
    counties_wa_king %>%
      ggplot() +
      geom_sf() +
      geom_point(data = houses_data(), aes(x = long, y = lat, color = price ), alpha= .05) +
      geom_point(data = sites, aes(x = longitude, y = latitude), size = 2,
                 shape = 23, fill = "red") +
      scale_colour_viridis_c("Price", limits = c(input$range[1], input$range[2]), labels = dollar) +
      facet_wrap(~decade) +
      theme(axis.text.x = element_text(angle =50, hjust=0.75))+
      labs(x = "Longitude",
           y = "Latitude")
    });
    
    # Printing input variables for debugging purpose
    observe({
      print(" ================================================== ")
      print(input$priceRange)
      print(input$condition)
    })
})







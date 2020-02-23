#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinytheme("superhero"),
    # Application title
    titlePanel(title=div(style="display:inline-block;width:100%;background-color:#4E5D6C !important",
            img(src="homelogo.png", style="height:100px;"), "King county house price data analysis")),
    
    sidebarLayout(
        
        sidebarPanel(
            # Price Range filter
            sliderInput("priceRange",
                        "Price range:",
                        pre = "$",
                        min = 75000,
                        max = 7700000,
                        value = c(75000, 2000000)),
            # Horizontal line ----
            tags$hr(),
            
            # House condition filter
            checkboxGroupInput(inputId = "condition",
                               label ="Condition:",
                               choiceNames = c("1", "2", "3", "4", "5"),  
                               choiceValues = c(1, 2, 3, 4, 5),
                               selected = c(1, 2, 3, 4, 5),
                               inline = TRUE),
            # Horizontal line ----
            tags$hr(),
            
            # Waterfront filter
            radioButtons(inputId = "waterfront",
                         label = "Waterfront:",
                         choiceNames = c("Yes", "No","Both"),  
                         choiceValues = c(1, 0, -1),
                         selected = c(-1), inline = TRUE),
            
            # Horizontal line ----
            tags$hr(),
            
            # Number of floors filter
            checkboxGroupInput(inputId = "floors",
                               label ="Floors:",
                               choiceNames = c("1", "1.5", "2", "2.5", "3", "3.5"),  
                               choiceValues = c(1, 1.5,2, 2.5, 3, 3.5),
                               selected = c(1, 1.5, 2, 2.5, 3, 3.5),
                               inline = TRUE),
            # Horizontal line ----
            tags$hr(),
            
            # Number of bedrooms filter
            selectInput("numberOfBedRooms", "Number of Bedrooms:",
                        c("Any" = 0,
                          "1+" = 1,
                          "2+" = 2,
                          "3+" = 3,
                          "4+" = 4,
                          "5+" = 5
                        )),
            # Horizontal line ----
            tags$hr(),
            
            #Living Square feet filter
            numericInput("livingSqFeet", "Living area(Sq ft) greater than:", 290, min = 290, max = 14000),
            # Horizontal line ----
            tags$hr(),
            
            # House grade filter
            sliderInput("gradeRange",
                        "Grade range:",
                        min = 1,
                        max = 13,
                        value = c(1, 13))
        ),
        
        # Show the genrated plots
        mainPanel(
            tabsetPanel(
                tabPanel("Average price Year on Year", plotOutput("averagePricePlot", height="700px")),
                tabPanel("Price by Geography", plotOutput("geopraphicPlot", height="700px")),
                tabPanel("Price over time and geographic space", plotOutput("geopraphicPlotByTime", height="700px"))
            )
        )
    )
))

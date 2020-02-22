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
    titlePanel(title=div(style="display:inline-block;width:100%;background-color:#4E5D6C !important",img(src="homelogo.png", style="height:100px;"), "King county house price data analysis")),
    
    sidebarLayout(
        
        sidebarPanel(
                    
            sliderInput("range",
                        "Price range:",
                        pre = "$",
                        min = 50000,
                        max = 1000000,
                        value = c(50000, 1000000)),
            # Horizontal line ----
            tags$hr(),
            checkboxGroupInput(inputId = "condition",
                               label ="Condition:",
                               choiceNames = c("1", "2", "3", "4", "5"),  
                               choiceValues = c(1, 2, 3, 4, 5),
                               selected = c(1, 2, 3, 4, 5),
                               inline = TRUE
                        ),
            # Horizontal line ----
            tags$hr(),
            radioButtons(inputId = "waterfront",
                         label = "Waterfront:",
                         choiceNames = c("Both", "Yes", "No"),  
                         choiceValues = c(-1, 1, 0),
                         selected = c(-1), inline = TRUE),
            # Horizontal line ----
            tags$hr(),
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
            numericInput("livingSqFeet", "Living area(Sq ft) greater than:", 290, min = 290, max = 14000),
            # Horizontal line ----
            tags$hr(),
            sliderInput("gradeRange",
                        "Grade range:",
                        min = 1,
                        max = 13,
                        value = c(1, 13)),
            # Horizontal line ----
            tags$hr(),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Average price Year on Year", plotOutput("averagePricePlot")),
                tabPanel("Price by Geography", plotOutput("geopraphicPlot")),
                tabPanel("Price over time and geographic space", plotOutput("geopraphicPlotByTime"))
            )
        )
    )
))

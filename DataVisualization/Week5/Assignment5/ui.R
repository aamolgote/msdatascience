#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("King county house prices data analysis"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("range",
                        "Price range:",
                        pre = "$",
                        min = 50000,
                        max = 1000000,
                        value = c(50000, 1000000))
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

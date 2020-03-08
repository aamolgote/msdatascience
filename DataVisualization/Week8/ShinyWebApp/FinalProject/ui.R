#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinycssloaders)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinytheme("slate"),
    # Application title
    titlePanel(""),
    titlePanel(title=div(style="display:inline-block;width:100%;",
                         img(src="homelogo.png", style="height:100px;"), "Data analysis (2007-2018)"), 
               windowTitle = "Lending club data analysis"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # Price Range filter
            sliderInput("loanAmountRange",
                        "Loan Amount:",
                        pre = "$",
                        min = 500,
                        max = 40000,
                        value = c(500, 40000)),
            # Horizontal line ----
            tags$hr(),
            
            # House condition filter
            checkboxGroupInput(inputId = "grades",
                               label ="Grade:",
                               choiceNames = c("A", "B", "C", "D", "E", "F", "G"),  
                               choiceValues = c("A", "B", "C", "D", "E", "F", "G"),
                               selected = c("A", "B", "C", "D", "E", "F", "G"),
                               inline = TRUE),
            # Horizontal line ----
            tags$hr(),
            
            # House condition filter
            checkboxGroupInput(inputId = "homeOwnerships",
                               label ="Home Ownership:",
                               choiceNames = c("ANY", "RENT", "MORTGAGE", "OWN"),  
                               choiceValues = c("ANY", "RENT", "MORTGAGE", "OWN"),
                               selected = c("ANY", "RENT", "MORTGAGE", "OWN"),
                               inline = TRUE),
            # Horizontal line ----
            tags$hr(),
            
            
            # Number of bedrooms filter
            selectInput("loanStatus", "Loan Status:",
                        c(
                            "Any" = "Any",
                            "Current" = "Current",
                            "Fully Paid" = "Fully Paid",
                            "Late (31-120 days)" = "Late (31-120 days)",
                            "In Grace Period" = "In Grace Period",
                            "Charged Off" = "Charged Off",
                            "Late (16-30 days)" = "Late (16-30 days)",
                            "Default" = "Default",
                            "Does not meet the credit policy. Status:Fully Paid" = "Does not meet the credit policy. Status:Fully Paid",
                            "Does not meet the credit policy. Status:Charged Off" = "Does not meet the credit policy. Status:Charged Off"
                        )),
            # Horizontal line ----
            tags$hr(),
            
            # Price Range filter
            sliderInput("dti",
                        "Debt to Income Ratio:",
                        pre = "%",
                        min = -1,
                        max = 999,
                        value = c(0, 100)),
            # Horizontal line ----
            tags$hr()
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Year wise loans trend", shinycssloaders::withSpinner(plotOutput("loanProcessesedEachYear")), 
                         shinycssloaders::withSpinner(plotOutput("totalFundedLoanAmountEachYear"))),
                tabPanel("Loan Amount Term Relation", 
                         shinycssloaders::withSpinner(plotOutput("loanAmtTermRelation", height="700px"))),
                tabPanel("DTI Trend", 
                         shinycssloaders::withSpinner(plotOutput("dtiTrend", height="700px"))),
                tabPanel("Loan Funded Amount, Income, Interest Relation", 
                         shinycssloaders::withSpinner(plotOutput("fundedAmtIncomeAndInterestRelation")),
                         shinycssloaders::withSpinner(plotOutput("incomeTrend"))),
                tabPanel("Loan Amount Funded by state", 
                         shinycssloaders::withSpinner(plotOutput("loanFundedAmtByState", height="700px"))),
                tabPanel("Loans status and purpose", 
                         shinycssloaders::withSpinner(plotOutput("loansByStatus")),
                         shinycssloaders::withSpinner(plotOutput("loansByPurpose")))
            )
        )
    )
))


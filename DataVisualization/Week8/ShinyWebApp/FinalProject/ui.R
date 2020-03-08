#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Loans processed in each Year", plotOutput("loanProcessesedEachYear", height="700px")),
                tabPanel("Price by Geography", plotOutput("totalFundedLoanAmountEachYear", height="700px")),
                tabPanel("Loan Amount Term Relation", plotOutput("loanAmtTermRelation", height="700px")),
                tabPanel("Loan Amount Term Relation", plotOutput("fundedAmtIncomeAndInterestRelation", height="700px")),
                tabPanel("Loan Amount Term Relation", plotOutput("loanFundedAmtByState", height="700px"))
            )
        )
    )
))

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
lendingClubData <- read.csv("data/lending_club_loan_data_final.csv")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  number_of_loans_each_year <- reactive({
    numberOfLoansEachYear <- lendingClubData %>%
      group_by(orig_year) %>%
      summarise(loanCountByYear=n())
  })
  
  
  
  output$loanProcessesedEachYear <- renderPlot({
    ggplot(data = number_of_loans_each_year()) +
      geom_line(size=1.5, aes(x=orig_year,y=loanCountByYear))+
      geom_point(size=2, aes(x=orig_year,y=loanCountByYear))+
      labs(x="Year",y="Count",title="Loans processed in each Year")+
      theme_minimal() 
  })
  
})

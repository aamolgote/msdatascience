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
library(shinycssloaders)
lendingClubLoanData <- read.csv("data/lending_club_loan_data_final.csv")
filteredLendingClubData <- lendingClubLoanData %>%
  drop_na(annual_inc)
fullStateNames <- read.csv("data/states.csv")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  number_of_loans_each_year <- reactive({
    numberOfLoansEachYear <- lendingClubLoanData %>%
      group_by(orig_year) %>%
      summarise(loanCountByYear=n())
  })

  output$loanProcessesedEachYear <- renderPlot({
    ggplot(data = number_of_loans_each_year())+
      geom_line(color="steelblue", size=1.2, aes(x=orig_year,y=loanCountByYear))+
      geom_point(color="steelblue", size=2.5, aes(x=orig_year,y=loanCountByYear))+
      scale_x_continuous(breaks = number_of_loans_each_year()$orig_year) +
      scale_y_continuous(labels = scales::comma_format()) +
      labs(x="Year",y="# Number of loans",title="Loans processed in each Year")+
      theme_minimal() 
  })
  
  total_amount_funded_each_year <- reactive({
    totalFundedAMountPerYear <- lendingClubLoanData %>% 
      group_by(orig_year)%>%
      summarise(totalFundedAmount= sum(as.numeric(funded_amnt)))
  })
  
  output$totalFundedLoanAmountEachYear <- renderPlot({
    ggplot(data = total_amount_funded_each_year(), aes(x=orig_year, y=totalFundedAmount)) +
      geom_bar(stat="identity", width=0.5, fill = "steelblue") +
      scale_x_continuous(breaks = total_amount_funded_each_year()$orig_year) +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Year", y = "$ Total funded loan",title="Total Funded Loan Amount each Year") +
      theme_minimal() 
  })
  
  loan_amt_term_relation <- reactive({
    lendingClubLoanData
  })
  
  output$loanAmtTermRelation <- renderPlot({
    ggplot(data = loan_amt_term_relation()) +
      geom_boxplot(aes(x=term, y=funded_amnt, color=term)) + 
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Term", y = "Loan Funded Amount", title="Loan Amount and term relation") +
      theme_minimal() 
  })
  
  funded_amt_term_interest_relation <- reactive({
    filteredLendingClubData <- filteredLendingClubData %>%
      filter(annual_inc <= 300000)
    
    lbls <- c('0-20K','20-40K','40-60K','60-80K','80-100K','100-120K','120-140K','140-160K','160-180K','180-200K', '200-220K', '220-240K', '240-260K', '260-280K', '280-300K')

    groupedData <- filteredLendingClubData %>% 
      group_by(incomeGroup = cut(annual_inc, breaks= seq(0, 300000, by = 20000), right = TRUE, include.lowest = TRUE, labels = lbls) ) %>% 
      summarise(averageInterest= mean(int_rate), averageLoanLoanFundedAmount = mean(funded_amnt)) 
    
  })
  
  
  output$fundedAmtIncomeAndInterestRelation <- renderPlot({
    ggplot(data =funded_amt_term_interest_relation(), aes(x=incomeGroup, y=averageLoanLoanFundedAmount)) +
      geom_point(colour="steelblue", shape=16, aes(size=averageInterest)) +
      geom_smooth(aes(incomeGroup, averageLoanLoanFundedAmount, group = 1), method = "lm") +
      scale_y_continuous(labels = scales::dollar) +
      labs(x="Annual Income ($)",y="Average loan funded amount",title="Relation between Funded Amt, Income and Interest Rate")+
      guides(size=guide_legend("Avg \nInterest Rate (%)")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle =50, hjust=0.75))+
      theme(legend.background = element_rect())
  })
  
  loan_funded_amt_by_state <- reactive({
    fundedAmountByState <- lendingClubLoanData %>% 
      group_by(addr_state)%>%
      summarise(totalFundedAmount= sum(as.numeric(funded_amnt)))
    
    fundedAmountByState <- fundedAmountByState %>%
      inner_join(fullStateNames, by = c("addr_state" = "abbreviation"))
    
    states2 <- states %>% left_join(fundedAmountByState, by = c("ID" = "state" ))
  })
  
  output$loanFundedAmtByState <- renderPlot({
    ggplot(data = loan_funded_amt_by_state()) + 
      geom_sf(aes(fill = totalFundedAmount)) +
      scale_fill_viridis_c("Loan Funded Amount", labels = scales::dollar) +
      labs(title = "Loans Funded amount by state") +
      theme_minimal()
  })
})

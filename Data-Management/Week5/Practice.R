# Install the tidyverse
# install.packages("tidyverse")


# Import the Tidyverse
library(tidyverse)

setwd("~/Data-Management/ClassicModels/")

employees <- read.csv(file = "Employees.txt",
                      header = TRUE,
                      stringsAsFactors = FALSE,
                      na.strings = "")

offices <- read.csv(file = "Offices.txt",
                    header = TRUE,
                    stringsAsFactors = FALSE,
                    na.strings = "")

names(employees) <- c("employeeNumber",
                      "lastName",
                      "firstName",
                      "extension",
                      "email",
                      "officeCode",
                      "reportsTo",
                      "jobTitle")
names(offices) <- c("code",
                    "city",
                    "phone",
                    "addressLine1",
                    "addressLine2",
                    "state",
                    "country",
                    "postalCode",
                    "territory")

employee_office <- inner_join(employees, offices,  by = c('officeCode' = 'code'))

employeesFromBoston <- employee_office %>%
  filter(city == "Boston") %>% 
  select(employeeNumber,lastName,firstName,extension,email,officeCode,reportsTo,jobTitle)

employeesFromBoston






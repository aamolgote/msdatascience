#0 Import the Tidyverse
library(tidyverse)
setwd/("~/Data-Management/ClassicModels/")
employees <- read.csv(file = "EmployeesWithSalary.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE,
                      na.strings = "")

employeesSortedBySalary %>% arrange(desc(salary))
slice(top3Salaries, 3:3) %>%
select(employeeNumber,lastName,firstName,salary)

employeesGroupedByEmployeeNumber <- employees %>% 
  group_by(employeeNumber)%>% 
  count(employeeNumber)%>% 
  filter ( n > 1)%>%
  arrange(desc(n))

employeesGroupedByEmployeeNumber


employeesGroupedByDeptWithMaxSalary <- employees %>% 
  group_by(deptCode) %>%
  filter(salary == max(salary)) %>%
  arrange(deptCode) %>%
  select(deptCode,salary, lastName,firstName)
distinctDeptWithMaxSalaries <- distinct(employeesGroupedByDeptWithMaxSalary, deptCode,salary)
distinctDeptWithMaxSalaries



employees %>% 
  filter(salary >= 50000, salary <= 100000) %>%
  select(employeeNumber,lastName,firstName,salary) %>%
  arrange(desc(salary))

library(stringr)
employees %>% 
  filter(str_detect(firstName, 'a'), str_detect(lastName, 'a')) %>%
  select(employeeNumber,lastName,firstName)
  



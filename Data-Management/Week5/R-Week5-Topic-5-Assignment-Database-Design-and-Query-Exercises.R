# Install the tidyverse
# install.packages("tidyverse")


# Import the Tidyverse
library(tidyverse)

setwd("~/Data-Management/ClassicModels/")

#Who are the employees in Boston?
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
names(offices) <- c("officeCode",
                    "city",
                    "phone",
                    "addressLine1",
                    "addressLine2",
                    "state",
                    "country",
                    "postalCode",
                    "territory")

employee_office <- inner_join(employees, offices, by = 'officeCode')

employeesFromBoston <- employee_office %>%
                        filter(city == "Boston") %>% 
                    select(employeeNumber,lastName,firstName,extension,email,officeCode,reportsTo,jobTitle)

# Below are employees in Boston
employeesFromBoston


#List the total value of all "On Hold" orders.
orderdetails <- read.csv(file = "OrderDetails.txt",
                         header = FALSE,
                         stringsAsFactors = FALSE,
                         na.strings = "")
orders <-read.csv(file = "Orders.txt",
                  header = FALSE,
                  stringsAsFactors = FALSE,
                  na.strings = "")

names(orderdetails) <- c("orderNumber",
                         "productCode",
                         "quantityOrdered",
                         "priceEach",
                         "orderLineNumber")
names(orders) <- c("orderNumber",
                   "orderDate",
                   "requiredDate",
                   "shippedDate",
                   "status",
                   "comments",
                   "customerNumber")

# Total value of all "On Hold" orders - For each order
order_orderdetails <- inner_join(orders, orderdetails, by = 'orderNumber') %>%
  filter(status =='On Hold') %>%
  group_by(orderNumber) %>%
  summarize(orderTotal = sum(quantityOrdered * priceEach)) %>%
  select(orderNumber,orderTotal)
order_orderdetails

totalValueOnHold <- sum(order_orderdetails$orderTotal)
# Total value of all "On Hold" orders - For All orders
totalValueOnHold





#Report total payments for Atelier Graphique
payments <- read.csv(file = "Payments.txt",
                     header = FALSE,
                     stringsAsFactors = FALSE,
                     na.strings = "")

names(payments) <- c("customerNumber",
                     "checkNumber",
                     "paymentDate",
                     "amount")

customers <- read.csv(file = "Customers.txt",
                      header = FALSE,
                      stringsAsFactors = FALSE,
                      na.strings = "")

names(customers) <- c("customerNumber",
                      "customerName",
                      "contactLastName",
                      "contactFirstName",
                      "phone",
                      "addressLine1",
                      "addressLine2",
                      "city",
                      "state",
                      "postalCode",
                      "country",
                      "salesRepEmployeeNumber",
                      "creditLimit")




# Total payments for Atelier Graphique
atelier_customer_payments <- inner_join(customers, payments, by = 'customerNumber')%>%
  filter(tolower(customerName) == tolower("Atelier Graphique")) %>%
  group_by(customerNumber, customerName) %>%
  summarize(totalPaymentAmount = sum(amount)) %>%
  select(customerNumber, customerName, totalPaymentAmount)
atelier_customer_payments

# Total payments for Atelier Graphique - Alternate way without group by
customer_payments <- inner_join(customers, payments, by = 'customerNumber')
paymentsForAtelier <- customer_payments %>%
  filter(tolower(customerName) == tolower("Atelier Graphique")) 
totalPaymentAmount <- sum(paymentsForAtelier$amount)
numberOfPayments <- nrow(paymentsForAtelier)
totalPaymentAmount
numberOfPayments





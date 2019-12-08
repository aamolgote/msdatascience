# Install the tidyverse
# install.packages("tidyverse")


# Import the Tidyverse
library(tidyverse)

setwd("~/Data-Management/ClassicModels/")


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
orders$orderDate <- strptime(orders$orderDate, format = "%Y/%m/%d %H:%M:%S")
orders$requiredDate <- strptime(orders$requiredDate, format = "%Y/%m/%d %H:%M:%S")
orders$shippedDate <- strptime(orders$shippedDate, format = "%Y/%m/%d %H:%M:%S")
orders$orderDate <- strftime(orders$orderDate, format = "%Y-%m-%d %H:%M:%S")
orders$requiredDate <- strftime(orders$requiredDate, format = "%Y-%m-%d %H:%M:%S")
orders$shippedDate <- strftime(orders$shippedDate, format = "%Y-%m-%d %H:%M:%S")


products <- read.csv(file = "Products.txt",
                     header = FALSE,
                     stringsAsFactors = FALSE,
                     na.strings = "")

names(products) <- c("productCode",
                     "productName",
                     "productLine",
                     "productScale",
                     "productVendor",
                     "productDescription",
                     "quantityInStock",
                     "buyPrice",
                     "MSRP")

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

#What is the total number of orders containing items sold at less than the MSRP?
numberOfOrdersLessThanMsrp <- inner_join(orderdetails, products, by = 'productCode') %>%
  filter(priceEach < MSRP) %>%
  select(orderNumber) %>% 
  distinct() %>%
  summarize(numberOfOrdersLessThanMsrp = n())
numberOfOrdersLessThanMsrp



#What is the average total for Monday orders?
avgTotalForMondayOrders <- inner_join(orders, orderdetails, by = 'orderNumber') %>%
  group_by(orderNumber) %>%
  filter(weekdays(as.Date(orderDate)) == 'Monday') %>%
  summarize(orderTotal = sum(quantityOrdered * priceEach)) %>%
  summarize(overallAvg = mean(orderTotal))
avgTotalForMondayOrders


#What is the total quantity on hand for products listed that are included in "On Hold" orders?
totalQuantityOnHold <- orders %>%
  inner_join(orderdetails, by = 'orderNumber') %>%
  inner_join(products, by = 'productCode') %>%
  filter(status == 'On Hold') %>%
  summarize(totalQuantity = sum(quantityInStock))
totalQuantityOnHold


#List the names of customers and the corresponding order numbers where a particular order from that customer has a value greater than $25,000.
customerWithHigherOrderValue <- orders %>%
  inner_join(orderdetails, by = 'orderNumber') %>%
  inner_join(customers, by = 'customerNumber') %>%
  group_by(customerNumber, customerName, orderNumber) %>%
  summarize(orderValue = sum(quantityOrdered * priceEach)) %>%
  filter(orderValue > 25000) %>%
  arrange(orderValue)
  
customerWithHigherOrderValue

  



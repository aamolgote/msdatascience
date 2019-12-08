library(DBI)
setwd("~/Data-Management/ClassicModels/")
if (exists("myDB")) {
  dbDisconnect(myDB)
}
myDB <- dbConnect(drv = RSQLite::SQLite(), dbname = "classicmodels.sqlite")

#Who are the employees in Boston?
dbGetQuery(conn = myDB, "SELECT 
	employees.employeeNumber
	,employees.lastName
	,employees.firstName
	,employees.extension
	,employees.email
	,employees.officeCode
	,employees.reportsTo
	,employees.jobTitle
	,(SELECT lastName FROM employees e WHERE e.employeeNumber = employees.reportsTo) as reportingManagerLastName
	,(SELECT firstName FROM employees e WHERE e.employeeNumber = employees.reportsTo) as reportingManagerFirstName
FROM 
	employees 
INNER JOIN offices ON employees.officeCode = offices.officeCode	
WHERE 
	UPPER(offices.city)  = UPPER('Boston');")


#List the total value of all "On Hold" orders - For each order
dbGetQuery(conn = myDB, "SELECT 
	orders.orderNumber, SUM(ifnull(orderdetails.quantityOrdered,0) * ifnull(orderdetails.priceEach,0)) as totalValueOfOnHoldOrders
FROM
	orderdetails INNER JOIN orders ON orderdetails.orderNumber = orders.orderNumber
WHERE
	UPPER(orders.status) = UPPER('On Hold')
GROUP BY orders.orderNumber;")

#List the total value of all "On Hold" orders- Sum of all orders.
dbGetQuery(conn = myDB, "SELECT 
	SUM(ifnull(orderdetails.quantityOrdered,0) * ifnull(orderdetails.priceEach,0)) as totalValueOfOnHoldOrders
FROM
	orderdetails INNER JOIN orders ON orderdetails.orderNumber = orders.orderNumber
WHERE
	UPPER(orders.status) = UPPER('On Hold');")

#Report total payments for Atelier Graphique - Using Where condition
dbGetQuery(conn = myDB, "SELECT	
	customers.customerNumber,customers.customerName, COUNT(*) as numberOfPayments, sum(amount) totalPaymentAmount
FROM
	payments
INNER JOIN customers ON payments.customerNumber = customers.customerNumber
WHERE
	UPPER(customers.customerName) = UPPER('Atelier Graphique');")

#Report total payments for Atelier Graphique - Using Group By
dbGetQuery(conn = myDB, "SELECT	
	customers.customerNumber,customers.customerName, COUNT(*) as numberOfPayments, sum(amount) totalPaymentAmount
FROM
	payments
INNER JOIN customers ON payments.customerNumber = customers.customerNumber
GROUP BY 
	customers.customerNumber, customers.customerName
Having 
	UPPER(customers.customerName) = UPPER('Atelier Graphique');")





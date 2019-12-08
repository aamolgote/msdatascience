library(DBI)
setwd("~/Data-Management/ClassicModels/")
if (exists("myDB")) {
  dbDisconnect(myDB)
}
myDB <- dbConnect(drv = RSQLite::SQLite(), dbname = "classicmodels.sqlite")


#What is the total number of orders containing items sold at less than the MSRP?
dbGetQuery(conn = myDB, "
SELECT  
	COUNT(DISTINCT(orderNumber)) as numberOfOrdersLessThanMsrp 
FROM 
	orderdetails INNER JOIN products ON orderdetails.productCode = products.productCode
WHERE 
	orderdetails.priceEach < products.MSRP
")


#What is the average total for Monday orders?
dbGetQuery(conn = myDB, "
SELECT AVG(orderTotal) as avgTotalForMondayOrders FROM
  (SELECT 
    orders.orderNumber, SUM(orderdetails.quantityOrdered * orderdetails.priceEach) as orderTotal
  FROM
    orderdetails INNER JOIN orders on orderdetails.orderNumber = orders.orderNumber
  WHERE
    strftime('%w', orderDate) = '1'
  GROUP BY 
    orders.orderNumber
  )"
);

#Alternate Way -> average total for Monday orders
dbGetQuery(conn = myDB, "
SELECT 
	Avg(orderTotal) as avgTotalForMondayOrders
FROM 
	orders INNER JOIN (SELECT 
							orders.orderNumber, SUM(orderdetails.quantityOrdered * orderdetails.priceEach) as orderTotal
						FROM
							orderdetails INNER JOIN orders on orderdetails.orderNumber = orders.orderNumber
						WHERE
							strftime('%w', orderDate) = '1'
						GROUP BY 
							orders.orderNumber) innerOrders ON orders.orderNumber = innerOrders.orderNumber"
);



#What is the total quantity on hand for products listed that are included in "On Hold" orders?
# Using Group By
dbGetQuery(conn = myDB, "
SELECT 
	SUM(products.quantityInStock) as totQtyOrdersOnHold 
FROM 
	orderdetails INNER JOIN orders on orderdetails.orderNumber = orders.orderNumber
	INNER JOIN products on orderdetails.productCode = products.productCode
GROUP BY
  orders.status
HAVING
	orders.status = 'On Hold'
")

# Using Simple where condition.
dbGetQuery(conn = myDB, "
SELECT 
	SUM(products.quantityInStock) as totQtyOrdersOnHold 
FROM 
	orderdetails INNER JOIN orders on orderdetails.orderNumber = orders.orderNumber
	INNER JOIN products on orderdetails.productCode = products.productCode
WHERE
	orders.status = 'On Hold'
")


#List the names of customers and the corresponding order numbers where a particular order 
#from that customer has a value greater than $25,000.
dbGetQuery(conn = myDB, "
SELECT 
	customers.customerNumber,customers.customerName, orders.orderNumber, SUM(orderdetails.quantityOrdered * orderdetails.priceEach) as totalPrice 
FROM 
	orderdetails INNER JOIN orders on orderdetails.orderNumber = orders.orderNumber
	INNER JOIN customers on orders.customerNumber = customers.customerNumber
GROUP BY
	customers.customerNumber, customers.customerName, orders.orderNumber
HAVING
	SUM(orderdetails.quantityOrdered * orderdetails.priceEach) > 25000
ORDER BY totalPrice
")



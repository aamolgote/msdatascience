library(DBI)
setwd("~/Data-Management/ClassicModels/")
if (exists("myDB")) {
  dbDisconnect(myDB)
}
myDB <- dbConnect(drv = RSQLite::SQLite(), dbname = "classicmodels.sqlite")

dbExecute(conn = myDB, "
  CREATE TABLE customers (
  customerNumber INTEGER NOT NULL,
  customerName TEXT,
  contactLastName TEXT,
  contactFirstName TEXT,
  phone TEXT,
  addressLine1 TEXT,
  addressLine2 TEXT,
  city TEXT,
  state TEXT,
  postalCode TEXT,
  country TEXT,
  salesRepEmployeeNumber INTEGER,
  creditLimit REAL,
  PRIMARY KEY (customerNumber)
  FOREIGN KEY (salesRepEmployeeNumber) REFERENCES employees(employeeNumber)
  );
")

dbExecute(conn = myDB, "
  CREATE TABLE employees (
  employeeNumber INTEGER NOT NULL,
  1
  lastName TEXT,
  firstName TEXT,
  extension TEXT,
  email TEXT,
  officeCode TEXT,
  reportsTo INTEGER,
  jobTitle TEXT,
  PRIMARY KEY (employeeNumber)
  FOREIGN KEY(officeCode) REFERENCES offices(officeCode)
  );
")

dbExecute(conn = myDB, "
  CREATE TABLE offices (
  officeCode TEXT NOT NULL,
  city TEXT,
  phone TEXT,
  addressLine1 TEXT,
  addressLine2 TEXT,
  state TEXT,
  country TEXT,
  postalCode TEXT,
  territory TEXT,
  PRIMARY KEY (officeCode)
  );
")

dbExecute(conn = myDB, "
  CREATE TABLE orderdetails (
  orderNumber INTEGER NOT NULL,
  productCode TEXT NOT NULL,
  quantityOrdered INTEGER,
  priceEach REAL,
  orderLineNumber INTEGER,
  PRIMARY KEY (orderNumber, productCode)
  FOREIGN KEY (orderNumber) REFERENCES orders(orderNumber)
  FOREIGN KEY (productCode) REFERENCES products(productCode)
  );
")

dbExecute(conn = myDB, "
  CREATE TABLE orders (
  orderNumber INTEGER NOT NULL,
  orderDate TEXT,
  requiredDate TEXT,
  shippedDate TEXT,
  status TEXT,
  comments TEXT,
  customerNumber INTEGER,
  PRIMARY KEY (orderNumber)
  FOREIGN KEY (customerNumber) REFERENCES customers(customerNumber)
  );
")


dbExecute(conn = myDB, "
  CREATE TABLE payments (
  customerNumber INTEGER NOT NULL,
  checkNumber TEXT NOT NULL,
  paymentDate TEXT,
  amount REAL,
  PRIMARY KEY (checkNumber)
  FOREIGN KEY (customerNumber) REFERENCES customers(customerNumber)
  );
")
dbExecute(conn = myDB, "
  CREATE TABLE products (
  productCode TEXT NOT NULL,
  productName TEXT,
  productLine TEXT,
  productScale TEXT,
  productVendor TEXT,
  productDescription TEXT,
  quantityInStock INTEGER,
  buyPrice REAL,
  MSRP REAL,
  PRIMARY KEY (productCode)
  );
")

dbGetQuery(conn = myDB, "SELECT name FROM sqlite_master WHERE type='table';")

#dbExecute(conn = myDB, "
#  INSERT INTO payments (customerNumber,checkNumber,paymentDate,amount) VALUES
#  (103, 'HQ336336', '2004/10/19 0:00:00', 5307.98),
#  (103, 'JM555205', '2003/6/5 0:00:00', 16560.30);
#")

customers <- read.csv(file = "Customers.txt",
                      header = FALSE,
                      stringsAsFactors = FALSE,
                      na.strings = "")
employees <- read.csv(file = "Employees.txt",
                      header = FALSE,
                      stringsAsFactors = FALSE,
                      na.strings = "")
offices <- read.csv(file = "Offices.txt",
                    header = FALSE,
                    stringsAsFactors = FALSE,
                    na.strings = "")
orderdetails <- read.csv(file = "OrderDetails.txt",
                         header = FALSE,
                         stringsAsFactors = FALSE,
                         na.strings = "")
orders <-read.csv(file = "Orders.txt",
                  header = FALSE,
                  stringsAsFactors = FALSE,
                  na.strings = "")

payments <- read.csv(file = "Payments.txt",
                     header = FALSE,
                     stringsAsFactors = FALSE,
                     na.strings = "")

products <- read.csv(file = "Products.txt",
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
names(payments) <- c("customerNumber",
                     "checkNumber",
                     "paymentDate",
                     "amount")
names(products) <- c("productCode",
                     "productName",
                     "productLine",
                     "productScale",
                     "productVendor",
                     "productDescription",
                     "quantityInStock",
                     "buyPrice",
                     "MSRP")

orders$orderDate <- strptime(orders$orderDate, format = "%Y/%m/%d %H:%M:%S")
orders$requiredDate <- strptime(orders$requiredDate, format = "%Y/%m/%d %H:%M:%S")
orders$shippedDate <- strptime(orders$shippedDate, format = "%Y/%m/%d %H:%M:%S")
payments$paymentDate <- strptime(payments$paymentDate, format = "%Y/%m/%d %H:%M:%S")
orders$orderDate <- strftime(orders$orderDate, format = "%Y-%m-%d %H:%M:%S")
orders$requiredDate <- strftime(orders$requiredDate, format = "%Y-%m-%d %H:%M:%S")
orders$shippedDate <- strftime(orders$shippedDate, format = "%Y-%m-%d %H:%M:%S")
payments$paymentDate <- strftime(payments$paymentDate, format = "%Y-%m-%d %H:%M:%S")

dbWriteTable(conn = myDB,
             name = "customers",
             value = customers,
             append = TRUE,
             header = FALSE)

dbWriteTable(conn = myDB,
             name = "employees",
             value = employees,
             append = TRUE,
             header = FALSE)

dbWriteTable(conn = myDB,
             name = "offices",
             value = offices,
             append = TRUE,
             header = FALSE)

dbWriteTable(conn = myDB,
             name = "orderdetails",
             value = orderdetails,
             append = TRUE,
             header = FALSE)

dbWriteTable(conn = myDB,
             name = "orders",
             value = orders,
             append = TRUE,
             header = FALSE)

dbWriteTable(conn = myDB,
             name = "payments",
             value = payments,
             append = TRUE,
             header = FALSE)

dbWriteTable(conn = myDB,
             name = "products",
             value = products,
             append = TRUE,
             header = FALSE)


payments
dbGetQuery(conn = myDB, "SELECT * FROM offices;")
dbGetQuery(conn = myDB, "SELECT * FROM payments;")

dbDisconnect(conn = myDB)





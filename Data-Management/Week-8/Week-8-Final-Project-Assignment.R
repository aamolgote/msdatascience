#install.packages("Hmisc")
# Import the Tidyverse
library(tidyverse)
library(Hmisc)
library(readr)
library(tibble)
library(dplyr)
library(ggplot2)

main_dir <- "~/ABCPharmacy/CSV"
csv_output_dir <- "~/ABCPharmacy/CSV/final-project"
setwd(main_dir)
sub_dir <- "analytical-files"
output_dir <- file.path(main_dir, sub_dir)
csv_output_dir <- file.path(csv_output_dir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}

if (!dir.exists(csv_output_dir)){
  dir.create(csv_output_dir)
} else {
  print("CSV output Dir already exists!")
}


MAJOR_PROD_CAT <- read_csv(file = "MAJOR PROD CAT.csv", col_names = TRUE, col_types = "ccc")
# get rid of empty column in MAJOR_PROD_CAT
MAJOR_PROD_CAT <- MAJOR_PROD_CAT[, -3]
describe(MAJOR_PROD_CAT)

PROD_CAT <- read_csv(file = "PROD CAT.csv", col_names = TRUE, col_types = "ccc")
describe(PROD_CAT)

PROD_SUB_CAT <- read_csv(file = "PROD SUB CAT.csv", col_names = TRUE, col_types = "ccc")
describe(PROD_SUB_CAT)

PROD_SEG <- read_csv(file = "PROD SEG.csv", col_names = TRUE, col_types = "ccc")
describe(PROD_SEG)

PROD_MASTER <- read_csv(file = "PROD MASTER.csv", col_names = TRUE, col_types = "cccccc")
prod_master_columns <- c("PROD_NBR","PROD_DESC", "SEGMENT_CD")
PROD_MASTER = PROD_MASTER[prod_master_columns]
describe(PROD_MASTER)

PHRMCY_MASTER <- read_csv(file = "PHRMCY MASTER.csv",col_names = TRUE, col_types = "cccc")

# Get rid of ZIP_3_CD
PHRMCY_MASTER <- PHRMCY_MASTER[, -4]
describe(PHRMCY_MASTER)


POS_TRANS <- read_csv(file = "POS Transactions.csv",col_names = TRUE, col_types = "ccccdi")
POS_TRANS$SLS_DTE_NBR <- paste(substr(POS_TRANS$SLS_DTE_NBR, 1, 4), substr(POS_TRANS$SLS_DTE_NBR, 5, 6), substr(POS_TRANS$SLS_DTE_NBR, 7, 8), sep="-")    
# Adjust date column
POS_TRANS$SLS_DTE_NBR <- strftime(POS_TRANS$SLS_DTE_NBR, format = "%Y-%m-%d %H:%M:%S")
colnames(POS_TRANS)[4] <- "SLS_DTE"
describe(POS_TRANS)


# Check Duplicate by primary key MAJOR_CAT_CD
DUP_MAJOR_PROD_CAT <- MAJOR_PROD_CAT %>% 
  group_by(MAJOR_CAT_CD)%>% 
  count(MAJOR_CAT_CD)%>% 
  filter ( n > 1)%>%
  arrange(desc(n))
nrow(DUP_MAJOR_PROD_CAT)

# Check Duplicate by primary key CAT_CD
PROD_CAT %>% 
  group_by(CAT_CD)%>% 
  count(CAT_CD)%>% 
  filter ( n > 1)%>%
  arrange(desc(n))
nrow(DUP_MAJOR_PROD_CAT)

# Check Duplicate by primary key SUB_CAT_CD
DUP_PROD_SUB_CAT <- PROD_SUB_CAT %>% 
  group_by(SUB_CAT_CD)%>% 
  count(SUB_CAT_CD)%>% 
  filter ( n > 1)%>%
  arrange(desc(n))
nrow(DUP_PROD_SUB_CAT)

# Check Duplicate by primary key SEG_CD
DUP_PROD_SEG <- PROD_SEG %>% 
  group_by(SEG_CD)%>% 
  count(SEG_CD)%>% 
  filter ( n > 1)%>%
  arrange(desc(n))
nrow(DUP_PROD_SEG)

# Check Duplicate by primary key PROD_NBR
DUP_PROD_MASTER <- PROD_MASTER %>% 
  group_by(PROD_NBR)%>% 
  count(PROD_NBR)%>% 
  filter ( n > 1)%>%
  arrange(desc(n))
nrow(DUP_PROD_MASTER)

# Check Duplicate by primary key PHRMCY_NBR
DUP_PHRMCY_MASTER <- PHRMCY_MASTER %>% 
  group_by(PHRMCY_NBR)%>% 
  count(PHRMCY_NBR)%>% 
  filter ( n > 1)%>%
  arrange(desc(n))
nrow(DUP_PHRMCY_MASTER)


# Check duplicates by BSKT_ID, PROD_NBR, PHRMCY_NBR
POS_TRANS_GROUP_COUNT <- POS_TRANS %>% 
  group_by(BSKT_ID, PROD_NBR, PHRMCY_NBR)%>% 
  count(BSKT_ID, PROD_NBR, PHRMCY_NBR)%>%
  filter(n > 1)
posTransDupCount <- nrow(POS_TRANS_GROUP_COUNT)
cat("Number of duplicated transactions:", posTransDupCount)
POS_TRANS_DISTINCT <- POS_TRANS %>% select(BSKT_ID, PHRMCY_NBR, PROD_NBR) %>% distinct()
cat("Number of duplicated transactions:", nrow(POS_TRANS) - nrow(POS_TRANS_DISTINCT))

POS_TRANS %>%
  filter(BSKT_ID == "100002002195001978261" | BSKT_ID == "200002244325000247930" | BSKT_ID == "200003389586000353747" | BSKT_ID == "200003389586000355543") %>%
  arrange(desc(BSKT_ID, PROD_NBR, PHRMCY_NBR))

# Duplicate row count > 0, so BSKT_ID, PROD_NBR, PHRMCY_NBR cannot be primary key
print("Duplicate row count > 0, so BSKT_ID, PROD_NBR, PHRMCY_NBR cannot be primary key")

# Check duplicates by BSKT_ID, PROD_NBR, PHRMCY_NBR, SLS_DTE, EXT_SLS_AMT, SLS_QTY
POS_TRANS_GROUP_COUNT_BY_DTE_AMT_QTY <- POS_TRANS %>% 
  group_by(BSKT_ID, PROD_NBR, PHRMCY_NBR, SLS_DTE, EXT_SLS_AMT, SLS_QTY)%>% 
  count(BSKT_ID, PROD_NBR, PHRMCY_NBR, SLS_DTE, EXT_SLS_AMT, SLS_QTY)%>%
  filter(n > 1)
posTransDupCountByDteAmtQty <- nrow(POS_TRANS_GROUP_COUNT_BY_DTE_AMT_QTY)
cat("Number of duplicated transactions:", posTransDupCountByDteAmtQty)
# Duplicate row count = 0, so BSKT_ID, PROD_NBR, PHRMCY_NBR, SLS_DTE, EXT_SLS_AMT, SLS_QTY is primary key
print("Duplicate row count = 0, so BSKT_ID, PROD_NBR, PHRMCY_NBR, SLS_DTE, EXT_SLS_AMT, SLS_QTY is primary key")


#What's total sales revenue for ABC Pharmacy for Jan 2016 to June 2016?
totalSalesRevenue <- POS_TRANS %>%
  summarise(TOTAL_SALES_REVENUE = sum(EXT_SLS_AMT))
totalSalesRevenue

#What's percentage gain/loss in sales revenue every month for Jan 2016 to June 2016?
monthwiseSales <- POS_TRANS %>%
  mutate(saleMonth = format(as.Date(SLS_DTE), "%b"),saleMonthNum = format(as.Date(SLS_DTE), "%m"), saleYear = format(as.Date(SLS_DTE), "%Y")) %>%
  group_by(saleYear, saleMonth, saleMonthNum) %>%
  summarise(monthlySaleValue = sum(EXT_SLS_AMT)) %>%
  arrange(saleMonthNum)
monthwiseSales <- unite(monthwiseSales, monthAndYear, c(saleMonth, saleYear), sep="-", remove=FALSE)
monthwiseSales
write.csv(monthwiseSales, file = "final-project/monthwiseSales.csv", row.names=FALSE)

# Which are the best-selling product?
# By Quantity
maxSellingProductsByQuantity <- POS_TRANS %>%
  inner_join(PROD_MASTER, by = 'PROD_NBR') %>%
  group_by(PROD_NBR, PROD_DESC) %>%
  summarise(PRODUCT_QTY_ORDER = sum(SLS_QTY)) %>%
  arrange(desc(PRODUCT_QTY_ORDER)) %>%
  ungroup %>%
  slice(1:10)
maxSellingProductsByQuantity
write.csv(maxSellingProductsByQuantity, file = "final-project/maxsellingproducts.csv", row.names=FALSE)

# By Sales Revenue
maxSellingProductsBySalesAmt <- POS_TRANS %>%
  inner_join(PROD_MASTER, by = 'PROD_NBR') %>%
  group_by(PROD_NBR, PROD_DESC) %>%
  summarise(PRODUCT_VALUE_ORDER = sum(EXT_SLS_AMT)) %>%
  arrange(desc(PRODUCT_VALUE_ORDER)) %>%
  ungroup %>%
  slice(1:10)
maxSellingProductsBySalesAmt
write.csv(maxSellingProductsBySalesAmt, file = "final-project/maxSellingProductsBySalesAmt.csv", row.names=FALSE)

# Non Perfroming products 
nonPerfProductsByQuantity <- PROD_MASTER  %>%
  left_join(POS_TRANS, by = 'PROD_NBR') %>%
  group_by(PROD_NBR, PROD_DESC) %>%
  summarise(PRODUCT_QTY_ORDER = sum(SLS_QTY)) %>%
  filter(is.na(PRODUCT_QTY_ORDER) || PRODUCT_QTY_ORDER <=0) %>%
  ungroup() %>%
  summarise(NON_PERF_PRODUCT_COUNT = n())
nonPerfProductsByQuantity
write.csv(nonPerfProductsByQuantity, file = "final-project/nonPerfProductsByQuantity.csv", row.names=FALSE)

#Which major product categories perform better?
majorProductCatgPerformance <- POS_TRANS %>%
  inner_join(PROD_MASTER, by = 'PROD_NBR') %>%  
  inner_join(PROD_SEG, by = c("SEGMENT_CD" = "SEG_CD")) %>%
  inner_join(PROD_SUB_CAT, by = c("SUB_CAT_CD" = "SUB_CAT_CD")) %>%
  inner_join(PROD_CAT, by = c("CAT_CD" = "CAT_CD")) %>%
  inner_join(MAJOR_PROD_CAT, by = c("MAJOR_CAT_CD" = "MAJOR_CAT_CD")) %>%
  group_by(MAJOR_CAT_CD, MAJOR_CAT_DESC) %>%
  summarise(monthlySaleValue = sum(EXT_SLS_AMT)) %>%
  arrange(desc(monthlySaleValue))
majorProductCatgPerformance
write.csv(majorProductCatgPerformance, file = "final-project/majorProductCatgPerformance.csv", row.names=FALSE)

#What are the average number of products sold per transaction, per order, per month?
#Per Order
averageNumberOfProductsAndValuePerOrder <- POS_TRANS %>%
  group_by(BSKT_ID) %>%
  summarise(numberOfProducts = sum(SLS_QTY), orderValue = sum(EXT_SLS_AMT)) %>%
  summarise(averageNumberOfProductsPerOrder = mean(numberOfProducts), averageOrderValue = mean(orderValue))
averageNumberOfProductsAndValuePerOrder

#Per Month
averageNumberOfProductsAndValueMonthly <- POS_TRANS %>%
  mutate(saleMonth = format(as.Date(SLS_DTE), "%b"),saleMonthNum = format(as.Date(SLS_DTE), "%m"), saleYear = format(as.Date(SLS_DTE), "%Y")) %>%
  group_by(saleYear, saleMonth, saleMonthNum) %>%
  summarise(numberOfProducts = sum(SLS_QTY), orderValue = sum(EXT_SLS_AMT)) %>%
  ungroup %>%
  summarise(averageNumberOfProducts = mean(numberOfProducts), averageOrderValue = mean(orderValue))
averageNumberOfProductsAndValueMonthly

#What's state wise sales revenue?
statewiseSalesRevenue <- POS_TRANS %>%
  left_join(PHRMCY_MASTER, by = 'PHRMCY_NBR') %>%
  group_by(ST_CD) %>%
  summarise(STATE_SALES_REVENUE = sum(EXT_SLS_AMT)) %>%
  arrange(desc(STATE_SALES_REVENUE)) %>%
  ungroup %>%
  slice(1:10)
statewiseSalesRevenue
write.csv(statewiseSalesRevenue, file = "final-project/statewiseSalesRevenue.csv", row.names=FALSE)

#	Which store has maximum performance based on sales revenue?
maxSalesRevenueStores <- POS_TRANS %>%
  inner_join(PHRMCY_MASTER, by = 'PHRMCY_NBR') %>%
  group_by(PHRMCY_NBR, PHRMCY_NAM) %>%
  summarise(PHRMCY_NBR_SALES_REVENUE = sum(EXT_SLS_AMT)) %>%
  arrange(desc(PHRMCY_NBR_SALES_REVENUE)) %>%
  ungroup %>%
  slice(1:10)
maxSalesRevenueStores
write.csv(maxSalesRevenueStores, file = "final-project/maxSalesRevenueStores.csv", row.names=FALSE)

# What's average sale per store?
averageSalePerStore <- POS_TRANS %>%
  left_join(PHRMCY_MASTER, by = 'PHRMCY_NBR') %>%
  group_by(PHRMCY_NBR) %>%
  summarise(SALES_REVENUE = sum(EXT_SLS_AMT)) %>%
  summarise(averageSalePerStore = mean(SALES_REVENUE))
averageSalePerStore


#What are number of stores state wise?
statewiseNUmberOfStores <- PHRMCY_MASTER %>%
  group_by(ST_CD) %>%
  summarise(NUMBER_OF_STORES = n()) %>%
  arrange(desc(NUMBER_OF_STORES))
statewiseNUmberOfStores
write.csv(statewiseNUmberOfStores, file = "final-project/statewiseNUmberOfStores.csv", row.names=FALSE)

#11.	Non Performing stores - If there is need to minimize the losses, then which stores ABC pharmacy should be closing? 
#(Note: Stores with least sales revenue can be closed)
# 1. Stores with no sales revenue
nonPerfPharmacyStores <- PHRMCY_MASTER %>%
  left_join(POS_TRANS, by = 'PHRMCY_NBR') %>%
  group_by(PHRMCY_NBR, PHRMCY_NAM, ST_CD) %>%
  summarise(PHRMCY_NBR_SALES_REVENUE = sum(EXT_SLS_AMT)) %>%
  filter(is.na(PHRMCY_NBR_SALES_REVENUE))
nonPerfPharmacyStores
write.csv(nonPerfPharmacyStores, file = "final-project/nonPerfPharmacyStores.csv", row.names=FALSE)

# State wide stores with no revenue, which indicates which are not operational or closed
nonPerfPharmacyStoresStatewise <- nonPerfPharmacyStores  %>%
  group_by(ST_CD) %>%
  summarise(NON_PERF_PHARMACY_COUNT = n())%>%
  arrange(ST_CD)
nonPerfPharmacyStoresStatewise
write.csv(nonPerfPharmacyStoresStatewise, file = "final-project/nonPerfPharmacyStoresStatewise.csv", row.names=FALSE)

# 10 stores which have revenue of less than 10K, 
nonPerfPharmacyStoresByMinRev <- PHRMCY_MASTER %>%
  left_join(POS_TRANS, by = 'PHRMCY_NBR') %>%
  group_by(PHRMCY_NBR, PHRMCY_NAM) %>%
  summarise(PHRMCY_NBR_SALES_REVENUE = sum(EXT_SLS_AMT)) %>%
  arrange(PHRMCY_NBR_SALES_REVENUE)
nonPerfPharmacyStoresByMinRev
write.csv(nonPerfPharmacyStoresByMinRev, file = "final-project/nonPerfPharmacyStoresByMinRev.csv", row.names=FALSE)


#12.	For any particular store which product generates maximum revenue? 
#4416100399456673861
#Top Perfroming Products
maxRevenueGeneratorProductForStore <- POS_TRANS %>%
  inner_join(PROD_MASTER, by = 'PROD_NBR') %>%  
  filter(PHRMCY_NBR == '4416100399456673861') %>%
  group_by(PROD_NBR, PROD_DESC) %>%
  summarise(salesRevenueBYProduct = sum(EXT_SLS_AMT)) %>%
  arrange(desc(salesRevenueBYProduct))
maxRevenueGeneratorProductForStore  
write.csv(maxRevenueGeneratorProductForStore, file = "final-project/maxRevenueGeneratorProductForStore.csv", row.names=FALSE)

#Worst Perfroming Products for a store
worstRevenueGeneratorProductForStore <- POS_TRANS %>%
  inner_join(PROD_MASTER, by = 'PROD_NBR') %>%  
  filter(PHRMCY_NBR == '4416100399456673861') %>%
  group_by(PROD_NBR, PROD_DESC) %>%
  summarise(salesRevenueBYProduct = sum(EXT_SLS_AMT)) %>%
  filter(salesRevenueBYProduct < 10) %>% 
  ungroup() %>%
  summarise(productCount = n()) 
worstRevenueGeneratorProductForStore  
write.csv(worstRevenueGeneratorProductForStore, file = "final-project/worstRevenueGeneratorProductForStore.csv", row.names=FALSE)

# Month on Month sales revenue for the store
monthOnMonthSalesRevenueForStore <- POS_TRANS %>%
  filter(PHRMCY_NBR == '4416100399456673861') %>%
  mutate(saleMonth = format(as.Date(SLS_DTE), "%b"),saleMonthNum = format(as.Date(SLS_DTE), "%m"), saleYear = format(as.Date(SLS_DTE), "%Y")) %>%
  group_by(saleYear, saleMonth, saleMonthNum) %>%
  summarise(salesRevenue = sum(EXT_SLS_AMT)) %>%
  arrange(saleYear, saleMonthNum)
monthOnMonthSalesRevenueForStore  
write.csv(monthOnMonthSalesRevenueForStore, file = "final-project/monthOnMonthSalesRevenueForStore.csv", row.names=FALSE)


# Sales Revenue By Product Segments 
salesRevenueByProductSegments <- POS_TRANS %>%
  inner_join(PROD_MASTER, by = 'PROD_NBR') %>%  
  inner_join(PROD_SEG, by = c("SEGMENT_CD" = "SEG_CD")) %>%
  group_by(SEGMENT_CD, SEG_DESC) %>%
  summarise(salesRevenue = sum(EXT_SLS_AMT)) %>%
  arrange(desc(salesRevenue))
salesRevenueByProductSegments
write.csv(salesRevenueByProductSegments, file = "final-project/salesRevenueByProductSegments.csv", row.names=FALSE)

# Sales Revenue By Product Sub Categories
salesRevenueByProductSubCat <- POS_TRANS %>%
  inner_join(PROD_MASTER, by = 'PROD_NBR') %>%  
  inner_join(PROD_SEG, by = c("SEGMENT_CD" = "SEG_CD")) %>%
  inner_join(PROD_SUB_CAT, by = c("SUB_CAT_CD" = "SUB_CAT_CD")) %>%
  group_by(SUB_CAT_CD, SUB_CAT_DESC) %>%
  summarise(salesRevenue = sum(EXT_SLS_AMT)) %>%
  arrange(desc(salesRevenue))
salesRevenueByProductSubCat 
write.csv(salesRevenueByProductSubCat, file = "final-project/salesRevenueByProductSubCat.csv", row.names=FALSE)

# Sales Revenue By Product Categories
salesRevenueByProductCat <- POS_TRANS %>%
  inner_join(PROD_MASTER, by = 'PROD_NBR') %>%  
  inner_join(PROD_SEG, by = c("SEGMENT_CD" = "SEG_CD")) %>%
  inner_join(PROD_SUB_CAT, by = c("SUB_CAT_CD" = "SUB_CAT_CD")) %>%
  inner_join(PROD_CAT, by = c("CAT_CD" = "CAT_CD")) %>%
  group_by(CAT_CD, CAT_DESC) %>%
  summarise(salesRevenue = sum(EXT_SLS_AMT)) %>%
  arrange(desc(salesRevenue))
salesRevenueByProductCat
write.csv(salesRevenueByProductCat, file = "final-project/salesRevenueByProductCat.csv", row.names=FALSE)


#Top Perfroming product in MISC GENERAL MERCHANDISE product category
topPerfromingProductsInGMCatg <- POS_TRANS %>%
  inner_join(PROD_MASTER, by = 'PROD_NBR') %>%  
  inner_join(PROD_SEG, by = c("SEGMENT_CD" = "SEG_CD")) %>%
  inner_join(PROD_SUB_CAT, by = c("SUB_CAT_CD" = "SUB_CAT_CD")) %>%
  inner_join(PROD_CAT, by = c("CAT_CD" = "CAT_CD")) %>%
  filter(CAT_CD == '6300') %>%
  group_by(PROD_NBR, PROD_DESC) %>%
  summarise(salesRevenue = sum(EXT_SLS_AMT)) %>%
  arrange(desc(salesRevenue))
topPerfromingProductsInGMCatg
write.csv(topPerfromingProductsInGMCatg, file = "final-project/topPerfromingProductsInGMCatg.csv", row.names=FALSE)
 

#Are some products categories getting sold in greater quantities based on seasonality? 
monthwiseSalesByProductMajorCat <- POS_TRANS %>%
  inner_join(PROD_MASTER, by = 'PROD_NBR') %>%  
  inner_join(PROD_SEG, by = c("SEGMENT_CD" = "SEG_CD")) %>%
  inner_join(PROD_SUB_CAT, by = c("SUB_CAT_CD" = "SUB_CAT_CD")) %>%
  inner_join(PROD_CAT, by = c("CAT_CD" = "CAT_CD")) %>%
  inner_join(MAJOR_PROD_CAT, by = c("MAJOR_CAT_CD" = "MAJOR_CAT_CD")) %>%
  mutate(saleMonth = format(as.Date(SLS_DTE), "%b"),saleMonthNum = format(as.Date(SLS_DTE), "%m"), saleYear = format(as.Date(SLS_DTE), "%Y")) %>%
  group_by(saleYear, saleMonth, saleMonthNum, MAJOR_CAT_CD, MAJOR_CAT_DESC) %>%
  summarise(monthlySaleValue = sum(EXT_SLS_AMT)) %>%
  arrange(saleMonthNum, desc(monthlySaleValue))
monthwiseSalesByProductMajorCat
write.csv(monthwiseSalesByProductMajorCat, file = "final-project/monthwiseSalesByProductMajorCat.csv", row.names=FALSE)

# For March Month which has highest sales revenue, in that month HEALTH CARE major product category
# had highest sales revenue, so below we trying to indentify which products were having high sales revenue.
highsellingProductInHealthCareCategoryInMarch <- POS_TRANS %>%
  inner_join(PROD_MASTER, by = 'PROD_NBR') %>%  
  inner_join(PROD_SEG, by = c("SEGMENT_CD" = "SEG_CD")) %>%
  inner_join(PROD_SUB_CAT, by = c("SUB_CAT_CD" = "SUB_CAT_CD")) %>%
  inner_join(PROD_CAT, by = c("CAT_CD" = "CAT_CD")) %>%
  inner_join(MAJOR_PROD_CAT, by = c("MAJOR_CAT_CD" = "MAJOR_CAT_CD")) %>%
  mutate(saleMonth = format(as.Date(SLS_DTE), "%b"),saleMonthNum = format(as.Date(SLS_DTE), "%m"), saleYear = format(as.Date(SLS_DTE), "%Y")) %>%
  filter(saleMonthNum == '03') %>%
  filter(MAJOR_CAT_CD == '5228') %>%
  group_by(PROD_NBR, PROD_DESC) %>%
  summarise(monthlySaleValue = sum(EXT_SLS_AMT)) %>%
  arrange(desc(monthlySaleValue))
highsellingProductInHealthCareCategoryInMarch
write.csv(highsellingProductInHealthCareCategoryInMarch, file = "final-project/highsellingProductInHealthCareCategoryInMarch.csv", row.names=FALSE)
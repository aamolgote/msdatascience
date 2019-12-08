install.packages("Hmisc")

library(Hmisc)
setwd("~/ABCPharmacy/CSV")
prod_master <- read.csv(file = "PROD MASTER.csv",
                 header = TRUE,
                 stringsAsFactors = FALSE,
                 na.strings = "")

describe(prod_master)

pharmacy_master <- read.csv(file = "PHRMCY MASTER.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        na.strings = "")

describe(pharmacy_master)


major_prod_catg <- read.csv(file = "MAJOR PROD CAT.csv",
                            header = TRUE,
                            stringsAsFactors = FALSE,
                            na.strings = "")
describe(major_prod_catg)

prod_catg <- read.csv(file = "PROD CAT.csv",
                            header = TRUE,
                            stringsAsFactors = FALSE,
                            na.strings = "")

describe(prod_catg)

prod_sub_catg <- read.csv(file = "PROD SUB CAT.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE,
                      na.strings = "")

describe(prod_sub_catg)

prod_seg <- read.csv(file = "PROD SEG.csv",
                          header = TRUE,
                          stringsAsFactors = FALSE,
                          na.strings = "")

describe(prod_seg)

pos_trans <- read.csv(file = "POS Transactions.csv",
                     header = TRUE,
                     stringsAsFactors = FALSE,
                     na.strings = "")

describe(pos_trans)

#major_prod_catg has last column which is empty for all rows, so deleting it.
describe(major_prod_catg)
colnames(major_prod_catg)
major_prod_catg$X <- NULL
colnames(major_prod_catg)
describe(major_prod_catg)

        




#0 Import the Tidyverse
library(readr)
setwd("~/Data-Management/ClassicModels/")
employees <- read.csv2(file = "EmployeesWithSalary.semicolon",
                                header = TRUE,
                                stringsAsFactors = FALSE,
                                na.strings = "")

employees
employees <- read.delim("EmployeesWithSalary.tablimited",sep="\t",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        na.strings = "")
employees
read




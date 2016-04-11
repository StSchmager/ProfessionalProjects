# Load packages
library(dplyr)

# Get data
setwd("~/Projects/Sales Data Analyses/3 PD/2 Marketing Mix Models")
data <- readRDS("ALL DEPT. Item Movement & Price Points Weekly per Store.rds")

DateBegin   <- min(data$Week)
DateEnd     <- max(data$Week)+6
DatePeriod  <- DateEnd-DateBegin

data <-      
# Ignore variables
select(data,
       -c(SubDept, PriceType, PriceLabel, UnitPrice, AvgPrice, Sales)) %>% 

# Filter out sales-irrelevant items
filter(!grepl("PROMO | COUPON | DISPLAY | OPEN RING | SHIPPER", Product)) %>% 

# Group by and aggregate across weeks
group_by(Dept, Store, UPC, Product) %>% 
      
# Aggregate movement in units sold
summarise(TotalUnitsSold = sum(UnitsSold, na.rm = T),
          Period         = paste(            min(Week), '-', max(Week+6)),
          Recent         = difftime(DateEnd, min(Week)))
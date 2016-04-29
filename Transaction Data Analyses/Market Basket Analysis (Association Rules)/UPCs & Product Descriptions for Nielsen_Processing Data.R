# Disable scientific notation and display UPCs correctly
options(scipen=999)
setwd("C:/Users/Stefan/Google Drive/Projects/4 Weekly Ad/2 Processed Data/2 Marketing Mix Models")
# Inactive items are not included since dataset consists only of items that have moved since Apr. 1, 2015
data <- readRDS("ALL DEPT. Item Movement & Price Points Weekly per Store.rds")
library(dplyr)
# and random weight items. 
data <-     select(data,
                   UPC, Product) %>% 
            # Remove duplicated items
            filter(!duplicated(UPC)) %>% 
            transmute(UPC   = factor(UPC),
                      Descr = as.character(Product))
data <-     mutate(data,
                   Type  = factor(nchar(as.character(UPC)))) %>% 
data <- filter(data, Type != "13")

sort(round(prop.table(table(data$Type))*100, 1), decreasing = T)
      
setwd("C:/Users/Stefan/Google Drive/Projects/3 Transaction-Data Analyses/0 Org/3 Market Basket Analysis/Product Categories/Nielsen")
write.csv2(data, "UPCs, Products, Number Type.csv", row.names = F)

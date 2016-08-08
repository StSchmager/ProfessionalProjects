library(dplyr)

## Read data ###########################################################################################################

setwd("C:/Users/Stefan/Google Drive/Projects/4 Sales-Data Analyses/2 Processed Data/2 Marketing Mix Models")
data0 <- readRDS("ALL DEPT. Item Movement & Price Points Weekly per Store.rds")
data0 <- mutate(data,
                Dept    = factor(Dept),
                SubDept = factor(Dept),
                UPC     = factor(UPC))

data1 <- 
      
## Conduct regression ##################################################################################################

# Determine regression coefficients per item x store location
# Make intercept more interpretable by centering AvgPrice; otherwise, intercept would have been at 0 USD
# Make slope more useful by changing AvgPrice to cents

coef(lm(Sales     ~ I(AvgPrice - mean(AvgPrice*100)), data = ))
coef(lm(UnitsSold ~ I(AvgPrice - mean(AvgPrice*100)), data = ))

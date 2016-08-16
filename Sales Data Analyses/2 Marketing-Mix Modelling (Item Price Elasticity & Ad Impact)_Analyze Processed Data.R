## 0) Settings #########################################################################################################

# Load packages
library(dplyr)

## 1) Read & Re-Process Data ###########################################################################################

setwd("/media/radmin/ExternalHD/Projects/Sales Data Analyses/3 PD/2 Marketing Mix Models")
data0 <- readRDS("ALL DEPT. Item Movement & Price Points Weekly per Store.rds")

# Determine sales-week (by date of starting day) related variables
dates <- select(data0, Week) %>% unique() %>% select(Week) %>% arrange(Week)  %>%
      mutate(WeekStart = Week,
             WeekEnd   = c(sort(unique(Week))[2:length(unique(Week))]-1, NA),
             WeekDates = factor(paste(WeekStart, "-", WeekEnd)),
             # How many days are contained in sales week (Christmas/NYE, etc. are unusually long)
             WeekDays  = as.numeric(WeekEnd-WeekStart+1),
             WeekNo    = as.numeric(c(1:length(WeekStart))),
             Week      = as.character(Week))

# Define all variables and join in additional sales-week/date variables                                                            
data0 <- mutate(data0,
                Week             = as.character(Week),
                Store            = factor(Store),
                Dept             = factor(Dept),
                SubDept          = factor(SubDept),
                UPC              = factor(UPC),
                Product          = as.character(Product),
                PriceType_fac    = factor(PriceType),
                PriceType_num    = as.numeric(as.character(factor(PriceType,
                                                                  labels = c("0", "1")))),
                SalesPriceLabel  = as.character(PriceLabel),
                SalesUnitPrice   = as.numeric(UnitPrice),
                AvgWeeklyPrice   = as.numeric(AvgPrice),
                DollarSales      = as.numeric(Sales),
                UnitSales        = as.numeric(UnitsSold)) %>%           left_join(dates, by = "Week") %>% 
      mutate(AvgDailyDollarSales = DollarSales/WeekDays,
             AvgDailyUnitSales   = UnitSales/WeekDays) %>% 
      select(WeekNo,
             WeekDates,
             WeekDays,
             Store,
             Dept,
             SubDept,
             UPC,
             Product,
             AvgWeeklyPrice,
             PriceType_fac,
             PriceType_num,
             SalesPriceLabel,
             SalesUnitPrice,
             DollarSales,
             UnitSales,
             AvgDailyDollarSales,
             AvgDailyUnitSales);                                        rm(dates)

products <- select(data0, UPC, Product) %>% unique() %>% mutate(UPC = factor(UPC))

## 2) Preliminary Analysis #############################################################################################

# Set filter variables
Dept_selected     <- "GROCERY" #unique(data0$Dept)
SubDept_selected  <- "GROCERY" #unique(data0$SubDept)
Week_selected     <- 52

# Aggregate data to UPC level and determine MVP products: most featured in sales paper & most sold (Pareto)
data1 <- 
      # Filtering according to selected (sub-)department & number of weeks
      filter(data0,
             (Dept    == Dept_selected) &
                   (SubDept == SubDept_selected) &
                   (WeekNo  <= Week_selected)) %>%
      # Aggregating data and calculate total sales features and total dollar sales (absolute) in USD
      group_by(UPC) %>% 
      summarize(TotalSalesFeat       = sum(PriceType_num, na.rm = T),
                TotalDollarSales_abs = sum(DollarSales,   na.rm = T)) %>% 
      # Add product counts
      mutate(   ProductCount         = 1) %>% 
      # Filter out products with negative sales (coupons, promotions with product code)
      filter(      TotalDollarSales_abs > 0) %>% 
      # Add product descriptions
      left_join(products, by = "UPC") %>% 
      # Re-factorize and see if number of variable factors matches number of dataset rows
      mutate(UPC = factor(UPC)) %>% 
      # Sort sales in descending order
      arrange(desc(TotalDollarSales_abs)) %>% 
      # Compute total dollar sales (relative) in percent of total
      mutate(      TotalDollarSales_rel = round(       TotalDollarSales_abs/sum(TotalDollarSales_abs)*100, 2)) %>% 
      # Compute cumulative dollar sales and product count in percent of total for Pareto distribution
      mutate(        CumDollarSales_rel = round(cumsum(TotalDollarSales_abs/sum(TotalDollarSales_abs)*100), 2)) %>%
      mutate(        CumProductCount    = round(cumsum(ProductCount        /sum(ProductCount)        *100), 2)) %>% 
      # Order all variables
      select(UPC, Product, TotalSalesFeat, TotalDollarSales_abs, TotalDollarSales_rel, CumDollarSales_rel, CumProductCount)



## Conduct regression ##################################################################################################

# Determine regression coefficients per item x store location
# Make intercept more interpretable by centering AvgPrice; otherwise, intercept would have been at 0 USD
# Make slope more useful by changing AvgPrice to cents

coef(lm(Sales     ~ I(AvgPrice - mean(AvgPrice*100)), data = ))
coef(lm(UnitsSold ~ I(AvgPrice - mean(AvgPrice*100)), data = ))

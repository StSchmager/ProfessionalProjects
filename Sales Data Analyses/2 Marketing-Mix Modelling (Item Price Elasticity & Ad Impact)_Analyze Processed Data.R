## 0) Settings #########################################################################################################

# Load packages
library(dplyr)
library(ggplot2)

## 1) Read & Re-Process Data ###########################################################################################

# Read data
setwd("/media/radmin/ExternalHD/Projects/Sales Data Analyses/3 PD/2 Marketing Mix Models")
data0 <- readRDS("ALL DEPT. Item Movement & Price Points Weekly per Store.rds")

# Compute week dates variable
dates <- select(data0,   Week) %>% unique() %>%
      arrange(           Week) %>%
      mutate(WeekStart = Week,
             # Day before starting day of the next week is end date of this week (the end date of the very last week is 7, because it is assumed that it's a regular week)
             WeekEnd   = c(sort(unique(Week))[2:length(unique(Week))]-1, sort(unique(data0$Week))[length(sort(unique(data0$Week)))]+6),
             WeekDates = factor(paste(WeekStart, "-", WeekEnd)),
             # How many days are contained in sales week (Christmas/NYE, etc. are unusually long)
             WeekDays  = as.numeric(WeekEnd-WeekStart+1),
             WeekNo    = as.numeric(c(1:length(WeekStart))),
             # Compute calendar week of each year
             WeekNo2   = paste0("Week #", strftime(WeekStart, format="%W, %Y")),
             Week      = as.character(Week))

# Define all variables
data0 <- mutate(data0,
                Week                  = as.character(Week),
                Store                 = factor(Store),
                Dept                  = factor(Dept),
                SubDept               = factor(SubDept),
                UPC                   = factor(UPC),
                Product               = as.character(Product),
                AdFeature             = factor(PriceType),
                SalesPriceLabel       = as.character(PriceLabel),
                SalesUnitPrice        = as.numeric(UnitPrice),
                AvgWeeklyPrice        = as.numeric(AvgPrice),
                DollarSales           = as.numeric(Sales),
                UnitSales             = as.numeric(UnitsSold)) %>%
      # Add computed Week variables from above
      left_join(dates, by = "Week") %>% 
      # Compute average sales per day to allow comparisons between weeks of different lengths
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
             AdFeature,
             SalesPriceLabel,
             SalesUnitPrice,
             DollarSales,
             UnitSales,
             AvgDailyDollarSales,
             AvgDailyUnitSales) %>% 
      # Filter out negative price & sales
      filter(DollarSales > 0,
             UnitSales   > 0,
             DollarSales > 0)

# Extract all product codes and descriptions
products_all <- select(data0, UPC, Product) %>%
      unique() %>%
      mutate(UPC = factor(UPC))

## 2) Determining Products of Interest #################################################################################

# Set filter variables
Dept_selected     <- "GROCERY" #unique(data0$Dept)
SubDept_selected  <- "GROCERY" #unique(data0$SubDept)
Store_selected    <- "12" 
Week_selected     <- 52

# Aggregate data to UPC level before determining...

data1 <- 
      # Filtering according to selected (sub-)department & number of weeks
      filter(data0, (Dept    ==    Dept_selected) &
                    (SubDept == SubDept_selected) &
                    (WeekNo  <=    Week_selected) &
                    (Store   ==   Store_selected))

# Calculate total number of ad weeks
TotalAdWeeks      <- length(unique(data1$WeekDates))

data1 <- 
      # Aggregating data and calculate total sales features and total dollar sales (absolute) in USD
      group_by(data1, Store, UPC) %>% 
      summarize(TotalAdFeat_abs      = sum(as.numeric(as.character(factor(AdFeature, levels = c("NON-SALE", "SALE"), labels = c("0", "1")))),
                                                            na.rm = T),
                TotalDollarSales_abs = sum(DollarSales,     na.rm = T),
                AvgWeeklyPrice       = mean(AvgWeeklyPrice, na.rm = T)) %>% 
      # Filter out products_all with negative sales (coupons, promotions with product code)
      filter(   TotalDollarSales_abs > 0) %>% 
      # Add product descriptions
      left_join(products_all, by = "UPC") %>% 
      # Add product counts
      mutate(ProductCount = 1) %>% 
      # Re-factorize and see if number of variable factors matches number of dataset rows
      mutate(UPC = factor(UPC))

# a) ... Most sold products
products_mostsold <-
      
      # Sort sales in descending order
      arrange(data1,
              Store,
              desc(TotalDollarSales_abs)) %>% 
      # Compute total dollar sales (relative) in percent of total
      mutate(      TotalDollarSales_rel = TotalDollarSales_abs/sum(TotalDollarSales_abs)*100) %>% 
      # Compute cumulative dollar sales and product count in percent of total for Pareto distribution (80% of total sales comes from 20% of products)
      mutate(        CumDollarSales_rel = round(cumsum(TotalDollarSales_rel),               2),
                     CumProductCount    = round(cumsum(ProductCount/sum(ProductCount)*100), 2)) %>% 
      select(UPC, Product, AvgWeeklyPrice,
             TotalDollarSales_abs, TotalDollarSales_rel,
               CumDollarSales_rel, CumProductCount) %>% 
      filter(CumDollarSales_rel < 80)

# a) ... Most featured in sales paper
products_mostfeat <-
      
      # Sort sales-feature count in descending order
      arrange(data1,
              Store,
              desc(TotalAdFeat_abs)) %>% 
      # Compute sales-feature count (relative) in percent of total
      mutate(      TotalAdFeat_rel = TotalAdFeat_abs/sum(TotalAdFeat_abs)*100,
                    FreqAdFeat     = TotalAdFeat_abs/TotalAdWeeks*100) %>% 
      # Compute cumulative sales-feature count and product count in percent of total for Pareto distribution
      mutate(CumAdFeat_rel    = round(cumsum(TotalAdFeat_rel),                     2),
             CumProductCount  = round(cumsum(ProductCount/sum(ProductCount)*100)), 2) %>% 
      select(UPC, Product, AvgWeeklyPrice,
             TotalAdFeat_abs, FreqAdFeat, TotalAdFeat_rel,
             CumAdFeat_rel, CumProductCount) %>% 
      filter(CumAdFeat_rel < 80)

## Conduct regression ##################################################################################################

# Look at correlation between regressors

# Include like codes

# Determine regression coefficients per item x store location
# Loop through each UPC and each stores
data2 <- filter(data0,
                UPC   == "9661908279",
                Store == Store_selected) %>% 
      mutate(AvgWeeklyPrice_centered = I(scale(AvgWeeklyPrice, center = T, scale = F)))
      

data3_byAdFeat <- 
      group_by(data2, Store, UPC, AdFeature) %>% 
      summarize(DollarSales           = mean(DollarSales,           na.rm = T),
                AvgWeeklyPrice = mean(AvgWeeklyPrice, na.rm = T),
                UnitSales             = mean(UnitSales,             na.rm = T),
                AvgDailyDollarSales   = mean(AvgDailyDollarSales,   na.rm = T),
                AvgDailyUnitSales     = mean(AvgDailyUnitSales,     na.rm = T))

data3 <- 
      group_by(data2, Store, UPC) %>% 
      summarize(AvgWeeklyPrice   = mean(AvgWeeklyPrice, na.rm = T),
                AvgWeeklyPrice = mean(AvgWeeklyPrice, na.rm = T),
                DollarSales           = mean(DollarSales,           na.rm = T),
                UnitSales             = mean(UnitSales,             na.rm = T),
                AvgDailyDollarSales   = mean(AvgDailyDollarSales,   na.rm = T),
                AvgDailyUnitSales     = mean(AvgDailyUnitSales,     na.rm = T))

plot_subtitle <- paste0(as.character(unique(data2$Product))," (", as.character(unique(data2$UPC)),") at Store ", as.character(unique(data2$Store)),
                        " on Avg. Sold for $",     format(round(mean(data2$AvgWeeklyPrice),                                    2), nsmall = 2),
                        " (ON Sale $",        format(round(mean(data2$AvgWeeklyPrice[data2$AdFeature == "SALE"]),     2), nsmall = 2),
                        " / NOT on Sale $",    format(round(mean(data2$AvgWeeklyPrice[data2$AdFeature == "NON-SALE"]), 2), nsmall = 2),
                        ")")
# AvgDailyUnitSales
ggplot(data2,
       aes(x = AvgWeeklyPrice,
           y = AvgDailyUnitSales,
           label = Store)) +
      ggtitle(bquote(atop(bold(.("Marketing-Mix Model: Price Elasticity & Ad-Feature Effectiveness")), atop(.(plot_subtitle)), ""))) +
      labs(x        = "Avg. Weekly Unit Price in USD",
           y        = "Avg. Daily Sales in Units") +
      geom_point( aes(color = AdFeature), size = 5, alpha = .3) +
      geom_smooth(aes(group = AdFeature,
                      color = AdFeature), method = "lm", fullrange = T, size = .5, show.legend = T, se = F, alpha = .1, linetype = "dotdash") +
      geom_smooth(    color = "black",        method = "lm", fullrange = T, size = .5, show.legend = F, se = F) +
      geom_text(  aes(color = AdFeature) ,show.legend = F) +
      geom_vline(xintercept = mean(data2$AvgWeeklyPrice), size = .5, linetype = "dotted") +
      geom_vline(aes(group = AdFeature), xintercept = mean(data2$AvgWeeklyPrice), size = .5, alpha = .3, linetype = "dotted") +
      geom_vline(xintercept = 0, size = .75) +
      geom_hline(yintercept = 0, size = .75) +
      geom_hline(yintercept = as.numeric(filter(data3_byAdFeat, AdFeature == "SALE")     %>% select(AvgDailyUnitSales)),
                 size = .5, alpha = .3, linetype="dashed",                color = "firebrick3") +
      geom_hline(yintercept = as.numeric(filter(data3_byAdFeat, AdFeature == "NON-SALE") %>% select(AvgDailyUnitSales)),
                 size = .5, alpha = .3, linetype="dashed",                color = "dodgerblue2") +
      geom_vline(xintercept = as.numeric(filter(data3_byAdFeat, AdFeature == "SALE")     %>% select(AvgWeeklyPrice)),
                 size = .5, alpha = .5, linetype="dotted",                color = "firebrick3") +
      geom_vline(xintercept = as.numeric(filter(data3_byAdFeat, AdFeature == "NON-SALE") %>% select(AvgWeeklyPrice)),
                 size = .5, alpha = .5, linetype="dotted",                color = "dodgerblue2") +
      geom_hline(yintercept = mean(data2$AvgDailyUnitSales, na.rm = T),
                 size = .5, alpha = .3, linetype="dashed") +
      theme(legend.position = "bottom") +
      scale_colour_manual(name   = "Ad Feature",
                          labels = c("NOT on Sale", "ON Sale"),
                          values = c("dodgerblue2","firebrick3")) +
      scale_y_continuous(breaks = seq(round(min(data2$AvgDailyUnitSales,     na.rm = T),     0),   round(max(data2$AvgDailyUnitSales, na.rm = T), 0),
                                  abs(round(min(data2$AvgDailyUnitSales,     na.rm = T),     0)-   round(max(data2$AvgDailyUnitSales, na.rm = T), 0))/10),
                         limits = c(        min(data2$AvgDailyUnitSales,     na.rm = T),                 max(data2$AvgDailyUnitSales, na.rm = T))) +
      scale_x_continuous(breaks = seq(round(min(data2$AvgWeeklyPrice, na.rm = T), 0)-1, round(max(data2$AvgWeeklyPrice, na.rm = T), 0)+1, 0.1),
                         limits = c(        min(data2$AvgWeeklyPrice, na.rm = T),             max(data2$AvgWeeklyPrice, na.rm = T)))
      


# Make intercept more interpretable by centering AvgPrice; otherwise, intercept would have been at 0 USD

## Response in Unit Sales
# Unadjusted
fit_Price <- 
      lm(data = data2, AvgDailyUnitSales ~ AvgWeeklyPrice_centered                )
fit_Ad <- 
      lm(data = data2, AvgDailyUnitSales ~                                AdFeature)
# Adjusted
fit_PriceAd <- 
      lm(data = data2, AvgDailyUnitSales ~ AvgWeeklyPrice_centered + AdFeature)
# Adjusted incl. Interaction Term
fit_PriceAd2 <- 
      lm(data = data2, AvgDailyUnitSales ~ AvgWeeklyPrice_centered + AdFeature +
                                           AvgWeeklyPrice_centered * AdFeature)

data3 <- as.data.frame(mutate(data3,
                              Coef1 = coef(fit_PriceAd)[1],
                              Coef2 = coef(fit_PriceAd)[2],
                              Coef3 = coef(fit_PriceAd)[3])) %>% 
      mutate(AdImpact   = round((Coef3/Coef1)*100, 2),
             PriceElast = round((Coef2/AvgDailyUnitSales)/(1/AvgWeeklyPrice),
                                2)# % Change in Demand divided by Change in Price
             )
      

resid(fit)
summary(fit)$sigma # Finding residual variance estimates.
sqrt(sum(resid(fit)^2) / (n - 2)) # Directly calculating from the residuals

# Residual plot
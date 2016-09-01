## 0) Settings #########################################################################################################

# Load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(xlsx)

## 1) Read & Re-Process Data ###########################################################################################

## Read data

# Like Codes
setwd("/media/radmin/ExternalHD/Projects/Sales Data Analyses/1 RD/0 Other")
LikeCodes   <- read.xlsx2("Like Codes.xlsx", 1) %>%
      transmute(UPC = as.character(UPC), LikeCode = as.character(Like.Code), Product = as.character(Product.Description)) %>% 
      select(LikeCode, UPC, Product)

# Weekly Sales
setwd("/media/radmin/ExternalHD/Projects/Sales Data Analyses/3 PD/2 Marketing Mix Models")
data0_UPC       <- readRDS("ALL DEPT. Item Movement & Price Points Weekly per Store.rds")

# Filter data by department
SelectedDept      <- "GROCERY" #unique(data0_UPC$Dept)
SelectedSDept     <- "GROCERY" #unique(data0_UPC$SubDept)

# Specify ad-week dates
dates0 <- select(data0_UPC,   Week) %>% unique() %>%
      arrange(           Week) %>%
      mutate(# Original ad-week date is starting day of the week
             WeekStart = Week,
             # Day before starting day of the next week is end date of this week (the end date of the very last week is 7, because it is assumed that it's a regular 7-day week)
             WeekEnd   = c(sort(unique(Week))[2:length(unique(Week))]-1, sort(unique(data0_UPC$Week))[length(sort(unique(data0_UPC$Week)))]+6),
             WeekDates = factor(paste(WeekStart, "-", WeekEnd)),
             # How many days are contained in sales week (Christmas/NYE, etc. have unusual length)
             WeekDays  = as.numeric(WeekEnd-WeekStart+1),
             WeekNo    = as.numeric(c(1:length(WeekStart))),
             # Compute calendar week of each year
             WeekNo2   = paste0("Week #", strftime(WeekStart, format="%W, %Y")),
             Week      = as.character(Week))

# Define all variables
data0_UPC <- mutate(data0_UPC,
                Week                  = as.character(Week),
                Store                 = factor(Store),
                Dept                  = factor(Dept),
                SubDept               = factor(SubDept),
                UPC                   = as.character(UPC),
                Product               = as.character(Product),
                AdFeature             = factor(PriceType),
                SalesPriceLabel       = as.character(PriceLabel),
                SalesUnitPrice        = as.numeric(UnitPrice),
                AvgWeeklyPrice        = as.numeric(AvgPrice),
                WeeklyDollarSales     = as.numeric(Sales),
                WeeklyUnitSales       = as.numeric(UnitsSold)) %>%
      # Add computed Week variables from above
      left_join(dates0, by = "Week") %>% 
      # Add Like Code per UPC
      left_join(select(LikeCodes, -Product), by = "UPC") %>% 
      # Create additional variable that distinguishes between UPC and Like Code since overlapping codes may exist
      mutate(UPC2      = as.character(paste(UPC,      "(UPC)")),
             LikeCode2 = as.character(paste(LikeCode, "(LC)"))) %>% 
      # Compute average sales per day to allow comparisons between weeks of different lengths
      mutate(LikeCode_UPC = LikeCode2,
             AvgDailyDollarSales    = WeeklyDollarSales/WeekDays,
             AvgDailyUnitSales      = WeeklyUnitSales/WeekDays) %>% 
      select(WeekNo,
             WeekNo2,
             WeekDates,
             WeekDays,
             Store,
             Dept,
             SubDept,
             UPC,
             LikeCode,
             UPC2,
             LikeCode2,
             LikeCode_UPC,
             Product,
             AvgWeeklyPrice,
             AdFeature,
             #SalesPriceLabel,
             #SalesUnitPrice,
             WeeklyDollarSales,
             WeeklyUnitSales,
             AvgDailyDollarSales,
             AvgDailyUnitSales) %>% 
      # Filter out negative price & sales
      filter(WeeklyDollarSales > 0,
             WeeklyUnitSales   > 0)

# Create hybrid variable: if no like code exist, take UPC instead in order to group/summarize later by liek code, if available, and UPC if like code doesn't exist
data0_UPC$LikeCode_UPC[data0_UPC$LikeCode_UPC == "NA (LC)"] <- data0_UPC$UPC2[data0_UPC$LikeCode_UPC == "NA (LC)"]

# Extract all product codes and description per SKU
products_UPC      <- select(data0_UPC,
                            UPC, LikeCode, LikeCode_UPC, Product) %>% unique()
# Group like codes (if none, just display UPC) and concatenate product descriptions per product group
products_LC       <- ddply(products_UPC, .(LikeCode_UPC), summarize, Product = paste(Product, collapse = ", "))

#Aggregate data by like code
data0_LC <- group_by(data0_UPC,
                     WeekNo,
                     WeekNo2,
                     WeekDates,
                     WeekDays,
                     Store,
                     Dept,
                     SubDept,
                     AdFeature,
                     LikeCode_UPC) %>% 
      summarize(AvgWeeklyPrice      = mean(AvgWeeklyPrice),
                #SalesPriceLabel    = ,
                #SalesUnitPrice     = ,
                WeeklyDollarSales   = sum(WeeklyDollarSales),
                WeeklyUnitSales     = sum(WeeklyUnitSales),
                AvgDailyDollarSales = sum(AvgDailyDollarSales),
                AvgDailyUnitSales   = sum(AvgDailyUnitSales)) %>% 
      left_join(products_LC, by = "LikeCode_UPC") %>% 
      select(WeekNo,
             WeekNo2,
             WeekDates,
             WeekDays,
             Store,
             Dept,
             SubDept,
             LikeCode_UPC,
             Product,
             AvgWeeklyPrice,
             AdFeature,
             WeeklyDollarSales,
             WeeklyUnitSales,
             AvgDailyDollarSales,
             AvgDailyUnitSales)

# Remove dummy variables
data0_UPC$UPC2          <- NULL
data0_UPC$LikeCode2     <- NULL

## 2) Determining Products of Interest per Store #######################################################################

# Filter Weeks & Store
SelectedWeek    <- c(2:53) # Exclude first week of April and exclude Easter 2015 (Easter 2016 is end of March)
SelectedStore    <- "3"

# Aggregate data to UPC level before determining...
data1 <- filter(data0_LC,
                # Filtering according to selected (sub-)department & number of weeks
                (Dept    ==   SelectedDept)  &
                (SubDept ==   SelectedSDept) &
                (Store   ==   SelectedStore) &
                (WeekNo  %in% SelectedWeek)) %>% 
      # Aggregating data and calculate total sales features and total dollar sales (absolute) in USD
      group_by(LikeCode_UPC) %>% 
      summarize(TotalAdFeat_abs      = sum(as.numeric(as.character(factor(AdFeature,
                                                                          levels = c("NON-SALE", "SALE"),
                                                                          labels = c("0",        "1")))), na.rm = T),
                TotalDollarSales_abs = sum(WeeklyDollarSales,                                             na.rm = T),
                AvgWeeklyPrice       = mean(AvgWeeklyPrice,                                               na.rm = T)) %>% 
      # Filter out products with negative sales (coupons, promotions with product code)
      filter(   TotalDollarSales_abs > 0) %>% 
      # Filter out products with negative prices (coupons, promotions with product code)
      filter(   AvgWeeklyPrice > 0) %>% 
      # Add product descriptions
      left_join(products_LC, by = "LikeCode_UPC") %>% 
      # Add product counts
      mutate(ProductCount = 1)

# Determine period of the dataset
dates1            <- filter(dates0, (WeekNo  %in%  SelectedWeek))
WeekDate_Begin    <- strsplit(as.character(sort(dates1$WeekDates)[1]),
                              " - ")[[1]][1]
WeekDate_End      <- strsplit(as.character(sort(dates1$WeekDates)[length(sort(dates1$WeekDates))]),
                              " - ")[[1]][2]

# Calculate total number of ad weeks
TotalAdWeeks      <- length(unique(dates1$WeekDates))
TotalCalenderWeeks<- sum(dates1$WeekDays)/7

# a) ... Most sold products
products_MostSold <-
      
      # Sort sales in descending order
      arrange(data1,
              #Store,
              desc(TotalDollarSales_abs)) %>% 
      # Compute total dollar sales (relative) in percent of total of store
      mutate(      TotalDollarSales_rel = TotalDollarSales_abs/
                                      sum(TotalDollarSales_abs)*100,
                   FreqAdFeat           = TotalAdFeat_abs/
                                          TotalAdWeeks*100) %>% 
      # Compute cumulative dollar sales and product count in percent of total for Pareto distribution (80% of total sales comes from 20% of products)
      mutate(        CumDollarSales_rel = round(cumsum(TotalDollarSales_rel), 2),
                     CumProductCount    = round(cumsum(ProductCount/
                                                   sum(ProductCount)*100),    2)) %>% 
      select(LikeCode_UPC, Product,
             AvgWeeklyPrice,
             TotalAdFeat_abs, FreqAdFeat,
             TotalDollarSales_abs,
             TotalDollarSales_rel,
               CumDollarSales_rel,
               CumProductCount) %>% 
      filter(CumDollarSales_rel < 80)

# a) ... Most featured in sales paper
products_MostFeat <-
      
      # Sort sales-feature count in descending order
      arrange(data1,
              #Store,
              desc(TotalAdFeat_abs)) %>% 
      # Compute sales-feature count (relative) in percent of total
      mutate(      TotalAdFeat_rel = TotalAdFeat_abs/
                                 sum(TotalAdFeat_abs)*100,
                    FreqAdFeat     = TotalAdFeat_abs/
                                     TotalAdWeeks*100) %>% 
      # Compute cumulative sales-feature count and product count in percent of total for Pareto distribution
      mutate(CumAdFeat_rel    = round(cumsum(TotalAdFeat_rel),   2),
             CumProductCount  = round(cumsum(ProductCount/
                                         sum(ProductCount)*100), 2)) %>% 
      select(LikeCode_UPC, Product,
             AvgWeeklyPrice,
             TotalDollarSales_abs,
             TotalAdFeat_abs,
             TotalAdFeat_rel,
               CumAdFeat_rel,
             FreqAdFeat,
             CumProductCount) %>% 
      filter(CumAdFeat_rel < 80)

## 3) Build regression model ###########################################################################################

## 3a) Response in Unit Sales by Price (Unadjusted) of Most Sold Products ##############################################

SelectedProducts        <- products_MostSold$LikeCode_UPC[1:10]
       ResultList       <- list()
length(ResultList)      <- length(SelectedProducts)

for (i in 1:length(SelectedProducts)) {

# Determine regression coefficients per item x store location
# Loop through each UPC and each stores
data2a <- filter(data0_LC,
                 LikeCode_UPC ==   SelectedProducts[i],
                 Store        ==   SelectedStore,
                 WeekNo       %in% SelectedWeek) #%>%
      # Make intercept more interpretable by centering AvgPrice; otherwise, intercept would have been at 0 USD
      #mutate(AvgWeeklyPrice_centered = scale(AvgWeeklyPrice, center = T, scale = F))

fit_Price <- lm(data = data2a, AvgDailyUnitSales ~ AvgWeeklyPrice_centered)

# Summarize results
data3_byAdFeat <- 
      group_by(data2a, Store, LikeCode_UPC, AdFeature) %>% 
      summarize(WeeklyDollarSales   = mean(WeeklyDollarSales,   na.rm = T),
                AvgWeeklyPrice      = mean(AvgWeeklyPrice,      na.rm = T),
                WeeklyUnitSales     = mean(WeeklyUnitSales,     na.rm = T),
                AvgDailyDollarSales = mean(AvgDailyDollarSales, na.rm = T),
                AvgDailyUnitSales   = mean(AvgDailyUnitSales,   na.rm = T))

data3 <- 
      group_by(data2a, Store, LikeCode_UPC) %>% 
      summarize(AvgWeeklyPrice      = round(mean(AvgWeeklyPrice,      na.rm = T), 2),
                #WeeklyDollarSales   = round(mean(WeeklyDollarSales,   na.rm = T), 0),
                #WeeklyUnitSales     = round(mean(WeeklyUnitSales,     na.rm = T), 0),
                AvgDailyDollarSales = round(mean(AvgDailyDollarSales, na.rm = T), 0),
                AvgDailyUnitSales   = round(mean(AvgDailyUnitSales,   na.rm = T), 0))

data3 <- as.data.frame(mutate(data3,
                              RSq    = round(summary(fit_Price)$r.squared,          3),
                              Beta0  = round(summary(fit_Price)$coefficients[1, 1], 0),
                              Sign0  = round(summary(fit_Price)$coefficients[1, 4], 3),
                              Beta1  = round(summary(fit_Price)$coefficients[2, 1], 2),
                              Sign1  = round(summary(fit_Price)$coefficients[2, 4], 3)#,
                              #Beta2  = coef(   fit_PriceAd)[3]
                              )) %>% 
      mutate(#AdImpact   = round((Beta2/Beta0)*100, 2),
             PriceElast = round((Beta1/Beta0)/(1/AvgWeeklyPrice), 2)# % Change in Demand divided by Change in Price
      )

ResultList[[i]] <- data3 

}

Results <- ldply(ResultList, data.frame)

## 3b) Response in Unit Sales by Ad Feature (Unadjusted) & Ad Feature x Price (Adjusted) of Most Featured Products #####
# Look at correlation between regressors

#fit_Ad            <- lm(data = data2a, AvgDailyUnitSales ~                           AdFeature)
# Adjusted
#fit_PriceAd       <- lm(data = data2a, AvgDailyUnitSales ~ AvgWeeklyPrice_centered + AdFeature)
# Adjusted incl. Interaction Term
#fit_PriceAd2      <- lm(data = data2a, AvgDailyUnitSales ~ AvgWeeklyPrice_centered + AdFeature +
#                                                          AvgWeeklyPrice_centered * AdFeature)

resid(fit)
summary(fit)$sigma # Finding residual variance estimates.
sqrt(sum(resid(fit)^2) / (n - 2)) # Directly calculating from the residuals

## 4) Plot regression model ############################################################################################

plot_subtitle <- paste0(as.character(unique(data2$Product))," (", as.character(unique(data2$UPC)),") at Store ", as.character(unique(data2$Store)),
                        " on Avg. Sold for $",     format(round(mean(data2$AvgWeeklyPrice),                                    2), nsmall = 2),
                        " (ON Sale $",        format(round(mean(data2$AvgWeeklyPrice[data2$AdFeature == "SALE"]),     2), nsmall = 2),
                        " / NOT on Sale $",    format(round(mean(data2$AvgWeeklyPrice[data2$AdFeature == "NON-SALE"]), 2), nsmall = 2),
                        ")")
# AvgDailyUnitSales
ggplot(data2,
       aes(x = AvgWeeklyPrice,
           #label = Store,
           y = AvgDailyUnitSales
           )) +
      ggtitle(bquote(atop(bold(.("Marketing-Mix Model: Price Elasticity & Ad-Feature Impact")), atop(.(plot_subtitle)), ""))) +
      labs(x        = "Avg. Weekly Unit Price in USD",
           y        = "Avg. Daily Sales in Units") +
      geom_point( aes(color = AdFeature), size = 5, alpha = .3) +
      geom_smooth(aes(group = AdFeature,
                      color = AdFeature), method = "lm", fullrange = T, size = .5, show.legend = T, se = F, alpha = .1, linetype = "dotdash") +
      geom_smooth(    color = "black",        method = "lm", fullrange = T, size = .5, show.legend = F, se = F) +
      #geom_text(  aes(color = AdFeature) ,show.legend = F) +
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

# Residual plot

## 5) Export results ####################################################################

setwd("/media/radmin/ExternalHD/Projects/Sales Data Analyses/3 PD/4 Product Mix")
write.csv(products_MostFeat, paste0("Most Featured Products_Store ", SelectedStore, "_", WeekDate_Begin, " - ", WeekDate_End, " (", TotalAdWeeks,  " Weeks).csv"), row.names = F)
write.csv(products_MostSold, paste0("Most Sold Products_Store ",     SelectedStore, "_", WeekDate_Begin, " - ", WeekDate_End, " (", TotalAdWeeks,  " Weeks).csv"), row.names = F)
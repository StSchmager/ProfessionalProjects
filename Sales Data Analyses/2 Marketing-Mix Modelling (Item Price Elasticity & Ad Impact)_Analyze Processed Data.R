## 0) Settings #########################################################################################################

# Load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(xlsx)
library(tidyr)

## 1) Read & Process Data ##############################################################################################

## 1a) Sales Data #################################################################################################

setwd("/media/radmin/ExternalHD/Projects/Sales Data Analyses/3 PD/2 Marketing Mix Models")
data0 <- readRDS("ALL DEPT. Item Movement & Price Points Weekly per Store.rds")
data0 <- transmute(data0,
                       Week                   = as.character(Week),
                       Store                  = factor(      Store),
                       Dept                   = factor(      Dept),
                       SubDept                = factor(      SubDept),
                       UPC                    = as.character(UPC),
                       Product                = as.character(Product),
                       AdFeature              = factor(      PriceType),
                       AdPriceLabel           = as.character(PriceLabel),
                       AdUnitPrice            = as.numeric(  UnitPrice),
                       AvgWeeklyUnitPrice     = as.numeric(  AvgPrice),
                       TotalWeeklyUnitSales   = as.numeric(  UnitsSold),
                       TotalWeeklyDollarSales = as.numeric(  Sales),
                       AvgWeeklyUnitCost      = as.numeric(  AvgUnitCost),
                       TotalWeeklyCOGS        = as.numeric(  Costs))
  
# Convert costs to N/A if they are 0
data0$AvgWeeklyUnitCost[
data0$AvgWeeklyUnitCost == 0]   <- NA

# Compute COGS, Profit Dollar/Margin
data0 <- mutate(data0,
                    TotalWeeklyCOGS          = round(AvgWeeklyUnitCost*TotalWeeklyUnitSales,          2)) %>% 
             mutate(TotalWeeklyProfitDollars = round(TotalWeeklyDollarSales-TotalWeeklyCOGS,          2)) %>% 
             mutate(  AvgWeeklyProfitMargin  = round(TotalWeeklyProfitDollars/TotalWeeklyDollarSales, 4))

## 1b) Auxiliary Data #############################################################################################

## LIKE CODES
setwd("/media/radmin/ExternalHD/Projects/Sales Data Analyses/1 RD/0 Other")
LikeCodes   <- read.xlsx2("Like Codes.xlsx", 1) %>%
      transmute(UPC      = as.character(UPC),
                LikeCode = as.character(Like.Code),
                Product  = as.character(Product.Description))

# Join like codes to each UPC in sales data
data0 <- left_join(data0,
                       select(LikeCodes, -Product), by = "UPC") %>% 
# Create additional variable that distinguishes between UPC and like code to avoid code overlap between those type of codes
      mutate(UPC2         = as.character(paste(UPC,      "(UPC)")),
             LikeCode2    = as.character(paste(LikeCode, "(LC)")),
# Create hybrid variable (dummy) that combines codes, but avoids code overlap
             LikeCode_UPC = NA)
# Populate hybrid variable: if no like code exist, take UPC instead in order to group/summarize later by like code if available, otherwise (not available-- NA), by UPC; Remove dummy variables
data0$LikeCode_UPC[!is.na(data0$LikeCode)] <- data0$LikeCode2[
                       !is.na(data0$LikeCode)];   data0$LikeCode2     <- NULL
data0$LikeCode_UPC[ is.na(data0$LikeCode)] <- data0$UPC2[
                        is.na(data0$LikeCode)];   data0$UPC2          <- NULL

## DATES of Ad Weeks

Dates0 <- select(data0, Week) %>% unique() %>%
      arrange(              Week) %>% mutate(
            # Convert week date from character to date data type in order to compute other date variables
            Week = as.Date(Week),
            # Original initial week date is starting day of the week
            WeekStart = Week,
            # Day before starting day of the NEXT week is end date of THIS week; ...
            WeekEnd   = c(sort(unique(Week))[2:length(unique(Week))]-1,
                          # ... End date of the VERY LAST week is starting date plus 6 days since the NEXT week is not yet part of the data
                          sort(unique(Week))[  length(sort(unique(Week)))]+6),
            # Stitch week start and end together
            WeekDates = factor(paste(WeekStart, "-", WeekEnd)),
            # How many days does each sales week have (usually 7 days, but irregular weeks like Christmas/NYE, etc. have unusual length)
            WeekDays  = as.numeric(WeekEnd-WeekStart+1),
            # Count weeks from first to last week
            WeekNo    = as.numeric(c(1:length(WeekStart))),
            # Compute calendar week of each year
            WeekNo2   = paste0("Week #", strftime(WeekStart, format="%W, %Y")),
            # Convert initial week date back to character in order to join with sales data
            Week      = as.character(Week))

# Join date variables to each Week in sales data
data0 <- left_join(data0,
                       Dates0, by = "Week") %>%
# Compute averages per day to allow comparisons between weeks of different lengths
mutate(AvgDailyUnitSales     = round(TotalWeeklyUnitSales/     WeekDays, 2),
       AvgDailyDollarSales   = round(TotalWeeklyDollarSales/   WeekDays, 2),
       AvgDailyCOGS          = round(TotalWeeklyCOGS/          WeekDays, 2),
       AvgDailyProfitDollars = round(TotalWeeklyProfitDollars/ WeekDays, 2))

## PRODUCTS

# Extract all unique products with their codes and description per SKU
Products_UPC      <- select(data0,
                            LikeCode, UPC, LikeCode_UPC, Product) %>% unique()
# Group all products by like codes (if none, just display UPC), extract them and describe by concatenating grouped products
Products_LC       <- ddply(Products_UPC,
                           .(LikeCode_UPC), summarize,
                           Product = paste(Product, collapse = ", "))

## 2a) Filter Data by Week, Dept., Store ###############################################################################

## DEPT.
#unique(                  data0$Dept)
#unique(data0$SubDept[data0$Dept==SelectedDept])
SelectedDept      <- c("GROCERY")          
SelectedSDept     <- c("GROCERY", "BEVERAGE")

## WEEKS
SelectedWeeks    <- c(22:74)
# Determine period of the dataset
Dates1                  <- filter(Dates0, WeekNo %in% SelectedWeeks)
SelectedWeeks_Begin     <- strsplit(as.character(sort(Dates1$WeekDates)[1]),
                                    " - ")[[1]][1]
SelectedWeeks_End       <- strsplit(as.character(sort(Dates1$WeekDates)[length(sort(Dates1$WeekDates))]),
                                    " - ")[[1]][2]
# Calculate total number of ad weeks
SelectedWeeks_TotalAdWeeks          <- length(unique(Dates1$WeekDates))
SelectedWeeks_TotalWeekDays         <- sum(Dates1$WeekDays)
SelectedWeeks_TotalCalenderWeeks    <- sum(Dates1$WeekDays)/7

## STORE
SelectedStores    <- c("3", "5", "6")

## 2b) Aggregate Data to Product Level (UPC) ###########################################################################

# Per store (loop through selected stores) for given weeks and depts.

       StoreList  <- list()
length(StoreList) <- length(SelectedStores)

#for (j in 1:length(SelectedStores)) {
      
SelectedStore     <- SelectedStores[1]

# Filter according to selected weeks, stores, depts.
data1 <- filter(data0,
                    (Dept    %in% SelectedDept)    &
                    (SubDept %in% SelectedSDept)   &
                    (Store   ==   SelectedStore)   &
                    (WeekNo  %in% SelectedWeeks)
                    ) %>% 
# Aggregating data to UPC level and summarize all other variables
      group_by(Store, Dept, SubDept, UPC) %>% 
      summarize(TotalAdFeat_abs      = sum(as.numeric(as.character(factor(AdFeature,
                                                                          levels = c("NON-SALE", "SALE"),
                                                                          labels = c("0",        "1")))), na.rm = T),
                AvgWeeklyUnitPrice    = round(mean(AvgWeeklyUnitPrice,                                    na.rm = T), 2),
                AvgWeeklyUnitCost     = round(mean(AvgWeeklyUnitCost,                                     na.rm = T), 2),
                TotalyUnitSales       =       sum(TotalWeeklyUnitSales,                                   na.rm = T),
                TotalDollarSales      =       sum(TotalWeeklyDollarSales,                                 na.rm = T),
                TotalCOGS             =       sum(TotalWeeklyCOGS,                                        na.rm = T),
                TotalProfitDollars    =       sum(TotalWeeklyProfitDollars,                               na.rm = T),
                AvgWeeklyProfitMargin = round(mean(AvgWeeklyProfitMargin,                                 na.rm = T), 2),
                AvgDailyUnitSales     = round(mean(AvgDailyUnitSales,                                     na.rm = T), 2),
                AvgDailyDollarSales   = round(mean(AvgDailyDollarSales,                                   na.rm = T), 2),
                AvgDailyCOGS          = round(mean(AvgDailyCOGS,                                          na.rm = T), 2),
                AvgDailyProfitDollars = round(mean(AvgDailyProfitDollars,                                 na.rm = T), 2)) %>% 
      ungroup() %>% 
      # Sort items by total sales in descending order
      arrange(desc(TotalDollarSales)) %>% 
      # Compute (relative) total dollar sales in percent of total of store    
      mutate(TotalDollarSales_rel  = TotalDollarSales/sum(TotalDollarSales),
             # Calculate ad features in percentage of total ad weeks (100%: item was featured in ad every single week)
             TotalAdFeat_rel       = round(TotalAdFeat_abs/SelectedWeeks_TotalAdWeeks, 4),
             # Add product count absolute and relative in percent of total
             ProductCount          = 1,
             ProductCount_rel      = ProductCount/sum(ProductCount)) %>% 
      # Cumulate sales and product counts for Pareto 80-20 rule
      mutate(CumDollarSales_rel    =       cumsum(TotalDollarSales_rel),
             CumProductCount       =       cumsum(ProductCount_rel)) %>%
      # Add product descriptions, and like codes
      left_join(Products_UPC, by = "UPC")

## 3) Build regression models ###########################################################################################

# Run regression per item
SelectedProducts        <- data1$UPC[1:10]
        ProductList     <- list()
 length(ProductList)    <- length(SelectedProducts)

for (i in 1:length(SelectedProducts)) {
SelectedProduct <- SelectedProducts[3]
 
      data2 <- ungroup(data0) %>% 
            filter(UPC    ==   SelectedProduct,
                   Store  ==   SelectedStore,
                   WeekNo %in% SelectedWeeks) %>%
            # Centering price by UPC, per store and in time frame (Make intercept more interpretable; otherwise, intercept would have been at 0 USD)
            mutate(AvgWeeklyUnitPrice_centered = scale(AvgWeeklyUnitPrice, center = T, scale = F))
      
      ## Create results table
      results <- as.data.frame(matrix(nrow = 1)) %>% 
            rename(UPC = V1) %>% 
            mutate(UPC = SelectedProduct) %>% 
            left_join(Products_UPC, by = "UPC") %>% 
            left_join(select(data1,       UPC, AvgWeeklyUnitCost),
                                    by = "UPC")
      # Calculate std. deviation for price variable and total ad features (both filter criteria for models)
      results$StdWeeklyUnitPriceDev <- round(sd(data2$AvgWeeklyUnitPrice), 2)
      results$TotalAdFeat_abs       <- sum(as.numeric(as.character(factor(data2$AdFeature,
                                                                levels = c("NON-SALE", "SALE"),
                                                                labels = c("0",        "1")))), na.rm = T)
      results$TotalAdFeat_rel       <- round(results$TotalAdFeat_abs/SelectedWeeks_TotalAdWeeks, 4)
      
      ## Build various models
      
      lm_Base     <- lm(data = data2, AvgDailyUnitSales  ~ 1) # !!! lm_Price2 yields the same results !!!
      
      lm_Price0   <- lm(data = data2,                      AvgWeeklyUnitPrice ~ 1)
      lm_Price    <- lm(data = data2, AvgDailyUnitSales  ~ AvgWeeklyUnitPrice)
      lm_Price2   <- lm(data = data2, AvgDailyUnitSales  ~ AvgWeeklyUnitPrice_centered)
      
      # If product was featured in ad at least once then build models that include ad-feature variable
      if (results$TotalAdFeat_abs > 0) { 
      
      lm_PriceAd0 <- lm(data = data2,                      AvgWeeklyUnitPrice          ~ AdFeature)
      lm_Ad       <- lm(data = data2, AvgDailyUnitSales  ~                               AdFeature)
      lm_PriceAd  <- lm(data = data2, AvgDailyUnitSales  ~ AvgWeeklyUnitPrice          + AdFeature)
      lm_PriceAd2 <- lm(data = data2, AvgDailyUnitSales  ~ AvgWeeklyUnitPrice_centered + AdFeature)
      
      }
      
      ## Save coefficients and significance levels
      
      results$AvgDailyUnitSales                       <- round(summary(lm_Base)$coefficients[1, 1], 2) # !!! lm_Price2 yields the same results !!!
      results$AvgDailyUnitSales_Sig                   <- round(summary(lm_Base)$coefficients[1, 4], 4)
      
      results$AvgWeeklyUnitPrice                      <- round(summary(lm_Price0)$coefficients[1, 1], 2)
      results$AvgWeeklyUnitPrice_Sig                  <- round(summary(lm_Price0)$coefficients[1, 4], 4)
      
      # If product has changed in price, models were built sensefully and coefficients can be saved
      if (results$StdWeeklyUnitPriceDev > 0) { 
      
      results$RSq_Price                               <- round(summary(lm_Price)$r.squared, 3)
      results$AdjRSq_Price                            <- round(summary(lm_Price)$adj.r.squared, 3) 
      results$AvgDailyUnitSales_ZeroPrice             <- round(summary(lm_Price)$coefficients[1, 1], 2)
      results$AvgDailyUnitSales_ZeroPrice_Sig         <- round(summary(lm_Price)$coefficients[1, 4], 4)
      results$AvgDailyUnitSales_PriceElasNotAdAdj     <- round(summary(lm_Price)$coefficients[2, 1], 2)
      results$AvgDailyUnitSales_PriceElasNotAdAdj_Sig <- round(summary(lm_Price)$coefficients[2, 4], 4)
      
      results$RSq_Price2                              <- round(summary(lm_Price2)$r.squared, 3)          # !!! Replicates results from lm_Price !!!
      results$AdjRSq_Price2                           <- round(summary(lm_Price2)$adj.r.squared, 3)      # !!! Replicates results from lm_Price !!!
      results$AvgDailyUnitSales_AvgPrice              <- round(summary(lm_Price2)$coefficients[1, 1], 2) # !!! Replicates results from lm_Base !!!
      results$AvgDailyUnitSales_AvgPrice_Sig          <- round(summary(lm_Price2)$coefficients[1, 4], 4)
      results$AvgDailyUnitSales_PriceElasNotAdAdj     <- round(summary(lm_Price2)$coefficients[2, 1], 2) # !!! Replicates results from lm_Price !!!
      results$AvgDailyUnitSales_PriceElasNotAdAdj_Sig <- round(summary(lm_Price2)$coefficients[2, 4], 4)
      
      # If product has NOT changed in price, models were built NOT sensefully and coefficients should be NA
      } else {
            
      results$RSq_Price                               <- NA
      results$AdjRSq_Price                            <- NA
      results$AvgDailyUnitSales_ZeroPrice             <- NA
      results$AvgDailyUnitSales_ZeroPrice_Sig         <- NA
      results$AvgDailyUnitSales_PriceElasNotAdAdj     <- NA
      results$AvgDailyUnitSales_PriceElasNotAdAdj_Sig <- NA
            
      results$RSq_Price2                              <- NA
      results$AdjRSq_Price2                           <- NA
      results$AvgDailyUnitSales_AvgPrice              <- NA
      results$AvgDailyUnitSales_AvgPrice_Sig          <- NA
      results$AvgDailyUnitSales_PriceElasNotAdAdj     <- NA
      results$AvgDailyUnitSales_PriceElasNotAdAdj_Sig <- NA
                  
      }
      
      # If product was featured in ad at least once then models were built and coefficients can be saved
      if (results$TotalAdFeat_abs > 0) { 
      
      results$PriceAdCorr                             <- round(summary(lm_PriceAd0)$r.squared, 3)
      results$PriceAdCorrAdj                          <- round(summary(lm_PriceAd0)$adj.r.squared, 3)
      results$AvgWeeklyUnitPrice_NoAd                 <- round(summary(lm_PriceAd0)$coefficients[1, 1], 2)
      results$AvgWeeklyUnitPrice_NoAd_Sig             <- round(summary(lm_PriceAd0)$coefficients[1, 4], 4)
      results$AvgWeeklyUnitPrice_Ad                   <- round(summary(lm_PriceAd0)$coefficients[2, 1], 2) + results$AvgWeeklyUnitPrice_NoAd
      results$AvgWeeklyUnitPrice_Ad_Sig               <- round(summary(lm_PriceAd0)$coefficients[2, 4], 4)      
            
      results$RSq_Ad                                  <- round(summary(lm_Ad)$r.squared, 3)
      results$AdjRSq_Ad                               <- round(summary(lm_Ad)$adj.r.squared, 3) 
      results$AvgDailyUnitSales_NoAd                  <- round(summary(lm_Ad)$coefficients[1, 1], 2)
      results$AvgDailyUnitSales_NoAd_Sig              <- round(summary(lm_Ad)$coefficients[1, 4], 4)
      results$AvgDailyUnitSales_Ad                    <- round(summary(lm_Ad)$coefficients[2, 1], 2)       + results$AvgDailyUnitSales_NoAd
      results$AvgDailyUnitSales_AdLiftNotPrAdj        <- round(summary(lm_Ad)$coefficients[2, 1], 2)       / results$AvgDailyUnitSales_NoAd
      results$AvgDailyUnitSales_Ad_Sig                <- round(summary(lm_Ad)$coefficients[2, 4], 4)
      
      results$RSq_PriceAd                             <- round(summary(lm_PriceAd)$r.squared, 3)
      results$AdjRSq_PriceAd                          <- round(summary(lm_PriceAd)$adj.r.squared, 3) 
      results$AvgDailyUnitSales_ZeroPrice_NoAd        <- round(summary(lm_PriceAd)$coefficients[1, 1], 2)
      results$AvgDailyUnitSales_ZeroPrice_NoAd_Sig    <- round(summary(lm_PriceAd)$coefficients[1, 4], 4)
      results$AvgDailyUnitSales_PriceElasAdAdj        <- round(summary(lm_PriceAd)$coefficients[2, 1], 2)
      results$AvgDailyUnitSales_PriceElasAdAdj_Sig    <- round(summary(lm_PriceAd)$coefficients[2, 4], 4)
      results$AvgDailyUnitSales_ZeroPrice_Ad          <- round(summary(lm_PriceAd)$coefficients[3, 1], 2) + results$AvgDailyUnitSales_ZeroPrice_NoAd
      results$AvgDailyUnitSales_ZeroPrice_AdLiftPrAdj <- round(summary(lm_PriceAd)$coefficients[3, 1], 2) / results$AvgDailyUnitSales_ZeroPrice_NoAd
      results$AvgDailyUnitSales_ZeroPrice_Ad_Sig      <- round(summary(lm_PriceAd)$coefficients[3, 4], 4)
      
      results$RSq_PriceAd2                            <- round(summary(lm_PriceAd2)$r.squared, 3)          # !!! Replicates results from lm_PriceAd !!!
      results$AdjRSq_PriceAd2                         <- round(summary(lm_PriceAd2)$adj.r.squared, 3) 
      results$AvgDailyUnitSales_AvgPrice_NoAd         <- round(summary(lm_PriceAd2)$coefficients[1, 1], 2)
      results$AvgDailyUnitSales_AvgPrice_NoAd_Sig     <- round(summary(lm_PriceAd2)$coefficients[1, 4], 4)
      results$AvgDailyUnitSales_PriceElasAdAdj        <- round(summary(lm_PriceAd2)$coefficients[2, 1], 2) # !!! Replicates results from lm_PriceAd !!!
      results$AvgDailyUnitSales_PriceElasAdAdj_Sig    <- round(summary(lm_PriceAd2)$coefficients[2, 4], 4)
      results$AvgDailyUnitSales_AvgPrice_Ad           <- round(summary(lm_PriceAd2)$coefficients[3, 1], 2) + results$AvgDailyUnitSales_AvgPrice_NoAd
      results$AvgDailyUnitSales_AvgPrice_AdLiftPrAdj  <- round(summary(lm_PriceAd2)$coefficients[3, 1], 2) / results$AvgDailyUnitSales_AvgPrice_NoAd
      results$AvgDailyUnitSales_AvgPrice_Ad_Sig       <- round(summary(lm_PriceAd2)$coefficients[3, 4], 4)
      
      # If product was NOT featured in ad at least once then models couldn't be built and coefficients are NA
      } else {
      
      results$PriceAdCorr                             <- NA
      results$PriceAdCorrAdj                          <- NA
      results$AvgWeeklyUnitPrice_NoAd                 <- NA
      results$AvgWeeklyUnitPrice_NoAd_Sig             <- NA
      results$AvgWeeklyUnitPrice_Ad                   <- NA
      results$AvgWeeklyUnitPrice_Ad_Sig               <- NA
                  
      results$RSq_Ad                                  <- NA
      results$AdjRSq_Ad                               <- NA
      results$AvgDailyUnitSales_NoAd                  <- NA
      results$AvgDailyUnitSales_NoAd_Sig              <- NA
      results$AvgDailyUnitSales_Ad                    <- NA
      results$AvgDailyUnitSales_AdLiftNotPrAdj        <- NA
      results$AvgDailyUnitSales_Ad_Sig                <- NA
            
      results$RSq_PriceAd                             <- NA
      results$AdjRSq_PriceAd                          <- NA
      results$AvgDailyUnitSales_ZeroPrice_NoAd        <- NA
      results$AvgDailyUnitSales_ZeroPrice_NoAd_Sig    <- NA
      results$AvgDailyUnitSales_PriceElasAdAdj        <- NA
      results$AvgDailyUnitSales_PriceElasAdAdj_Sig    <- NA
      results$AvgDailyUnitSales_ZeroPrice_Ad          <- NA
      results$AvgDailyUnitSales_ZeroPrice_AdLiftPrAdj <- NA
      results$AvgDailyUnitSales_ZeroPrice_Ad_Sig      <- NA
            
      results$RSq_PriceAd2                            <- NA
      results$AdjRSq_PriceAd2                         <- NA
      results$AvgDailyUnitSales_AvgPrice_NoAd         <- NA
      results$AvgDailyUnitSales_AvgPrice_NoAd_Sig     <- NA
      results$AvgDailyUnitSales_PriceElasAdAdj        <- NA
      results$AvgDailyUnitSales_PriceElasAdAdj_Sig    <- NA
      results$AvgDailyUnitSales_AvgPrice_Ad           <- NA
      results$AvgDailyUnitSales_AvgPrice_AdLiftPrAdj  <- NA
      results$AvgDailyUnitSales_AvgPrice_Ad_Sig       <- NA
            
      }
      
      ## Diagnose models
      #anova(lm_Base,
      #      lm_Ad,
      #      lm_Price,   lm_Price2,
      #      lm_PriceAd, lm_PriceAd2)
      
      ProductList[[i]] <- results
      
      ## Visualize models
      
      setwd("/media/radmin/ExternalHD/Projects/Sales Data Analyses/3 PD/2 Marketing Mix Models (Results)/Plots")

      plot_subtitle <- paste0(results$Product, " (UPC: ", results$UPC, ", Like Code: ", results$LikeCode, ") at Store ", SelectedStore,
                              " (", SelectedWeeks_TotalAdWeeks, " Ad Weeks: ", SelectedWeeks_Begin, " thru ", SelectedWeeks_End, ")"
                              #", \n Avg. Price $", round(results$AvgWeeklyUnitPrice, 2),
                              #" (ON Sale $",       round(results$AvgWeeklyUnitPrice_Ad, 2),
                              #" / NOT on Sale $",  round(results$AvgWeeklyUnitPrice_NoAd, 2), ")"
                              )
      plot_filename <- paste(results$LikeCode_UPC, results$UPC, results$Product, "at Store", SelectedStore, ".png")
      
      # Draw plot 1) regardless of ad feature
      plot1 <-
      ggplot(data = data2,
             aes(x = AvgWeeklyUnitPrice,
                 y = AvgDailyUnitSales)) +
            # Describe title and axes
            ggtitle(bquote(atop(bold(.("Weekly Movement, Price Points, Ad Features")),
                                atop(.(plot_subtitle)), ""))) +
            labs(x        = "Unit Price (in USD, per week)",    
                 y        = "Unit Sales (on avg. day per week)") +
            # Draw points colored (regardless of ad feature)
            geom_point(size = 5, alpha = .3) +
            # Draw regression line (regardless of ad feature)
            geom_smooth(    color = "black",    method = "lm", fullrange = T, size = .5, show.legend = F, se = F) +
            # Add average price & unit sales (regardless of ad-feature)
            geom_vline(xintercept = results$AvgWeeklyUnitPrice,      size = .5, alpha = .3, linetype = "dotted", color = "black") +
            geom_hline(yintercept = results$AvgDailyUnitSales,       size = .5, alpha = .3, linetype = "dashed", color = "black") +
            # Add axis tick marks according to data
            scale_y_continuous(breaks = seq(    round(min(data2$AvgDailyUnitSales,  na.rm = T), 0),   round(max(data2$AvgDailyUnitSales,  na.rm = T), 0),
                                            abs(round(min(data2$AvgDailyUnitSales,  na.rm = T), 0)-   round(max(data2$AvgDailyUnitSales,  na.rm = T), 0))/10),
                               limits = c(            min(data2$AvgDailyUnitSales,  na.rm = T),             max(data2$AvgDailyUnitSales,  na.rm = T))) +
            scale_x_continuous(breaks = seq(    round(min(data2$AvgWeeklyUnitPrice, na.rm = T), 0)-1, round(max(data2$AvgWeeklyUnitPrice, na.rm = T), 0)+1, 0.1),
                               limits = c(            min(data2$AvgWeeklyUnitPrice, na.rm = T),             max(data2$AvgWeeklyUnitPrice, na.rm = T)))
      
      # Draw plot 2) by ad feature
      # If was featured in ad at least once
      if (results$TotalAdFeat_abs > 0) { 
      
      plot2 <- plot1 +
            # Draw points colored by ad feature
            geom_point( aes(color = AdFeature), size = 5, alpha = .3) +
            # Position point-color legend on bottom
            theme(legend.position = "bottom") +
            scale_colour_manual(name   = "Ad Feature",
                                labels = c("NOT on Sale", "ON Sale"),
                                values = c("dodgerblue2", "firebrick3")) + 
            # Draw regression line (by ad feature)
            #geom_smooth(aes(group = AdFeature, color = AdFeature), method = "lm", fullrange = T, size = .5, show.legend = T, se = F, alpha = .1, linetype = "dotdash") +
            # Add average price & unit sales (by ad-feature)
            geom_vline(xintercept = results$AvgWeeklyUnitPrice_Ad,   size = .5, alpha = .5, linetype = "dotted", color = "firebrick3") +
            geom_hline(yintercept = results$AvgDailyUnitSales_Ad,    size = .5, alpha = .5, linetype = "dashed", color = "firebrick3") +
            geom_vline(xintercept = results$AvgWeeklyUnitPrice_NoAd, size = .5, alpha = .5, linetype = "dotted", color = "dodgerblue2") +
            geom_hline(yintercept = results$AvgDailyUnitSales_NoAd,  size = .5, alpha = .5, linetype = "dashed", color = "dodgerblue2")
      
      ggsave(filename = paste0(plot_filename), plot = plot2, width = 25, units = "cm")
      
      # Else draw 1st plot
      } else {
            
      ggsave(filename = paste0(plot_filename), plot = plot1, width = 25, units = "cm")
            
      }
      
      plot3 <- plot2 + stat_function(fun = (results$AvgDailyUnitSales_ZeroPrice + results$AvgDailyUnitSales_PriceElasNotAdAdj*AvgWeeklyUnitPrice)*(AvgWeeklyUnitPrice-AvgWeeklyUnitCost),
                                     geom = "line")
            
      
      # Remove models so that they don't skew the models for the enxt product in the loop
      rm(lm_Price2, lm_Price, lm_Price0, lm_Base, lm_Ad, lm_PriceAd, lm_PriceAd0, lm_PriceAd2)
}

results <- ldply(ProductList, data.frame)

data1 <- left_join(data1,
                   results, by = "UPC") %>% 
      mutate(AvgWeeklyUnitPriceChange = round(StdWeeklyUnitPriceDev/AvgWeeklyUnitPrice*100, 2),
             PriceElast               = round((Beta1/AvgDailyUnitSales)/     # Percent change in quantity demanded (dollar sales) divided by
                                              (1/    AvgWeeklyUnitPrice), 2) # Percent change in price
             ) %>% 
      filter(!is.na(RSq))

StoreList[[j]] <- data1_UPC_MostSold

} # Close store-loop

Results_MostSoldProducts <- ldply(StoreList, data.frame)

## 3b) Response in UNIT Sales by Price x Ad-Feature (ADJUSTED) of Top 80% Most Featured Products #############################

# Select products that regression runs over
          SelectedMostFeatProducts  <- data1_UPC_MostFeat$UPC[1:10]
       ResultList_MostFeatProducts  <- list()
length(ResultList_MostFeatProducts) <- length(SelectedMostFeatProducts)

for (i in 1:length(SelectedMostFeatProducts)) {
      
      # Run regression computationally per item
      
      # Loop through each UPC in each store
      data2 <- ungroup(data0) %>% 
            filter(UPC    ==   SelectedMostFeatProducts[1],
                   Store  ==   SelectedStore,
                   WeekNo %in% SelectedWeeks) %>%
            # Centering price by UPC, per store and in time frame (Make intercept more interpretable; otherwise, intercept would have been at 0 USD)
            mutate(AvgWeeklyUnitPrice_centered = scale(AvgWeeklyUnitPrice, center = T, scale = F))
      
      # Create table where results are going to be stored
      results <- as.data.frame(matrix(nrow = 1)) %>% 
            rename(UPC = V1) %>% 
            mutate(UPC = SelectedMostFeatProducts[i]
      
      
      
      
      
      
      results$RSq                   <- round(summary(lm_PriceAd)$r.squared,          3)
      results$RSqAdj                <- round(summary(lm_PriceAd)$adj.r.squared,      3)
      results$Beta0                 <- round(summary(lm_PriceAd)$coefficients[1, 1], 2)
      results$Sign0                 <- round(summary(lm_PriceAd)$coefficients[1, 4], 3)
      
      # If price does not change (std. dev. is 0), then no price elasticity can be estimated
      if(sd(data2$AvgWeeklyUnitPrice)[1] == 0) {
            
            results$Beta1 <- NA
            results$Sign1 <- NA
            
      } else {
            
            results$Beta1           <- round(summary(lm_PriceAd)$coefficients[2, 1], 2)
            results$Sign1           <- round(summary(lm_PriceAd)$coefficients[2, 4], 3)
            
      }
      
      results$Beta2                 <- round(summary(lm_PriceAd)$coefficients[3, 1], 2)
      results$Sign2                 <- round(summary(lm_PriceAd)$coefficients[3, 4], 3)
      
      ResultList_MostFeatProducts[[i]] <- results
}

results <- ldply(ResultList_MostFeatProducts, data.frame)

data1_UPC_MostSold      <- left_join(data1_UPC_MostSold,
                                     results, by = "UPC") %>% 
      mutate(AvgWeeklyUnitPriceChange = round(StdWeeklyUnitPriceDev/AvgWeeklyUnitPrice*100, 2),
             PriceElast               = round((Beta1/AvgDailyUnitSales)/     # Percent change in quantity demanded (dollar sales) divided by
                                                    (1/    AvgWeeklyUnitPrice), 2) # Percent change in price
      ) %>% 
      filter(!is.na(RSq))

StoreList[[j]] <- data1_UPC_MostSold

} # Close store-loop

Results_MostSoldProducts <- ldply(StoreList, data.frame)

# Select products that regression runs over
SelectedMostFeatProducts      <- data1_UPC_MostFeat$UPC[1:10]
# Save Results in list that will eventually be collapsed into dataframe
       ResultList_MostFeat          <- list()
length(ResultList_MostFeat)         <- length(SelectedMostFeatProducts)

for (i in 1:length(SelectedMostFeatProducts)) {
            
      # Loop through each UPC in each store
            data2a <- ungroup(data0_LC) %>%
                  filter(LikeCode_UPC ==   SelectedMostFeatProducts[i],
                         Store        ==   SelectedStore,
                         WeekNo       %in% SelectedWeeks) %>%
                  # Make intercept more interpretable by centering AvgPrice; otherwise, intercept would have been at 0 USD
                  mutate(AvgWeeklyUnitPrice_centered = scale(AvgWeeklyUnitPrice, center = T, scale = F))
            
            # Summarize Results
            data3_byAdFeat    <- group_by(data2a,
                                           Store, LikeCode_UPC, AdFeature) %>% 
                  summarize(WeekCount           = n(),
                            AvgPrice            = round(mean(AvgWeeklyUnitPrice,      na.rm = T), 2),
                            StDPrice            = round(sd(AvgWeeklyUnitPrice,        na.rm = T), 2),
                            #TotalWeeklyDollarSales   = round(mean(TotalWeeklyDollarSales,   na.rm = T), 0),
                            #TotalWeeklyUnitSales     = round(mean(TotalWeeklyUnitSales,     na.rm = T), 0),
                            AvgDailyDollarSales = round(mean(AvgDailyDollarSales, na.rm = T), 0),
                            AvgDailyUnitSales   = round(mean(AvgDailyUnitSales,   na.rm = T), 0))  %>% 
                  left_join(Products_LC, by = "LikeCode_UPC")
            
            data3             <- group_by(data2a,
                                           Store, LikeCode_UPC) %>%
                  summarize(WeekCount           = n(),
                            AvgPrice            = round(mean(AvgWeeklyUnitPrice,      na.rm = T), 2),
                            StDPrice            = round(sd(AvgWeeklyUnitPrice,        na.rm = T), 2),
                            ChgPrice            = round(StDPrice/AvgPrice*100               , 2),
                            #TotalWeeklyDollarSales   = round(mean(TotalWeeklyDollarSales,   na.rm = T), 0),
                            #TotalWeeklyUnitSales     = round(mean(TotalWeeklyUnitSales,     na.rm = T), 0),
                            AvgDailyDollarSales = round(mean(AvgDailyDollarSales, na.rm = T), 0),
                            AvgDailyUnitSales   = round(mean(AvgDailyUnitSales,   na.rm = T), 0)) %>% 
                  left_join(Products_LC, by = "LikeCode_UPC")
            
            lm_PriceAd <- lm(data = data2a, AvgDailyDollarSales ~ AvgWeeklyUnitPrice_centered + AdFeature)
            
            data3$RSq        <- round(summary(lm_PriceAd)$r.squared,          3)
            data3$Beta0      <- round(summary(lm_PriceAd)$coefficients[1, 1], 0)
            data3$Sign0      <- round(summary(lm_PriceAd)$coefficients[1, 4], 3)
            data3$Beta1      <- round(summary(lm_PriceAd)$coefficients[2, 1], 2)
            data3$Sign1      <- round(summary(lm_PriceAd)$coefficients[2, 4], 3)
            data3$Beta2      <- round(summary(lm_PriceAd)$coefficients[3, 1], 2)
            data3$Sign2      <- round(summary(lm_PriceAd)$coefficients[3, 4], 3)
            data3$PriceElast <- round((data3$Beta1/data3$Beta0)/ # Percent change in quantity demanded (dollar sales) divided by
                                       (1/    data3$AvgPrice),     # Percent change in price
                                       2)
            data3$AdImpact   <- round((data3$Beta2/data3$Beta0)*100, 2)        # Lift in sales when featured in ad divided by average sales without the ad
      
      ResultList_MostFeat[[i]] <- data3 
      
}

Results_MostFeat <- ldply(ResultList_MostFeat, data.frame)

#resid(fit)
#summary(fit)$sigma # Finding residual variance estimates.
#sqrt(sum(resid(fit)^2) / (n - 2)) # Directly calculating from the residuals

## 4) Plot regression model ############################################################################################





# Residual plot

## 5) Export results ####################################################################

setwd("/media/radmin/ExternalHD/Projects/Sales Data Analyses/3 PD/2 Marketing Mix Models (Results)")
write.csv(Results_MostSoldProducts, paste0("Most Sold Products_", SelectedWeeks_Begin, " - ", SelectedWeeks_End, " (", SelectedWeeks_TotalAdWeeks,  " Ad Weeks).csv"), row.names = F)

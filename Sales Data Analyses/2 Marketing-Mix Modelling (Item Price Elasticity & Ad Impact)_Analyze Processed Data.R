## 0) Settings #########################################################################################################

# Load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(xlsx)
library(tidyr)
library(grid)

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
                       TotalWeeklyCOGS        = as.numeric(  Costs)) %>% 
      filter(!grepl("OPEN RING", Product))
  
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
data0$LikeCode_UPC[!is.na(data0$LikeCode)] <-     data0$LikeCode2[
                       !is.na(data0$LikeCode)];   data0$LikeCode2     <- NULL
data0$LikeCode_UPC[ is.na(data0$LikeCode)] <-     data0$UPC2[
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
                            LikeCode, UPC, LikeCode_UPC, Product) %>% unique() %>%
      filter(which(duplicated(Products_UPC$UPC)))
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
SelectedWeeks    <- c(1:75)
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
SelectedStores    <- as.character(c(1:12))
#SelectedStores    <- as.character(c("3", "5", "6"))

## 2b) Aggregate Data to Product Level (UPC) per Store #################################################################

# Per store (loop through selected stores) for given weeks and depts.

       StoreList  <- list()
length(StoreList) <- length(SelectedStores)

for (j in 1:length(SelectedStores)) {
      
SelectedStore     <- SelectedStores[3]

# Filter according to selected weeks, stores, depts.
data1 <- filter(data0,
                    (Dept    %in% SelectedDept)    &
                    (SubDept %in% SelectedSDept)   &
                    (Store   ==   SelectedStore)   &
                    (WeekNo  %in% SelectedWeeks)
                    )  %>% 
      # Remove sub-dept. column b/c some products are grouped in more than one sub-dept. although it's the same product
      select(-SubDept) %>% 
      # Aggregating data to UPC level and summarize all other variables
      group_by(Store,
               Dept,
               #SubDept,
               UPC) %>% 
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

## 3) Regression Analysis ##############################################################################################

## 3a) Prepare input data ##############################################################################################
# Run regression per item
SelectedProducts        <- data1$UPC#[1:50]
        ProductList     <- list()
 length(ProductList)    <- length(SelectedProducts)

for (i in 1:length(SelectedProducts)) {
SelectedProduct <- SelectedProducts[i] # SelectedProduct <- data1$UPC[data1$UPC==""]
 
      data2 <- ungroup(data0) %>% select(-SubDept) %>% 
            filter(UPC    ==   SelectedProduct,
                   Store  ==   SelectedStore,
                   WeekNo %in% SelectedWeeks) %>%
            # Centering price by UPC, per store and in time frame (Make intercept more interpretable; otherwise, intercept would have been at 0 USD)
            mutate(AvgWeeklyUnitPrice_centered = scale(AvgWeeklyUnitPrice, center = T, scale = F))
      
      ## Create results table
      results <- as.data.frame(matrix(nrow = 1)) %>% 
            rename(UPC = V1) %>% 
            mutate(#Store = SelectedStore,
                   UPC   = SelectedProduct
                   ) %>% 
            #left_join(Products_UPC, by = "UPC") %>% 
            left_join(select(data1,       UPC, AvgWeeklyUnitCost),
                                    by = "UPC")
      ## Calculate std. deviation for price variable / How much does price fluctuate (filter criteria for models)
      results$StdWeeklyUnitPriceDev             <- round(sd(data2$AvgWeeklyUnitPrice), 2)
      results$StdWeeklyUnitPriceDev_Ad          <- round(sd(data2$AvgWeeklyUnitPrice[data2$AdFeature == "SALE"]),     2)
      results$StdWeeklyUnitPriceDev_NoAd        <- round(sd(data2$AvgWeeklyUnitPrice[data2$AdFeature == "NON-SALE"]), 2)
      # If std. dev. cannot be computed (NA b/c just one price exist) set to 0 to allow filtering when modelling
      results$StdWeeklyUnitPriceDev_Ad[
is.na(results$StdWeeklyUnitPriceDev_Ad)]        <- 0
      results$StdWeeklyUnitPriceDev_NoAd[
is.na(results$StdWeeklyUnitPriceDev_NoAd)]      <- 0
      
      ## Calculate total ad features (filter criteria for models)
      results$TotalAdFeat_abs                   <- sum(as.numeric(as.character(factor(data2$AdFeature,
                                                                                      levels = c("NON-SALE", "SALE"),
                                                                                      labels = c("0",        "1")))), na.rm = T)
      results$TotalAdFeat_rel                   <- round(results$TotalAdFeat_abs/SelectedWeeks_TotalAdWeeks, 4)
      
      ## 3b) Build models ##############################################################################################
      
      lm0_Base          <- lm(data = data2, AvgDailyUnitSales  ~ 1) # !!! lm1a_Price2 yields the same results !!!
      
      lm0_Price         <- lm(data = data2,                      AvgWeeklyUnitPrice ~ 1)
      lm1a_Price        <- lm(data = data2, AvgDailyUnitSales  ~ AvgWeeklyUnitPrice)
      lm1a_Price2       <- lm(data = data2, AvgDailyUnitSales  ~ AvgWeeklyUnitPrice_centered)
      
      # If product was featured in ad at least once then build models that include ad-feature variable
      if (results$TotalAdFeat_abs > 0) { 
      
      lm0_PriceAd       <- lm(data = data2,                      AvgWeeklyUnitPrice          ~ AdFeature)
      lm1b_Ad           <- lm(data = data2, AvgDailyUnitSales  ~                               AdFeature)
      
      lm2_PriceAd       <- lm(data = data2, AvgDailyUnitSales  ~ AvgWeeklyUnitPrice          + AdFeature)
      lm2_PriceAd2      <- lm(data = data2, AvgDailyUnitSales  ~ AvgWeeklyUnitPrice_centered + AdFeature)
      
      }
      
      ## 3c) Diagnose models ###########################################################################################
      
      ## Test nested models
      # P-values test whether new variables are necessary or not
      
      if (results$TotalAdFeat_abs > 0) { 
      
      ModelCompPriceAd  <- anova(lm0_Base,
                                 lm1a_Price,
                                 lm2_PriceAd)
      
      ModelCompAdPrice  <- anova(lm0_Base,
                                 lm1b_Ad,
                                 lm2_PriceAd)
      
      results$ModelCompPrice        <- round(ModelCompPriceAd[2,6], 3)
      results$ModelCompPriceAd      <- round(ModelCompPriceAd[3,6], 3)
      results$ModelCompAd           <- round(ModelCompAdPrice[2,6], 3)
      results$ModelCompAdPrice      <- round(ModelCompAdPrice[3,6], 3)
      
      } else {
            
      results$ModelCompPrice        <- NA
      results$ModelCompPriceAd      <- NA
      results$ModelCompAd           <- NA
      results$ModelCompAdPrice      <- NA
            
      }
      
      
      
      ## Plot residuals
      # Look for systematic patterns or not constant variance around regression line (Heteroskedasticity); errors are supposed to be Gaussian with constant error)
      
      #ggplot(data = data2,
      #       aes(x = AvgWeeklyUnitPrice,
      #           y = resid(lm1a_Price))) +
      #      geom_point() +
      #      geom_hline(yintercept = 0)
      
      #plot(lm1a_Price)
      
      ## Test normality of errors
      #shapiro.test(rstandard(lm1a_Price))$p.value
      
      ## 3d) Save regression coefficients, significance values, model fit ##############################################
      
      results$AvgDailyUnitSales                                   <- round(summary(lm0_Base)$coefficients[1, 1],  2)    # !!! lm1a_Price2 yields the same results !!!
      results$AvgDailyUnitSales_Sig                               <- round(summary(lm0_Base)$coefficients[1, 4],  4)
      
      results$AvgWeeklyUnitPrice                                  <- round(summary(lm0_Price)$coefficients[1, 1], 2)
      results$AvgWeeklyUnitPrice_Sig                              <- round(summary(lm0_Price)$coefficients[1, 4], 4)
      results$AvgWeeklyUnitPriceChange                            <- results$StdWeeklyUnitPriceDev/results$AvgWeeklyUnitPrice
      
      # If product has changed in price, models were built sensefully and coefficients can be saved
      if (results$StdWeeklyUnitPriceDev > 0) { 
      
      results$lm1a_RSq_Price                                      <- round(summary(lm1a_Price)$r.squared,          3)
      results$lm1a_AdjRSq_Price                                   <- round(summary(lm1a_Price)$adj.r.squared,      3) 
      results$lm1a_a_AvgDailyUnitSales_ZeroPrice                  <- round(summary(lm1a_Price)$coefficients[1, 1], 2)
      #results$lm1a_a_AvgDailyUnitSales_ZeroPrice_CIlow            <- round((summary(lm1a_Price)$coefficients[1, 1]+c(-1, 1)*qt(.975, df = lm1a_Price$df)*summary(lm1a_Price)$coefficients[1, 2]), 2)[1]
      #results$lm1a_a_AvgDailyUnitSales_ZeroPrice_CIhigh           <- round((summary(lm1a_Price)$coefficients[1, 1]+c(-1, 1)*qt(.975, df = lm1a_Price$df)*summary(lm1a_Price)$coefficients[1, 2]), 2)[2]
      results$lm1a_a_AvgDailyUnitSales_ZeroPrice_Sig              <- round(summary(lm1a_Price)$coefficients[1, 4], 4)
      results$lm1a_b_AvgDailyUnitSales_PriceElasNotAdAdj          <- round(summary(lm1a_Price)$coefficients[2, 1], 2)
      #results$lm1a_b_AvgDailyUnitSales_PriceElasNotAdAdj_CIlow    <- round(summary(lm1a_Price)$coefficients[2, 1]+c(-1, 1)*qt(.975, df = lm1a_Price$df)*summary(lm1a_Price)$coefficients[2, 2], 2)[1]
      #results$lm1a_b_AvgDailyUnitSales_PriceElasNotAdAdj_CIhigh   <- round(summary(lm1a_Price)$coefficients[2, 1]+c(-1, 1)*qt(.975, df = lm1a_Price$df)*summary(lm1a_Price)$coefficients[2, 2], 2)[2]
      results$lm1a_b_AvgDailyUnitSales_PriceElasNotAdAdj_Sig      <- round(summary(lm1a_Price)$coefficients[2, 4], 4)
      
      results$lm1a_RSq_Price2                                     <- round(summary(lm1a_Price2)$r.squared,          3)  # !!! Replicates results from lm1a_Price !!!
      results$lm1a_AdjRSq_Price2                                  <- round(summary(lm1a_Price2)$adj.r.squared,      3)  # !!! Replicates results from lm1a_Price !!!
      results$lm1a_a_AvgDailyUnitSales_AvgPrice                   <- round(summary(lm1a_Price2)$coefficients[1, 1], 2)  # !!! Replicates results from lm0_Base !!!
      results$lm1a_a_AvgDailyUnitSales_AvgPrice_Sig               <- round(summary(lm1a_Price2)$coefficients[1, 4], 4)  # !!! Replicates results from lm0_Base !!!
      results$lm1a_b_AvgDailyUnitSales_PriceElas2NotAdAdj         <- round(summary(lm1a_Price2)$coefficients[2, 1], 2)*(results$AvgWeeklyUnitPrice/results$lm1a_a_AvgDailyUnitSales_AvgPrice)
      results$lm1a_b_AvgDailyUnitSales_PriceElas2NotAdAdj_Sig     <- round(summary(lm1a_Price2)$coefficients[2, 4], 4)  # !!! Replicates results from lm1a_Price !!!
      
      # If product has NOT changed in price, models were built NOT sensefully and coefficients should be NA
      } else {
            
      results$lm1a_RSq_Price                                      <- NA
      results$lm1a_AdjRSq_Price                                   <- NA
      results$lm1a_a_AvgDailyUnitSales_ZeroPrice                  <- NA
      results$lm1a_a_AvgDailyUnitSales_ZeroPrice_Sig              <- NA
      results$lm1a_b_AvgDailyUnitSales_PriceElasNotAdAdj          <- NA
      results$lm1a_b_AvgDailyUnitSales_PriceElasNotAdAdj_Sig      <- NA
            
      results$lm1a_RSq_Price2                                     <- NA
      results$lm1a_AdjRSq_Price2                                  <- NA
      results$lm1a_a_AvgDailyUnitSales_AvgPrice                   <- NA
      results$lm1a_a_AvgDailyUnitSales_AvgPrice_Sig               <- NA
      results$lm1a_b_AvgDailyUnitSales_PriceElas2NotAdAdj         <- NA
      results$lm1a_b_AvgDailyUnitSales_PriceElas2NotAdAdj_Sig     <- NA
                  
      }
      
      # If product was featured in ad at least once then models were built and coefficients can be saved
      if (results$TotalAdFeat_abs > 0) { 
      
      results$lm0_RSq_PriceAdCorr                                 <- round(summary(lm0_PriceAd)$r.squared,          3)
      results$lm0_AdjRSq_PriceAdCorrAdj                           <- round(summary(lm0_PriceAd)$adj.r.squared,      3)
      results$lm0_a_AvgWeeklyUnitPrice_NoAd                       <- round(summary(lm0_PriceAd)$coefficients[1, 1], 2)
      results$lm0_a_AvgWeeklyUnitPrice_NoAd_Sig                   <- round(summary(lm0_PriceAd)$coefficients[1, 4], 4)
      results$AvgWeeklyUnitPriceChange_NoAd                       <- results$StdWeeklyUnitPriceDev_NoAd/results$lm0_a_AvgWeeklyUnitPrice_NoAd
      results$lm0_b_AvgWeeklyUnitPrice_Ad                         <- round(summary(lm0_PriceAd)$coefficients[2, 1], 2)  + results$lm0_a_AvgWeeklyUnitPrice_NoAd
      results$lm0_b_AvgWeeklyUnitPrice_Ad_Sig                     <- round(summary(lm0_PriceAd)$coefficients[2, 4], 4)
      results$AvgWeeklyUnitPriceChange_Ad                         <- results$StdWeeklyUnitPriceDev_Ad/results$lm0_b_AvgWeeklyUnitPrice_Ad
            
      results$lm1b_RSq_Ad                                         <- round(summary(lm1b_Ad)$r.squared,              3)
      results$lm1b_AdjRSq_Ad                                      <- round(summary(lm1b_Ad)$adj.r.squared,          3) 
      results$lm1b_a_AvgDailyUnitSales_NoAdNotPrAdj               <- round(summary(lm1b_Ad)$coefficients[1, 1],     2)
      results$lm1b_a_AvgDailyUnitSales_NoAdNotPrAdj_Sig           <- round(summary(lm1b_Ad)$coefficients[1, 4],     4)
      results$lm1b_b_AvgDailyUnitSales_AdNotPrAdj                 <- round(summary(lm1b_Ad)$coefficients[2, 1],     2)  + results$lm1b_a_AvgDailyUnitSales_NoAdNotPrAdj
      results$lm1b_b_AvgDailyUnitSales_AdLiftNotPrAdj             <- round(summary(lm1b_Ad)$coefficients[2, 1],     2)  / results$lm1b_a_AvgDailyUnitSales_NoAdNotPrAdj
      results$lm1b_b_AvgDailyUnitSales_AdNotPrAdj_Sig             <- round(summary(lm1b_Ad)$coefficients[2, 4],     4)
      
      # If product has NOT changed in price while on/off the ad, models were built NOT sensefully and coefficients should be NA
      if (results$StdWeeklyUnitPriceDev_Ad > 0) {
      
      results$lm2_RSq_PriceAd                                     <- round(summary(lm2_PriceAd)$r.squared,           3)
      results$lm2_AdjRSq_PriceAd                                  <- round(summary(lm2_PriceAd)$adj.r.squared,       3) 
      results$lm2_a_AvgDailyUnitSales_ZeroPrice_NoAd              <- round(summary(lm2_PriceAd)$coefficients[1, 1],  2)
      results$lm2_a_AvgDailyUnitSales_ZeroPrice_NoAd_Sig          <- round(summary(lm2_PriceAd)$coefficients[1, 4],  4)
      results$lm2_b_AvgDailyUnitSales_PriceElasAdAdj              <- round(summary(lm2_PriceAd)$coefficients[2, 1],  2)
      results$lm2_b_AvgDailyUnitSales_PriceElasAdAdj_Sig          <- round(summary(lm2_PriceAd)$coefficients[2, 4],  4)
      results$lm2_c_AvgDailyUnitSales_ZeroPrice_Ad                <- round(summary(lm2_PriceAd)$coefficients[3, 1],  2)  + results$lm2_a_AvgDailyUnitSales_ZeroPrice_NoAd
      results$lm2_c_AvgDailyUnitSales_ZeroPrice_AdLiftPrAdj       <- round(summary(lm2_PriceAd)$coefficients[3, 1],  2)  / results$lm2_a_AvgDailyUnitSales_ZeroPrice_NoAd
      results$lm2_c_AvgDailyUnitSales_ZeroPrice_Ad_Sig            <- round(summary(lm2_PriceAd)$coefficients[3, 4],  4)
      
      results$lm2_RSq_PriceAd2                                    <- round(summary(lm2_PriceAd2)$r.squared,          3) # !!! Replicates results from lm2_PriceAd !!!
      results$lm2_AdjRSq_PriceAd2                                 <- round(summary(lm2_PriceAd2)$adj.r.squared,      3) 
      results$lm2_a_AvgDailyUnitSales_AvgPrice_NoAd               <- round(summary(lm2_PriceAd2)$coefficients[1, 1], 2)
      results$lm2_a_AvgDailyUnitSales_AvgPrice_NoAd_Sig           <- round(summary(lm2_PriceAd2)$coefficients[1, 4], 4)
      results$lm2_b_AvgDailyUnitSales_PriceElas2AdAdj             <- round(summary(lm2_PriceAd2)$coefficients[2, 1], 2) * (results$lm0_a_AvgWeeklyUnitPrice_NoAd/results$lm2_a_AvgDailyUnitSales_AvgPrice_NoAd)
      results$lm2_b_AvgDailyUnitSales_PriceElas2AdAdj_Sig         <- round(summary(lm2_PriceAd2)$coefficients[2, 4], 4)
      results$lm2_c_AvgDailyUnitSales_AvgPrice_Ad                 <- round(summary(lm2_PriceAd2)$coefficients[3, 1], 2) + results$lm2_a_AvgDailyUnitSales_AvgPrice_NoAd
      results$lm2_c_AvgDailyUnitSales_AvgPrice_AdLiftPrAdj        <- round(summary(lm2_PriceAd2)$coefficients[3, 1], 2) / results$lm2_a_AvgDailyUnitSales_AvgPrice_NoAd
      results$lm2_c_AvgDailyUnitSales_AvgPrice_Ad_Sig             <- round(summary(lm2_PriceAd2)$coefficients[3, 4], 4)
      
      } else {
            
            results$lm2_RSq_PriceAd                               <- round(summary(lm2_PriceAd)$r.squared,           3)
            results$lm2_AdjRSq_PriceAd                            <- round(summary(lm2_PriceAd)$adj.r.squared,       3) 
            results$lm2_a_AvgDailyUnitSales_ZeroPrice_NoAd        <- round(summary(lm2_PriceAd)$coefficients[1, 1],  2)
            results$lm2_a_AvgDailyUnitSales_ZeroPrice_NoAd_Sig    <- round(summary(lm2_PriceAd)$coefficients[1, 4],  4)
            results$lm2_b_AvgDailyUnitSales_PriceElasAdAdj        <- round(summary(lm2_PriceAd)$coefficients[2, 1],  2)
            results$lm2_b_AvgDailyUnitSales_PriceElasAdAdj_Sig    <- round(summary(lm2_PriceAd)$coefficients[2, 4],  4)
            results$lm2_c_AvgDailyUnitSales_ZeroPrice_Ad          <- NA
            results$lm2_c_AvgDailyUnitSales_ZeroPrice_AdLiftPrAdj <- NA
            results$lm2_c_AvgDailyUnitSales_ZeroPrice_Ad_Sig      <- NA
            
            results$lm2_RSq_PriceAd2                              <- round(summary(lm2_PriceAd2)$r.squared,          3) # !!! Replicates results from lm2_PriceAd !!!
            results$lm2_AdjRSq_PriceAd2                           <- round(summary(lm2_PriceAd2)$adj.r.squared,      3) 
            results$lm2_a_AvgDailyUnitSales_AvgPrice_NoAd         <- round(summary(lm2_PriceAd2)$coefficients[1, 1], 2)
            results$lm2_a_AvgDailyUnitSales_AvgPrice_NoAd_Sig     <- round(summary(lm2_PriceAd2)$coefficients[1, 4], 4)
            results$lm2_b_AvgDailyUnitSales_PriceElas2AdAdj       <- round(summary(lm2_PriceAd2)$coefficients[2, 1], 2) * (results$lm0_a_AvgWeeklyUnitPrice_NoAd/results$lm2_a_AvgDailyUnitSales_AvgPrice_NoAd)
            results$lm2_b_AvgDailyUnitSales_PriceElas2AdAdj_Sig   <- round(summary(lm2_PriceAd2)$coefficients[2, 4], 4)
            results$lm2_c_AvgDailyUnitSales_AvgPrice_Ad           <- NA
            results$lm2_c_AvgDailyUnitSales_AvgPrice_AdLiftPrAdj  <- NA
            results$lm2_c_AvgDailyUnitSales_AvgPrice_Ad_Sig       <- NA
      }
      
      # If product was NOT featured in ad at least once then models couldn't be built and coefficients are NA
      } else {
      
      results$lm0_RSq_PriceAdCorr                                 <- NA
      results$lm0_AdjRSq_PriceAdCorrAdj                           <- NA
      results$lm0_a_AvgWeeklyUnitPrice_NoAd                       <- NA
      results$lm0_a_AvgWeeklyUnitPrice_NoAd_Sig                   <- NA
      results$lm0_b_AvgWeeklyUnitPrice_Ad                         <- NA
      results$lm0_b_AvgWeeklyUnitPrice_Ad_Sig                     <- NA
            
      results$lm1b_RSq_Ad                                         <- NA
      results$lm1b_AdjRSq_Ad                                      <- NA
      results$lm1b_a_AvgDailyUnitSales_NoAdNotPrAdj               <- NA
      results$lm1b_a_AvgDailyUnitSales_NoAdNotPrAdj_Sig           <- NA
      results$lm1b_b_AvgDailyUnitSales_AdNotPrAdj                 <- NA
      results$lm1b_b_AvgDailyUnitSales_AdLiftNotPrAdj             <- NA
      results$lm1b_b_AvgDailyUnitSales_AdNotPrAdj_Sig             <- NA
            
      results$lm2_RSq_PriceAd                                     <- NA
      results$lm2_AdjRSq_PriceAd                                  <- NA
      results$lm2_a_AvgDailyUnitSales_ZeroPrice_NoAd              <- NA
      results$lm2_a_AvgDailyUnitSales_ZeroPrice_NoAd_Sig          <- NA
      results$lm2_b_AvgDailyUnitSales_PriceElasAdAdj              <- NA
      results$lm2_b_AvgDailyUnitSales_PriceElasAdAdj_Sig          <- NA
      results$lm2_c_AvgDailyUnitSales_ZeroPrice_Ad                <- NA
      results$lm2_c_AvgDailyUnitSales_ZeroPrice_AdLiftPrAdj       <- NA
      results$lm2_c_AvgDailyUnitSales_ZeroPrice_Ad_Sig            <- NA
            
      results$lm2_RSq_PriceAd2                                    <- NA
      results$lm2_AdjRSq_PriceAd2                                 <- NA
      results$lm2_a_AvgDailyUnitSales_AvgPrice_NoAd               <- NA
      results$lm2_a_AvgDailyUnitSales_AvgPrice_NoAd_Sig           <- NA
      results$lm2_b_AvgDailyUnitSales_PriceElas2AdAdj             <- NA
      results$lm2_b_AvgDailyUnitSales_PriceElas2AdAdj_Sig         <- NA
      results$lm2_c_AvgDailyUnitSales_AvgPrice_Ad                 <- NA
      results$lm2_c_AvgDailyUnitSales_AvgPrice_AdLiftPrAdj        <- NA
      results$lm2_c_AvgDailyUnitSales_AvgPrice_Ad_Sig             <- NA
      
      }
      
      ProductList[[i]] <- results
      
      ## 3e) Optimize Prices (Maximize Profits) by Applying Price Models for Movement Estimation #######################
      
      ## Model 1: Price not Adjusted for Ad
      
      # Estimate daily profits based on avg. weekly unit price
      ProfitFunction_lm1a_PriceNotAdAdj   <- function(AvgWeeklyUnitPrice) {
            EstimatedDailyUnitSales       <- predict(lm1a_Price, newdata = data.frame(AvgWeeklyUnitPrice = AvgWeeklyUnitPrice))
            EstimatedDailyUnitSales*
                  (AvgWeeklyUnitPrice-results$AvgWeeklyUnitCost)
            }
      
      # Maximize estimated daily profits...
      Optimum_lm1a_PriceNotAdAdj <- optimize(ProfitFunction_lm1a_PriceNotAdAdj, maximum = T,
                                             lower = results$AvgWeeklyUnitCost,
                                             upper = max(data2$AvgWeeklyUnitPrice)
                                             )
      
      # Find optimal price & maximum profit at optimal price
      results$OptPrice_lm1a_PriceNotAdAdj      <- Optimum_lm1a_PriceNotAdAdj$maximum
      results$MaxProfit_lm1a_PriceNotAdAdj     <- Optimum_lm1a_PriceNotAdAdj$objective
      
      ## Model 2: Price Adjusted for Ad
      
      # If at least one ad feature was present
      if (results$TotalAdFeat_abs > 0) {
      
      # Estimate daily profits based on avg. weekly unit price
      ProfitFunction_lm2_PriceAdAdj_NoAd     <- function(AvgWeeklyUnitPrice) {
            EstimatedDailyUnitSales <- predict(lm2_PriceAd, newdata = data.frame(AvgWeeklyUnitPrice = AvgWeeklyUnitPrice, AdFeature = "NON-SALE"))
            EstimatedDailyUnitSales*
                  (AvgWeeklyUnitPrice-results$AvgWeeklyUnitCost)
            }
      
      # Maximize estimated daily profits...
      Optimum_lm2_PriceAdAdj_NoAd <- optimize(ProfitFunction_lm2_PriceAdAdj_NoAd, maximum = T,
                                              lower = results$AvgWeeklyUnitCost,
                                              upper = max(data2$AvgWeeklyUnitPrice)
      )
      
      # Find optimal price & maximum profit at optimal price
      results$OptPrice_lm2_PriceAdAdj_NoAd      <- Optimum_lm2_PriceAdAdj_NoAd$maximum
      results$MaxProfit_lm2_PriceAdAdj_NoAd     <- Optimum_lm2_PriceAdAdj_NoAd$objective
      
      } else { # If no ad feature was present, ad-adjusted profit-maximizing optimal price could not be determined
      
      results$OptPrice_lm2_PriceAdAdj_NoAd      <- NA
      results$MaxProfit_lm2_PriceAdAdj_NoAd     <- NA
            
      }
      
      ## 3f) Visualize Price Optimization & Profit Maximization ########################################################
      
      #plot_opt <- ggplot(data.frame(AvgWeeklyUnitPrice = c(results$AvgWeeklyUnitCost,
      #                                                  results$AvgWeeklyUnitPrice+2*results$StdWeeklyUnitPriceDev)), 
      #                aes(AvgWeeklyUnitPrice)) +
      #      stat_function(fun = ProfitFunction_lm1a_PriceNotAdAdj,  geom = "line", color = "springgreen1") +
      #      stat_function(fun = ProfitFunction_lm2_PriceAdAdj_NoAd, geom = "line", color = "springgreen4") +
      #      labs(x        = "Unit Price (in USD, per week)",    
      #           y        = "Estimated Unit Profit (in USD, on avg. day per week)") +
      #      scale_x_continuous(breaks = seq(    round(min(data2$AvgWeeklyUnitPrice, na.rm = T), 0)-1, round(max(data2$AvgWeeklyUnitPrice, na.rm = T), 0)+1, 0.1),
      #                         limits = c(            min(data2$AvgWeeklyUnitPrice, na.rm = T),             max(data2$AvgWeeklyUnitPrice, na.rm = T)))
      #      # Add optimal prices & maximezed profits
      #      geom_vline(xintercept = results$OptPrice_lma1a_PriceNotAdAdj,  size = .5, alpha = 1, linetype = "dotted", color = "springgreen1") +
      #      geom_vline(xintercept = results$OptPrice_lm2_PriceAdAdj_NoAd,  size = .5, alpha = 1, linetype = "dotted", color = "springgreen4")  +
      #      geom_hline(yintercept = results$MaxProfit_lma1a_PriceNotAdAdj, size = .5, alpha = 1, linetype = "dashed", color = "springgreen1") + 
      #      geom_hline(yintercept = results$MaxProfit_lm2_PriceAdAdj_NoAd, size = .5, alpha = 1, linetype = "dashed", color = "springgreen4")
      
      ## 3g) Visualize models ##########################################################################################
      
      #setwd("/media/radmin/ExternalHD/Projects/Sales Data Analyses/3 PD/2 Marketing Mix Models (Results)/Plots")

      #plot_subtitle <- paste0(results$Product, " (UPC: ", results$UPC, ", Like Code: ", results$LikeCode, ") at Store ", SelectedStore,
      #                        " (", SelectedWeeks_TotalAdWeeks, " Ad Weeks: ", SelectedWeeks_Begin, " thru ", SelectedWeeks_End, ")"
      #                        )
      
      #plot_filename <- paste(results$LikeCode_UPC, results$UPC, results$Product, "at Store", SelectedStore, ".png")
      
      # Draw plot 1) regardless of ad feature
      #plot_reg1 <-
      #ggplot(data = data2,
      #       aes(x = AvgWeeklyUnitPrice,
      #           y = AvgDailyUnitSales)) +
      #      # Describe title and axes
      #      ggtitle(bquote(atop(bold(.("Optimal, Profit-maximizing Price Based on Movement Estimation")),
      #                          atop(.(plot_subtitle)), ""))) +
      #      labs(x        = "Unit Price (in USD, per week)",    
      #           y        = "Unit Sales (on avg. day per week)") +
      #      # Draw points colored (regardless of ad feature)
      #      geom_point(size = 5, alpha = .3) +
      #      # Draw regression line (regardless of ad feature)
      #      geom_smooth(    color = "black",    method = "lm", fullrange = T, size = .5, show.legend = F, se = F) +
      #      # Add average price & unit sales (regardless of ad-feature)
      #      geom_vline(xintercept = results$AvgWeeklyUnitCost,       size = .5, alpha = .3, linetype = "dotdash", color = "black") +
      #      geom_vline(xintercept = results$AvgWeeklyUnitPrice,      size = .5, alpha = .3, linetype = "dotted",  color = "black") +
      #      geom_hline(yintercept = results$AvgDailyUnitSales,       size = .5, alpha = .3, linetype = "dashed",  color = "black") +
      #      # Add axis tick marks according to data
      #      scale_y_continuous(breaks = seq(    round(min(data2$AvgDailyUnitSales,  na.rm = T), 0),   round(max(data2$AvgDailyUnitSales,  na.rm = T), 0),
      #                                          abs(round(min(data2$AvgDailyUnitSales,  na.rm = T), 0)-   round(max(data2$AvgDailyUnitSales,  na.rm = T), 0))/10),
      #                         limits = c(            min(data2$AvgDailyUnitSales,  na.rm = T),             max(data2$AvgDailyUnitSales,  na.rm = T))) +
      #      scale_x_continuous(breaks = seq(    round(min(data2$AvgWeeklyUnitPrice, na.rm = T), 0)-1, round(max(data2$AvgWeeklyUnitPrice, na.rm = T), 0)+1, 0.1),
      #                         limits = c(            min(data2$AvgWeeklyUnitPrice, na.rm = T),             max(data2$AvgWeeklyUnitPrice, na.rm = T)))
      
      # Draw plot 2) by ad feature
      # If was featured in ad at least once
      #if (results$TotalAdFeat_abs > 0) { 
      
      #plot_reg2 <- plot_reg1 +
      #      # Draw points colored by ad feature
      #      geom_point( aes(color = AdFeature), size = 5, alpha = .3) +
      #      # Position point-color legend on bottom
      #      theme(legend.position = "top") +
      #      scale_colour_manual(name   = "Ad Feature",
      #                          labels = c("NOT on Sale", "ON Sale"),
      #                          values = c("dodgerblue2", "firebrick3")) + 
      #      # Draw regression line (by ad feature)
      #      geom_smooth(aes(group = AdFeature, color = AdFeature), method = "lm", fullrange = T, size = .5, show.legend = T, se = F, alpha = .1, linetype = "dotdash") +
      #      # Add average price & unit sales (by ad-feature)
      #      geom_vline(xintercept = results$lm0_a_AvgWeeklyUnitPrice_NoAd,          size = .5, alpha = .5, linetype = "dotted", color = "dodgerblue2") +
      #      geom_vline(xintercept = results$lm0_b_AvgWeeklyUnitPrice_Ad,            size = .5, alpha = .5, linetype = "dotted", color = "firebrick3")  +
      #      geom_hline(yintercept = results$lm1b_a_AvgDailyUnitSales_NoAdNotPrAdj,  size = .5, alpha = .5, linetype = "dashed", color = "dodgerblue2") + 
      #      geom_hline(yintercept = results$lm1b_b_AvgDailyUnitSales_AdNotPrAdj,    size = .5, alpha = .5, linetype = "dashed", color = "firebrick3")
            
      #grid.newpage()
      #plot2 <- grid.draw(rbind(ggplotGrob(plot_reg2), ggplotGrob(plot_opt), size = "last"))
      
      #ggsave(filename = paste0(plot_filename), plot = plot2, width = 25, units = "cm")
      
      # Else draw 1st plot
      #} else {
      
      #grid.newpage()
      #plot1 <- grid.draw(rbind(ggplotGrob(plot_reg1), ggplotGrob(plot_opt), size = "last"))
                        
      #ggsave(filename = paste0(plot_filename), plot = plot1, width = 25, units = "cm")
            
      #}
      
      # Remove models so that they don't skew the models for the enxt product in the loop
      rm(#plot_reg1, plot_reg2, plot1, plot2, plot_opt,
         lm1a_Price2, lm1a_Price, lm0_Price, lm0_Base, lm1b_Ad, lm2_PriceAd, lm0_PriceAd, lm2_PriceAd2
         )
}

results <- ldply(ProductList, data.frame)

data1 <- left_join(data1,
                   results, by = "UPC")

StoreList[[j]] <- data1

} # Close store-loop

data1 <- ldply(StoreList, data.frame)

## 5) Export results ####################################################################

setwd("/media/radmin/ExternalHD/Projects/Sales Data Analyses/3 PD/2 Marketing Mix Models (Results)")
write.csv(results, paste0(paste(SelectedSDept, collapse = " "), " Products_", SelectedWeeks_Begin, " - ", SelectedWeeks_End, " (", SelectedWeeks_TotalAdWeeks,  " Ad Weeks).csv"), row.names = F)

## Data Preparation ####################################################################################################

# Load packages
library(dplyr); library(xlsx)

# Get basic dataset
setwd("~/Projects/Sales Data Analyses/3 PD/2 Marketing Mix Models")
data0        <- readRDS("ALL DEPT. Item Movement & Price Points Weekly per Store.rds") %>% mutate(Week_factor = factor(Week))

# Get time horizon of dataset
DateBegin         <- min(data0$Week)
DateEnd           <- max(data0$Week)+6
DatePeriod_weeks  <- as.integer(difftime(DateEnd+1, DateBegin))/7

# Get 1st data subset
  data1        <-
        
# Ignore variables that are irrelevant for this analysis
select(data0, -c(SubDept, PriceType, PriceLabel, UnitPrice, AvgPrice, Sales)) %>% 

# Filter out   IMMATERIAL items (see below) irrelevant to the analysis of slow-moving PHYSICAL items on the shelves
filter(!grepl("PROMO|PROMOTION|COUPON|DISPLAY|OPEN RING|SHIPPER", Product)) %>% 

# Group by UPC-level products per department per store for all stores
group_by( Dept, Store, UPC, Product) %>% 
      
# Aggregate across weeks (since ignored in group_by):  to its total, and compute date features
summarise(# Total movement (in units sold)
          TotalUnitsSold = sum(UnitsSold, na.rm = T),
          # Time span of first and last sales week
          Period         = paste(            min(Week), '-', max(Week+6)),
          # Recency (in weeks) since item was last purchased
          Recency_weeks  = round(as.integer(difftime(DateEnd+1, min(Week)))/7, 0)) %>% as.data.frame()

## Data Analysis #######################################################################################################
## Determine items per department / per store with slow movement

# Get 2nd data subset
  data2 <-

# Filter department-specific items with less than / equal to ONE unit sold      
filter(data1,
       Dept == "GROCERY", TotalUnitsSold <= 1) %>% 

# Convert to 13-digit UPC with leading zeros as character in apostrophes separated by commas
mutate(UPC_factor = paste0("'",
                           as.character(suppressWarnings(formatC(UPC, digits = 0, width  = 13, format = "f", flag   = "0"))),
                           "',"),
       Store      = as.character(Store),
       NewItem    = NA) %>% 
# Sort items by store and by recency in descending order; that is, item that hasn't been sold teh longest is on top
arrange(Store, desc(Recency_weeks))
       
  
  
## Data Export #########################################################################################################

setwd("~/Projects/Sales Data Analyses/3 PD/4 Determine Slow-Moving Items") 

# Add sub-header row to dataset where SQL command is added (see below here) that feeds selected slow-moving items into database browser
rbind(c(colnames(data2)[1:length(colnames(data2))-1], "SELECT F01 FROM OBJ_TAB WHERE F01 IN ("),
      data2) %>%
# Save final dataset to Excel file
write.xlsx2(paste0("Slow-moving Items per Dept x per Store_All Stores_", DateBegin, " - ", DateEnd, ".xlsx"), row.names = F)
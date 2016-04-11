# 0) Initial Settings ##################################################################################################

# Load packages
library(plyr)
library(dplyr)
library(XML)
library(zipcode)
library(lubridate)
library(ggplot2)
library(scales)
#library(tidyr)

# Load ZIP-code dataset from 'zipcode' package
data(       zipcode, package = "zipcode")
ZIPCodes <- zipcode
         rm(zipcode)

# Disable default string-to-factor conversion
options(stringsAsFactors = FALSE)

# 1) Prepare XML transaction-data reading and processing ###############################################################

## Read XML-file names (not yet the files themselves) the follow naming convention ("YMMDDSSS.LLL")
## Derive file features from this convention and compile information in a file index
# Y   = Year - last digit (e.g. 5 for year 2015)
# MM  = Month
# DD  = Day
# SSS = Store (e.g. 012 for store 12)
# LLL = Lane  (e.g. 001 for lane 1)

## Determine (store) input folder that contains XML files and list them
Store             <- "8"        
      filefolder  <- paste0("~/Projects/Transaction Data Analyses/1 RD/XML Transaction Files/", Store)
setwd(filefolder)
FileNames         <- list.files()

## Extract the information coded in the file names, and organize them in a dataframe
FileIndex <- as.data.frame(cbind(       FileNames,
                                        # Lane
                                        substr(FileNames, 10, 12),
                                        # Store
                                        substr(FileNames,  6,  8),
                                        # Day
                                        substr(FileNames,  4,  5),
                                        # Month
                                        substr(FileNames,  2,  3),
                                        # Year
                                        substr(FileNames,  1,  1))) %>%
      dplyr::rename(Lane  = V2,
                    Store = V3,
                    Day   = V4,
                    Month = V5,
                    Year  = V6) %>%
      ## Define data types of columns and build new Date column
      mutate(# Convert lane number to numeric so that irrelevant lanes can be filtered out later
            Lane  = as.numeric(Lane),
            # Remove leading zero in store number by converting from character to numeric
            Store = as.numeric(Store),
            # Complete the year digit from the file name
            Year  = paste0(201, Year),
            # Build a date column by concatenating day, month, and year
            Date  = as.Date(paste(Year, Month, Day, sep = "-"))) %>% 
      # Remove files with lane numbers 100+ that do not represent checkout lanes and do not contain customer transactions
      filter(Lane  < 100) %>% 
      # Exclude XML files from today ALWAYS as they are still being written in the course of the day and would be corrupted otherwise
      filter(Date  != today())

rm(FileNames)

## Draw histogram to check if any dates (gaps in graph) or files (unusual dips or spikes) were not transfered from store server to R server
ggplot(FileIndex, aes(Date)) +
      geom_histogram(binwidth = 1, alpha=.5) +
      ylab("Number of XML Files Transferred") +
      scale_x_date(labels = date_format("%m/%d/%y"),
                   breaks = date_breaks("2 days")) +
      scale_y_discrete(breaks = seq(0, 20, 1)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Select just those XML files that lie within a date range of interest

# a) Default date range of earliest/latest date contained
date_start        <- min(as.Date(FileIndex$Date))
date_end          <- max(as.Date(FileIndex$Date))

# b) Manual date range
#date_start        <- as.Date("2015-03-05")
#date_end          <- as.Date("2016-02-23")

# populate date range completely and fill in dates between start and end date
date_range        <- seq.Date(from = date_start,
                              to   = date_end,
                              by   = 1)

## Select files names according to date-range criteria that are fed into the actual XML-file reading process
files_selected    <- subset(FileIndex, Date %in% date_range, FileNames)[, 1]
rm(date_range)

## Create list with length equal to the length of files to be read
FileList   <- list()
length(FileList)  <- length(files_selected)

# 2) Read and process XML transaction data #############################################################################

# Extract relevant information from transaction files by using the XPath XML querying language

# Loop through each selected XML file and extract the same information which will be stored as dataframe and placed in list element
for (i in 1:length(files_selected)) { 
      
      setwd(filefolder)
      tree <- xmlInternalTreeParse(files_selected[i], getDTD = F)
      # May experiment with xmlInternalTreeParse(), xmlTreeParse() or xmlEventParse() for more processing speed
      
      # If at least one transaction type (F1068) "SALE" is contained in the XML file, continue and...
      if ("SALE" %in% xpathSApply(tree, "//*[@F1068]", xmlGetAttr, "F1068", default = NA)) {
            
            ### I) Read BASIC transaction data (stored in element trs) with the XPath querying commands
            
            ## Find any transaction element in the XML tree that has attribute "F1068" (transaction type) with value "SALE" and...
            ## Get the value of the defined co-attribute within the element
            
            # Transaction Type (F1068) & Number (F1032)
            TransType   <- xpathSApply(tree, "//*[@F1068='SALE']", xmlGetAttr, "F1068", default = NA)
            TransNo     <- xpathSApply(tree, "//*[@F1068='SALE']", xmlGetAttr, "F1032", default = NA)
            # Store (F1056) & Lane Number (F1057)
            StoreNo     <- xpathSApply(tree, "//*[@F1068='SALE']", xmlGetAttr, "F1056", default = NA)
            LaneNo      <- xpathSApply(tree, "//*[@F1068='SALE']", xmlGetAttr, "F1057", default = NA)
            # Date (F253) & Start/End Time (F1035/F1036)
            Date        <- xpathSApply(tree, "//*[@F1068='SALE']", xmlGetAttr, "F253" , default = NA)
            STime       <- xpathSApply(tree, "//*[@F1068='SALE']", xmlGetAttr, "F1035", default = NA)
            ETime       <- xpathSApply(tree, "//*[@F1068='SALE']", xmlGetAttr, "F1036", default = NA)
            # Cashier Number (F1126) & Name (F1127)
            CashNo      <- xpathSApply(tree, "//*[@F1068='SALE']", xmlGetAttr, "F1126", default = NA)
            CashName    <- xpathSApply(tree, "//*[@F1068='SALE']", xmlGetAttr, "F1127", default = NA)
            # Customer Number (F1148)
            CustNo      <- xpathSApply(tree, "//*[@F1068='SALE']", xmlGetAttr, "F1148", default = NA)
            
            ## Bind vectors and create base dataset
            data        <- as.data.frame(cbind(TransType,
                                               TransNo,
                                               StoreNo,
                                               LaneNo,
                                               Date,
                                               STime,
                                               ETime,
                                               CashNo,
                                               CashName,
                                               CustNo))
            
            ### II) Read TOTAL SALES amount of transactions (stored in transaction-element CHILD totalizer element t)
            
            ## If at least one totalizer number (F1034) "2"* is contained in the XML file, continue and...
            ## *(Totalizer number      (F1034) "2" corresponds to...
            ##   Totalizer description (F02)   "TOTAL SALES")
            
            if ("2" %in% xpathSApply(tree, "//*[@F1068='SALE']/descendant::*[@F1034]", xmlGetAttr, "F1034", default = NA)) {
                  
                  # Find any element in the XML tree that contains the "F1034" (totalizer number) attribute value "2" and...
                  # Get the entire text value of its first following sibling leaf node, the totalizer-dollar-amount element "F65" 
                  
                  TotalSales              <- xpathSApply(tree, "//*[@F1068='SALE']/descendant::*[@F1034='2']/following-sibling::*[1]",
                                                         xmlValue, "F65")
                  # Go back up from the above mentioned node to the ancestor node and get the value of the  "F1032" (transaction number) attribute
                  TotalSales_TransNo      <- xpathSApply(tree, "//*[@F1068='SALE']/descendant::*[@F1034='2']/following-sibling::*[1]/ancestor::*[@F1032]",
                                                         xmlGetAttr, "F1032", default = NA)
                  
                  ## Identify transactions with "Open Ring" items that are wholesale-like transactions (and not retail-like transactions with consumers); therefore, inflate basket sizes unrealistically due to high total sales
                  
                  # Find any node in the XML tree whose "F02" (item description) attribute value starts with "OPEN RING"
                  # (Thus, an open-ring item was rung up by the cashier) and
                  # Get the ancestor-node attribute (F1032) value, which is the number of the  transaction that contained the open-ring item
                  OpenRing_TransNo        <- xpathSApply(tree, "//*[starts-with(@F02, 'OPEN RING')]/ancestor::*[@F1032]",
                                                         xmlGetAttr, "F1032")
                  
                  ## Bind vectors and add Total Sales to basic transaction data by joining datasets
                  data_sales        <- as.data.frame(cbind(  TotalSales, TotalSales_TransNo))
                  names(data_sales) <-                    c("TotalSales",          "TransNo")
                  data              <- left_join(data, data_sales,          by = c("TransNo")) %>%
                        # Filter out transactions with open-ring items
                        filter(!(TransNo %in% OpenRing_TransNo))
                  # Remove dataframe that is not needed any longer
                  data_sales  <- NULL
                  
            } # ends sales-totalizer if-clause
            
            ### III) Read cashier-entered ZIP CODES of transactions (stored in transaction CHILD line/row element r) 
            
            ## If at least one function code (F1063) "760"* is contained in the XML file, continue and...
            ## *(Function code        (F1063) "760" corresponds to...
            ##   Function description (F02)   "P.Code")
            
            if ("760" %in% xpathSApply(tree, "//*[@F1068='SALE']/descendant::*[@F1063]", xmlGetAttr, "F1063", default = NA)){
                  
                  # Find any node in the XML tree that contains the "F02" (function description) attribute value "P.Code"
                  # (Thus, a ZIP code was entered by the cashier) and
                  # Get the entire value of that attribute
                  # Strip off "P.CODE=" and extract five-digit ZIP code
                  ZIPCash     <- xpathSApply(tree, "//*[@F1068='SALE']/descendant::*[contains(@F02, 'P.CODE')][last()]",
                                             xmlGetAttr, "F02", default = NA)
                  ZIPCash     <- substr(ZIPCash, 8, nchar(ZIPCash))
                  
                  # Find ancestor node of the abovementioned node that has an "F1032" (transaction number) attribute and
                  # Get the value of the this attribute
                  ZIP_TransNo <- xpathSApply(tree, "//*[@F1068='SALE']/descendant::*[contains(@F02, 'P.CODE')]/ancestor::*[@F1032]",
                                             xmlGetAttr, "F1032", default = NA)
                  
                  ## Bind vectors and add ZIP Codes to basic transaction data by joining datasets
                  # Bind vectors and add  to base dataset by joining
                  data_zip          <- as.data.frame(cbind(  ZIPCash, ZIP_TransNo))
                  names(data_zip)   <-                    c("ZIPCash",   "TransNo")
                  data              <- plyr::join(data, data_zip, by = c("TransNo"))
                  # Remove dataframe that is not needed any longer
                  #data_zip    <- NULL
                  
            } # Ends ZIP-code if-clause
      } # Ends SALE-transaction if-clause
      
      
      ##  Write each XML-file dataframe into the list element 
      FileList[[i]]     <- data
      
} # Ends for-loop

# Collapse dataframes of the list into one dataframe
Trans       <- plyr::ldply(FileList, data.frame)

# Remove data objects that are not needed anymore since stored in dataframe and may cause confusion later
rm(filefolder, tree, i,
   TransType, TransNo, StoreNo, LaneNo, STime, ETime, CashNo, CashName, CustNo,
   TotalSales, TotalSales_TransNo, OpenRing_TransNo,
   ZIPCash, ZIP_TransNo,
   Date, data, data_sales, data_zip)

# 3) Read and process CSV customer data ################################################################################

# Retrieve account numbers of customers and their self-entered home ZIP codes from customer CSV file

setwd("~/Projects/Transaction Data Analyses/1 RD/CSV Customer File")
Customers <- read.csv(list.files(), colClasses = "character", na.strings = c("")) %>%
      select(         CardNumber,           PostalCode) %>%
      dplyr::rename(CustNo = CardNumber,       # Customer number
             ZIPCust = PostalCode) %>%  # Customer ZIP code
      # Correct typos in customer ZIP codes by replacing letters "o/O" with digit "0"
      mutate(ZIPCust = gsub("o", "0", ZIPCust, ignore.case = T)) %>% 
      # Clean ZIP codes and convert invalid entries to NAs
      mutate(ZIPCust = clean.zipcodes(ZIPCust))

# 4) Join ZIP codes from customer data with transaction data ###########################################################

# Clean ZIP codes by erasing non-ZIP-code-format entries and non-existing ones; Customer ZIP codes were already cleaned in 3)
Trans$ZIPCash <- clean.zipcodes(Trans$ZIPCash)

Trans <- left_join(Trans, Customers, by = c("CustNo")) %>%
      # Compare ZIP codes from cashiers and customers: TRUE if both match; FALSE if they don't; NA if one of the two is NA
      mutate(ZIPComp = ZIPCash == ZIPCust)

# If available, take ZIP-code entered by customer; else, ...
Trans$ZIP                           <- as.character(Trans$ZIPCust)
# Take ZIP-code entered by cashier
Trans$ZIP[which(is.na(Trans$ZIP))]  <- as.character(Trans$ZIPCash[which(is.na(Trans$ZIP))])

# Create boolean variables that indicate whether entered ZIP code is a valid, existing one

Trans$IsZIPCash                                 <- Trans$ZIPCash %in% ZIPCodes$zip
Trans$IsZIPCash[which(is.na(Trans$ZIPCash))]    <- NA
Trans$IsZIPCust                                 <- Trans$ZIPCust %in% ZIPCodes$zip
Trans$IsZIPCust[which(is.na(Trans$ZIPCust))]    <- NA
Trans$IsZIP                                     <- Trans$ZIP     %in% ZIPCodes$zip
Trans$IsZIP[which(is.na(Trans$ZIP))]            <- NA

# Flag rewards custumers
Trans$RewCust                                   <- NA
Trans$RewCust[which(!is.na(Trans$CustNo))]      <- TRUE
Trans$RewCust[which(is.na(Trans$RewCust))]      <- FALSE

# 5) Finish transaction data set ######################################################################################

# Remove duplicated rows (first instance which is duplicated is not removed, just the second instance that duplicates)
Trans <- filter(Trans, !duplicated(Trans)) %>% 
      # Define columns
      mutate(TransType  = as.factor(as.character(TransType)),
             TransNo    = as.factor(as.character(TransNo)),
             StoreNo    = as.factor(as.numeric(as.character(StoreNo))),
             LaneNo     = as.factor(as.numeric(as.character(LaneNo))),
             Date       = as.Date(Date),
             CustNo     = as.factor(as.character(CustNo)),
             RewCust    = as.factor(as.character(RewCust)),
             CashNo     = as.factor(as.character(CashNo)),
             CashName   = as.character(CashName),
             TotalSales = as.numeric(as.character(TotalSales)),
             ZIPCash    = as.factor(as.character(ZIPCash)),
             ZIPCust    = as.factor(as.character(ZIPCust)),
             ZIP        = as.factor(as.character(ZIP)),
             ID         = as.factor(paste(Date, StoreNo, LaneNo, TransNo, sep = "-")))

# Define time variables separately since dplyr::mutate doesn't support POSIXlt
Trans$SDateTime   <- strptime(paste(Trans$Date, Trans$STime), format = "%Y-%m-%d %H:%M:%S")
Trans$EDateTime   <- strptime(paste(Trans$Date, Trans$ETime), format = "%Y-%m-%d %H:%M:%S")

# Group dates to Wed-Tue sales weeks
# (Trick cut.Date function with the help of dummy variable that just groups Mon-Sun or Sun-Sat weeks)
Trans <- subset(Trans, !duplicated(Date), Date) %>%
      arrange(Date) %>% 
      # Create dummy variable that's used to group dates by weeks (which takes Monday as the group starts)
      mutate( DateDummyVar = Date - 2) %>% 
      # Groups dates by weeks and uses dummy variable
      mutate( Week         = factor(cut.Date(DateDummyVar, breaks = "week", labels = F,
                                             start.on.monday = TRUE))) %>%
      select(-DateDummyVar) %>% 
      plyr::join(Trans, by = "Date", type = "right") %>%

# Sort columns and finalize transaction dataset      
      select(ID,
             TransType,
             TransNo,
             LaneNo,
             StoreNo,
             Week,
             Date,
             STime,
             ETime,
             SDateTime,
             EDateTime,
             CashNo,
             CashName,
             CustNo,
             RewCust,
             TotalSales,
               ZIPCash,
             IsZIPCash,
               ZIPCust,
             IsZIPCust,
               ZIP,
             IsZIP,
             ZIPComp)

# 6) Export whole transaction data set #################################################################################

# Save R data workspace
save.image("~/Projects/Transaction Data Analyses/2 DP/Workspaces/Transaction Data Analysis Workspace.RData")

# Save as R data object
setwd(     "~/Projects/Transaction Data Analyses/3 PD/0 Transaction Data")
saveRDS(  Trans, paste0("Daily Transactions_Store ", Store, "_", date_start, " - ", date_end, ".rds"))

# Save as CSV file
setwd(     "~/Projects/Transaction Data Analyses/3 PD/1 ZIP-Code Analysis")
write.csv(Trans, paste0("Daily Transactions_Store ", Store, "_", date_start, " - ", date_end, " (ZIP Codes).csv"))

# 7) Read and process CSV ad-distribution data #########################################################################

# Contains household counts per store, per ZIP code that ad is distributed to weekly
# (had been previously processed and cleaned from the original XLSX spreadsheet provided by the ad distributor)

setwd("~/Projects/Transaction Data Analyses/1 RD/XLS Ad Distribution")
AdDistribution <-    read.csv(list.files(), colClasses = c("factor","factor", "factor", "numeric")) %>% 
      dplyr::rename(StoreNo        = Store,
             City           = City,
             ZIP            = ZIP,
             HouseholdCount = HouseholdCount) 

########################################################################################################################
########################################################################################################################

## 8) Create and save subsets of whole transaction data set to answer project-specific research questions ##############

# Save as CSV file that is smaller in size than the whole transaction data set in order to feed more manageably into further EDA or data viz tools

# 8-I) Project 1: ZIP-Code Analysis ####################################################################################

## Load Data from saved workspace ######################################################################################

Trans <- readRDS("~/Projects/Transaction Data Analyses/3 PD/0 Transaction Data/Single Stores & All Weeks/Daily Transactions_Store 10_2015-01-01 - 2016-02-02.rds")

## Prelimenary Analysis on data quality (cashier ZIP-code validity) externally in Tableau ##############################

# How many transactions / How much sales were tied to ZIP Codes-- both entered by cashiers and enhanced by customers?
# Did ZIP-code enhancement by customers decrease number of NAs?
# Which cashiers do not enter ZIP codes at all or enter invalid or out-of-radius ZIP codes?
setwd("~/Projects/Transaction Data Analyses/3 PD/1 ZIP-Code Analysis")
select(Trans, ID,
       StoreNo, Date, Week,
       CashNo, CashName,
       ZIPCash, IsZIPCash,
       ZIPCust, IsZIPCust,
       ZIP,     IsZIP,
       ZIPComp,
       TotalSales) %>%
      # Export data set and feed into Tableau
      write.csv(paste0("Daily Transactions_Store ", Store, "_", date_start, " - ", date_end, " (ZIP Codes).csv"), row.names = F)

## Main Analysis #######################################################################################################

setwd("~/Projects/Transaction Data Analyses/3 PD/1 ZIP-Code Analysis")
# Disable scientific notation and display UPCs correctly
options(scipen=999)

## Define analysis store, period, and parameter
Store             <- "10"

# Analysis period
date_start        <-  as.Date("2015-01-01")
date_end          <-  as.Date("2015-12-31")
date_range        <- seq.Date(from = date_start,
                              to   = date_end,
                              by   = 1)
PeriodDays        <- length(date_range)
PeriodWeeks       <- round(PeriodDays/7, 2)

# Analysis parameter
OverrunRate <- 1.03
DistrCPM    <- 17
PrintCPM    <- 19.5

## Determine total ad cost, deliveries per ZIP Code, per store

ZIPCost <- select(AdDistribution, StoreNo, ZIP, HouseholdCount) %>% 
      # Filter store that's analyzed
      filter(StoreNo == Store) %>%
      # Re-factorize store & ZIP-code variable and check if no. of levels/ZIP-codes equals no. of observations
      mutate(ZIP     = factor(ZIP),
             StoreNo = factor(StoreNo)) %>% 
      dplyr::rename(AdDeliveriesWeekly    = HouseholdCount) %>% 
      mutate(AdDeliveriesAnnualy   = round(AdDeliveriesWeekly *PeriodWeeks,              1),
             AdPrintsAnnualy       = round(AdDeliveriesAnnualy*OverrunRate,              1),
             AdDeliveryCostAnnualy = round(AdDeliveriesAnnualy*DistrCPM/1000,            2),
             AdPrintingCostAnnualy = round(AdPrintsAnnualy    *PrintCPM/1000,            2),
             TotalAdCostAnnualy    = round(AdDeliveryCostAnnualy+AdPrintingCostAnnualy,  2))

## Determine TOTAL sales, transaction count overall (across ZIP codes) per STORE
STORE <- select(Trans, StoreNo, Date,
                TotalSales) %>%
      # Filter store that's being analyzed
      filter(StoreNo == Store) %>% 
      # Filter analysis period
      filter(Date   %in% date_range) %>% 
      # Filter out return transactions (neg. sales)
      filter(TotalSales > 0) %>%
      # Group by store (across ZIP codes)
      group_by(StoreNo) %>% 
      # Compute sales, transaction
      summarise(TotalSales       = sum(TotalSales, na.rm = T),
                TotalTransCount  = n())
      
## Determine sales, transaction count per VALID ZIP code, per store

ZIPSales          <- select(Trans, StoreNo, Date,
                            IsZIP, ZIP,
                            TotalSales) %>%
      # Filter store that's being analyzed
      filter(StoreNo == Store) %>%
      # Filter analysis period
      filter(Date  %in% date_range) %>% 
      # Filter out return transactions (neg. sales)
      filter(TotalSales > 0) %>%
      # Filter VALID ZIP codes
      filter(IsZIP ==   TRUE) %>% 
      # Re-factorize store & ZIP-code variable and check if no. of levels (ZIP codes) equals no. of observations
      mutate(ZIP     = factor(ZIP),
             StoreNo = factor(StoreNo)) %>% 
      # Group by store, per ZIP code
      group_by(StoreNo, ZIP) %>% 
      # Compute sales, transaction count per valid ZIP code
      summarise(Sales_valid             = sum(TotalSales, na.rm = T),
                TransCount_valid        = n()) %>% 
      # Compute sales, transaction count per valid ZIP code as percentage of store total
      mutate(   SalesOfTotal_valid      = Sales_valid     /sum(Sales_valid)      * 100,
                TransCountOfTotal_valid = TransCount_valid/sum(TransCount_valid) * 100) %>% 
      # Extrapolate sales, transaction count per VALID ZIP code
      # To          sales, transaction count per       ZIP code
      mutate(Sales      = SalesOfTotal_valid      * STORE$TotalSales      / 100,
             TransCount = TransCountOfTotal_valid * STORE$TotalTransCount / 100) %>% 
      # Join with total ad cost per ZIP code, per store
      left_join(ZIPCost, by = c("StoreNo", "ZIP")) %>% 
      # Replace NA's in cost variables with 0 since no cost are spent on those ZIP codes
      replace(is.na(.), 0) %>%
      # Compute ROI measures
      mutate(AdProfit         = round(Sales-TotalAdCostAnnualy,       2),
             TransPerDelivery = round(TransCount/AdDeliveriesAnnualy, 3),
             CostAsPercentage = round(TotalAdCostAnnualy/Sales,       2))

## Save to visualize (map) in Tableau
write.csv(ZIPSales, paste0("Annual Sales & Ad Cost per ZIP Code_Store ", Store, "_", date_start , " - ", date_end, ".csv"), row.names = F)

# 8-II) Project 2: Rewards-customer related data #######################################################################

# 8-IIa) Basket sizes by customer type (regular vs. rewards Customers) #################################################

## Load Data from saved workspace ######################################################################################
data <- rbind(readRDS("~/Projects/Transaction Data Analyses/3 PD/0 Transaction Data/Single Stores & All Weeks/Daily Transactions_Store 12_2015-09-02 - 2016-02-16.rds"),
              readRDS("~/Projects/Transaction Data Analyses/3 PD/0 Transaction Data/Single Stores & All Weeks/Daily Transactions_Store 11_2015-04-22 - 2016-02-16.rds"),
              readRDS("~/Projects/Transaction Data Analyses/3 PD/0 Transaction Data/Single Stores & All Weeks/Daily Transactions_Store 10_2015-01-01 - 2016-02-16.rds"),
              readRDS("~/Projects/Transaction Data Analyses/3 PD/0 Transaction Data/Single Stores & All Weeks/Daily Transactions_Store 9_2015-02-19 - 2016-02-23.rds"),
              readRDS("~/Projects/Transaction Data Analyses/3 PD/0 Transaction Data/Single Stores & All Weeks/Daily Transactions_Store 8_2015-03-05 - 2016-02-16.rds"),
              readRDS("~/Projects/Transaction Data Analyses/3 PD/0 Transaction Data/Single Stores & All Weeks/Daily Transactions_Store 7_2015-02-26 - 2016-02-23.rds"),
              readRDS("~/Projects/Transaction Data Analyses/3 PD/0 Transaction Data/Single Stores & All Weeks/Daily Transactions_Store 5_2015-03-17 - 2016-02-23.rds"))

Trans_RewCust <- select(data, ID,
                        StoreNo, Date,
                        RewCust,
                        TotalSales) %>% 
      # Filter 2015 transactions
      # filter(Date >= "2015-01-01", Date <= "2015-12-31") %>% 
      # Filter out observations with NA
      filter(complete.cases(.)) %>%
      # Filter out return transactions (neg. sales)
      filter(TotalSales > 0) %>% 
      # LOGARITHMIZE Total Sales
      mutate(TotalSales_log = log(TotalSales))

## Hypothesis Formulation ##############################################################################################

# Null Hypothesis: Average basket sizes of Rewards and non-Rewards customers do NOT differ and are equal
# Alt. Hypothesis: Average basket sizes of Rewards are non-Rewards customers DO differ where
#                  Average basket sizes of Rewards are bigger than their non-Rewards counterparts

## Exploratory Analysis of Raw Data #####################################################################################

# Show summary statistics overall and by customer type
summary(Trans_RewCust)
by(Trans_RewCust, Trans_RewCust$RewCust, summary)

# Compute statistics per store, per customer group
Trans_RewCust_Stats <- summarise(group_by(Trans_RewCust, StoreNo, RewCust),
                                 TotalSales_Mean   = mean(  TotalSales, na.rm = T),
                                 TotalSales_Median = median(TotalSales, na.rm = T),
                                 TotalSales_StdDev = sd(    TotalSales, na.rm = T),
                                 
                                 TotalSales_Mean_log   = mean(  TotalSales_log, na.rm = T),
                                 TotalSales_Median_log = median(TotalSales_log, na.rm = T),
                                 TotalSales_StdDev_log = sd(    TotalSales_log, na.rm = T))

# Draw boxplot
ggplot(Trans_RewCust, aes(y = TotalSales, colour = RewCust)) +
      geom_boxplot(aes(x = RewCust)) + 
      coord_cartesian(ylim=c(0, 60)) +
      scale_y_continuous(breaks=seq(0, 100, 5)) +
      # Draw overall median
      geom_hline(aes(yintercept = median(TotalSales)),
                 linetype = "solid", size = 0.5, colour = "grey") +
      # Draw overall mean
      geom_hline(aes(yintercept = mean(TotalSales)),
                 linetype = "dashed", size = 0.5, colour = "grey") +
      # Draw group means
      geom_hline(data = Trans_RewCust_Stats,
                 aes(yintercept = TotalSales_Mean, colour = RewCust),
                 linetype = "dashed", size = 0.5) +
      facet_grid(. ~ StoreNo)

# Draw histograms
ggplot(Trans_RewCust, aes(x = TotalSales)) +
      geom_histogram(aes(fill = RewCust),
                     binwidth=1, alpha = .5) +
      coord_cartesian(xlim=c(0, 100)) +
      scale_x_continuous(breaks=seq(0, 100, 5)) +
      geom_vline( data = Trans_RewCust_Stats,
                 aes(xintercept = TotalSales_Mean,  colour = RewCust),
                 linetype="dashed", size=1) +
      geom_vline( data = Trans_RewCust_Stats,
                  aes(xintercept = TotalSales_Median,  colour = RewCust),
                  linetype="solid", size=1) +
      facet_grid(. ~ StoreNo)

## Log-Transformation of Data ##########################################################################################

# Draw boxplot
ggplot(Trans_RewCust, aes(y = TotalSales_log, colour = RewCust)) +
      geom_boxplot(aes(x = RewCust)) + 
      coord_cartesian(ylim=c(0, 5)) +
      # Draw overall median
      geom_hline(aes(yintercept = median(TotalSales_log)),
                 linetype = "solid", size = 0.5, colour = "grey") +
      # Draw overall mean
      geom_hline(aes(yintercept = mean(TotalSales_log)),
                 linetype = "dashed", size = 0.5, colour = "grey") +
      # Draw group means
      geom_hline(data = Trans_RewCust_Stats,
                 aes(yintercept = TotalSales_Mean_log, colour = RewCust),
                 linetype = "dashed", size = 0.5) +
      facet_grid(StoreNo ~ .)

# Draw histograms
ggplot(Trans_RewCust, aes(x = TotalSales_log)) +
      geom_histogram(aes(fill = RewCust),
                     binwidth=.1, alpha = .5) +
      coord_cartesian(xlim=c(0, 5)) +
      geom_vline( data = Trans_RewCust_Stats,
                 aes(xintercept = TotalSales_Mean_log,  colour = RewCust),
                  linetype="dashed", size=1) +
      geom_vline(data = Trans_RewCust_Stats,
                 aes(xintercept = TotalSales_Median_log,  colour = RewCust),
                 linetype="solid", size=1) +
      facet_grid(StoreNo ~ .)

## Hypothesis Testing with t Confidence Intervals ######################################################################
t.test(Trans_RewCust$TotalSales_log[Trans_RewCust$RewCust == T],
       Trans_RewCust$TotalSales_log[Trans_RewCust$RewCust == F],
       alternative = "greater")

# 8-IIb) Purchase history/behavior of rewards customers ######################################################################
setwd("~/Projects/Transaction Data Analyses/3 PD/Rewards Customer")
Trans_RewCust     <- select(Trans, ID,
                            StoreNo, Date,
                            RewCust, CustNo,
                            TotalSales) %>% 
      filter(RewCust == "1") %>% 
      group_by(CustNo) %>% 
      summarise(TotalSales       = sum(TotalSales, na.rm = T),
                TransactionCount = n(),
                FirstDate        = min(Date),
                LastDate         = max(Date)) %>% 
      write.csv(paste0("Rewards Customers_Store ", Store, "_", date_start, " - ", date_end, ".csv"), row.names = F)
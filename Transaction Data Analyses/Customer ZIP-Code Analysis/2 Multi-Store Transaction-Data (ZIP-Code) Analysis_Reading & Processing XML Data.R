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

# Disable automatic string-to-factor conversion
options(stringsAsFactors = FALSE)

# 1) Prepare XML transaction-data reading and processing ###############################################################

# 1a) Read XML-file names ##############################################################################################

## Derive file characteristics from file-name convention and compile information in a file index
## File-name convention "YMMDDSSS.LLL":
# Y   = Year - last digit (e.g. 5 for year 2015)
# MM  = Month
# DD  = Day
# SSS = Store (e.g. 012 for store 12)
# LLL = Lane  (e.g. 001 for lane 1)

# Determine (store) input folders that contain XML files and list them
setwd("~/Projects/Transaction Data Analyses/1 RD/XML Transaction Files")
Stores                  <- list.files()
       StoreList        <- list()
length(StoreList)       <- length(Stores)

# Loop through each store folder and read the filenames
for (j in 1:length(Stores)) {
      filefolder  <- paste0("~/Projects/Transaction Data Analyses/1 RD/XML Transaction Files/", Stores[j])
setwd(filefolder)
FileNames         <- list.files()

## Extract the information coded in the file name, and organize in a dataframe
StoreList[[j]] <- as.data.frame(cbind(FileNames,
                                      # Lane
                                      substr(FileNames, 10, 12),
                                      # Komma            9 
                                      # Store
                                      substr(FileNames,  6,  8),
                                      # Day
                                      substr(FileNames,  4,  5),
                                      # Month
                                      substr(FileNames,  2,  3),
                                      # Year
                                      substr(FileNames,  1,  1))) %>%
      rename(Lane  = V2,
             Store = V3,
             Day   = V4,
             Month = V5,
             Year  = V6) %>%
      ## Define data types of columns and build new column
      mutate(# Convert lane number to numeric so that irrelevant lanes can be filtered out later
            Lane  = suppressWarnings(as.numeric(Lane)),
            # Remove leading zero in store number by converting from character to numeric
            Store = suppressWarnings(as.numeric(Store)),
            # Complete the year digit from the file name
            Year  = paste0(201, Year),
            # Build a date column by concatenating day, month, and year
            Date  = as.Date(paste(Year, Month, Day, sep = "-"))) %>% 
      # Remove files with lane numbers 100+ that do not represent checkout lanes and do not perform customer transactions
      filter(Lane  < 100) %>% 
      # Exclude XML files from today ALWAYS as they are still being written in the course of the day and would be corrupted otherwise
      filter(Date  != today())
rm(FileNames)

} # End of store-folder loop

# Collapse list of identical dataframes to one dataframe
FileIndex <- ldply(StoreList, data.frame)

# Draw histogram to check if any dates (gaps in graph) or files (unusual dips or spikes) were
# not transfered from store server to R server
ggplot(FileIndex, aes(Date)) +
      geom_histogram(binwidth = 1, alpha=.5) +
      ggtitle("Transaction Files from All Stores / All Dates Transferred") +
      xlab("Check for gaps or dips to determine whether some XML files / whole dates were not transfered from the store servers") +
      ylab("Number of XML Files Transferred") +
      scale_x_date(labels = date_format("%m/%d"),
                   breaks = date_breaks("1 week")) +
      scale_y_discrete(breaks = seq(0, 20, 5)) +
      #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      facet_grid(Store ~ ., scales="free_y")

# 1b) Delete XML files that are older than one year (more exactly 52 weeks) ############################################

ReportingDate <- as.Date("2016-04-27")
ReportingDate <- Sys.Date()

# Determine data a year ago via Sys.Date() or date of past reporting (Wednes)day
YearAgoDate <- ReportingDate- (7*52); difftime(ReportingDate, YearAgoDate, units = "weeks")

# Delete the XML files (from from storage* AND dataframe**) that are OLDER than 52 weeks days 
         FileIndex_OlderThanYear    <- filter(FileIndex, Date < YearAgoDate) %>% select(Store, FileNames) %>% mutate(Store = as.character(Store))
if (nrow(FileIndex_OlderThanYear) > 0) { 
for (j in 1:length(Stores)) {
# *Loop through each store folder and delete files with the help of FileNames vector
      setwd(paste0("~/Projects/Transaction Data Analyses/1 RD/XML Transaction Files/", Stores[j]));
      file.remove(FileIndex_OlderThanYear$FileNames[FileIndex_OlderThanYear$Store ==   Stores[j]])
}
      }
      rm(FileIndex_OlderThanYear)
# ** Remove the same XML-file names from dataframe, keep the files that are YOUNGER than 52 weeks from dataframe
FileIndex                           <- filter(FileIndex, Date >= YearAgoDate)

# 1c) Prepare most recent week #########################################################################################

# Determine most recent week/s AUTOMATICALLY
WeekEndDate       <- max(FileIndex$Date)
WeekStartDate     <- WeekEndDate-6

# Determine most recent week/s MANUALLY
WeekEndDate       <- as.Date("2016-04-19")
WeekStartDate     <- as.Date("2016-04-13")

# Check if available data dates comprise 52 weeks
FirstDate         <- min(FileIndex$Date)
difftime(WeekEndDate, FirstDate-1, units = "weeks")

# Subset data from most recent week
FileIndex_MostRecentWeek <- filter(FileIndex, Date %in% seq.Date(from = WeekStartDate,
                                                                 to   = WeekEndDate,
                                                                 by   = 1))

# Draw histogram to check if any dates (gaps in graph) or files (unusual dips or spikes) were
# not transfered from store server to R server
ggplot(FileIndex_MostRecentWeek, aes(Date)) +
      geom_histogram(binwidth = 1, alpha=.5) +
      ggtitle("Transaction Files from All Stores / Most Recent (Week) Dates Transferred") +
      xlab("Check for gaps or dips to determine whether some XML files / whole dates were not transfered from the store servers") +
      ylab("Number of XML Files Transferred") +
      scale_x_date(labels = date_format("%m/%d/%y"),
                   breaks = date_breaks("1 day")) +
      scale_y_discrete(breaks = seq(0, 20, 2)) +
      #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      facet_grid(Store ~ ., scales="free_y")

# 2) Read and process XML transaction data #############################################################################
# Extract relevant information from transaction files by using the XPath XML querying language
# Reading/processing is time and memory intense

# Leftover from underlying single-store / all-weeks script, but unnessecary here
# since WeekStartDate, WeekEndDate, and WeekDates were defined and
# XML files filtered already accordingly
####
# Select just those XML files that lie within a date range of interest
#date_start        <- as.Date("2015-09-02")
#date_end          <- as.Date("2016-01-19")
#date_start        <- min(as.Date(FileIndex$Date))
#date_end          <- max(as.Date(FileIndex$Date))
#date_range        <- seq.Date(from = date_start,
#                              to   = date_end,
#                              by   = 1)
####

# 1st Loop through store folders
for (j in 1:length(Stores)) {
      
      filefolder  <- paste0("~/Projects/Transaction Data Analyses/1 RD/XML Transaction Files/", Stores[j])

files_selected    <- subset(FileIndex_MostRecentWeek, Store == as.numeric(Stores[j]))[, 1]

# Create list with length equal to the length of files to be read
       FileList   <- list()
length(FileList)  <- length(files_selected)

# 2nd Loop through each XML file WITHIN the store folder and extract the same information which will be stored as dataframe and placed in list element
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
                  
                  ## Identify transactions with "Open Ring" items that are wholesale-like transactions (and not retail-like transactions with consumers); therefore, inflate basket sizes
                  
                  # Find any node in the XML tree whose "F02" (item description) attribute value starts with "OPEN RING"
                  # (Thus, an open-ring item was rung up by the cashier) and
                  # Get the ancestor-node attribute (F1032) value, which is the number of the  transaction that contained the open-ring item
                  OpenRing_TransNo        <- xpathSApply(tree, "//*[starts-with(@F02, 'OPEN RING')]/ancestor::*[@F1032]",
                                                         xmlGetAttr, "F1032")
                  
                  ## Bind vectors and add Total Sales to basic transaction data by joining datasets
                  data_sales  <- as.data.frame(cbind(  TotalSales, TotalSales_TransNo))
                  names(data_sales) <-                    c("TotalSales",          "TransNo")
                  data        <- left_join(data, data_sales,          by = c("TransNo")) %>%
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
                  data_zip    <- as.data.frame(cbind(  ZIPCash, ZIP_TransNo))
                  names(data_zip)   <-                    c("ZIPCash",   "TransNo")
                  data        <- plyr::join(data, data_zip, by = c("TransNo"))
                  # Remove dataframe that is not needed any longer
                  #data_zip    <- NULL
                  
            } # Ends ZIP-code if-clause
      } # Ends SALE-transaction if-clause
      
      ##  Write each XML-file dataframe into the list element 
      FileList[[i]]     <- data
      
} # Ends 2nd XML-file loop

# Collapse dataframes of the list into one dataframe
StoreList[[j]]    <- plyr::ldply(FileList, data.frame)

} # Ends 1st store-folder loop

Trans             <- plyr::ldply(StoreList, data.frame)

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
      rename(CustNo = CardNumber,       # Customer number
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
             ID         = as.factor(paste(Date, StoreNo, LaneNo, TransNo, sep = "-"))) %>% 
      # Filter out return transactions (neg. sales)
      filter(TotalSales > 0)

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
                                             start.on.monday = TRUE),
                                    labels = paste(as.character(min(Trans$Date)), as.character(max(Trans$Date)), sep = " - "))) %>%
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

# 6) Import transaction data set from past weeks #######################################################################

setwd("~/Projects/Transaction Data Analyses/3 PD/1 ZIP-Code Analysis")
Trans_Past  <- readRDS( "Daily Transactions_All Stores_Recent Weeks (ZIP Codes).rds")

setwd("~/Projects/Transaction Data Analyses/3 PD/1 ZIP-Code Analysis")
Trans_Past  <- read.csv("Daily Transactions_All Stores_Recent Weeks (ZIP Codes).csv") %>% 
      mutate(ID         = factor(ID),
             StoreNo    = factor(StoreNo),
             Date       = as.Date(Date),
             Week       = factor(Week),
             CashNo     = factor(CashNo),
            #CashName   = CashName,
             ZIPCash    = factor(ZIPCash), 
            #IsZIPCash  = IsZIPCash,
             ZIPCust    = factor(ZIPCust),
            #IsZIPCust  = IsZIPCust,
             ZIP        = factor(ZIP),
            #IsZIP      = factor(IsZIP),
            #ZIPComp    = ZIPComp,
             TotalSales = as.numeric(TotalSales))

# 7) Read and process CSV ad-distribution data #########################################################################
# Contains household counts per store per ZIP code that ad is distributed to weekly
# (had been previously processed and cleaned from the original XLSX spreadsheet provided by the ad distributor)
#setwd("~/Projects/Transaction Data Analyses/1 RD/XLS Ad Distribution")
#AdDistribution <-    read.csv(list.files(), colClasses = c("factor","factor", "factor", "numeric")) %>% 
#      rename(StoreNo        = Store,
#             City           = City,
#             ZIP            = ZIP,
#             HouseholdCount = HouseholdCount) 

# 8) Create and save subsets of whole transaction data set to answer project-specific research questions ###############
# Save as CSV file that is smaller in size than the whole transaction data set in order to
# Feed manageable into further EDA or data viz

# 8a) Project 1: ZIP-Code Analysis #####################################################################################

# Does total sales and transaction count per week/day reflect other reports that are available?
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
      rbind(Trans_Past) %>% 
      unique() %>% 
      write.csv(paste0("Daily Transactions_All Stores_",
                       #length(unique(Trans$Week)),
                       "Recent Weeks",
                       #"_", min(Trans$Date), " - ", max(Trans$Date),
                       " (ZIP Codes).csv"), row.names = F)

saveRDS(Trans, "Daily Transactions_All Stores_Recent Weeks (ZIP Codes).rds")

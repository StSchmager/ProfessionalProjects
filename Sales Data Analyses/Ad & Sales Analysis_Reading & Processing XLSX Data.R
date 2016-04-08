# 0a) Pre-Settings #####################################################################################################

# Load packages
library(openxlsx)
library(xlsx)
library(plyr)
library(dplyr)
library(stringr)
library(XLConnect)

# Disable scientific notation and display UPCs correctly
options(scipen=999)

# Disable default string-to-factor conversion
options(stringsAsFactors = FALSE)

# 0b) Settings to Re-Run all past weeks in one step ####################################################################

# If all past reporting weeks need to be re-run
# 1) Run the first complete week (04-01-2015) individually. Then, ...
# 2) Enable (uncomment) code below in this section
# 3) Enable/disable code accordingly at Loop START: Set Week/s
# 4) Re-run the whole script

setwd("~/Projects/Sales Data Analyses/1 RD/7 Item Movement, Sub-department Sales & Costs")
                 Weeks <- list.files()
                 Weeks <- Weeks[Weeks != "2015-04-01"]
                 #Weeks <- Weeks[1:43] # Weeks up to Feb. 10, 2016

# STATIC DATA: Doesn't have to be looped over / repeatedly read and saved  #############################################

# 1) DATA IMPORT: SUB-DEPT. List w/ main departments ###################################################################
# Contains all sub-departments in rows with their particular main department

setwd("~/Projects/Sales Data Analyses/1 RD/1 Sub-department List")
## Read data
Departments <- xlsx::read.xlsx("Sub-department List.xlsx",
                               sheetIndex = 1, startRow = 5,
                               colIndex = c(2:3, 6, 8),
                               colClasses = c("character"),
                               header = F, row.names = NULL) %>%
## Process data
#  Rename default headers since data-source doesn't include correct headers
      rename(SubDeptNo = X2,
             SubDept   = X3,
             DeptNo    = X6,
             Dept      = X8) %>%
#  Determine data type of columns (Suppress specific warnings "NAs introduced by coercion" since they are intended)
      mutate(SubDeptNo = suppressWarnings(as.numeric(SubDeptNo)),
             DeptNo    = suppressWarnings(as.numeric(   DeptNo))) %>%
# Filter data "signal", rows with department information indicated by sub-department numbers and filter out data "noise", rows with titles, footers, page breaks, etc. (coerced to NAs) 
      filter(!is.na(SubDeptNo)) %>% 
# Factorize the department variables
      mutate(DeptNo    = factor(DeptNo),
             SubDeptNo = factor(SubDeptNo))

# Filter out duplicated rows
Departments <- filter(Departments, !duplicated(Departments))

# 2) DATA IMPORT: ITEM Price List by CATEGORY ##########################################################################

setwd("~/Projects/Sales Data Analyses/1 RD/2 Item Price List by Category")
files             <- list.files()
       FileList   <- list()
length(FileList)  <- length(files)

for (h in 1:length(files)) {
# Read containing file, subset the relevant columns and name them
FileList[[h]]    <- openxlsx::read.xlsx(files[h], colNames = F) %>% 
      select(X1:X3) %>% 
      rename(UPC        = X1,
             CategoryNo = X2,
             Category   = X3) %>% 
      # Remove the "data noise" (empty rows, titles, headers, footers, etc.) from the messy report by coercing to NAs and removing them;  Keep the meaningful "data signal" which are characterized by numeric values in UPC column.
      mutate(UPC        = suppressWarnings(as.numeric(UPC))) %>%
      filter(!is.na(UPC)) %>% 
      # Define residual columns
      mutate(Category   =        as.character(Category),
             CategoryNo = factor(as.character(CategoryNo)))
}

# Collapse dataframes of the list into one dataframe
Categories  <- ldply(FileList, data.frame)
# Filter out duplicated rows
Categories <- filter(Categories, !duplicated(Categories))

rm(files, FileList)

# 3) DATA IMPORT: STORE Transaction Count & Sub-department Sales (source is prev. project Univision 2015 project) ######

## Transaction Count 2014 and first 13 weeks of 2015 (before first reporting week of April 1, 2015)
setwd("~/Projects/Sales Data Analyses/1 RD/3 Store Customer Counts, Sales by Sub-department & Store 2014-15")
StoreTransCount2014 <- read.csv("Customer Count per Store 2014-15 (weekly).csv", colClasses = c("character")) %>% 
      # Define and select columns
      mutate(Week              = as.Date(Week, format = "%m/%d/%Y"),
             Store             = factor(Store),
             StoreTransactions = round(as.numeric(CustomerCount), 0)) %>% 
      select(Week, Store, StoreTransactions)

## Sales by Sub-department 2014 and first 13 weeks of 2015 (before April 1, 2015)
setwd("~/Projects/Sales Data Analyses/1 RD/3 Store Customer Counts, Sales by Sub-department & Store 2014-15")
OverallSubDepts2014 <- read.csv("Overall Sales per Store, Sub-, Dept 2014-15 (weekly).csv", colClasses = c("character")) %>% 
      # Define columns and exclude rows with irrelevant sub-departments
      mutate(Store                = factor(Store),
             Week                 = as.Date(Week, format = "%m/%d/%Y"),
             SubDeptOverallSales  = round(as.numeric(Sales), 2),
             Sales                = NULL,
             # Data not contained in document, but columns necessary for binding later
             SubDeptOverallCosts  = as.numeric(NA),
             SubDeptOverallProfit = as.numeric(NA),
             SubDeptOverallMargin = as.numeric(NA),
             SubDeptOverallMarkup = as.numeric(NA)) %>% 
      filter(Dept != "PROMOTION") %>% 
      filter(Dept != "GIFT CARDS") %>% 
      filter(Dept != "OTHER") %>% 
      filter(Dept != "SUPPLIES") %>% 
      filter(Dept != "CHICAGO WATER TAX") %>% 
      filter(Dept != "COUNTY LIQUOR TAX") %>% 
      filter(Dept != "CHICAGO LIQUOR TAX") %>% 
      # Enhance the 2014 sales data by joining in SubDept & Dept (Department-related data is complemented: sub- and department descriptions numbers)
      join(Departments, by = c("Dept", "SubDept")) %>% 
      select(Week, Store,
             SubDept, Dept, SubDeptNo, DeptNo, 
             SubDeptOverallSales, SubDeptOverallCosts, SubDeptOverallProfit, SubDeptOverallMargin, SubDeptOverallMarkup)

# Filter out duplicated rows
OverallSubDepts2014 <- filter(OverallSubDepts2014, !duplicated(OverallSubDepts2014))

# DYNAMIC DATA: Read most recent data or Loop through all past weeks  ##################################################

# +++++ Processing Timer +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#timer_begin <- Sys.time()
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Re-run ALL past weeks
#for (i in 1:length(Weeks)) {
#      Week <- as.character(Weeks[i])
      
      ## Run ONE PARTICULAR week (for example, first week: 2015-04-01)
      #Week <- as.character("2015-04-01")
      
      ## Run the MOST RECENT week
      Week <- Weeks[length(Weeks)]

# 4) DATA IMPORT: AD-ITEM Price List with Cost #########################################################################

# Set folder, then list containing files, and create dataframe list with length that equals the number of files (one dataframe per file)
# Each folder within the following directories represents one sales week; within the folders, files represent store data
setwd(paste0("~/Projects/Sales Data Analyses/1 RD/4 Ad-Item Price List with Cost/", Week))
files             <- list.files()
       FileList   <- list()
length(FileList)  <- length(files)

# Loop through each file from folder and read information as dataframe into the list
for (j in 1:length(files)) {
      # Read complete raw data; it is skewed and will be re-organized in the few following steps
      dat0              <- openxlsx::read.xlsx(files[j], sheet = 1, colNames = F)
      # Extract store no. from report, for example: "Target: 001" ("Target: " is cut off so that store number remains)
      Store <- substring(dat0[4, 1], 8)
      
      # Subset rows that do NOT have a price quantity (e.g. $0.68) listed and are, therefore, NOT skewed; there's NO data (NA) in the last column no. 15
      dat1              <- select(dat0[which( is.na(dat0$X15)),], c(X1:X3, X6:X7, X9 , X14))
      
      # Subset rows that DO have a price quantity (e.g. 2/$0.98) and are, therefore, skewed; there IS data, thus, no NA in last column no. 15
      dat2              <- select(dat0[which(!is.na(dat0$X15)),], c(X1:X3, X7:X8, X10, X15))
      
      # Unify column names, so rows can be bound                    X1:3,  X4:5,  X6,  X7 (in collapsed dataframe below)
      colnames(dat1)    <- c(1:7)
      colnames(dat2)    <- c(1:7)
      
      # Subsets are bound by rows and raw data is straightened
      dat               <- rbind(dat1, dat2)
      
      # Add store no. (from spreadsheet) and week ID (which is not contained in spreadsheet)
      FileList[[j]]     <- cbind(Week, # Parameter that was set at the very beginning of script
                                 Store,
                                 dat)
}

# Collapse dataframes of the list into one dataframe
AdItems  <- ldply(FileList, data.frame) %>% 
      # Name columns
      rename(UPC                 = X1,
             Product             = X2,
             SubDept             = X3,
             
             ActivePriceQuantity = X4,
             ActivePrice         = X5,
             
             PriceType           = X6,
             UnitCostLOC         = X7) %>% 
      # Remove the "data noise" (empty rows, titles, headers, footers, etc.) from the messy report by coercing to NAs and removing them; Keep the meaningful "data signal" which are characterized by numeric values in UPC column.
      mutate(UPC                 = suppressWarnings(as.numeric(UPC))) %>% 
      filter(!is.na(UPC)) %>% 
      # Define residual columns after UPC was already defined as numeric
      mutate(Week                = as.Date(Week, format = "%Y-%m-%d"),
             Store               = factor(as.character(suppressWarnings(as.numeric(Store))),
                                          levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
             ActivePriceQuantity = suppressWarnings(as.numeric(ActivePriceQuantity)),
             ActivePrice         = round(suppressWarnings(as.numeric(ActivePrice)), 2)) %>% 
      # Keep the rows with items that are have a UPC (remove those that have an NA in UPC column) and are on sale only (have a sales price)
      filter(!is.na(UPC),
             PriceType == "SALE") %>% 
      # Re-factorize price type w/ just one level "SALE"
      mutate(PriceType = factor(PriceType))

## Before more item measures are computed
# Some unit costs from report (automatically recognized as numeric) need to be replaced by NA since they amount to $0.00 
AdItems$UnitCostLOC[AdItems$UnitCostLOC == 0]                     <- NA
# Active price quantities of 1 from report are NA / not specifically specified (e.g. $0.68 instead of 1/$0.68) and must therefore by 1
AdItems$ActivePriceQuantity[is.na(AdItems$ActivePriceQuantity)]   <- 1
## Compute the unit price
AdItems$UnitPrice <- round(AdItems$ActivePrice/AdItems$ActivePriceQuantity, 2)

# Create a price label that reflects the appearance in the ad paper (e.g. $0.99 when the price quantity is equal to 1 or 10/$10 when the price quantity is not equal to 1
AdItems$PriceLabel[AdItems$ActivePriceQuantity == 1] <- paste0(                                                                              "$", as.character(AdItems$ActivePrice[AdItems$ActivePriceQuantity == 1]))
AdItems$PriceLabel[AdItems$ActivePriceQuantity != 1] <- paste0(as.character(AdItems$ActivePriceQuantity[AdItems$ActivePriceQuantity != 1]), "/$", as.character(AdItems$ActivePrice[AdItems$ActivePriceQuantity != 1]))

# Sort dataframe columns logically and remove variables that are not necessary anymore
AdItems <- select(AdItems,
                  Week, Store,
                  UPC, Product, UnitPrice, PriceLabel, PriceType, UnitCostLOC, SubDept) %>% 
      # Enhance the price list in joining with Sub-Dept. List by SubDept (Department-related data is complemented: sub- and department descriptions and numbers)
      join(Departments, by = "SubDept") %>% 
      # Enhance the original price list in joining it with this dataframe by UPC (# )Category-related data is complemented: category number and description)
      join(Categories,  by = "UPC") %>% 
      # Re-factorize the category variable to refresh total number of factors
      mutate(CategoryNo = factor(CategoryNo))

# Group items as advertised in the ad paper (grouping variable consists of price label, category description, and sub-dept.
AdItems$AdItemGroup     <- paste(AdItems$PriceLabel,AdItems$Category, paste0("(",AdItems$SubDept,")"))
AdItems$AdItemGroup     <- factor(AdItems$AdItemGroup)

# Remove data objects that are not needed anymore
rm(FileList, files, dat0, Store, dat1, dat2, dat)

# Filter out duplicated rows
AdItems <- filter(AdItems, !duplicated(AdItems))

# 5a) DATA IMPORT: ITEM Scans & Costs from FRITO LAY ###################################################################

# Index all Excel files from the folder and list them by name
setwd("~/Projects/Sales Data Analyses/1 RD/5 Item Scans & Costs/Frito Lay")
files <- list.files()

# Create list that in length equals the list of Excel files
       FileList  <- list()
length(FileList) <- length(files)

# Loop through each Excel file at a time
for (k in 1:length(files)) {
      
      Workbook    <- XLConnect::loadWorkbook(files[k])
      SheetList   <- XLConnect::readWorksheet(Workbook, sheet = XLConnect::getSheets(Workbook), header = F)
      
      for (l in 1:length(SheetList)) {
            
            SheetList[[l]] <- cbind(SheetList[[l]][ 4,   8], 
                                    # Excel columns D,   H
                                    # Data          UPC, Unit Cost
                                    SheetList[[l]][,c(4, 8)])  
                                    # Excel cell      H  4
                                    # Data            Week
      }
      
      FileList[[k]]     <- ldply(SheetList, data.frame)
      
}

FritoLay                <- ldply(FileList, data.frame) %>% 
      rename(Week       = SheetList..l...4..8.,
             UPC        = Col4,
             UnitCostFL = Col8) %>% 
      # Remove hyphen (replace punctuation & symbols with empty space)
      mutate(UPC        = str_replace_all(UPC, "[[:punct:]]", "")) %>% 
      # Remove the "data noise" (empty rows, titles, headers, footers, etc.) from the messy report by coercing to NAs and removing them; Keep the meaningful "data signal" which are characterized by numeric values in UPC and UnitCostFL column.
      mutate(UPC        = suppressWarnings(as.numeric(UPC)),
             UnitCostFL = suppressWarnings(as.numeric(sub('\\$', '', UnitCostFL)))) %>% 
      filter(!is.na(UnitCostFL)) %>% 
      filter(!is.na(UPC)) %>% 
      filter(Week != "EDLP") %>% 
      mutate(Week  = as.Date(Week)) %>% 
      select(Week, UPC, UnitCostFL)

# Filter out duplicated rows
FritoLay <- filter(FritoLay, !duplicated(FritoLay))

# 5b) DATA IMPORT: ITEM Scans & Costs from NABISCO ###################################################################

setwd("~/Projects/Sales Data Analyses/1 RD/5 Item Scans & Costs/Nabisco")
files             <- list.files()
       FileList   <- list()
length(FileList)  <- length(files)

for (m in 1:length(files)) {
      FileList[[m]]     <- openxlsx::read.xlsx(files[m], colNames = F,
                                               sheet = 1,
                                               cols = c(1,   8,    13,   14), detectDates = TRUE)   
      # Excel columns                                   A,   H,    M,    N
      # Data                                            UPC, cost, week, scan amount
}

Nabisco                 <- ldply(FileList, data.frame) %>% 
      rename(UPC      = X1,
             UnitCost = X2,
             Week     = X3,
             Scan     = X4) %>% 
      # Remove space (replace with empty space)
      mutate(UPC      = str_replace_all(UPC, "[[:blank:]]", "")) %>% 
      # Remove the "data noise" (empty rows, titles, headers, footers, etc.) from the messy report by coercing to NAs and removing them; Keep the meaningful "data signal" which are characterized by numeric values in UPC column.
      mutate(UPC      = suppressWarnings(as.numeric(UPC)),
             UnitCost = suppressWarnings(as.numeric(UnitCost)),
             Scan     = suppressWarnings(as.numeric(Scan))) %>% 
      filter(!is.na(UPC),
             !is.na(UnitCost),
             !is.na(Scan)) %>% 
      mutate(UnitCostNabisco = UnitCost-Scan) %>% 
      select(Week, UPC, UnitCostNabisco)

# Filter out duplicated rows
Nabisco <- filter(Nabisco, !duplicated(Nabisco))

# 5c) DATA IMPORT: ITEM Scans & Costs from CENTRELLA ###################################################################

# Set folder and read containing file
setwd(paste0("~/Projects/Sales Data Analyses/1 RD/5 Item Scans & Costs/Centrella/", Week))
Central                 <- xlsx::read.xlsx(list.files(), sheetIndex = 1,
                                           startRow = 3, colIndex = c(6, 25),
                                           # Excel columns            F, Y
                                           row.names = NULL, header = F) %>% 
      # Name and define columns
      rename(UPC             = X6,
             UnitCostCentral = X25) %>% 
      # Remove the "data noise" (empty rows, titles, headers, footers, etc.) from the messy report by coercing to NAs and removing them; Keep the meaningful "data signal" which are characterized by numeric values in UPC and UnitCost column.
      mutate(UPC             = suppressWarnings(as.numeric(UPC)),
             UnitCostCentral = round(as.numeric(UnitCostCentral), 2)) %>% 
      filter(!is.na(UPC)) %>% 
      filter(!is.na(UnitCostCentral))

# Filter out duplicated rows
Central <- filter(Central, !duplicated(Central))

# 5c) DATA PROCESSING: Determine Unit Costs ############################################################################

# Determine final unit costs and source of cost information

AdItems                 <- join(AdItems, Central,  by =   "UPC") %>% 
      join(FritoLay, by = c("UPC", "Week")) %>% 
      join(Nabisco,  by = c("UPC", "Week")) %>%
      mutate(UnitCost       = UnitCostLOC,
                 CostSource = "LOC")

# If FritoLay costs are available, overwrite cost information from LOC
AdItems$UnitCost  [which(!is.na(AdItems$UnitCostFL))]       <- AdItems$UnitCostFL         [which(!is.na(AdItems$UnitCostFL))]
AdItems$CostSource[which(!is.na(AdItems$UnitCostFL))]       <- "FritoLay"

# If Nabisco costs are available, overwrite cost information from LOC
AdItems$UnitCost  [which(!is.na(AdItems$UnitCostNabisco))]  <- AdItems$UnitCostNabisco    [which(!is.na(AdItems$UnitCostNabisco))]
AdItems$CostSource[which(!is.na(AdItems$UnitCostNabisco))]  <- "Nabisco"

# If Centrella costs are available, overwrite cost information from LOC
AdItems$UnitCost  [which(!is.na(AdItems$UnitCostCentral))]  <- AdItems$UnitCostCentral    [which(!is.na(AdItems$UnitCostCentral))]
AdItems$CostSource[which(!is.na(AdItems$UnitCostCentral))]  <- "Central"

AdItems$CostSource <- factor(AdItems$CostSource)

# Set unit costs of all hot-foods items to NA

AdItems$UnitCost[AdItems$Dept == "DELI HOT"]                <- as.numeric(NA)

# 6a) DATA IMPORT: STORE Customer Counts ################################################################################

# Set folder, list its containing files, create dataframe-list with length that equals the number of files
# Each folder within the following directories represents one sales week; within the folders, files contain store data
setwd(paste0("~/Projects/Sales Data Analyses/1 RD/6 Store Customer Counts/", Week))
files             <- list.files()
       FileList   <- list()
length(FileList)  <- length(files)

# Loop through each file from folder and read information in the list of dataframes
for (n in 1:length(files)) {
      dat <- openxlsx::read.xlsx(files[n], colNames = F)
      
      FileList[[n]]     <- cbind(dat[2,2], 					# Week in the header
      # Excel cell                   B 2
                                 substring(dat[nrow(dat),1], 22), 	# Store in the footer (last row of document)
                                 dat[, c(5,    7,          8)])         # Hour(ly) Transactions & Sales in the columns
      # Excel columns                    E,    G,          H
      # Data                             Hrs., Trs. Count, Sales
}

# Collapse elements/dataframes of the list into one dataframe
StoreTransCount               <- ldply(FileList, data.frame) %>% 
      # Name columns
      rename(Week         = dat.2..2.,
             Store        = substring.dat.nrow.dat...1...22.,
             Hour         = X5,
             Transactions = X7,
             Sales        = X8) %>% 
      ## Define columns
      mutate(Week  = as.Date(as.character(Week), format = "%m/%d/%Y"),
             Store = factor(as.character(Store),
                            levels = c("5724 KEDZIE", "4700 KEDZIE", "4343 PULASKI", "5838 PULASKI", "118TH ST", "2526 CERMAK", "7 SIBLEY", "3720 W", "OAK BROOK TERRACE", "MADISON", "BRIDGEVIEW", "OAK PARK"),
                            labels = c("1",           "2",           "3",            "4",            "5",        "6",           "7",        "8",      "9",                 "10",      "11",         "12")),
             # Remove the "data noise" (empty rows, titles, headers, footers, etc.) from the messy report by coercing to NAs and removing them; Keep the meaningful "data signal" which are characterized by numeric values in Hour column.
             Hour         = suppressWarnings(as.numeric(Hour)),
             Transactions = suppressWarnings(as.numeric(Transactions)),
             Sales        = round(suppressWarnings(as.numeric(Sales)), 2)) %>% 
      filter(!is.na(Hour)) %>% 
      # Aggregate store transaction count and from hourly to overall
      group_by(Week, Store) %>% 
      summarize(StoreTransactions = sum(Transactions, na.rm = T)) %>% 
      as.data.frame()

# Remove list, vector of files, and index as they are not needed anymore
rm(FileList, files, dat)

# 6b) DATA EXPORT: STORE Customer Counts - Bind past weeks with current one ############################################

# Skip reading data from the PAST week if the currently processed week is the very FIRST week
if (Week != "2015-04-01") {

# Load processed data from past weeks
setwd("~/Projects/Sales Data Analyses/3 PD/1 Ad & Sales Reporting")
StoreTransCountPastWeeks <- read.csv("0 Store Transcount.csv", colClasses = c("character")) %>% 
      # Match column data types
      mutate(Week              = as.Date(Week),
             Store             = factor(Store),
             StoreTransactions = round(as.numeric(StoreTransactions), 0))

# Bind current week with past weeks and weeks from past year
StoreTransCount <- rbind(StoreTransCount,
                         StoreTransCountPastWeeks,
                         StoreTransCount2014)
} # end of if-clause (Skip reading data from the PAST week if the currently processed week is the very FIRST week)

# Remove duplicate that exist due row-binding of weeks from 2014
StoreTransCount <- unique(StoreTransCount)

# Save
setwd("~/Projects/Sales Data Analyses/3 PD/1 Ad & Sales Reporting")
write.csv(StoreTransCount, "0 Store Transcount.csv", row.names = F)

# Aggregate store transaction count to company transaction count
CompanyTransCount <- as.data.frame(summarize(group_by(StoreTransCount, Week),
                                             CompanyTransactions = sum(StoreTransactions, na.rm = T)))

# 7a) DATA IMPORT: ITEM Movement & SUB-DEPT. Sales & Costs #############################################################
# Set folder, list containing files, create two dataframe-list with length that equals the number of files
# Each folder within the following directories represents one sales week; within the folders, files contain store data
setwd(paste0("~/Projects/Sales Data Analyses/1 RD/7 Item Movement, Sub-department Sales & Costs/", Week))
files             <- list.files()

# 1st dataframe-list for overall item movement
       FileList1  <- list()
length(FileList1) <- length(files)

# 2nd dataframe-list for overall sub-department sales and costs
       FileList2  <- list()
length(FileList2) <- length(files)

# Loop through the files

for (o in 1:length(files)) {
      
      # Read complete raw data from Excel report
      dat                           <- openxlsx::read.xlsx(files[o], colNames = F)
      
      ## ITEM Movement & Sales
      
      # Extract data of interest from certain cells and columns of the report
      FileList1[[o]]                <- cbind(dat[2,2],                           # Week in the header
      # Excel cell                               B 2
                                             substring(dat[nrow(dat),1], 22),    # Store in the footer (last row of document)
                                             dat[,c(1:3, 6:7)])                  # UPC, Product, Sub-Dept, Movement, Sales
      # Excel columns                               A:C, G:H
      # 5th column E in Excel report is enitrely empty and, therefore, skipped by reading function
      # Empty column shifts following columns: Excel columns G:H are dataframe columns 6:7
      
      # Name columns
      colnames(FileList1[[o]])      <- c("Week",                                            # B2
                                         "Store",                                           # Foother
                                         "UPC", "Product", "SubDept", "UnitsSold", "Sales") # A:C, G:H
      
      ## SUB-DEPT. Sales & Costs
      
      FileList2[[o]]                <- cbind(dat[2,2],                           
                                             substring(dat[nrow(dat),1], 22),    
                                             dat[,c(1, 3, 7:8)])    
      # Excel columns                               A, C, H:I
      # 5th column E in Excel report is entirely empty and, therefore, skipped by reading function
      # Missing column shifts following columns: Excel columns H:o / 8:9 are 7:8 in dataframe
      
      colnames(FileList2[[o]])      <- c("Week",                                                           # B2
                                         "Store",                                                          # Footer
                                         "Filter", "SubDept", "SubDeptOverallSales","SubDeptOverallCosts") # A, C, H:I
                                       # *Filter to find sub-dept. related numbers
           
      
      # Filter rows with sub-department sales and costs that is characterized by the term "Total" in 1st column
      FileList2[[o]]$Filter   <- as.character(FileList2[[o]]$Filter)
      FileList2[[o]]          <- FileList2[[o]][FileList2[[o]]$Filter == "Total",]
      }

# Collapse elements/dataframes of the list into one dataframe
OverallItemMovement           <- ldply(FileList1, data.frame)
# Collapse elements/dataframes of the list into one dataframe
OverallSubDepts               <- ldply(FileList2, data.frame)

# Remove data objects as they are not needed anymore
rm(FileList1, FileList2, files, dat)

# 7b) DATA PROCESSING & EXPORT: ITEM Movement (Marketing Mix Models) ###################################################

# Collapse elements/dataframes of the list into one dataframe
OverallItemMovement_MMM <- mutate(OverallItemMovement,
                                   Week      = as.Date(as.character(Week), format = "%m/%d/%Y"),
                                   Store     = factor(as.character(Store),
                                                      levels = c("5724 KEDZIE", "4700 KEDZIE", "4343 PULASKI", "5838 PULASKI", "118TH ST", "2526 CERMAK", "7 SIBLEY", "3720 W", "OAK BROOK TERRACE", "MADISON", "BRIDGEVIEW", "OAK PARK"),
                                                      labels = c("1",           "2",           "3",            "4",            "5",        "6",           "7",        "8",      "9",                 "10",      "11",         "12")),
                                   UPC       = suppressWarnings(as.numeric(UPC)),
                                   UnitsSold = round(suppressWarnings(as.numeric(UnitsSold)), 2),
                                   Sales     = round(suppressWarnings(as.numeric(Sales)), 2),
                                   AvgPrice  = round(Sales/UnitsSold, 2)) %>%
      # Select data "signal", rows with product information indicated by UPC and; Filter out rows with "data noise" such as titles, footers, page breaks, etc. (coerced to NAs) 
      filter(!is.na(UPC)) %>%
      # Join sub-department and ad-item data and weave in additional columns
      left_join(Departments,
                by =   "SubDept") %>%
      left_join(select(AdItems,
                        Week,   Store,   UPC, UnitPrice, PriceLabel, PriceType),
                by = c("Week", "Store", "UPC")) %>%
      # Select, sort data columns of interest including the ones just joined in (see below)
      select(Week, Store,
             Dept, SubDept,                    # sub-department list
             UPC, Product,
             PriceType, PriceLabel, UnitPrice, # ad-item information
             AvgPrice, Sales, UnitsSold) %>% 
      mutate(PriceType = as.character(PriceType))

# Label all items that were not included in the ad-item list accordingly
OverallItemMovement_MMM$PriceType[is.na(OverallItemMovement_MMM$PriceType)] <- "NON-SALE"
OverallItemMovement_MMM$PriceType                                           <- factor(OverallItemMovement_MMM$PriceType)

# Filter out duplicated rows
OverallItemMovement_MMM <- filter(OverallItemMovement_MMM, !duplicated(OverallItemMovement_MMM))

# Subset item-movement data by (sub-)department and save as CSV file of manageable size to be read into TABLEAU visualizaton program
setwd("~/Projects/Sales Data Analyses/3 PD/2 Marketing Mix Models")

# Skip reading data from the PAST week if the currently processed week is the very FIRST week
if (Week != "2015-04-01") {
      
# Bind with past weeks' data
OverallItemMovement_MMM <- rbind(OverallItemMovement_MMM,
                                 readRDS("ALL DEPT. Item Movement & Price Points Weekly per Store.rds"))
} # end of if-clause (Skip reading data from the PAST week if the currently processed week is the very FIRST week)

# Remove duplicated rows that have built up in the past
#OverallItemMovement_MMM <- OverallItemMovement_MMM[!duplicated(OverallItemMovement_MMM)]

filter(OverallItemMovement_MMM, Dept == "PRODUCE")  %>%     write.csv("PRODUCE Item Movement & Price Points Weekly per Store.csv",  row.names = F)
filter(OverallItemMovement_MMM, Dept == "DELI")     %>%     write.csv("DELI Item Movement & Price Points Weekly per Store.csv",     row.names = F)
filter(OverallItemMovement_MMM, Dept == "BAKERY")   %>%     write.csv("BAKERY Item Movement & Price Points Weekly per Store.csv",   row.names = F)
filter(OverallItemMovement_MMM, Dept == "FLORAL")   %>%     write.csv("FLORAL Item Movement & Price Points Weekly per Store.csv",   row.names = F)
filter(OverallItemMovement_MMM, Dept == "DELI HOT") %>%     write.csv("DELI HOT Item Movement & Price Points Weekly per Store.csv", row.names = F)
filter(OverallItemMovement_MMM, Dept == "LIQUOR")   %>%     write.csv("LIQUOR Item Movement & Price Points Weekly per Store.csv",   row.names = F)
filter(OverallItemMovement_MMM, Dept == "FROZEN")   %>%     write.csv("FROZEN Item Movement & Price Points Weekly per Store.csv",   row.names = F)
filter(OverallItemMovement_MMM, Dept == "DAIRY")    %>%     write.csv("DAIRY Item Movement & Price Points Weekly per Store.csv",    row.names = F)
filter(OverallItemMovement_MMM, Dept == "DAIRY")    %>%     write.csv("DAIRY Item Movement & Price Points Weekly per Store.csv",    row.names = F)

filter(OverallItemMovement_MMM, Dept == "GROCERY" & SubDept == "GROCERY") %>%
                                                            write.csv("GROCERY-GROCERY Item Movement & Price Points Weekly per Store.csv", row.names = F)
filter(OverallItemMovement_MMM, Dept == "GROCERY" & SubDept == "BEVERAGE") %>%
                                                            write.csv("GROCERY-BEVERAGE Item Movement & Price Points Weekly per Store.csv", row.names = F)
filter(OverallItemMovement_MMM, Dept == "GROCERY" & SubDept == "NON FOOD") %>%
                                                            write.csv("GROCERY-NON FOOD Item Movement & Price Points Weekly per Store.csv", row.names = F)

saveRDS(OverallItemMovement_MMM, "ALL DEPT. Item Movement & Price Points Weekly per Store.rds")

# 7c) DATA PROCESSING: ITEM Movement (Ad & Sales Reporting) ###########################################################

# Select columns of interest for ad & sales analysis
OverallItemMovement <- select(OverallItemMovement,
                              Week, Store, UPC, UnitsSold) %>% 
      # Remove the "data noise" (empty rows, titles, headers, footers, etc.) from the messy report by coercing to NAs and removing them; Keep the meaningful "data signal" which are characterized by numeric values in UPC column.
      mutate(UPC       = suppressWarnings(as.numeric(UPC)),
             UnitsSold = round(as.numeric(UnitsSold), 2)) %>% 
      filter(!is.na(UPC)) %>% 
      # Define residual columns
      mutate(Week      = as.Date(Week, format = "%m/%d/%Y"),
             Store     = factor(Store,
                                levels = c("5724 KEDZIE", "4700 KEDZIE", "4343 PULASKI", "5838 PULASKI", "118TH ST", "2526 CERMAK", "7 SIBLEY", "3720 W", "OAK BROOK TERRACE", "MADISON", "BRIDGEVIEW", "OAK PARK"),
                                labels = c("1",           "2",           "3",            "4",            "5",        "6",           "7",        "8",      "9",                 "10",      "11",         "12")))

# Filter out duplicated rows
OverallItemMovement <- filter(OverallItemMovement, !duplicated(OverallItemMovement))

# Join movement (units sold) with ad items
AdItems                       <- join(AdItems,
                                      OverallItemMovement, by = c("Week", "Store", "UPC")) %>% 
      ## Calculate sales and profitability metrics for each row per AD ITEM
      mutate(AdItemSales  = round(UnitsSold*UnitPrice,      2),
             AdItemCosts  = round(UnitsSold*UnitCost,       2),
             AdItemProfit = round(AdItemSales-AdItemCosts,  2),
             AdItemMargin = round(AdItemProfit/AdItemSales, 4),
             AdItemMarkup = round(AdItemProfit/AdItemCosts, 4)) %>% 
      # Select and order columns (remove unnecessary columns)
      select(Week, Store, UPC, Product,                                             # Which AD items were sold when and where?
             AdItemGroup, Category, CategoryNo, SubDept, SubDeptNo, Dept, DeptNo,   # How can the items be grouped?
             UnitsSold, UnitPrice, UnitCost, CostSource,                            # Primary information from the ad report?
             AdItemSales, AdItemCosts, AdItemProfit, AdItemMargin, AdItemMarkup,    # How did the items perform?
             PriceLabel, PriceType) %>%                                             # How where ad items labeled?
      # Order rows of dataframe by columns
      arrange(Week, Store, AdItemGroup, UPC) %>% 
      # Removes rows with NA UPC that may have been created along the way
      filter(!is.na(UPC))

AdItems <- filter(AdItems, !duplicated(AdItems))

# 7d) DATA IMPORT: SUB-DEPT. Sales & Costs (Overall) ###################################################################

# Remove filter column as not needed anymore
OverallSubDepts <- select(OverallSubDepts, -Filter) %>% 
      # Define columns
      mutate(Week  = as.Date(Week, format = "%m/%d/%Y"),
             Store = factor(OverallSubDepts$Store,
                            levels = c("5724 KEDZIE", "4700 KEDZIE", "4343 PULASKI", "5838 PULASKI", "118TH ST", "2526 CERMAK", "7 SIBLEY", "3720 W", "OAK BROOK TERRACE", "MADISON", "BRIDGEVIEW", "OAK PARK"),
                            labels = c("1",           "2",           "3",            "4",            "5",        "6",           "7",        "8",      "9",                 "10",      "11",         "12"))) %>% 
      # Remove rows of straigt NAs that has been created along the way
      filter(complete.cases(OverallSubDepts)) %>% 
      # Complement dataframe with department-related information from sub-department list: descriptions and numbers
      join(Departments, by = "SubDept") %>% 
      # Re-factorize the department variables to refresh total number of factors
      mutate(SubDeptNo = factor(SubDeptNo),
             DeptNo    = factor(DeptNo)) %>% 
      # Calculate sales and profitability metrics for each sub-dept. 
      mutate(SubDeptOverallSales  = round(as.numeric(SubDeptOverallSales), 2),
             SubDeptOverallCosts  = round(as.numeric(SubDeptOverallCosts), 2),
             SubDeptOverallProfit = round(SubDeptOverallSales-SubDeptOverallCosts, 2),
             SubDeptOverallMargin = round(SubDeptOverallProfit/SubDeptOverallSales, 4),
             SubDeptOverallMarkup = round(SubDeptOverallProfit/SubDeptOverallCosts, 4)) %>% 
      # Remove sub-departments with not relevance for ad reporting
      filter(Dept != "PROMOTION") %>% 
      filter(Dept != "BOTTLE DEPOSIT") %>% 
      filter(Dept != "GIFT CARDS") %>% 
      filter(Dept != "OTHER") %>% 
      filter(Dept != "SUPPLIES") %>% 
      filter(Dept != "CHICAGO WATER TAX") %>% 
      filter(Dept != "COUNTY LIQUOR TAX") %>% 
      filter(Dept != "CHICAGO LIQUOR TAX")

# Set costs of hot-foods department to NA
OverallSubDepts$SubDeptOverallCosts[OverallSubDepts$Dept == "DELI HOT"] <- NA

# Filter out duplicated rows
OverallSubDepts <- filter(OverallSubDepts, !duplicated(OverallSubDepts))

# 8) DATA EXPORT: Bind past weeks with current one (Overall SUB-DEPT.) #################################################

# Skip reading data from the PAST week if the currently processed week is the very FIRST week
if (Week != "2015-04-01") {
      
# Load processed data from past weeks
setwd("~/Projects/Sales Data Analyses/3 PD/1 Ad & Sales Reporting")
OverallSubDeptsPastWeeks <- read.csv("0 Overall Sub-Depts.csv", colClasses = c("character")) %>% 
      # Define columns exactly as they are defined before
      mutate(Week                 = as.Date(Week),
             Store                = factor(Store),
             SubDeptOverallSales  = round(as.numeric(SubDeptOverallSales), 2),
             SubDeptOverallCosts  = round(as.numeric(SubDeptOverallCosts), 2),
             SubDeptNo            = factor(SubDeptNo),
             DeptNo               = factor(DeptNo),
             SubDeptOverallProfit = round(as.numeric(SubDeptOverallProfit), 2),
             SubDeptOverallMargin = round(as.numeric(SubDeptOverallMargin), 4),
             SubDeptOverallMarkup = round(as.numeric(SubDeptOverallMarkup), 4))

# Bind current week with past weeks and weeks from past year
OverallSubDepts <- rbind(OverallSubDepts,
                         OverallSubDeptsPastWeeks,
                         OverallSubDepts2014)
} # end of if-clause (Skip reading data from the PAST week if the currently processed week is the very FIRST week)

# Remove duplicate that exist due row-binding of weeks from 2014
OverallSubDepts <- unique(OverallSubDepts)

# Save
setwd("~/Projects/Sales Data Analyses/3 PD/1 Ad & Sales Reporting")
write.csv(OverallSubDepts, "0 Overall Sub-Depts.csv", row.names = F)

# 9) DATA PROCESSING: SUB-DEPT Sales & Costs (Aggregations) ############################################################

# Sub-Department (ACROSS Stores)
OverallSubDepts2 <- as.data.frame(summarize(group_by(OverallSubDepts, Week, Dept, SubDept),
                                            SubDeptOverallSales = sum(SubDeptOverallSales, na.rm = T),
                                            SubDeptOverallCosts = sum(SubDeptOverallCosts, na.rm = T))) %>% 
      mutate(SubDeptOverallProfit = SubDeptOverallSales- SubDeptOverallCosts,
             SubDeptOverallMargin = SubDeptOverallProfit/SubDeptOverallSales,
             SubDeptOverallMarkup = SubDeptOverallProfit/SubDeptOverallCosts,
             Dept                 = factor(Dept),
             SubDept              = factor(SubDept))

OverallSubDepts2$SubDeptOverallCosts[OverallSubDepts2$Dept == "DELI HOT"] <- as.numeric(NA)

# Department
OverallDepts <- as.data.frame(summarize(group_by(OverallSubDepts, Week, Store, Dept),
                                        DeptOverallSales = sum(SubDeptOverallSales, na.rm = T),
                                        DeptOverallCosts = sum(SubDeptOverallCosts, na.rm = T))) %>% 
      mutate(DeptOverallProfit = DeptOverallSales- DeptOverallCosts,
             DeptOverallMargin = DeptOverallProfit/DeptOverallSales,
             DeptOverallMarkup = DeptOverallProfit/DeptOverallCosts,
             Store             = factor(Store),
             Dept              = factor(Dept))

OverallDepts$DeptOverallCosts[OverallDepts$Dept == "DELI HOT"] <- as.numeric(NA)

# Department (ACROSS Stores)
OverallDepts2 <- as.data.frame(summarize(group_by(OverallSubDepts, Week, Dept),
                                         DeptOverallSales = sum(SubDeptOverallSales, na.rm = T),
                                         DeptOverallCosts = sum(SubDeptOverallCosts, na.rm = T))) %>% 
      mutate(DeptOverallProfit = DeptOverallSales- DeptOverallCosts,
             DeptOverallMargin = DeptOverallProfit/DeptOverallSales,
             DeptOverallMarkup = DeptOverallProfit/DeptOverallCosts,
             Dept              = factor(Dept))

OverallDepts2$DeptOverallCosts[OverallDepts2$Dept == "DELI HOT"] <- as.numeric(NA)

# Stores
OverallStores <- as.data.frame(summarize(group_by(subset(OverallSubDepts, Dept != "DELI HOT"), Week, Store),
                                         StoreOverallSales = sum(SubDeptOverallSales, na.rm = T),
                                         StoreOverallCosts = sum(SubDeptOverallCosts, na.rm = T))) %>% 
      mutate(StoreOverallProfit = StoreOverallSales- StoreOverallCosts,
             StoreOverallMargin = StoreOverallProfit/StoreOverallSales,
             StoreOverallMarkup = StoreOverallProfit/StoreOverallCosts,
             Store              = factor(Store)) %>% 
      join(  StoreTransCount, by = c("Week", "Store"))

# Company
OverallCompany <- as.data.frame(summarize(group_by(OverallSubDepts, Week),
                                          CompanyOverallSales = sum(SubDeptOverallSales, na.rm = T),
                                          CompanyOverallCosts = sum(SubDeptOverallCosts, na.rm = T))) %>% 
      mutate(CompanyOverallProfit = CompanyOverallSales- CompanyOverallCosts,
             CompanyOverallMargin = CompanyOverallProfit/CompanyOverallSales,
             CompanyOverallMarkup = CompanyOverallProfit/CompanyOverallCosts) %>% 
      join(  CompanyTransCount, by = "Week")

# 10) DATA EXPORT: Collapse, join data sets w/ different tiers and detail degrees ######################################

# 10a) 1 Ad Items ######################################################################################################

# Skip reading data from the PAST week if the currently processed week is the very FIRST week
if (Week != "2015-04-01") {

## Bind past weeks with current week (Ad Items)

# Load processed data from past weeks
setwd("~/Projects/Sales Data Analyses/3 PD/1 Ad & Sales Reporting")
AdItemsPastWeeks <- read.csv("0 Ad Items.csv", colClasses = c("character")) %>% 
      # Define columns exactly as they are defined before
      mutate(Week         = as.Date(Week),
             Store        = factor(Store),
             UPC          = as.numeric(UPC),
             AdItemGroup  = factor(AdItemGroup),
             CategoryNo   = factor(CategoryNo),
             SubDeptNo    = factor(SubDeptNo),
             DeptNo       = factor(DeptNo),
             UnitsSold    = round(as.numeric(UnitsSold), 2),
             UnitPrice    = round(as.numeric(UnitPrice), 2),
             UnitCost     = round(as.numeric(UnitCost), 2),
             CostSource   = factor(CostSource),
             AdItemSales  = round(as.numeric(AdItemSales), 2),
             AdItemCosts  = round(as.numeric(AdItemCosts), 2),
             AdItemProfit = round(as.numeric(AdItemProfit), 2),
             AdItemMargin = round(as.numeric(AdItemMargin), 4),
             AdItemMarkup = round(as.numeric(AdItemMarkup), 4),
             PriceType    = factor(PriceType))

# Bind current week with past weeks
AdItems <- rbind(AdItems,
                 AdItemsPastWeeks)

} # end of if-clause (Skip reading data from the PAST week if the currently processed week is the very FIRST week)

AdItems <- unique(AdItems)

# Save
setwd("~/Projects/Sales Data Analyses/3 PD/1 Ad & Sales Reporting")
write.csv(AdItems, "0 Ad Items.csv", row.names = F)

# 10b) 2  Sub-departments ##############################################################################################

SubDepts <- as.data.frame(summarize(group_by(AdItems, Week, #Ad,
                               Store, Dept, SubDept),
                               SubDeptAdSales = sum(AdItemSales, na.rm = T),
                               SubDeptAdCosts = sum(AdItemCosts, na.rm = T),
                               AdItemCount    = length(unique(UPC)))) %>% 
      mutate(SubDeptAdProfit = SubDeptAdSales-SubDeptAdCosts,
             SubDeptAdMargin = SubDeptAdProfit/SubDeptAdSales,
             SubDeptAdMarkup = SubDeptAdProfit/SubDeptAdCosts,
             Store           = factor(Store)) %>% 
      join(OverallSubDepts, type = "right", by = c("Week", "Store", "Dept", "SubDept")) %>% 
      select(-DeptNo, -SubDeptNo) %>% 
      mutate(SubDeptAdSalesShare  = SubDeptAdSales/SubDeptOverallSales,
             SubDeptAdProfitShare = SubDeptAdProfit/SubDeptOverallProfit)

SubDepts$SubDeptAdCosts[SubDepts$Dept == "DELI HOT"] <- as.numeric(NA)

setwd("~/Projects/Sales Data Analyses/3 PD/1 Ad & Sales Reporting")
write.csv(SubDepts, "2 Sub-departments.csv", row.names = F)

# 10c) 2  Sub-departments (ACROSS Stores) ###############################################################################

SubDepts2 <- as.data.frame(summarize(group_by(AdItems, Week, Dept, SubDept),
                                     SubDeptAdSales = sum(AdItemSales, na.rm = T),
                                     SubDeptAdCosts = sum(AdItemCosts, na.rm = T),
                                     AdItemCount    = length(unique(UPC)))) %>% 
      mutate(SubDeptAdProfit = SubDeptAdSales-SubDeptAdCosts,
             SubDeptAdMargin = SubDeptAdProfit/SubDeptAdSales,
             SubDeptAdMarkup = SubDeptAdProfit/SubDeptAdCosts,
             Dept            = factor(Dept),
             SubDept         = factor(SubDept)) %>% 
      join(OverallSubDepts2, type = "right", by = c("Week", "Dept", "SubDept")) %>% 
      mutate(SubDeptAdSalesShare  = SubDeptAdSales/SubDeptOverallSales,
             SubDeptAdProfitShare = SubDeptAdProfit/SubDeptOverallProfit)

SubDepts2$SubDeptAdCosts[SubDepts2$Dept == "DELI HOT"] <- as.numeric(NA)

setwd("~/Projects/Sales Data Analyses/3 PD/1 Ad & Sales Reporting")
write.csv(SubDepts2, "2 Sub-departments (ACROSS Stores).csv", row.names = F)

# 10d) 3 Departments ####################################################################################################

Depts <- as.data.frame(summarize(group_by(AdItems, Week, Store, Dept),
                                 DeptAdSales = sum(AdItemSales, na.rm = T),
                                 DeptAdCosts = sum(AdItemCosts, na.rm = T),
                                 AdItemCount = length(unique(UPC)))) %>% 
      mutate(DeptAdProfit = DeptAdSales-DeptAdCosts,
             DeptAdMargin = DeptAdProfit/DeptAdSales,
             DeptAdMarkup = DeptAdProfit/DeptAdCosts,
             Store        = factor(Store),
             Dept         = factor(Dept)) %>% 
      join(OverallDepts, type = "right", by = c("Week", "Store", "Dept")) %>% 
      mutate(DeptAdSalesShare  = DeptAdSales/DeptOverallSales,
             DeptAdProfitShare = DeptAdProfit/DeptOverallProfit)

Depts$DeptAdCosts[Depts$Dept == "DELI HOT"] <- as.numeric(NA)

setwd("~/Projects/Sales Data Analyses/3 PD/1 Ad & Sales Reporting")
write.csv(Depts, "3 Departments.csv", row.names = F)

# 10e) 3 Departments (ACROSS Stores) ####################################################################################

Depts2 <- as.data.frame(summarize(group_by(AdItems, Week, Dept),
                                  DeptAdSales = sum(AdItemSales, na.rm = T),
                                  DeptAdCosts = sum(AdItemCosts, na.rm = T),
                                  AdItemCount = length(unique(UPC)))) %>% 
      mutate(DeptAdProfit = DeptAdSales-DeptAdCosts,
             DeptAdMargin = DeptAdProfit/DeptAdSales,
             DeptAdMarkup = DeptAdProfit/DeptAdCosts,
             Dept         = factor(as.character(Dept))) %>% 
      join(OverallDepts2, type = "right", by = c("Week", "Dept")) %>% 
      mutate(DeptAdSalesShare  = DeptAdSales/DeptOverallSales,
             DeptAdProfitShare = DeptAdProfit/DeptOverallProfit)

Depts2$DeptAdCosts[Depts2$Dept == "DELI HOT"] <- as.numeric(NA)

setwd("~/Projects/Sales Data Analyses/3 PD/1 Ad & Sales Reporting")
write.csv(Depts2, "3 Departments (ACROSS Stores).csv", row.names = F)

# 10f) 4 Stores #########################################################################################################

Stores <- as.data.frame(summarize(group_by(subset(AdItems, Dept != "DELI HOT"), Week, Store),
                             StoreAdSales = sum(AdItemSales, na.rm = T),
                             StoreAdCosts = sum(AdItemCosts, na.rm = T),
                             AdItemCount      = length(unique(UPC)))) %>% 
      mutate(StoreAdProfit = StoreAdSales-StoreAdCosts,
             StoreAdMargin = StoreAdProfit/StoreAdSales,
             StoreAdMarkup = StoreAdProfit/StoreAdCosts,
             Store         = factor(Store)) %>% 
      join(OverallStores, type = "right", by = c("Week", "Store")) %>% 
      mutate(StoreAvgBasketSize = StoreOverallSales/StoreTransactions,
             StoreAdSalesShare  = StoreAdSales/StoreOverallSales,
             StoreAdProfitShare = StoreAdProfit/StoreOverallProfit)

setwd("~/Projects/Sales Data Analyses/3 PD/1 Ad & Sales Reporting")
write.csv(Stores, "4 Stores.csv", row.names = F)

# 10g) 5 Company #######################################################################################################

Company <- as.data.frame(summarize(group_by(AdItems, Week),
                                   CompanyAdSales = sum(AdItemSales, na.rm = T),
                                   CompanyAdCosts = sum(AdItemCosts, na.rm = T),
                                   AdItemCount    = length(unique(UPC)))) %>% 
      mutate(CompanyAdProfit = CompanyAdSales-CompanyAdCosts,
             CompanyAdMargin = CompanyAdProfit/CompanyAdSales,
             CompanyAdMarkup = CompanyAdProfit/CompanyAdCosts) %>% 
      join(OverallCompany, type = "right", by = "Week") %>% 
      mutate(CompanyAvgBasketSize = CompanyOverallSales/CompanyTransactions,
             CompanyAdSalesShare  = CompanyAdSales/CompanyOverallSales,
             CompanyAdProfitShare = CompanyAdProfit/CompanyOverallProfit)

setwd("~/Projects/Sales Data Analyses/3 PD/1 Ad & Sales Reporting")
write.csv(Company, "5 Company.csv", row.names = F)

# 11) Finish ###########################################################################################################

# +++++ Processing Timer & Status ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
timer_split       <- Sys.time()
progress_time     <- difftime(timer_split, timer_begin, units = c("mins"))
progress_perc     <- i/length(Weeks)
timer_end         <- timer_begin + (progress_time/progress_perc)
print(paste("Week of ", Weeks[i], "successfully processed........................", round(progress_perc*100, 2), "%"))
print(paste(round(progress_time, 1), "minutes since processing started. Expected to be completed.......", timer_end))
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

}

print(paste("Process ended: ", Sys.time()))
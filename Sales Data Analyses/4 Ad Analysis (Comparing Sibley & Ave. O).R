# 0) Settings ##########################################################################################################

# Load packages
library(openxlsx)
library(xlsx)
library(plyr)
library(dplyr)
library(stringr)
#options(java.parameters = "-Xmx4g" )
library(XLConnect)
library(tidyr)

# Disable scientific notation and display UPCs correctly
options(scipen=999)

# Disable default string-to-factor conversion
options(stringsAsFactors = FALSE)

Week        <- "2016-08-24"
Stores      <- c("5", "7")

# 1) DATA IMPORT: SUB-DEPT. List w/ main departments ###################################################################
# Contains all sub-departments in rows with their particular main department

setwd("/media/radmin/ExternalHD/Projects/Sales Data Analyses/1 RD/1 Sub-department List")
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

# 2) DATA IMPORT: ITEM List by CATEGORY ################################################################################

setwd("/media/radmin/ExternalHD/Projects/Sales Data Analyses/1 RD/2 Item Price List by Category")
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

rm(files, FileList, h)

# 3) DATA IMPORT: Like Codes ###########################################################################################

setwd("/media/radmin/ExternalHD/Projects/Sales Data Analyses/1 RD/0 Other")
LikeCodes   <- read.xlsx2("Like Codes.xlsx", 1) %>%
      transmute(UPC = as.character(UPC), LikeCode = as.character(Like.Code), Product = as.character(Product.Description)) %>% 
      select(LikeCode, UPC #,
             #Product
             )
 

# 4) DATA IMPORT: AD-ITEM Price List with Cost #########################################################################

setwd(paste0("/media/radmin/ExternalHD/Projects/Sales Data Analyses/1 RD/4 Ad-Item Price List with Cost/", Week))
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
                  UPC, Product, UnitPrice, PriceLabel, PriceType, #UnitCostLOC,
                  SubDept) %>% 
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
AdItems <- filter(AdItems, !duplicated(AdItems)) %>% 
      mutate(UPC = as.character(UPC)) %>% 
      left_join(LikeCodes, by = "UPC") %>% 
      # Create additional variable that distinguishes between UPC and Like Code since overlapping codes may exist
      mutate(UPC2      = as.character(paste(UPC,      "(UPC)")),
             LikeCode2 = as.character(paste(LikeCode, "(LC)"))) %>% 
      # Compute average sales per day to allow comparisons between weeks of different lengths
      mutate(LikeCode_UPC = LikeCode2) %>% 
      select(Week, Store,
             UPC, UPC2, LikeCode, LikeCode_UPC, Product,
             Dept, SubDept, Category,
             UnitPrice, PriceLabel, PriceType #, UnitCostLOC
             )

# Create hybrid variable: if no like code exist, take UPC instead in order to group/summarize later by liek code, if available, and UPC if like code doesn't exist
AdItems$LikeCode_UPC[AdItems$LikeCode_UPC == "NA (LC)"] <- AdItems$UPC2[AdItems$LikeCode_UPC == "NA (LC)"]; AdItems$UPC2 <- NULL

# Extract all product codes and description per SKU, Group like codes (if none, just display UPC) and concatenate product descriptions per product group
Products_LC      <- select(AdItems,
                            UPC, LikeCode, LikeCode_UPC, Product) %>% unique() %>%
      ddply(.(LikeCode_UPC), summarize, Product = paste(Product, collapse = ", "))

# 5) DATA IMPORT: ITEM Movement & SUB-DEPT. Sales & Costs ##############################################################
# Set folder, list containing files, create two dataframe-list with length that equals the number of files
# Each folder within the following directories represents one sales week; within the folders, files contain store data
setwd(paste0("/media/radmin/ExternalHD/Projects/Sales Data Analyses/1 RD/7 Item Movement, Sub-department Sales & Costs/", Week))
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
OverallItemMovement           <- ldply(FileList1, data.frame) %>%
      select(Week, Store, UPC, UnitsSold, Sales) %>% 
      # Remove the "data noise" (empty rows, titles, headers, footers, etc.) from the messy report by coercing to NAs and removing them; Keep the meaningful "data signal" which are characterized by numeric values in UPC column.
      mutate(UPC       = suppressWarnings(as.numeric(UPC)),
             UnitsSold = round(as.numeric(UnitsSold), 2),
             Sales     = round(as.numeric(Sales), 2)) %>% 
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
      filter(Store %in% Stores)

AdItems$UnitsSold[is.na(AdItems$UnitsSold)]     <- 0
AdItems$Sales[is.na(AdItems$Sales)]             <- 0

## 6) DATA EXPORT ######################################################################################################

AdItems2_UPC <- filter(AdItems) %>%
      select(-c(Category, UnitsSold, UnitPrice, PriceLabel, PriceType)) %>% 
      mutate(Store = paste0("Store", Store)) %>% 
      spread(Store, Sales)

AdItems2_LC <- group_by(AdItems2_UPC,
                        Week, Dept, SubDept, LikeCode_UPC) %>% 
      summarize(Store5 = sum(Store5, na.rm = T),
                Store7 = sum(Store7, na.rm = T)) %>% 
      left_join(Products_LC, by = "LikeCode_UPC")

setwd("/media/radmin/ExternalHD/Projects/Sales Data Analyses/3 PD/5 Ad Analysis")
write.xlsx2(as.data.frame(AdItems2_LC), paste0("Ad Analysis_Stores 5 & 7_", Week, ".xls"), row.names = F)

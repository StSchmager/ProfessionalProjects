# 0 Initial Settings ###################################################################################################

## Load packages (that will carry out certain functions within the data pipeline)

# Read Excel reports that contain the data of interest
library(xlsx)     
library(openxlsx) # More suited for larger Excel files (reading function performs faster) 

# Process, clean, munge, and wrangle data
library(plyr)
library(dplyr)

## Disable scientific notation and display UPCs properly 
options(scipen=999)

# 1 Sub-department List ################################################################################################
# Contains all sub-departments in rows with their particular main department

# Read data from Excel report
setwd("C:/Users/Stefan/Google Drive/Projects/4 Weekly Ad/1 Raw Data/2 Sub-department List")
Departments <- xlsx::read.xlsx("Sub-department List.xlsx", sheetIndex = 1, startRow = 5,
                               colIndex = c(2:3, 6, 8), colClasses = c("character"),
                               header = F, row.names = NULL) %>%
      ## Process data
      #  Rename default headers since data-source doesn't include correct headers
      rename(SubDeptNo = X2,
             SubDept   = X3,
             DeptNo    = X6,
             Dept      = X8) %>%
      #  Determine data type of columns
      mutate(SubDeptNo = as.numeric(as.character(SubDeptNo)),
             SubDept   =            as.character(SubDept),
             DeptNo    = as.numeric(as.character(   DeptNo)),
             Dept      =            as.character(   Dept)) %>%
      # Filter in data "signal", rows with department information indicated by sub-department numbers and
      # Filter out data "noise", rows with titles, footers, page breaks, etc. (coerced to NAs) 
      filter(!is.na(SubDeptNo)) %>% 
      # Select data columns of interest and filter out auxiliary columns for data-processing purposes
      select(SubDept, Dept)
# Warning messages displayed above (in the console) are expected and needed to select data "signal"

# 2 Data per Sales-Week Folder & Store File ############################################################################
# Each folder within the following directories represents one sales week; within the folders, files contain store data

# List sales weeks
setwd("C:/Users/Stefan/Google Drive/Projects/4 Weekly Ad/1 Raw Data/6 Item Cost of Goods by Sub-department")
Weeks              <- list.files()

# Create list with a length that equals the number of sales weeks (FolderList might have been just named WeekList)
       FolderList  <- list()
length(FolderList) <- length(Weeks)

# Loop through the listed sales weeks and read a) ad-item information and b) all item movement data
for (j in 1:length(Weeks)) {

# 2a) Ad-Item Information ##############################################################################################
# Stored in sales-week folders and contains ad items (that are featured in the sales paper) per store

# Set week folder and list store files
setwd(paste0("C:/Users/Stefan/Google Drive/Projects/4 Weekly Ad/1 Raw Data/1 Price List with Cost/", Weeks[j]))
files             <- list.files()

# Create list with a length that equals the number of stores (FileList might have been just named StoreList)
       FileList   <- list()
length(FileList)  <- length(files)

# Loop through the listed files in Week folder, and write ad-item data as dataframe in list elements
for (i in 1:length(files)) {
      
      # Read complete raw data from Excel report
      dat0              <- openxlsx::read.xlsx(files[i], sheet = 1, colNames = F)               
      
      ## Data rows are shifted column-wise and need to be read separately in order to be re-aligned
      
      # Subset rows that do NOT have a price quantity (e.g. $0.68) listed
      # (Those rows      do NOT have values in the last column 15 (R) and are therefore NA 
      dat1              <- select(dat0[which( is.na(dat0$X15)),],
                                  c(X1,    # UPC
                                    X6:X7, # Price Quantity and Price
                                    X9))   # Price Type
      
      # Subset rows that DO have a price quantity (e.g. 2/$0.98) listed; hence, shift to the right
      # (Those rows      DO have values in the last column 15 (R) and are therefore NOT NA
      dat2              <- select(dat0[which(!is.na(dat0$X15)),],
                                  c(X1,    # UPC
                                    X7:X8, # Price Quantity and Price
                                    X10))  # Price Type
      
      # Subset rows need can be re-aligned if they share the same column names (1-4 for now, renamed later)
      colnames(dat1)    <- c(1:4)
      colnames(dat2)    <- c(1:4)                                     
      dat               <- rbind(dat1, dat2)                                                    
      
      # Write clean data in list element per week and add week and store infromation
      FileList[[i]]     <- cbind(Weeks[j],                  # Week
                                 substring(dat[4, 1], 8),   # Store digit from report header stripped from report description
                                 dat)                       # Data
      
      # Name columns
      colnames(FileList[[i]]) <- c("Week",
                                   "Store",
                                   "UPC",
                                   "PriceQuantity",
                                   "Price",
                                   "PriceType")
}      

# Collapse dataframes in list elements into one dataframe
AdItems  <- plyr::ldply(FileList, data.frame) %>%
      # Determine data type of columns
      mutate(Week          = as.Date(as.character(Week), format = "%m-%d-%Y"),
             Store         = factor(as.character(as.numeric(as.character(Store)))),
             UPC           = as.numeric(as.character(UPC))) %>%
      # Select data "signal", rows with product information indicated by UPC and
      # Filter out rows with "data noise" such as titles, footers, page breaks, etc. (coerced to NAs) 
      filter(!is.na(UPC)) %>%
      # Determine data type of more columns and compute new ones
      mutate(PriceQuantity = as.numeric(as.character(PriceQuantity)),
             Price         = as.numeric(as.character(Price)),
             PriceType     = as.character(PriceType),
             UnitPrice     = round(Price/PriceQuantity, 2)) %>%
      # Select, sort data columns of interest and filter out auxiliary columns for data-processing purposes
      select(Week,
             Store,
             UPC,
             Price, PriceQuantity, UnitPrice, PriceType)

# Warning messages displayed above (in the console) are expected and needed to select data "signal"


# Fill in unit price if unit price is NA
AdItems$UnitPrice[is.na(AdItems$UnitPrice)]   <- round(AdItems$Price[is.na(AdItems$UnitPrice)], 2)

## Create additiona column to dataset that indicates the price label as it appeared in the sales paper
AdItems$PriceLabel <- NA
# e.g. $0.99 when the price quantity is NA
AdItems$PriceLabel[is.na(AdItems$PriceQuantity)]      <- paste0("$", as.character(AdItems$Price[is.na(AdItems$PriceQuantity)]))
# e.g. 10/$10 when the price quantity is available (not NA)
AdItems$PriceLabel[!is.na(AdItems$PriceQuantity)]     <- paste0(as.character(AdItems$PriceQuantity[!is.na(AdItems$PriceQuantity)]),
                                                                "/$",
                                                                as.character(AdItems$Price[!is.na(AdItems$PriceQuantity)]))

# Remove auxiliary data objects that helpedwere used to read and process the data, but are not needed any longer
rm(dat, dat0, dat1, dat2, FileList, files, i)

# 2b) All item movement information ####################################################################################
# Stored in sales-week folders and contains movement (in units sold) and sales (in USD) per item for each store

# Set week folder and list store files
setwd(paste0("C:/Users/Stefan/Google Drive/Projects/4 Weekly Ad/1 Raw Data/6 Item Cost of Goods by Sub-department/", Weeks[j]))
files             <- list.files()

# Create list with a length that equals the number of stores (FileList might have been just named StoreList)
       FileList  <- list()
length(FileList) <- length(files)

# Loop through the listed files in Week folder, and write movement data as dataframe in list elements
for (i in 1:length(files)) {
      
      # Read complete raw data from Excel report
      Data <- openxlsx::read.xlsx(files[i], colNames = F)
      
      # Extract data of interest from certain cells and columns of the report
      FileList[[i]] <- cbind(Data[2,2],                           # Week
                             substring(Data[nrow(Data),1], 22),   # Store digit from report footer stripped from report description
                             Data[,c(1:3, 6:7)])                  # UPC, Product, Sub-Dept, Movement, Sales
                             # 5th column E in Excel report is enitrely empty and, therefore, skipped by reading function
                             # Empty column shifts following columns: Excel columns G:H are dataframe columns 6:7
      
      # Name columns
      colnames(FileList[[i]]) <- c("Week",
                                   "Store",
                                   "UPC", "Product", "SubDept", "Movement", "Sales")
      }

# Collapse dataframes in list elements into one dataframe
Data <- plyr::ldply(FileList,data.frame) %>%
      # Determine data type of columns and compute new ones
      mutate(Week     = as.Date(as.character(Week), format = "%m/%d/%Y"),
             Store    = factor(as.character(Store),
                               levels = c("5724 KEDZIE", "4700 KEDZIE", "4343 PULASKI", "5838 PULASKI", "118TH ST", "2526 CERMAK", "7 SIBLEY", "3720 W", "OAK BROOK TERRACE", "MADISON", "BRIDGEVIEW", "OAK PARK"),
                               labels = c("1",           "2",           "3",            "4",            "5",        "6",           "7",        "8",      "9",                 "10",      "11",         "12")),
             UPC      = as.numeric(as.character(UPC)),
             Movement = round(as.numeric(as.character(Movement)), 2),
             Sales    = round(as.numeric(as.character(Sales)), 2),
             AvgPrice = round(Sales/Movement, 2)) %>%
      # Select data "signal", rows with product information indicated by UPC and
      # Filter out rows with "data noise" such as titles, footers, page breaks, etc. (coerced to NAs) 
      filter(!is.na(UPC)) %>%
      # Join sub-department and ad-item data and weave in additional columns
      left_join(Departments, by =   "SubDept") %>%
      left_join(AdItems,     by = c("Week", "Store", "UPC")) %>%
      # Select, sort data columns of interest including the ones just joined in (see below)
      select(Week, Store,
             Dept, SubDept,                    # sub-department list
             UPC, Product,
             PriceType, PriceLabel, UnitPrice, # ad-item information
             AvgPrice, Sales, Movement)
# Warning messages are expected (Coerced NAs needed to filter meaningful item data by UPC)

# Label all items that were not included in the ad-item list accordingly
Data$PriceType[is.na(Data$PriceType)]   <- "NON-SALE"

# Remove auxiliary data objects that helpedwere used to read and process the data, but are not needed any longer
rm(AdItems, FileList, files, i)

# Write dataframe in list element
FolderList[[j]] <- Data

}

# Collapse dataframes in list elements into one dataframe
Data <- ldply(FolderList,data.frame); rm(FolderList, j, Weeks)

# Subset item-movement data by (sub-)department and save as CSV file of manageable size to be read into TABLEAU visualizaton program
setwd("C:/Users/Stefan/Google Drive/Projects/4 Weekly Ad/2 Processed Data/2 Price Elasticity")

saveRDS(Data, "All Department Item Movement & Price Points Weekly per Store.rds")

filter(Data, Dept == "PRODUCE")     %>% write.csv("PRODUCE Item Movement & Price Points Weekly per Store.csv",  row.names = F)
filter(Data, Dept == "DELI")        %>% write.csv("DELI Item Movement & Price Points Weekly per Store.csv",     row.names = F)
filter(Data, Dept == "BAKERY")      %>% write.csv("BAKERY Item Movement & Price Points Weekly per Store.csv",   row.names = F)
filter(Data, Dept == "FLORAL")      %>% write.csv("FLORAL Item Movement & Price Points Weekly per Store.csv",   row.names = F)
filter(Data, Dept == "DELI HOT")    %>% write.csv("DELI HOT Item Movement & Price Points Weekly per Store.csv", row.names = F)
filter(Data, Dept == "LIQUOR")      %>% write.csv("LIQUOR Item Movement & Price Points Weekly per Store.csv",   row.names = F)
filter(Data, Dept == "FROZEN")      %>% write.csv("FROZEN Item Movement & Price Points Weekly per Store.csv",   row.names = F)
filter(Data, Dept == "DAIRY")       %>% write.csv("DAIRY Item Movement & Price Points Weekly per Store.csv",    row.names = F)
filter(Data, Dept == "DAIRY")       %>% write.csv("DAIRY Item Movement & Price Points Weekly per Store.csv",    row.names = F)

filter(Data, Dept == "GROCERY" & SubDept == "GROCERY")  %>%
      write.csv(     "GROCERY-GROCERY Item Movement & Price Points Weekly per Store.csv", row.names = F)
filter(Data, Dept == "GROCERY" & SubDept == "BEVERAGE") %>%
      write.csv(     "GROCERY-BEVERAGE Item Movement & Price Points Weekly per Store.csv", row.names = F)
filter(Data, Dept == "GROCERY" & SubDept == "NON FOOD") %>%
      write.csv(     "GROCERY-NON FOOD Item Movement & Price Points Weekly per Store.csv", row.names = F)
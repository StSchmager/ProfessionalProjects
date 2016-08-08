# 0) Settings ##########################################################################################################

# Load packages
library(plyr)
library(dplyr)
library(XML)
library(zipcode)
library(lubridate)
library(ggplot2)
library(scales)
library(data.table)
library(reshape2)
library(tidyr)

# Disable default string-to-factor conversion
options(stringsAsFactors = FALSE)

# I) PREPARE XML transaction-data reading and processing ###############################################################

## Read XML-file names (not yet the files themselves), and
## Extract file features from the naming convention ("YMMDDSSS.LLL")
# Y   = Year - last digit (e.g. 5 for year 2015)
# MM  = Month
# DD  = Day
# SSS = Store (e.g. 012 for store 12)
# LLL = Lane  (e.g. 001 for lane 1)
## Organize information in a file index (dataframe)

# Determine (store) input folders that contain XML files and list them
setwd("~/Projects/Transaction Data Analyses/1 RD/XML Transaction Files")
Stores            <- as.character(sort(as.numeric(list.files())))
       StoreList  <- list()
length(StoreList) <- length(Stores)
names( StoreList) <-        Stores

# Loop through each folder and read the file name-- not the file (content) itself
for (j in 1:length(Stores)) {
      StoreFolder       <- paste0("~/Projects/Transaction Data Analyses/1 RD/XML Transaction Files/", Stores[j])
      setwd(StoreFolder)
      FileNames         <- list.files()
      
      # Extract the information coded in the file name ("YMMDDSSS.LLL"), and organize them in a dataframe which is then
      # Attached as an element to the list with each element presenting a store
      StoreList[[j]] <- as.data.frame(cbind( FileNames,
                                      substr(FileNames,  1,  1), # Year   (Y)
                                      substr(FileNames,  2,  3), # Month  (MM)
                                      substr(FileNames,  4,  5), # Day    (DD)
                                      substr(FileNames,  6,  8), # Store  (SSS)
                                      #                  9         Period (.)
                                      substr(FileNames, 10, 12)) # Lane   (LLL)
                                      ) %>%
            # Rename columns, define data types of columns and build new columns
            mutate(Year  = paste0(201, V2),                             # Complete the year digit from the file name
                   Month = V3,
                   Day   = V4,
                   Date  = as.Date(paste(Year, Month, Day, sep = "-")), # Build a date column by concatenating day, month, and year
                   Store = suppressWarnings(as.numeric(V5)),                              # Remove leading zero in store number by converting from character to numeric
                   Lane  = suppressWarnings(as.numeric(V6))) %>%                          # Convert lane number to numeric so that irrelevant lanes can be numerically filtered out by ... 
            # Removing files with lane numbers 100+ that do not represent checkout lanes and do not perform customer transactions
            filter(Lane  < 100) %>% 
            # Exclude XML files from today ALWAYS as they are still being written in the course of the day and would be corrupted otherwise
            filter(Date  != today()) %>% 
            select(-c(V2:V6))
            
      rm(FileNames, StoreFolder)
      
} # End of store-folder loop

# Collapse the list into one dataframe, and remove list and index variable
FileIndex <- ldply(StoreList, data.frame) %>% 
      mutate(FileNames = factor(FileNames),
             Lane      = factor(Lane),
             Store     = factor(Store),
             Day       = factor(Day),
             Month     = factor(Month),
             Year      = factor(Year)) %>% 
      select(-.id)

rm(StoreList, j)

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

# II) READ and PRE-PROCESS XML Transaction Data #########################################################################
## Extract relevant information now from files themselves (before: just file names) by using the XPath XML querying language
## Select just those XML files of interest by determining a certain store and date range

## DATES

# a) Default DATE range: most recent week
#date_end    <- max(as.Date(FileIndex$Date))
#date_begin  <- date_end-6


# b) Manual DATE range
date_begin  <- as.Date("2016-05-25")
date_end    <- as.Date("2016-06-01")

# c) Populate date range completely and fill in dates between start and end date
date_range  <- seq.Date(from = date_begin,
                        to   = date_end,
                        by   = 1)

## Select file names as VECTOR filtered by store and date-range criteria that are then fed into the actual XML-file reading process
# Subset data from 
FileIndex_Period <- filter(FileIndex, Date %in% seq.Date(from = date_begin,
                                                         to   = date_end,
                                                         by   = 1))

# Draw histogram to check if any dates (gaps in graph) or files (unusual dips or spikes) were
# not transfered from store server to R server
ggplot(FileIndex_Period, aes(Date)) +
      geom_histogram(binwidth = 1, alpha=.5) +
      ggtitle("Transaction Files from All Stores / Selected Period Dates Transferred") +
      xlab("Check for gaps or dips to determine whether some XML files / whole dates were not transfered from the store servers") +
      ylab("Number of XML Files Transferred") +
      scale_x_date(labels = date_format("%m/%d/%y"),
                   breaks = date_breaks("1 day")) +
      scale_y_discrete(breaks = seq(0, 20, 2)) +
      #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      facet_grid(Store ~ ., scales="free_y")

## Create list with length equal to the amount of stores, where datasets (representing information per FILE/STORE) are saved in
# TRANSACTION Attributes
       StoreList_TransAttr    <- list()
length(StoreList_TransAttr)   <- length(Stores)
# TRANSACTION ITEMs (Baskets)
       StoreList_TransItems   <- list()
length(StoreList_TransItems)  <- length(Stores)
# ITEM Attributes
       StoreList_ItemAttr     <- list()
length(StoreList_ItemAttr)    <- length(Stores)

# +++++ Processing Timer +++++ 
timer_begin <- Sys.time()
# ++++++++++++++++++++++++++++

# 1st Loop through store folders
for (j in 1:length(Stores)) {
      
      # Set file folder to read from
      StoreFolder       <- paste0("~/Projects/Transaction Data Analyses/1 RD/XML Transaction Files/", Stores[j])
      
      
      files_selected    <- as.character(subset(FileIndex_Period, Store == as.numeric(Stores[j]))[, 1])
      
      # Create list with length equal to the length of files to be read
      FileList          <- list()
      length(FileList)  <- length(files_selected)
      
      ## Create list with length equal to the length of files per store, where datasets (representing information per FILE/STORE) are saved in
      # TRANSACTION Attributes
             FileList_TransAttr     <- list()
      length(FileList_TransAttr)    <- length(files_selected)
      # TRANSACTION ITEMs (Baskets)
             FileList_TransItems    <- list()
      length(FileList_TransItems)   <- length(files_selected)
      # ITEM Attributes
             FileList_ItemAttr      <- list()
      length(FileList_ItemAttr)     <- length(files_selected)
      
# 2nd Loop through each XML file WITHIN the store folder and extract the same information which will be stored as dataframe and placed in list element

# Loop through each selected XML file and extract the same information repeatedly which will then be stored as dataframe and placed in list element
for (i in 1:length(files_selected)) { 
      
      # Read XML file
      setwd(StoreFolder)
      tree <- xmlInternalTreeParse(files_selected[i], getDTD = F)
      
      # Check if at least one "SALE" (F1068) transaction (trs) node has an ancestor totalizer (tlz) node whose description is "TOTAL SALES",
      
      if ("TOTAL SALES" %in% xpathSApply(tree, '//*[@F1068 = "SALE"]/*/*[@F02]', xmlGetAttr, "F02", default = NA))
            
            {# If so, continue and read further information...
             # If not, skip this file and check next file...
            
            ## II-0) TRANSACTION nodesets & numbers ####################################################################
            TransNodes        <- getNodeSet( tree, "//*[@F1032 and @F1068 = 'SALE']")
            TransNo           <- xpathSApply(tree, "//*[@F1032 and @F1068 = 'SALE']", xmlGetAttr, "F1032", default = NA)
            
            ## II-1a) TRANSACTION Attributes (initial) #################################################################
            # Contained in trs node as node attributes
            
            # Create list of all standard TRANSACTION attributes: transaction type, number, dates, times, store, lane, cashier, etc.
            TransAttr <- xpathApply(tree, "//*[@F1032 and @F1068 = 'SALE']", xmlAttrs) %>% plyr::ldply(rbind)
            
            # Check if TRANSACTION contain optional attributes:   customer no. (F1148), hence implicitly, customer name (F1155) and shopper level (F1152) 
            if (c("F1148") %in% colnames(TransAttr) == FALSE)
            # If NO (not contained in attributes), create dummy (NA) attribute columns, so that columns exist to be renamed subsequently      
                  {TransAttr <- mutate(TransAttr,
                                       F1148 = as.character(NA),
                                       F1155 = as.character(NA),
                                       F1152 = as.character(NA))}
            # If YES (contained in attributes), NO action is required; carry on and...
            
            # Rename transaction-number variable (the other ones will be renamed at the end of the data processing)
            TransAttr <- rename(TransAttr, TransNo  = F1032) %>% select(TransNo, F1068:F1152)
                               
            ## II-1b) TRANSACTION (Customer) ZIP Codes #################################################################
            # Not contained in previous trs node, but in one of its child-nodes-- namely the r(ow)/line nodes
            
            # Create a list of TRANSACTION ZIP Codes, and
            ZIPCash <- lapply(TransNodes, xpathSApply, path = './*/*[contains(@F02, "P.CODE")]', xmlGetAttr, "F02", default = NA)
            # Extract five-digit ZIP code (strip off string "P.CODE="), and convert to vector
            ZIPCash <- substr(ZIPCash, 8, nchar(ZIPCash))
            # Name vector of ZIP codes with corresponding TRANSACTION number
      names(ZIPCash) <- TransNo
            # Convert named vector to to dataframe (with transaction no. as rowname), and
            ZIPCash        <- data.frame(unlist(ZIPCash)) %>% rename(ZIPCash = unlist.ZIPCash.)
            # Add TRANSACTION numbers as separate column from now row (formerly vector) names
            ZIPCash$TransNo<- rownames(ZIPCash)
             
            ## II-1c) TRANSACTION Sales Totals #########################################################################
            # Not contained in previous trs node, but in one of its child-nodes-- namely the t(otalizer)/tlz nodes
            
            # Create a list of TRANSACTION total-sale amounts, and
            TotalSales        <- lapply(TransNodes, xpathSApply, path = './*/*[@F02 = "TOTAL SALES"]/following-sibling::*[1]', xmlValue)
            # Name list elements by their corresponding TRANSACTION  number
      names(TotalSales) <- TransNo
            # Remove TRANSACTIONs w/o total-sale amounts; they are NULL and do not have any items attached
            TotalSales        <- TotalSales[!sapply(TotalSales, is.null)]
            # Convert list of TRANSACTION total-sale amounts to dataframe, and
            TotalSales        <- data.frame(unlist(TotalSales)) %>% rename(TotalSales = unlist.TotalSales.) 
            # Add TRANSACTION numbers as separate column from now row (formerly list element) names
            TotalSales$TransNo<- rownames(TotalSales)
            
            ## II-1) TRANSACTION Attributes (All) ######################################################################
            
            # Join TRANSACTION total-sale amounts & ZIP Codes with TRANSACTION attributes, and
            TransAttr <- left_join(TotalSales, TransAttr, by = c("TransNo")) %>%
                  left_join(ZIPCash,           by = c("TransNo")) %>%
            # Rename the columns (omit those attributes that are not interesting)
                  select(TotalSales, TransNo, ZIPCash,
                         Type         = F1068,
                         SDate        = F254,
                         EDate        = F253,
                         STime        = F1035,
                         ETime        = F1036,
                         Store        = F1056,
                         Lane         = F1057,
                        #InvoiceNo    = F1764,
                        #OperatorNo   = F1185,
                         CashNo       = F1126,
                         CashName     = F1127,
                         CustNo       = F1148,
                        #ShopperLevel = F1152,
                         CustName     = F1155) %>% 
            # Transform to numeric values (and back to character values in order to remove leading zeros in store/lane no.)
                  mutate(TotalSales   = as.numeric(TotalSales),
                         Store        = as.character(as.numeric(Store)),
                         Lane         = as.character(as.numeric(Lane))) %>% 
            # Create transaction ID that's applicable across stores and dates since transaction no. is just unique to certain store-lane-date combinations
                  mutate(TransID      = as.factor(paste(SDate, Store, Lane, TransNo, sep = "-")))
            
            rm(TotalSales, ZIPCash)

            ## II-2) TRANSACTIONS ITEMS (Baskets) #######################################################################
            # Contained in itm nodes sub-listed via r(ow)/line nodes under trs node
            
            # Create a nested list, a list of lists (each list represents one TRANSACTION and contains the ITEMS purchased)
            BasketsList       <- lapply(TransNodes, xpathApply, path = './*/*[@F01]', xmlGetAttr, "F01", default = NA)
            # Name the nested list elements / transactions (1st tier) with their corresponsing TRANSACTION no.
      names(BasketsList)      <- TransNo
            # Remove lists (transaction) in nested list that have NO element (item); in other words, list length equals zero (corresponds to those that don't have total-sale amounts)
            BasketsList       <- BasketsList[lapply(BasketsList, length) != 0]      
            # Remove duplicated / keep unique elements (items) in lists of list; in other words, if product is scanned twice in transaction, it's just listed once
            BasketsList <- lapply(BasketsList, unique)
            # Number the nested list elements / items (2nd tier) in ascending order
            for (j in 1:length(BasketsList)) 
                  {names(BasketsList[[j]]) <- seq(1, length(BasketsList[[j]]))}
            
            # Convert list of lists to dataframe in "basket" format (arules-package)
            BasketsDFb              <- rbindlist(BasketsList, fill = T)
            # Identify baskets by transaction number: either as row names or as additional column
   rownames(BasketsDFb)             <- as.character(names(BasketsList))
            BasketsDFb$TransNo      <- as.character(names(BasketsList))
   
            # Convert dataframe to a different, data-identical one; however, with different, LESS memory intense "single" format (arules-package)
            BasketsDFs        <- gather(BasketsDFb, ItemID, UPC, -TransNo, na.rm = T) %>% 
                  left_join(select(TransAttr, TransNo, TransID), by = "TransNo") %>%
                  mutate(TransItemID = as.character(paste(TransID, ItemID, sep = "-"))) %>% 
                  select(-ItemID) # not needed anymore since incorporated in
            
            TransItems <- BasketsDFs
                       
                       rm(BasketsDFs, BasketsDFb, BasketsList)
            
            # II-3) ITEM attributes ######################################################################################
            # Processed equivalent to reading the Baskets above
            
            # Create list of ITEM attributes: UPC, item description, sub-dept., active price, quantity, and price description
            ItemAttr    <- lapply(TransNodes, xpathApply, path = './*/*[@F01]', xmlAttrs)
            # Create list of ITEM dollar/unit sales that's stored in its next sibling node/s F65/F64 (Sales/Units per UPC)
            #ItemMove    <- lapply(TransNodes, xpathApply, path = './*/*[@F01]/following-sibling::*[2]', xmlValue)
            ItemSales    <- lapply(TransNodes, xpathApply, path = './*/*[@F01]/following-sibling::*[1]', xmlValue)
            
            ## Treat both lists subsequently in an equivalent manner
            
            # Name the nested list elements (1st tier) with the vector of TRANSACTION numbers
      names(ItemAttr)   <- TransNo
      names(ItemSales)  <- TransNo
            # Remove lists (transaction) in nested list that do have NO element (item); in other words, list length equals zero
            ItemAttr    <- ItemAttr[ lapply(ItemAttr,  length) != 0]
            ItemSales   <- ItemSales[lapply(ItemSales, length) != 0] 
            # Remove attribute (F1080 = Elapse time) and its value from each item attribute (named) VECTOR in each transaction LIST
            #ItemAttr    <- lapply(ItemAttr, sapply, function(vec) vec[names(vec)!="F1080"])
            # Remove duplicated elements (items) in list of lists; just unique items are listed (if product is scanned twice in transaction, it's just counted once)
            #ItemAttr    <- lapply(ItemAttr, unique)
            
            # Number the nested list elements (2nd tier) in ascending order
            #(duplicated items were not yet removed/aggregated**; therefore, this item enumeration is just temporary to merge* and has to be re-done** later)
            for (j in 1:length(ItemAttr)) 
                  {names(ItemAttr[[j]]) <- seq(1, length(ItemAttr[[j]]))}
            for (j in 1:length(ItemSales)) 
                  {names(ItemSales[[j]]) <- seq(1, length(ItemSales[[j]]))}
            
            # Convert list of lists to list of dataframes and then finally to one dataframe
            ItemSales   <- lapply(ItemSales, ldply, rbind) %>% rbindlist(fill = T, idcol = "TransNo") %>% plyr::rename(c("1" = "Sales", ".id" = "ItemID"))
                                                                                                         #plyr::rename(c("1" = "Movement"))
            ItemAttr    <- lapply(ItemAttr,  ldply, rbind) %>% rbindlist(fill = T, idcol = "TransNo") %>% plyr::rename(c(               ".id" = "ItemID")) %>% 
            
            # *Merge ITEM attributes with sales/movement by Transaction No. & Item ID and remove "Elapse Time" variable
                  left_join(ItemSales, by = c("TransNo", "ItemID")) %>%                   select(-F1080)
                         rm(ItemSales)
            
            # Check if item attributes contain (active) price description/type (F113)
            if (c("F113") %in% colnames(ItemAttr) == FALSE)
                  # If NO (attribute is not contained), then create dummy (NA) attribute columns, so that columns exist to be renamed subsequently      
                  {ItemAttr <- mutate(ItemAttr, F113 = as.character(NA))}
                  # If YES (attribute is not contained), NO action is required; carry on and...
            
            # Add unique transaction ID from transaction-attribute dataframe with the help of transaction no. and...
            ItemAttr <- left_join(ItemAttr,
                                  as.data.table(select(TransAttr, TransID,
                                                       TransNo)),
                                                 by = "TransNo") %>% 
            # Rename the columns (omit those attributes that are not interesting)
                  select(#TransNo not needed anymore since incorporated in TransID
                         ItemID,
                         UPC        = F01,
                         Product    = F02,
                         Dept       = F03,
                         SubDept    = F04,
                         #FoodStamp  = F79,
                         #TaxFlag1   = F81,
                         #Scalable   = F82,
                         #TaxFlag2   = F96,
                         #TaxFlag3   = F97,
                         PriceType  = F113,
                         #WIC        = F178,
                         PriceQty   = F1006,
                         Price      = F1007,
                         #ElapseTime = F1080,
                        #Movement,
                         Sales,
                         TransID) %>% 
                  mutate(   Sales = as.numeric(Sales))
            
            # **Aggregate sales of the same UPC that is listed more than once in transaction
            ItemAttr                <-#filter(ItemAttr, !is.na(Price)) %>% 
                       group_by(ItemAttr, TransID, Dept, SubDept, PriceType, PriceQty, Price, Product, UPC) %>% 
                       summarise(Sales = sum(Sales, na.rm = T)) %>% 
            # **Re-name items with (transaction) item ID that's aligned to the ones previously created in Baskets
                       left_join(TransItems, by=c("TransID", "UPC"), copy = T) %>% select(-TransNo) # not needed anymore since incorporated in transaction-item no 

            # Save dataframes per file in file ist
            FileList_TransItems[[i]]      <- TransItems
            FileList_TransAttr[[i]]       <- TransAttr
            FileList_ItemAttr[[i]]        <- ItemAttr
            
            rm(tree,
               TransNodes, TransNo,
               TransItems, ItemAttr, TransAttr)
         
      # +++++ Processing Timer & Status ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      timer_split       <- Sys.time()
      progress_time     <- difftime(timer_split, timer_begin, units = c("mins"))
      progress_perc     <- i/length(files_selected)
      timer_end         <- timer_begin + (progress_time/progress_perc)
      print(paste(files_selected[i], "XML file (no. ",i ,") successfully processed........................", round(progress_perc*100, 2), "%"))
      print(paste(round(progress_time, 1), "minutes since processing started. Expected to be completed.......", timer_end))
      # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         
      } # if-clause ends
      
      else print(paste(files_selected[i], "XML file skipped due to missing 'Sale' Transaction with Total Sales amount!"))
      
      } # Ends 2nd XML-file loop
      
      # Collapse dataframes of the lists into store dataframes
      StoreList_TransItems[[j]]     <- plyr::ldply(FileList_TransItems, data.frame)
      StoreList_TransAttr[[j]]      <- plyr::ldply(FileList_TransAttr,  data.frame) 
      StoreList_ItemAttr[[j]]       <- plyr::ldply(FileList_ItemAttr,   data.frame)
      
} # Ends 1st store-folder loop

rm(timer_begin, timer_split, timer_end,
   progress_perc, progress_time,
   i, j)

# III) PROCESS XML Transaction Data ####################################################################################

# III-1) ITEM attributes ###############################################################################################
# Number of observations does NOT equal / is greater than TransItemID factor levels because
# Coupons are included, that have the same UPC like their applicable product

ItemAttr    <- plyr::ldply(StoreList_ItemAttr, data.frame) %>% 
      mutate(UPC         = factor(UPC),
             Product     = as.character(Product),
             Dept        = factor(Dept),
             SubDept     = factor(SubDept),
             TransItemID = factor(TransItemID), 
             TransID     = factor(TransID),
            #Movement    = as.numeric(Movement),
             Sales       = as.numeric(Sales),
             PriceType   = factor(PriceType),
             PriceQty    = as.numeric(PriceQty),
             Price       = as.numeric(Price)) %>% 
      mutate(UnitPrice   = Price/PriceQty,
             Movement    = Sales/UnitPrice) %>%
            #Sales       = UnitPrice*Movement) %>% 
      select(TransItemID, TransID, UPC:Movement)
     #select(TransItemID, TransID, UPC:Sales)

# This operation is included in the loop                                  
TransItemCount <- group_by(ItemAttr, TransID) %>% 
      summarise(TotalMovement  = sum(Movement, na.rm = T),
                UniqueUPCs     = length(unique(UPC)))

# III-2) TRANSACTION ITEMs #############################################################################################
# Number of observations does EQUAL the TransItemID factor levels because all duplicates were removed

TransItems  <- plyr::ldply(StoreList_TransItems, data.frame) %>% 
      transmute(TransItemID = factor(TransItemID),
                TransID     = factor(TransID),
                UPC         = factor(UPC))

# III-3) TRANSACTION Attributs #########################################################################################
# Number of observations does EQUAL the TransID factor levels

TransAttr   <- plyr::ldply(StoreList_TransAttr, data.frame) %>% 
      mutate(TransID       = factor(TransID),
            #TransNo         not needed anymore 
             Type          = factor(Type),
             Store         = factor(Store),
             Lane          = factor(Lane),
             CashNo        = factor(CashNo),
             CashName      = as.character(CashName),
             CustNo        = factor(CustNo),
             CustName      = as.character(CustName),
             ZIPCash       = factor(clean.zipcodes(ZIPCash))) %>% 
      left_join(TransItemCount, by = "TransID")
             rm(TransItemCount)

# Create start/end date-time variables that cannot be built in mutate/transmute command
TransAttr$SDateTime            <- strptime(paste(TransAttr$SDate, TransAttr$STime), format = "%Y-%m-%d %H:%M:%S")
TransAttr$EDateTime            <- strptime(paste(TransAttr$EDate, TransAttr$ETime), format = "%Y-%m-%d %H:%M:%S")
TransAttr$Duration_sec         <- TransAttr$EDateTime-TransAttr$SDateTime

TransAttr <- select(TransAttr,
                    TransID, Type,
                    TotalSales, UniqueUPCs,
                    SDateTime, EDateTime, Duration_sec,
                    Store, Lane,
                    CashNo, CashName, CustNo, CustName,
                    ZIPCash)

rm(FileList_TransItems,
   FileList_ItemAttr,
   FileList_TransAttr)

# IV) SAVE Processed XML Transaction Data ######################################################################################

setwd("~/Projects/Transaction Data Analyses/3 PD/3 Market Basket Analysis/RedPlum Coupon Wrapper")

saveRDS(  TransAttr,  paste0("Transaction Attributes_All Stores_", date_begin," - ", date_end,".rds")) 
write.csv(TransAttr,  paste0("Transaction Attributes_All Stores_", date_begin," - ", date_end,".csv"), row.names = F)

saveRDS(  ItemAttr,   paste0("Item Attributes_All Stores_", date_begin," - ", date_end,".rds")) 
write.csv(ItemAttr,   paste0("Item Attributes_All Stores_", date_begin," - ", date_end,".csv"), row.names = F)

saveRDS(  TransItems, paste0("Transaction Items_All Stores_", date_begin," - ", date_end,".rds")) 
write.csv(TransItems, paste0("Transaction Items_All Stores_", date_begin," - ", date_end,".csv"), row.names = F)
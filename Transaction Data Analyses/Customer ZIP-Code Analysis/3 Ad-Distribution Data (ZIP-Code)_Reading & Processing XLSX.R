# Settings #############################################################################################################

library(xlsx)
library(dplyr)

options(stringsAsFactors = FALSE)

# Read, Process Data ###################################################################################################

# Ad-Distribution Data per Store, ZIP code, ATZ

setwd("C:/Users/Stefan/Google Drive/Projects/3 Transaction-Data Analyses/1 Raw Data/Valassis")

Distribution_ATZ  <- read.xlsx2(list.files(), 1,
                        colIndex   = c(1:4, 13),
                        colClasses = c(rep("character", 3), rep("numeric", 2)),
                        stringsAsFactors = F) %>%
      rename(Store = Site,
             City  = Name,
             #ATZ already correctly named
             HouseholdCount = Household.Count,
             StoreDistance  = Distance.to.Site..miles.) %>%
      filter(!is.na(StoreDistance)) %>%
      mutate(ZIP   = factor(substr(ATZ, 1, 5)),
             Sub   = factor(substr(ATZ,     6, nchar(ATZ))),
             Store = factor(Store,
                            levels = c("57th & Kedzie - 5724 S Kedzie Ave, Chicago, IL, 60629",               # Store 1
                                       "47th & Kedzie - 4700 S Kedzie Ave, Chicago, IL, 60632",               #       2
                                       "43rd & Pulaski - 4343 S Pulaski Rd, Chicago, IL, 60632",              #       3
                                       "West Lawn - 5838 S Pulaski Rd, Chicago, IL, 60629",                   #       4
                                       "118th & Ave O - 3448 E 118th St, Chicago, IL, 60617",                 #       5
                                       "Cermak - 2526 W Cermak Rd, Chicago, IL, 60608",                       #       6
                                       "Calumet City - 1968 Sibley Blvd, Calumet City, IL, 60409",            #       7
                                       "Evergreen Park - 3720 W 95th St, Evergreen Park, IL, 60805",          #       8
                                       "OakBrook Terrace - 17w675 Roosevelt Rd, Oakbrook Terrace, IL, 60181", #       9
                                       "Madison - 2333 W Madison St, Chicago, IL, 60612",                     #       10
                                       "Bridgeview - 10280 S Harlem Ave, Bridgeview, IL, 60455",              #       11
                                       "Oak Park - 259 Lake St, Oak Park, IL, 60302"),                        #       12
                            labels = as.character(seq(1, 12, 1)))) %>% 
      select(Store,
             City, 
             ZIP,
             ATZ,
             Sub,
             HouseholdCount)

Distribution_ATZ$Sub[Distribution_ATZ$Sub == ""]    <- NA

# Sales Data per Store

setwd("C:/Users/Stefan/Google Drive/Projects/4 Weekly Ad/2 Processed Data/1 Ad & Sales Reporting")

StartWeek   <- "2015-01-21"
EndWeek     <- "2016-01-13"

Count_Store <- read.csv("4 Stores.csv", colClasses = "character") %>% 
      select(Week, Store, StoreTransactions) %>% 
      transmute(Week          = as.Date(   Week),
                Store         = factor(    Store, levels = as.character(seq(1, 12, 1))),
                CustomerCount = as.numeric(StoreTransactions)) %>% 
      filter(Week >= StartWeek & Week <= EndWeek) %>% 
      group_by(Store) %>% 
      summarise(TotalCustomerCount = sum(CustomerCount, na.rm = T),
                MinDate            = min(Week),
                MaxDate            = max(Week)+6) %>% 
      mutate(   WeekSpan2          = paste(MinDate, MaxDate, sep = " - "),
                WeekAmount2        = (MaxDate-MinDate+1)/7) %>% 
      select(-MinDate, -MaxDate)

# Retrieve store sales from aggregated department sales because hot foods would have been excluded in store sales
Sales_Store <- read.csv("3 Departments.csv", colClasses = "character") %>% 
      select(Week, Store, Dept, DeptOverallSales) %>% 
      transmute(Week          = as.Date(   Week),
                Store         = factor(    Store, levels = as.character(seq(1, 12, 1))),
                Dept          = factor(Dept),
                Sales         = as.numeric(DeptOverallSales)) %>% 
      filter(Week >= StartWeek & Week <= EndWeek) %>% 
      group_by(Store) %>% 
      summarise(TotalSales         = sum(Sales, na.rm = T),
                MinDate            = min(Week),
                MaxDate            = max(Week)+6) %>% 
      mutate(   WeekSpan           = paste(MinDate, MaxDate, sep = " - "),
                WeekAmount         = (MaxDate-MinDate+1)/7) %>% 
      select(-MinDate, -MaxDate) %>% 
      left_join(Count_Store, by = c("Store")) %>% 
      select(-WeekSpan2, -WeekAmount2)

# Export Data ##########################################################################################################

# Store, ZIP-code, ATZ level

setwd("C:/Users/Stefan/Google Drive/Projects/3 Transaction-Data Analyses/2 Processed Data")
      
Distribution_ATZ                                                  %>% 
      write.csv("Weekly Ad Distribution_Household Counts per Store, ZIP, ATZ.csv", row.names = F)

Distribution_ZIP    <- summarize(group_by(Distribution_ATZ, Store, City, ZIP),
                         HouseholdCount = sum(HouseholdCount))    %>%
      write.csv("Weekly Ad Distribution_Household Counts per Store, ZIP.csv", row.names = F)
      
Distribution_Store  <- summarize(group_by(Distribution_ATZ, Store),
                         HouseholdCount = sum(HouseholdCount))    %>%
      left_join(Sales_Store, by = "Store") %>% 
      write.csv("Weekly Ad Distribution_Household Counts per Store.csv", row.names = F)
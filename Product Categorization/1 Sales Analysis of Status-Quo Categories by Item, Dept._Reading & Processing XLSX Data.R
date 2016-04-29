# 0) Settings ##########################################################################################################

# Load packages
library(openxlsx)
library(xlsx)
library(plyr)
library(dplyr)
library(stringr)
library(XLConnect)

# Disable scientific notation and display UPCs correctly
options(scipen=999)

# 1) DATA IMPORT: SUB-DEPT. List w/ main departments ###################################################################
## Contains all sub-departments in rows with their particular main department

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
      #  Determine data type of columns (Suppress specific warnings "NAs introduced by coercion" since that's intended)
      mutate(SubDeptNo = suppressWarnings(as.numeric(SubDeptNo)),
             DeptNo    = suppressWarnings(as.numeric(   DeptNo))) %>%
      # Filter data "signal", rows with department information indicated by sub-department numbers and
      # Filter out  "noise",  rows with titles, footers, page breaks, etc. (coerced to NAs) 
      filter(!is.na(SubDeptNo)) %>% 
      # Factorize the department variables
      mutate(DeptNo    = factor(DeptNo),
             SubDeptNo = factor(SubDeptNo))

# Filter out duplicated rows
Departments <- filter(Departments, !duplicated(Departments))

# 2) DATA IMPORT: ITEM Price List by CATEGORY ##########################################################################

setwd("~/Projects/Sales Data Analyses/1 RD/2 Item Price List by Category")
files             <- list.files()
FileList          <- list()
length(FileList)  <- length(files)

for (h in 1:length(files)) {
      # Read containing file, subset the relevant columns and name them
      FileList[[h]]    <- openxlsx::read.xlsx(files[h], colNames = F) %>% 
            select(X1:X3) %>% 
            rename(UPC        = X1,
                   CategoryNo = X2,
                   Category   = X3) %>% 
            # Remove "data noise" (empty rows, titles, headers, footers, etc.) from the messy report by coercing to NAs and removing them; 
            # Keep meaningful "data signal" which are characterized by numeric values in UPC column.
            mutate(UPC        = suppressWarnings(as.numeric(UPC))) %>%
            filter(!is.na(UPC)) %>% 
            # Define residual columns
            mutate(Category   =        as.character(Category),
                   CategoryNo = factor(as.character(CategoryNo)))
}

# Collapse dataframes of the list into one dataframe (Remove data objects that are not needed anymore)
Categories  <- ldply(FileList, data.frame); rm(files, FileList, h)
# Filter out duplicated rows
Categories <- filter(Categories, !duplicated(Categories))

# 3) DATA IMPORT: PRODUCT w/ movement ##################################################################################

# Read item movement data
Items <- readRDS("~/Projects/Sales Data Analyses/3 PD/2 Marketing Mix Models/ALL DEPT. Item Movement & Price Points Weekly per Store.rds")
Items <- mutate(Items,
                Week_factor = factor(as.character(Week)),
                UPC         = as.character(UPC))
                      
# Determine sales week w/ 7 days & 7+ days (special sales weeks like Thanksgiving & Christmas)                      
Items$WeekAfterNo_factor <- sapply(Items$Week_factor,        function(x) {grep(x, levels(Items$Week_factor))+1})
Items$WeekAfter_factor   <- sapply(Items$WeekAfterNo_factor, function(x) {c(levels(Items$Week_factor), 
                                                                            as.character(max(Items$Week)+6))[x]})
                      

# 4) DATA JOIN: INSTACART Category List ################################################################################

# Read & join INSTACART Category List w/ Items
setwd("~/Projects/Sales Data Analyses/1 RD/0 Other/1 Instacart Category List/1 RD")
Insta <- read.csv(list.files()) %>% 
      transmute(UPC         = as.character(Data.Entry.Items.Lookup.Code),
                Dept_Insta  = as.character(Departments.Name),
                Aisle_Insta = as.character(Aisles.Name))

Items <- left_join(Items,
                   Insta, by = "UPC")

# Export enhanced INSTACART Category List w/ Product descr & (Sub-)Dept.
setwd("~/Projects/Sales Data Analyses/1 RD/0 Other/1 Instacart Category List/2 PD")
Insta2 <- select(Items,
                 Dept_Insta, Aisle_Insta,
                 UPC, Product,
                 Dept, SubDept) %>% unique()

write.csv(Insta2, "Instacart_Dept, Aisle, UPC, Product, Dept, Sub-Dept.csv", row.names = F)
      

# 5) DATA EXPORT: CATEGORY Lists w/ (Sub-)Depts. & Items ############################################################

DeptCatProd <- select(Items,
                      Week, # ---------> Aggregate Sales across Weeks
                      Dept, SubDept, UPC, Product, Sales)               %>%
      group_by(       Dept, SubDept, UPC, Product)                      %>% 
      summarise(TotalSales     = sum(              Sales, na.rm = T),
                AvgWeeklySales = sum(              Sales, na.rm = T)/
                                                   # Number of weeks reported: Last reporting day - First reporting day divided by 7 days            
                                                   round(as.numeric((         (max(Items$Week)+6) - min(Items$Week)     )/7),            0)) %>%
      left_join(Categories, by = "UPC") %>% 
      mutate(UPC = factor(UPC))
      #select(Dept, SubDept, Category, UPC, Product) %>% 
      

DeptCat     <-  
      group_by(DeptCatProd,
                      Dept, SubDept, CategoryNo, Category)%>%
      summarise(TotalSales = sum(TotalSales, na.rm = T),
                UPCCount   = n_distinct(UPC)) %>% 
      ungroup() %>% 
      #distinct(Category) %>% 
      mutate(CategoryNo = factor(CategoryNo),
             Category   = factor(Category))

setwd("~/Projects/Sales Data Analyses/3 PD/3 Reform Product Category System")
write.csv(DeptCatProd, "Category List_Depts & Products.csv", row.names = F)
write.csv(DeptCat,     "Category List_Depts.csv", row.names = F)

## Load Packages #######################################################################################################

library(stringr)
library(tidyr)
library(dplyr)

## 0 Select Department to be categorized ###############################################################################

SelectedDept <- "LIQUOR"

## 1 Read Category/Item Data ###########################################################################################

## a) Read Category data

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
            filter(!is.na(UPC))
}

# Collapse dataframes of the list into one dataframe
Categories  <- ldply(FileList, data.frame); rm(files, FileList, h)
# Filter out duplicated rows
Categories <- filter(Categories, !duplicated(Categories)) %>% 
      # Define columns
      mutate(UPC        =        as.character(UPC),
             Category   = factor(as.character(Category)),
             CategoryNo = factor(as.character(CategoryNo)))

## b) Read Item data
Items <- readRDS("~/Projects/Sales Data Analyses/3 PD/2 Marketing Mix Models/ALL DEPT. Item Movement & Price Points Weekly per Store.rds") %>% 
      dplyr::filter(Dept == SelectedDept) %>% 
      select(  Dept, SubDept, Product, UPC, Sales) %>% 
      mutate(UPC = as.character(UPC)) %>% 
      group_by(Dept, SubDept, Product, UPC) %>%
      summarize(TotalSales = sum(Sales)) %>% 
      join (Categories) %>% 
      mutate(UPC        = factor(as.character(UPC)),
             CategoryNo = factor(as.character(CategoryNo)),
             Category   = factor(as.character(Category)))

colnames(Items) <- c("Dept", "SubDept", "Descr", "No", "Sales", "CatNo", "CatDesc")

rm(Categories)

## c) Read Category/Item Instacart data

setwd("~/Projects/Meta Data/Instacart")
ItemsInsta <- read.csv("Instacart_Dept, Aisle, UPC_2016-02-25.csv") %>% 
      transmute(Dept  = factor(Departments.Name),
                Aisle = factor(Aisles.Name),
                No    = suppressWarnings(as.numeric(Data.Entry.Items.Lookup.Code))) %>%
      mutate(   No    = as.character(No))

## 2a Manipulate Item-Description Data #################################################################################

## I) Clean item descriptions (Remove punctuation, symbols & numbers)

Items_wide <- mutate(Items,
                Descr = str_replace_all(as.character(Descr),  "[[:punct:]]", " ")) %>%
         mutate(Descr = str_replace_all(as.character(Descr),  "\\(?[0-9,.]+\\)?", ""))

## II) Dissect item descriptions into single strings and add as columns (one per description string)


Item_Descr_split  <- sapply(Items_wide$Descr, str_split, " ")
Items_wide        <- suppressWarnings(cbind(Items_wide,
                                            as.data.frame(t(sapply(Item_Descr_split, "[", i = seq_len(max(sapply(Item_Descr_split, length))))))))
                                                                                                         rm(Item_Descr_split)
## III) Reshape data set from wide format (strings side by side) to long format (strings below each other)
                                                                                                         
Items_long <- gather(Items_wide,"DescrStringNo", "DescrString", -c(1:7), na.rm = TRUE) %>% 
      arrange(Dept, No, DescrStringNo) %>% 
      mutate(DescrString = factor(DescrString))

## IV) Extract unique item-description strings and compute frequencies

ItemDescrStrings <- data.frame(sort(table(Items_long$DescrString), decreasing = T)) %>%
      add_rownames("DescrString")
# Sort strings by appearance frequency in item descriptions          
names(ItemDescrStrings)[2] <-                 "Frequency"
ItemDescrStrings <- arrange(ItemDescrStrings, desc(Frequency)) %>% 
      # Add category column that will be populated shortly
      mutate(Category = NA,
             Dept     = SelectedDept) %>% 
      select(Category, DescrString, Frequency, Dept)

## 2b Manipulate Item-Category Data ####################################################################################

ItemCategories <- select(Items_wide,
                         Dept, CatDesc, No, Sales) %>% 
      group_by(          Dept, CatDesc) %>% 
      summarise(NoItems    = n_distinct(No),
                TotalSales = sum(Sales)) %>% 
      # Add category column that will be populated shortly
      mutate(Category = NA)

## 2c Manipulate Item-Sub-Department Data ##############################################################################

ItemSubDepts <- select(Items_wide,
                       Dept, SubDept, No, Sales) %>% 
      group_by(        Dept, SubDept) %>% 
      summarise(NoItems    = n_distinct(No),
                TotalSales = sum(Sales)) %>% 
      # Add category column that will be populated shortly
      mutate(Category = NA)

## 2d Manipulate Category/Item Instacart Data ##########################################################################

ItemInstaAisles <- group_by(ItemsInsta,
                            Dept, Aisle) %>% 
      summarize(NoItems    = n_distinct(No)) %>% 
      # Add category column that will be populated shortly
      mutate(Category = NA)

## 3a Categorize w/ help of Item-Description Strings ###################################################################

ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="VODKA"]                      <- "Vodka"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="PINOT"]                      <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="TEQUILA"]                    <- "Tequila/Mezcal"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="CABERNET"]                   <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SAUVIGNON"]                  <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="MOSCATO"]                    <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="WINE"]                       <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="CHARDONNAY"]                 <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="RUM"]                        <- "Rum"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="MERLOT"]                     <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BEER"]                       <- "Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SMIRNOFF"]                   <- "Vodka"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="NOIR"]                       <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="CUERVO"]                     <- "Tequila/Mezcal"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SUTTER"]                     <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="ALE"]                        <- "Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BAREFOOT"]                   <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BLANC"]                      <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BACARDI"]                    <- "Rum"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="GRIGIO"]                     <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BRANDY"]                     <- "Brandy/Cognac"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BUD"]                        <- "Domestic Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="WHISKEY"]                    <- "Whiskey"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="IPA"]                        <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="REPOSADO"]                   <- "Tequila/Mezcal"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SCOTCH"]                     <- "Whiskey"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="ROSE"]                       <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="MILLER"]                     <- "Domestic Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="AMSTERDAM"]                  <- "Vodka"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BOURBON"]                    <- "Whiskey"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BERINGER"]                   <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="ZINFANDEL"]                  <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="MARGARITA"]                  <- "Cocktails, Alcoholic Pre-mixed RTD"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="GIN"]                        <- "Gin"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="RIESLING"]                   <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SPARKLING"]                  <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="LAGUNITAS"]                  <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="LIQUEUR"]                    <- "Liqueur/Cordial"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SANGRIA"]                    <- "Cocktails, Alcoholic Pre-mixed RTD"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BRUT"]                       <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="FRANZIA"]                    <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="PALE"]                       <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SAUZA"]                      <- "Tequila/Mezcal"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="STELLA"]                     <- "Imported Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="ABSOLUT"]                    <- "Vodka"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SVEDKA"]                     <- "Vodka"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="MALBEC"]                     <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="MORGAN"]                     <- "Rum"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="ADAMS"]                      <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="CUPCAKE"]                    <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="REVOLUTION"]                 <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="HEINEKEN"]                   <- "Imported Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="CIROC"]                      <- "Vodka"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="RABBIT"]                     <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="COGNAC"]                     <- "Brandy/Cognac"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="KENDALL"]                    <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="LITE"]                       <- "Domestic Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="MONDAVI"]                    <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SHIRAZ"]                     <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="ANCHOR"]                     <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="COORS"]                      <- "Domestic Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="GOLIATH"]                    <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="DRAFT"]                      <- "Domestic Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="REMY"]                       <- "Brandy/Cognac"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SAUV"]                       <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="CORONA"]                     <- "Imported Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="MODELO"]                     <- "Imported Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="ANGRY"]                      <- "Hard Cider"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BOLLA"]                      <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="CHIANTI"]                    <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="DANIELS"]                    <- "Whiskey"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="FETZER"]                     <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="GATO"]                       <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="GUINNESS"]                   <- "Imported Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BEAM"]                       <- "Whiskey"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="MALIBU"]                     <- "Rum"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BUDWEISER"]                  <- "Domestic Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="CRUZAN"]                     <- "Rum"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="MARTINI"]                    <- NA
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="PROSECCO"]                   <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="REDDS"]                      <- "Hard Cider"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BUZZBALLZ"]                  <- "Cocktails, Alcoholic Pre-mixed RTD"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="NEVADA"]                     <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="STOLICHNAYA"]                <- "Vodka"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="WHISKY"]                     <- "Whiskey"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="ASTI"]                       <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="CASILLERO"]                  <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="LEINENKUGELS"]               <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SANTERO"]                    <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="DESCHUTES"]                  <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="MENAGE"]                     <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="LIVINGSTON"]                 <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="MOET"]                       <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="CHAMPAGNE"]                  <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="LEINENKUGEL"]                <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="STIEGL"]                     <- "Imported Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BAILEYS"]                    <- "Cream Liqueur"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="COPPOLA"]                    <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="DEKUYPER"]                   <- "Liqueur/Cordial"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="DOGFISH"]                    <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="MIRASSOU"]                   <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="RUFFINO"]                    <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SKYY"]                       <- "Vodka"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="TECATE"]                     <- "Imported Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="TROIS"]                      <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="WINES"]                      <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="FAIRBANKS"]                  <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="FIRESTONE"]                  <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SAPPORO"]                    <- "Imported Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SOHNE"]                      <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SVYTURYS"]                   <- "Imported Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="HENNESSY"]                   <- "Brandy/Cognac"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="JIMADOR"]                    <- "Tequila/Mezcal"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="APPLETON"]                   <- "Rum"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BARON"]                      <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BELVEDERE"]                  <- "Vodka"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BERGHOFF"]                   <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="EQUIS"]                      <- "Imported Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="COLLINS"]                    <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="RTD"]                        <- "Cocktails, Alcoholic Pre-mixed RTD"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="TAIL"]                       <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="MIKES"]                      <- "Hard Cider"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SILVER"]                     <- "Tequila"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="HARD"]                       <- "Hard Cider"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BROTHERS"]                   <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="DAILYS"]                     <- "Cocktails, Alcoholic Pre-mixed RTD"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="NAKED"]                      <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="FRONTERA"]                   <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="CREAM"]                      <- "Cream Liqueur"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="PINNACLE"]                   <- "Vodka"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="LAKES"]                      <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="JACKSON"]                    <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="ESCAPES"]                    <- "Cooler"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="REX"]                        <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="LAGER"]                      <- "Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="FLYING"]                     <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="LAKES"]                      <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="LEFT"]                       <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="OPERA"]                      <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="PATRON"]                     <- "Spirit"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="CELLARS"]                    <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="JOHNNIE"]                    <- "Whiskey"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BELGIUM"]                    <- "Craft Beer"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="DUBOUCHETT"]                 <- "Spirit"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="JULIO"]                      <- "Tequila"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="VIEJO"]                      <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="VINES"]                      <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="CHATEAU"]                    <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="DIVA"]                       <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="GREY"]                       <- "Vodka"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="VELLA"]                      <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="WARKA"]                      <- "Imported Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="KOVAL"]                      <- "Spirit"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="STERLING"]                   <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="ANDRE"]                      <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BUSCH"]                      <- "Domestic Beer (Mainstream)"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="KORBEL"]                     <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="SERA"]                       <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="BELLA"]                      <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="COASTAL"]                    <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="COLUMBIA"]                   <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="LOUIS"]                      <- "Wine"
ItemDescrStrings$Category[ItemDescrStrings$Dept=="LIQUOR" & ItemDescrStrings$DescrString=="TANQUERAY"]                  <- "Spirit"

ItemDescrStrings$Category <- factor(ItemDescrStrings$Category)

## 3b Categorize w/ help of Item Categories (Old) ######################################################################

ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="AMARETTO"]                            <- "Liqueur/Cordial"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="CABERNET"]                            <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="CHARDONNAY"]                          <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="CONDIMENTS"]                          <- NA
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="GIFT-BAGS/WRAP/TOTES"]                <- NA
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="MALBEC"]                              <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="MIMOSA"]                              <- "Cocktails, Alcoholic Pre-mixed RTD"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="PAPER/PLASTIC PLATES/BOWLS/CUP"]      <- NA
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="PORT"]                                <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="ROSE"]                                <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="RUM"]                                 <- "Rum"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="SHERRY"]                              <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="SPICED RUM"]                          <- "Rum"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="WHISKEY"]                             <- "Whiskey"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="BEER"]                                <- "Beer"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="CABERNET MERLOT"]                     <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="CHIANTI"]                             <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="CORDIALS"]                            <- "Liqueur/Cordial"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="GIN"]                                 <- "Gin"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="MERLOT"]                              <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="MOSCATO"]                             <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="PINOT"]                               <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="PROSECCO"]                            <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="RTD-MIX DRINKS "]                     <- "Cocktails, Alcoholic Pre-mixed RTD"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="SALT"]                                <- "Liquor Ingredients (edible)"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="SHIRAZ"]                              <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="STOUTS/PORTER"]                       <- "Beer"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="WHITE"]                               <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="BOURBON"]                             <- "Whiskey"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="CABERNET SAUVIGNON"]                  <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="CHOCOLATES"]                          <- "Liquor Chocolate"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="EGG NOG"]                             <- "Cocktails, Alcoholic Pre-mixed RTD"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="GROCERY-CANDY"]                       <- "Liquor Chocolate"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="MEXICAN BEER"]                        <- "Imported Beer (Mainstream)"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="NON-ALCOHOLIC MIX"]                   <- "Cocktail Mixes, Non-Alcoholic , To Be Mixed"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="PINOT GRIGIO"]                        <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="RED"]                                 <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="RTD POUCHEES"]                        <- "Cocktails, Alcoholic Pre-mixed RTD"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="SANGRIA"]                             <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="SOUTHERN SIGNATURE WINE"]             <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="TEMPRANILLO"]                         <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="WINE"]                                <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="BRANDY"]                              <- "Brandy/Cognac"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="CABERNET SHIRAZ"]                     <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="CIDERS"]                              <- "Hard Ciders"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="FLORAL MISC"]                         <- NA
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="LIQUOR DEPT SUPPLIES"]                <- "Liquor Supplies (non-edible/drinkable)"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="MEZCAL"]                              <- "Tequila/Mezcal"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="NON FOOD"]                            <- NA
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="PINOT GRIS"]                          <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="RIESLING"]                            <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="RTD-READY TO DRINK"]                  <- "Cocktails, Alcoholic Pre-mixed RTD"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="SAUVIGNON BLANC"]                     <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="SOUTHERN WINE"]                       <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="TEQUILA"]                             <- "Tequila/Mezcal"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="WINE COOLERS"]                        <- "Cooler"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="BRUT"]                                <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="CHAMPAGNE"]                           <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="COGNAC"]                              <- "Brandy/Cognac"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="GEWURZTRAMINER"]                      <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="LIQUOR/SPIRITS"]                      <- "Spirit"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="MICHELADA"]                           <- "Beer"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="OTR-OVER THE ROCKS"]                  <- "Cocktails, Alcoholic Pre-mixed RTD"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="PINOT NOIR"]                          <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="ROMPOPE"]                             <- "Cocktails, Alcoholic Pre-mixed RTD"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="RTS-READY TO SERVE"]                  <- "Cocktails, Alcoholic Pre-mixed RTD"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="SCOTCH"]                              <- "Whiskey"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="SPARKLING"]                           <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="VODKA"]                               <- "Vodka"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="ZINFANDEL"]                           <- "Wine"
ItemCategories$Category[ItemCategories$Dept=="LIQUOR" & ItemCategories$CatDesc =="<NA>"]                                <- NA

ItemCategories$Category <- factor(ItemCategories$Category)

## 3c Categorize w/ help of Item-Sub-Departments #######################################################################

ItemSubDepts$Category[ItemSubDepts$Dept=="LIQUOR" & ItemSubDepts$SubDept =="BAR"]                                       <- "Bar"
ItemSubDepts$Category[ItemSubDepts$Dept=="LIQUOR" & ItemSubDepts$SubDept =="BEER"]                                      <- "Beer"
ItemSubDepts$Category[ItemSubDepts$Dept=="LIQUOR" & ItemSubDepts$SubDept =="LIQUOR"]                                    <- "Spirit"
ItemSubDepts$Category[ItemSubDepts$Dept=="LIQUOR" & ItemSubDepts$SubDept =="WINE"]                                      <- "Wine"
ItemSubDepts$Category[ItemSubDepts$Dept=="LIQUOR" & ItemSubDepts$SubDept =="LIQUOR MISC HT"]                            <- NA
ItemSubDepts$Category[ItemSubDepts$Dept=="LIQUOR" & ItemSubDepts$SubDept =="LIQUOR MISC LT"]                            <- NA

ItemSubDepts$Category <- factor(ItemSubDepts$Category)

## 3d Categorize w/ help of Instacart Item-Department-Aisles ###########################################################

ItemInstaAisles$Category[ItemInstaAisles$Dept=="Alcohol" & ItemInstaAisles$Aisle =="Cocktail Mixes"]                    <- "Cocktail"
ItemInstaAisles$Category[ItemInstaAisles$Dept=="Alcohol" & ItemInstaAisles$Aisle =="Red Wines"]                         <- "Wine"
ItemInstaAisles$Category[ItemInstaAisles$Dept=="Alcohol" & ItemInstaAisles$Aisle =="Rosés"]                             <- "Wine"
ItemInstaAisles$Category[ItemInstaAisles$Dept=="Alcohol" & ItemInstaAisles$Aisle =="Specialty Wines & Champagnes"]      <- "Wine"
ItemInstaAisles$Category[ItemInstaAisles$Dept=="Alcohol" & ItemInstaAisles$Aisle =="White Wines"]                       <- "Wine"
ItemInstaAisles$Category[ItemInstaAisles$Dept=="Alcohol" & ItemInstaAisles$Aisle =="Spirits"]                           <- "Spirit"

## 3' Check Categorizations ############################################################################################

# Check all categories for typos
sort(unique(c(unique(as.character(ItemDescrStrings$Category)),
              unique(as.character(ItemCategories$Category)),
              unique(as.character(ItemSubDepts$Category)),
              unique(as.character(ItemInstaAisles$Category))
              )))

## 4 Join Categorizations with Items ###################################################################################


Data <- spread(DescrString_Cat, DescrString_No, DescrString)

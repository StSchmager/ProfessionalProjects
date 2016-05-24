library(stringr)
library(tidyr)

Dept <- "LIQUOR"

# Read item data
Item <- readRDS("~/Projects/Sales Data Analyses/3 PD/2 Marketing Mix Models/ALL DEPT. Item Movement & Price Points Weekly per Store.rds") %>% 
      select(Dept, UPC, Product) %>% 
      filter(Dept == Dept) %>% 
      unique() %>% 
      transmute(Dept    = factor(Dept), 
                Item_No = as.character(UPC),
                Descr   = str_replace_all(as.character(Product),  "[[:punct:]]", " ")) %>% 
      mutate(   Descr   = str_replace_all(as.character(Descr),  "\\(?[0-9,.]+\\)?", ""))

# Dissect item descriptions into single strings
Item_Descr_split  <- sapply(Item$Descr, str_split, " ")

# Bind columns (containing single strings) to Item dataset
Item        <- cbind(Item, as.data.frame(t(sapply(Item_Descr_split, "[", i = seq_len(max(sapply(Item_Descr_split, length)))))))

# Reshape data set from wide format to long format
Item_long   <- gather(Item, "String_No", "String", -c(1:3), na.rm = TRUE) %>% 
      arrange(Dept, Item_No, String_No) %>% 
      mutate(String = factor(String))

# Get frequency table of strings
setwd("/home/radmin/Projects/Sales Data Analyses/3 PD/3 Reform Product Category System/Categorize Items by Dept")
          String_Freq          <- data.frame(sort(table(Item_long$String), decreasing = T)) %>% add_rownames("String")
    names(String_Freq)[2]      <- "Frequency"
          String_Freq$Category <- NA 
write.csv(select(String_Freq, Category, String, Frequency),
                                  paste(Dept, "Product Categorization.csv"), row.names = F)
String_Cat <- openxlsx::read.xlsx(paste(Dept, "Product Categorization.xlsx"), 1) %>% 
      mutate(String = factor(String)) %>% 
      select(-Frequency) %>% 
      left_join(Item_long)

Data <- spread(String_Cat, String_No, String)

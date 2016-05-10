# 1) Download Raw (XML) Data ###########################################################################################

# View mindmap on Coggle: https://coggle.it/diagram/VxZRI2-gIRhJZNvh
# Download this mindmap as .mm File and name file
File <- "Product Categories.mm"
# Save into working directory and read file from there
setwd("C:/Users/Stefan/Google Drive/Projects/3 Transaction-Data Analyses/0 Org/3 Market Basket Analysis/Product Categorization/Coggl Mindmap")

# 2) Read tree-shaped XML Data & Process/Convert to Tabular XLSX Data ##################################################

## I. Read XML

# Load packages
library(XML)
library(plyr)
library(dplyr)
library(data.table)
library(xlsx)

# Read complete XML tree
CategoryTree      <- xmlInternalTreeParse(File, getDTD = F)

## II. Convert XML to (Nested) List in R

# Get all nodes
AllNodes                <- xpathSApply(CategoryTree, '//node', xmlGetAttr, "TEXT", default = NA)
AllNodeSet              <- getNodeSet( CategoryTree, "//*[@TEXT]")
# Get leave nodes (only the nodes without child nodes)
LeaveNodes              <- xpathSApply(CategoryTree, '//node[not(node)]', xmlGetAttr, "TEXT", default = NA)
LeaveNodeSet            <- getNodeSet( CategoryTree, '//node[not(node)]')

# Get ancestor nodes (parents, grandparents) of all leave nodes in nested list
LeaveNodeAncestors_List <- sapply(LeaveNodeSet, xmlAncestors, xmlGetAttr, "TEXT", default = NA)

# Name nested list
names(LeaveNodeAncestors_List) <- paste0("LeaveNode", (as.character(seq(1,length(LeaveNodeAncestors_List)))))
for (                                                              i in 1:length(LeaveNodeAncestors_List)) {
      names(LeaveNodeAncestors_List[[i]]) <- paste0("AncestorNode",
                                                    (as.character(seq(  1,length(LeaveNodeAncestors_List[[i]])))))
      }

## III. Convert (Nested) List to Data Frame in R
LeaveNodeAncestors_Table<- rbindlist(LeaveNodeAncestors_List, fill = T) %>% as.data.frame() %>% 
      select(-c(1:2)) %>% 
      transmute(Dept    = gsub("#", "", AncestorNode3),
                SubDept = gsub("#", "", AncestorNode4),
                Cat1    = gsub("#", "", AncestorNode5),
                Cat2    = gsub("#", "", AncestorNode6),
                Cat3    = gsub("#", "", AncestorNode7)) %>% 

# 3) Export Processed XLSX Data ########################################################################################

write.xlsx("Product Categories.xlsx", row.names = F)
# 1) Download Raw (XML) Data ###########################################################################################

# View mindmap on Coggle: https://coggle.it/diagram/VxZRI2-gIRhJZNvh
# Download this mindmap as .mm File and name file
FileName <- "Product Categories.mm"
# Save into working directory and read file from there
setwd("C:/Users/Stefan/Google Drive/Projects/3 Transaction-Data Analyses/0 Org/3 Market Basket Analysis/Product Categorization")

# 2) Read & Process Data ###############################################################################################

# Load packages
library(XML)
library(plyr)

## Read XML data and convert to R dataobjects

# Read complete XML tree
CategoryTree      <- xmlInternalTreeParse(FileName, getDTD = F)
CategoryNodes     <- getNodeSet(CategoryTree, "//*[@TEXT]")

# Select all nodes
AllNodes          <- xpathSApply(CategoryTree, '//node', xmlGetAttr, "TEXT", default = NA)
# Select only the leave nodes (nodes without child nodes)
LeaveNodes        <- xpathSApply(CategoryTree, '//node[not(node)]', xmlGetAttr, "TEXT", default = NA)
LeaveNodeSet      <- getNodeSet( CategoryTree, '//node[not(node)]')

# 3) Export Processed Data #############################################################################################


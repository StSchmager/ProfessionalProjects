# 1) Download Raw (XML) Data ###########################################################################################

# View mindmap on Coggle: https://coggle.it/diagram/VxZRI2-gIRhJZNvh
# Download this Coggl mindmap as .mm File and name file
FileName <- "Product Categories.mm"
# Save into working directory and read file from there
setwd("C:/Users/Stefan/Google Drive/Projects/3 Transaction-Data Analyses/0 Org/3 Market Basket Analysis/Product Categorization")

# 2) Read & Process Data ###############################################################################################

# Load packages
library(XML)
library(plyr)

# Read XML data and convert to R dataobjects
setwd("C:/Users/Stefan/Google Drive/Projects/3 Transaction-Data Analyses/0 Org/3 Market Basket Analysis/Product Categorization")
CategoryTree      <- xmlInternalTreeParse(FileName, getDTD = F)
CategoryNodes     <- getNodeSet(CategoryTree, "//*[@TEXT]")
CategoryList      <- lapply(CategoryNodes, xpathApply, path = './/*[@TEXT]', xmlGetAttr, "TEXT", default = NA)

# 3) Export Processed Data #############################################################################################


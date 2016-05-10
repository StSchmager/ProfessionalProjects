# Sales Data Analyses

These data-processing scripts written in R essentially read, clean, manipulate, and join various Excel reports which are exported from a SQL-based database application. (Underlying data cannot be queried immediately from the database at this point because the overall data infrastructure is not in place yet that allows an RODBC, R open database connection.)
The aspect of data cleaning on this occasion is important since the raw Excel reports are very ill-formatted and messy.
That is because the database export is intended to produce PDF reports; the conversion thereof to an Excel spreadsheet results in data-table breaks caused by headers and footers, as well as skewed data rows and columns.
The processing script fixes these flaws, re-aligns the data, and produces a clean data format with consistent rows and observations.

The final output is a system of datasets with varying aggregation/detail levels that are machine-readable for further data exploration, analysis, and visualization in both R and Tableau. Further, the processed data serves as input data for other reading/processing scripts.

## 1 Ad & Sales Reporting to Company Owners, Department Buyers, and Store Managers

### 1.1 Raw Data (Input)

#### Where is the raw data retrieved from? When is it retrieved? How is it stored?
* Raw data is provided in form of reports which are exported from a database application and saved as Excel spreadsheets (.xlsx)
* Excel reports are pulled separately from each store server for each store on a weekly basis, usually on Wednesdays after the Wednesday-through-Tuesday sales period ends
* Data are locally and systematically stored in work directory _"~/Projects/Sales Data Analyses/1 RD"_ (Raw Data), in
	+ main folders by respective report type,
	+ sub folders by sales week (indicated by the date of the starting Wednesday of sales week), and 
	+ Excel report by store
*  Report types contained in main folders are listed in order by their appearance in the processing script
	+ __Sub-department List__
	+ __Item Price List by Category__
	+ __Store Customer Counts, Sales by Sub-department & Store 2014-15__ (contains already processed CSV datasets from another project that is utilized here)
	+ __Ad-Item Price List with Cost__
	+ __Item Scans & Costs__ (contains database- and company-external Excel spreadsheets with item-cost infromation from vendors)
	+ __Store Customer Counts__
	+ __Item Movement, Sub-department Sales & Costs__
* Each Excel report contains data per store and per sales period (Wed-Tue week) and is named following a consistent naming convention: _Report Title_Date of starting Wednesday of sales week_Store number.xlsx_

#### What information do raw-data Excel reports contain?

The content of the above mentioned report types (exact report title and filename exemplified in parantheses) are explained here in more detail.
Data points in data set are described by what each data observation (table row) represent and how these obsdervations are characterized by different data variables (table columns).

* __Sub-department List__ contains information at the level of sub-departments and their assignment to main-departments (data is not store-/week-specific and is applied across weeks/stores)
	+ Sub-dept. number
	+ Sub-dept. description
	+ Main-Dept. number
	+ Main-Dept. description
* __Item Price List by Category__ contains information at the level of items/products (SKUs) and their assignment to categories (data is not store-/week-specific and is applied across weeks/stores)
	+ Product code (UPC)
	+ Category number
	+ Category description
* __Ad-Item Price List with Cost__ (_Price List with Cost_04-01-2015_1.xlsx_) contains information at the level of items/products (SKUs) that are on-sale during a particular week and at a specific store (assignment thereof to sub-departments, as well as product-price/cost information)
	+ Week ID
	+ Store number
	+ Product code (UPC)
	+ Product description
	+ Sub-dept. description
	+ Price quantity in units
	+ Price per quantity in dollars
	+ Price type
	+ Unit cost in dollars
* __Item Scans & Costs__ contains information at the level of items/products (SKUs) that are sold by specific vendors and put on sale during a particular week and at a specific store. Yet varying in format, the different vendor-specific Excel reports essentially contain the same information. The more acccurate cost information from the vendor validated the above mentioned product unit costs from the database.
	+ Week ID	
	+ Product code (UPC)
	+ Unit cost in dollars
* __Store Customer Counts__ (_Hourly Single Total_04-01-2015_1.xlsx_) contains information at the level of hours of the day aggregated across the days of a particular week and at a specific store
	+ Week ID
	+ Store number
	+ Hour of day
	+ Customer count
	+ Sales in dollars
* __Item Movement, Sub-department Sales & Costs__ (_Item Cost of Goods by Sub-department_04-01-2015_1.xlsx_) contains information at the level of all sold items/products (SKUs), whether on-sale or not, during a particular week and at a specific store (assignment thereof to sub-departments, as well as product-movement/cost/profit information)
	+ Week ID
	+ Store number
	+ Product code (UPC)
	+ Product description
	+ Sub-dept. description
	+ Movement in units sold
	+ Movement in dollars sold (Sales)
	+ Cost in dollars
	+ Profit in dollars
	+ Profit as percentage of dollars sold (Margin)
	
### 1.2 Data Processing

The raw data described above is automatically processed with the help of a script written in the statistical programming language R. More specifically, yet described in total detail in the script comments, the raw data is
* read from Excel files,
* cleaned from unnecessary information,
* joined between various data sources,
* computed to determine more metrics based on the underlying metrics,
* aggregated across varying levels of detail, and
* exported/written to CSV files.

### 1.3 Processed Data: Data-Processing Output and Input for Further Analyses

#### 1.3.a Ad & Sales Reports to Company Owners, Department Buyers, and Store Managers

Output of the whole data pipeline is a system of datasets (.csv). The content of the resulting datasets are explained here in more detail.
Equivalently to the raw data described before, data points are described by what each data row represents (data object) and how these observations are characterized by different metrics (variables) reprsented by columns in the data set.

* __Metrics__ (Columns/Variables)
	+ __Unit Price__ (in USD per unit, e.g. $0.25)
	+ __Sales Price per Price Quantity__ (e.g. 4/$1)
	+ __Unit Cost__ (in USD per unit)
	+ __Unit Movement__ (in units sold)
	+ __Dollar Sales__ (Unit Movement x Unit Price in USD) generated by Ad-Items and overall by All Items
	+ __Costs of Goods Sold__ (Unit Movement x Unit Cost in USD) generated by Ad-Items and overall by All Items
	+ __Profit__ (Dollar Sales - Costs of Goods Sold in USD) generated by Ad-Items and overall by All Items
	+ __Profit__ (as Percentage of Sales): __Margin__ generated by Ad-Items and overall by All Items
	+ __Profit__ (as Percentage of COGS): __Markup__ generated by Ad-Items and overall by All Items
	+ __Ad Sales Share__ (Dollar Sales generated by Ad-Items as Percentage of Overall Sales)
	+ __Ad Profit Share__ (Profit generated by Ad-Items as Percentage of Overall Sales)
	+ __Customer Count__ per Store
	+ __Average Basket Size__ per Store (Overall Sales in USD divided by Customer Count per Store)
* __Objects__ (Observations/Rows)
	+ __Ad-Items__ (individual SKUs/items at UPC-level)
	+ __Categories__ (per store and across stores)
	+ __Sub-Departments__ (per store and across stores)
	+ __Main-Departments__ (per store and across stores)
	+ __Stores__
	+ __Company__

Most metrics are maintained across datasets, although have varying meaning since each dataset has different data objects/observations (rows). For example: __Dollar Sales__ may describe sales for each __Ad-Items_ in one dataset, whereas it describes sales for each __Category__ in the next-level dataset where individual items are grouped by categories.
Some metrics become obsolete when aggregating from one data level to the next one: e.g. Unit Price, Sales Price, Unit Cost, and Unit Movement become irrelevant when aggregating data from an individual SKUs/items at UPC-level to category level where all items of the same kind are grouped.
All other metrics (from Dollar Sales to Ad Profit Share) are now aggregated, i.e. summed or re-computed, at the level of categories across all SKUs/items contained in that category.
Further, the same metrics would be aggregated (summed or re-computed) again when moving from the level of categories to the level of sub-departments. The same logic is applied when moving up further from sub-departments to main-departments, stores, and the company as a whole.
Categories, sub- and main-departments are aggregated two-fold: both per store (e.g. Grocery department at store 1) and aggregated across stores (e.g. Grocery department not store specific).
The metrics Customer Count and Average Basket Size just kick in at the level of stores and company as a whole since they do nt provide any value on the previous levels.

The data described is visualized from different perspectives in Tableau dashboards and provided to different business stakeholders.

#### 1.3.b Marketing-Mix-Model Dataset

This data set contains information at the level of individual SKUs/items at __UPC__-level and their __Week__- and __Store__-specific __Unit Movement__, __Average Price__, and consequently __Dollar Sales__.
Further, the dataset contains item-specific filter variables like __Category, Sub- and Main-Department__, as well as price-specific filter variables; that is __Price Type__ and __Price Label__ (Price per Price Quantity, e.g. 4/$1).

The data described is used as data input for __Marketing-Mix-Modelling__ and further __Sales Analysis (Determining Slow-Moving Items)__ in the following data-analysis script.

## 2 Marketing-Mix Modelling: Determining Price Elasticity & Impact of Ad-Features/Sales-Pricing

## 3 Sales Analysis: Determining Slow-Moving Items per Department & Store
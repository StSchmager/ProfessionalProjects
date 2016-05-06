# Sales Data Analyses

These R data-processing scripts essentially read, clean, manipulate, and join various Excel reports which are pulled from a SQL-based database. (Underlying data cannot be pulled immediately from the database at this point because the overall data infrastructure is not in place yet that allows an ODBC, open database connection.)
The aspect of data cleaning on this occation is important since the raw Excel-based reports are very ill-formatted and messy.
That's because the database export is intended to produce PDF reports; the conversion thereof to an Excel spreadsheet results in data table breaks caused by headers and footers, as well as skewed data rows and columns.
The processing script fixes these shortcomings and re-aligns the data to a clean format with consistent rows and observations.

The final output of the data processing executed by the R scripts is a system of datasets with varying aggregation/detail levels that are readable for further data exploration, analysis, and visualization in Tableau. Further, the processed data serves as input data for other reading/processing scripts.

## 1 Ad & Sales Reporting to Company Owners, Department Buyers, and Store Managers (Reading & Processing XLSX Data)

### Raw Data 

#### Where is the raw data retrieved from? When is it retrieved? How is it stored?
* Raw data is provided in form of reports exported from a database application and saved as Excel spreadsheets (.xlsx)
* Excel reports are pulled seperately for each store weekly, usually Wednesdays, after the Wednesday-through-Tuesday sales period ends
* Each report summarizes particular data per store and per sales period/week
* Data are locally and systematically stored in main-folders named after the respective report type, in sub-folders by sales week (indicated by date of starting Wednesday of sales week), in Excel spreadsheets by store
* Work directory "~/Projects/Sales Data Analyses/1 RD" (Raw Data) contains Excel (XLSX) files which are listed in order by their appearance in the processing script
	+ __Sub-department List__
	+ __Item Price List by Category__
	+ __Store Customer Counts, Sales by Sub-department & Store 2014-15__ (contains already processed CSV datasets from another project that is utilized here)
	+ __Ad-Item Price List with Cost__
	+ __Item Scans & Costs__ (contains database- and company-external Excel spreadsheets with item-cost infromation from vendors)
	+ __Store Customer Counts__
	+ __Item Movement, Sub-department Sales & Costs__
* File name of Excel reports follow universal naming convention: <Report Title>_<Date of starting Wednesday of sales week>_<Store number>.xlsx

#### What information do raw-data Excel reports contain?

The content of the above mentioned report types (exact report titles provided in example filename in parantheses) are explained here in more detail.
Data points are described by what each data observation (table row) represent and how these are characterized by different data variables (table columns).

* __Sub-department List__ contains information at the level of sub-departments and their assignment to main-departments (data is not store or week specific and is applied across weeks/stores)
	+ Sub-dept. number
	+ Sub-dept. description
	+ (Main-)Dept. number
	+ (Main-)Dept. description
* __Item Price List by Category__ contains information at the level of SKU-items/products and their assignment to categories (data is not store or week specific and is applied across weeks/stores)
	+ Product code (UPC)
	+ Category number
	+ Category description
* __Ad-Item Price List with Cost__ (Price List with Cost_04-01-2015_1.xlsx) contains information at the level of items/products (SKUs) that are on-sale during a particular week and at a specific store (assignment thereof to sub-departments, as well as product-price/cost information)
	+ Week ID
	+ Store number
	+ Product code (UPC)
	+ Product description
	+ Sub-dept. description
	+ Price quantity in units
	+ Price per quantity in dollars
	+ Price type
	+ Unit cost in dollars
* _Item Scans & Costs_ contains information at the level of items/products (SKUs) that are sold by specific vendors and put on sale during a particular week and at a specific store. Yet varying in format, the different vendor-specific Excel reports essentially contain the same information. (Additonally, more acccurate cost information from vendor supplements the above mentioned product unit costs from the database)
	+ Week ID	
	+ Product code (UPC)
	+ Unit cost in dollars
* __Store Customer Counts__ (Hourly Single Total_04-01-2015_1.xlsx) contains information at the level of hours of the day aggregated across the days of a particular week and at a specific store
	+ Week ID
	+ Store number
	+ Hour of day
	+ Customer count
	+ Sales in dollars
* __Item Movement, Sub-department Sales & Costs__ (Item Cost of Goods by Sub-department_04-01-2015_1.xlsx) contains information at the level of items/products (SKUs), whether on-sale or not, during a particular week and at a specific store of interest (assignment thereof to sub-departments, as well as product-movement/cost/profit information)
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
	
### Data Processing

The raw data described above is automatically processed with the help of a script written in the statistical programming language R. More specifically, yet described in total detail in the script's comments, the raw data is
* read from Excel files,
* cleaned from unnecessary information,
* joined across various data sources,
* computed to determine more metrics based on the underlying metrics,
* aggregated across varying levels of detail, and
* exported/written to CSV files.

### Processed Data

Output of the whole data pipeline is a systems of datasets (CSV). The content of the resulting datasets are explained here in more detail.
Equivalently to the raw data described before, data points are described by what each data row represents (data object) and how these observations are characterized by different metrics (variables) reprsented by columns in the data set.

* __Metrics__ (Columns/Variables)
	+ __Unit Price__ (in USD per unit)
	+ __Sales Price per Price Quantity__
	+ __Unit Cost__ (in USD per unit)
	+ __Unit Movement__ (in units sold)
	+ __Dollar Sales__ (Unit Movement x Unit Price in USD) generated by Ad-Items and overall by All Items
	+ __Costs of Goods Sold__ (Unit Movement x Unit Cost in USD) generated by Ad-Items and overall by All Items
	+ __Profit__ (Dollar Sales - Costs of Goods Sold in USD) generated by Ad-Items and overall by All Items
	+ __Profit__ (as Percentage of Sales) __Margin__ generated by Ad-Items and overall by All Items
	+ __Profit__ (as Percentage of COGS) __Markup__ generated by Ad-Items and overall by All Items
	+ __Ad Sales Share__ (Dollar Sales generated by Ad-Items as Percentage of Overall Sales)
	+ __Ad Profit Share__ (Profit generated by Ad-Items as Percentage of Overall Sales)
	+ __Customer Count__ per Store
	+ __Average Basket Size__ per Store (Overall Sales in USD divided by Customer Count per Store)
* __Objects__ (Observations/Rows)
	+ Ad-Items (individual SKUs/items at UPC-level)
	+ Categories (per store and across stores)
	+ Sub-Departments (per store and across stores)
	+ Main-Departments (per store and across stores)
	+ Stores
	+ Company

	Metrics are mainly maintained across datasets with varying levels of detail, respectively with varying meanings what each data-set row represents (data objects/observations).
	Some metrics become obsolete when aggregating from one data level to the next one: e.g. Unit Price, Sales Price, Unit Cost, and Unit Movement become irrelevant when aggregating data from an individual SKUs/items at UPC-level to category level where all items of the same kind are grouped.
	All other metrics (from Dollar Sales to Ad Profit Share) are now aggregated, i.e. summed or re-computed, at the level of categories across all SKUs/items contained in that category.
	Further, the same metrics would be aggregated (summed or re-computed) again when moving from the level of categories to the level of sub-departments across all categories contained in a specific sub-department. The same logic is applied when moving up further from sub-departments to main-departments, stores, and the company as a whole.
	Categories, sub- and main-departments are aggregated two-fold: both per store and aggregated across stores.
	The metrics Customer Count and Average Basket Size just kick in at the level of stores and company as a whole since they don't provide any value on the previous levels.


	
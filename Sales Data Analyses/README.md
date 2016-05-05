# Sales Data Analyses

These R data-processing scripts essentially read, clean*, manipulate, and join various Excel reports *(ill-formatted, messy) which are pulled from a SQL-based database.
The underlying data cannot be pulled immediately from the database at this point because the overall data infrastructure is not in place yet that allows an open database connection (ODBC).
* The raw Excel-based reports are ill-formatted and messy in the sense that the data export is intended to produce PDFs and the conversion to an Excel spreadsheet results in table breaks by reports headers/footers and skewed data rows and columns. The processing script re-formats the data in a clean table with consistent rows and observations.
The final output of the processing scripts is a a system of datasets with different aggregation/detail levels that are manageable for further data exploration, analysis, and visualization in Tableau. Further, the processed data serves as input data for other reading/processing scripts.

## 1 Ad & Sales Reporting (to Company Owners, Department Buyers, and Store Managers): Reading & Processing XLSX Data

### Raw Data 

#### Where is raw data retrieved from? When is it retrieved? How is it stored?
* Raw data is pulled in form of reports from the database application and saved as an Excel spreadsheet (.xlsx)
* Excel reports are pulled for each store and once a week, usually Wednesdays, after the sales period ranging from Wed-Tue expires
* Reports summarize data per store and over the course of one sales week
* Data are therefore locally, systematically stored generally in main-folders by report type, in sub-folders by sales week (indicated by date of starting Wednesday of sales week), in Excel spreadsheets by store
* File name of Excel report follows naming convention: <Report Title>_<Date of starting Wednesday of sales week>_<Store number>.xlsx

* The LOC Excel reports which serve as input for the data processing are stored systematically in sub-folders in the raw-data repository C:\Users\Stefan\Google Drive\Projects\4 Weekly Ad\1 Raw Data\
* Main folders in the directory "~/Projects/Sales Data Analyses/1 RD" (Raw Data) are listed in order by their appearance in the processing script and are described by the report type contained
	+ Sub-department List
	+ Item Price List by Category
	+ Store Customer Counts, Sales by Sub-department & Store 2014-15*
	+ Ad-Item Price List with Cost
	+ Item Scans & Costs**
	+ Store Customer Counts
	+ Item Movement, Sub-department Sales & Costs
* Main folders without asterisks are pulled weekly from the database and contain sub-folders that are described by the date of the starting Wednesday of the sales week
* The main folder with one (*) asterisk contains an already processed dataset (.csv) from another project that is read with its clean format
* The main folder with two (**) asterisks contains database- and company-external Excel spreadsheets with item-cost infromation from vendors

#### What information do raw-data Excel reports contain?

The aforementioned report types (and their exact report titles in parentheses) are explained here in more detail. It is described what each data observation (table row) represents and how these are described by different data variables (table columns).

* __Sub-department List__ contains information at the level of sub-departments (assignment thereof to main-departments)
	+ Sub-dept. number
	+ Sub-dept. description
	+ (Main-)Dept. number
	+ (Main-)Dept. description
* __Item Price List by Category__ contains information at the level of SKU-items/products (assignment thereof to categories)
	+ Product code (UPC)
	+ Category number
	+ Category description
* __Ad-Item Price List with Cost__ (Price List with Cost) contains information at the level of items/products (SKUs) that are on-sale during the week and at the store of interest (assignment thereof to sub-departments, as well as product-price/cost information)
	+ Week ID
	+ Store number
	+ Product code (UPC)
	+ Product description
	+ Sub-dept. description
	+ Price quantity in units
	+ Price per quantity in dollars
	+ Price type
	+ Unit cost in dollars
* _Item Scans & Costs_ contains information at the level of items/products (SKUs) that are sold by specific vendors and put on sale during the week and at the store of interest (additonal, more acccurate cost information thereof that supplements the above mentioned product unit costs); yet varying in format the different vendor-specific Excel reports essentially contain the following columns:
	+ Week ID	
	+ Product code (UPC)
	+ Unit cost in dollars
* __Store Customer Counts__ (Hourly Single Total) contains information at the level of hours of the day summed across the days during the week and at store of interest
	+ Week ID
	+ Store number
	+ Hour of day
	+ Customer count
	+ Sales in dollars
* __Item Movement, Sub-department Sales & Costs__ (Item Cost of Goods by Sub-department) contains information at the level of items/products (SKUs), whether on-sale or not, during the week and at the store of interest (assignment thereof to sub-departments, as well as product-movement/cost/profit information)
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
	

	
# Professional Projects
As a Marketing Analyst at a local and multi-store grocery retailer, I utilized corporate data with the help of the statistical programming language R. In essence, I used R to read, clean, manipulate, join, analyze, and export data. All my scripts are documented here.

Overall, I worked one two kinds of projects that may be grouped according to the underlying data that was utilized. On the one hand,
item __sales data__ per SKU/UPC on the most granular level aggregated up to department, store, sompany levels. On the other hand, __transaction data__ which is collected at the POS (scanner data). Further, there is auxiliary data such as __product categories__ which needs to be established systematically to facilitate more effective analysis of sales and transaction Data.

## Product Categorization

The category of a product (e.g. "Canned Tomato Sauce") is an important (meta-)data point that bridges the gap between a too broad grouping variables like main- or sub-departments (e.g. "Grocery") on the one hand and the very granular item level, SKUs (e.g. "16 oz. Tomato Sauce of Brand XYZ"), on the other hand.
Accurate product categories provide additional value to any analyses. First, they help break down aggregated department data (e.g. sales, margins) and determine the (sales or margin) drivers within a departemnt (see Ad & Sales Reporting). More, categories help lift SKU-level findings to a more general, more meaningful level that is not too general, hence, not meaningful as in the case of departments. For example, a Market Basket Analysis based on scanner data conducted on a SKU-level would render a minimum of association rules; conducted on a department-level the same analysis would render the maximal amount of association rules since it's likely that items from many departments land in the same transaction. Conducting a market-basket analysis on a category-level would be the sweet spot and promise the most meaningful product-category associations.

## Sales-Data Analyses

Specific analyses and their desired outcomes that I worked on are:

* __Ad & Sales Reporting__ to inform the company owners, department buyers, and store managers about sales of ad-featured items in certain categories, departments, stores, and combinations thereof from various perspectives.
* __Marketing-Mix-Modelling__ to determine price elasticity, sales impact of various price points, as well as ad lifts, the impact of an ad-feature of items in certain categories, departments, stores, and combinations thereof.

## Transaction-Data Analyses

* __Customer ZIP-Code Analysis__: Utilize ZIP-code related sales and customers-count data collected by cashiers who survey customers for their home ZIP codes and tie them to customer transactions. Compare to ZIP-level ad-printing/distribution marketing costs, and built ROI model to help re-allocate ad spending for each store location.
* __Market Basket Analysis__: Find _Association Rules_ among (sets of items) in order to generate incremental sales by cross-merchandising associated items, e.g. "If item of category X is purchased, then there's a ##.#% probability that product of categories Y and Z are contained in the same market basket / customer transaction".

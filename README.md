# Professional Projects
As a Marketing Analyst at a local and multi-store grocery retailer, I utilize (read/clean/manipulate/join/analyze/export) a seizable amount of sales and transaction data with the help of the R statistical programming language. All my data-reading/processing/analysis scripts are documented here.

I work one two overall kinds of projects that may be clustered according to their underlying data utilized: sales and transactional data. Further, auxiliary data such as __Product Categorization__ needs to be established to facilitate better effective __Analysis of Sales and Transaction Data__.

## Product Categorization

The category of a product (e.g. "Canned Tomato Sauce") is an important (meta-)data point that bridges the gap between a rather broad grouping variables like main- or sub-departments (e.g. "Grocery" or "Produce-Fruit") on the one hand and the too detailed item entity, SKUs (e.g. "16 oz. Tomato Sauce of Brand XYZ"), on the other hand.
Accurate product categories provide additional value to my reporting and analyses. First, they help break down aggregated department data (e.g. sales, margins) and determine the (sales or margin) drivers within a departemnt (see Ad & Sales Reporting). More, categories help lift SKU-level findings to a more general, more meaningful level that is not too general, hence, not meaningful as in the case of departments. For example, a market-basket analysis conducted on a SKU-level would render a minimum of association rules; conducted on a department-level the same analysis would render the maximal amount of association rules since it's likely that items from many departments land in the same transaction. Conducting a market-basket analysis on a category-level would be the sweet spot and promise the most meaningful product-category associations.

## Sales-Data Analyses

Specific analyses and their desired outcomes that I have been working on are:

* __Ad & Sales Reporting__ to inform the company owners, department buyers, and store managers about sales of ad-featured items in certain categories, departments, stores, and combinations thereof from various perspectives.
* __Marketing-Mix-Modelling__ to determine price elasticity, sales impact of various price points, as well as the impact of an ad-feature of items in certain categories, departments, stores, and combinations thereof.

## Transaction-Data Analyses

Specific analyses and their desired outcomes that I have been working on are:

* __Customer ZIP-Code Analysis__: Utilize ZIP-code related sales and customers counts (cashiers survey customers for home ZIP codes and tie them to customer transaction), compare to ZIP-code specific ad-printing/distribution marketing costs, and built ROI model to help re-allocate ad spending perfor each store location.
* __Market Basket Analysis__: Find _Association Rules_ among (sets of items) in order to generate incremental sales by cross-merchandising associated items, e.g. "If item of category X is purchased, then there's a ##.#% probability that product of categories Y and Z are in the same market basket / customer transaaction".

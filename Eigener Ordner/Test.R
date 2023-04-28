# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----
bikes_tbl      <- read_excel(path = "/Users/felixadamaszek/Documents/GitHub/ss23-bdsb-FelixAdams1827/Eigener Ordner/Intro to the tidyverse/Business Case/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("/Users/felixadamaszek/Documents/GitHub/ss23-bdsb-FelixAdams1827/Eigener Ordner/Intro to the tidyverse/Business Case/01_bike_sales/01_raw_data/orderlines.xlsx")

# Not necessary for this task, but for completeness
bikeshops_tbl  <- read_excel("/Users/felixadamaszek/Documents/GitHub/ss23-bdsb-FelixAdams1827/Eigener Ordner/Intro to the tidyverse/Business Case/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----

# Method 1: Print it to the console
orderlines_tbl


# Method 2: Clicking on the file in the environment tab (or run View(orderlines_tbl)) There you can play around with the filter.

# Method 3: glimpse() function. Especially helpful for wide data (data with many columns)
glimpse(orderlines_tbl)


# 4.0 Joining Data ----


# If the data has no common column name, you can provide each column name in the "by" argument. For example, by = c("a" = "b") will match x.a to y.b. The order of the columns has to match the order of the tibbles).
left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

# Chaining commands with the pipe and assigning it to order_items_joined_tbl
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# Examine the results with glimpse()
bike_orderlines_joined_tbl %>% glimpse()


# 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  # 5.1 Separate category name ----
separate(col    = location,
         into   = c("city", "state"),
         sep    = ", ") %>%
  
  # 5.2 Add the total price (price * quantity) 
  # Add a column to a tibble that uses a formula-style calculation of other columns
  mutate(total.price = price * quantity) %>%
  
  # 5.3 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once) ----
rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Sales by location (state) ----
#. Which state has the highest revenue?

# Step 1 - Manipulate

library(lubridate)
sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns
  select(state, total_price) %>%
  
  # Grouping by year and summarizing sales
  group_by(state) %>% 
  summarize(sales = sum(total_price)) %>%
  
  # Optional: Add a column that turns the numbers into a currency format 
  # (makes it in the plot optically more appealing)
  # mutate(sales_text = scales::dollar(sales)) <- Works for dollar values
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_state_tbl



# Step 2 - Visualize

sales_by_state_tbl %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = state, y = sales, fill = state)) +
  
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Hint
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by location",
    subtitle = "Upward Trend",
    x = "States", # Override defaults for x and y
    y = "Revenue"
  )



# 6.2 Sales by State and Year ----

# Step 1 - Manipulate

sales_by_state_year_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, state) %>%
  mutate(year = year(order_date)) %>%
  
  # Group by and summarize year and main catgegory
  group_by(year, state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_state_year_tbl  

# Step 2 - Visualize

sales_by_state_year_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = state, y = sales, fill = year)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ year) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by state and year",
    fill = "year" # Changes the legend name
  )

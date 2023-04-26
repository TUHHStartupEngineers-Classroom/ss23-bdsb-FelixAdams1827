# Intro to the tidyverse Challenge

# 1.0 Load libraries ----

          library(tidyverse)
          library(readxl)

# 2.0 Importing needed files ----

          bikes_tbl      <- read_excel(path = "/Users/felixadamaszek/Documents/GitHub/ss23-bdsb-FelixAdams1827/Eigener Ordner/Intro to the tidyverse/Business Case/01_bike_sales/01_raw_data/bikes.xlsx")
          orderlines_tbl <- read_excel("/Users/felixadamaszek/Documents/GitHub/ss23-bdsb-FelixAdams1827/Eigener Ordner/Intro to the tidyverse/Business Case/01_bike_sales/01_raw_data/orderlines.xlsx")
          bikeshops_tbl  <- read_excel("/Users/felixadamaszek/Documents/GitHub/ss23-bdsb-FelixAdams1827/Eigener Ordner/Intro to the tidyverse/Business Case/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Joining Data ----

          bike_orderlines_joined_tbl <- orderlines_tbl %>%
          left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
          left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

          bike_orderlines_joined_tbl %>% glimpse()


# 4.0 Wrangling Data ----

          bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  
  # 4.1 Separate column location ----

          separate(col    = location,
          into   = c("city", "state"),
          sep    = ", ") %>%
  
  # 4.2 Add the total price (price * quantity) 

          mutate(total.price = price * quantity) %>%
  
  # 4.3 Rename columns 
  
          rename(bikeshop = name) %>%
          set_names(names(.) %>% str_replace_all("\\.", "_"))

# 5.0 Business Insights ----
  # 5.1 Sales by location (state) ----
    # Which state has the highest revenue?

    # Step 1 - Manipulating the data

        library(lubridate)
        sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>%
  
    # Select needed columns
        select(state, total_price) %>%
  
    # Grouping by state and summarizing the sales
        group_by(state) %>% 
        summarize(sales = sum(total_price)) %>%
        
    # Adding a column that turns the numbers into a currency format
        mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

        sales_by_state_tbl



    # Step 2 - Visualize the data

        sales_by_state_tbl %>%
  
    # Setup canvas with the columns year (x-axis) and sales (y-axis)
        ggplot(aes(x = state, y = sales, fill = state)) +
  
  
    # Geometries
        geom_col(fill = "#2DC6D6") + 
        geom_label(aes(label = sales_text)) + 
        geom_smooth(method = "lm", se = FALSE) + 
  
    # Hint
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
    # Formatting
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
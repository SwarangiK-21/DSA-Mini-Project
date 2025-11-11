#install.packages("tidyverse")
#install.packages("lubridate")
library(tidyverse)
library(lubridate)

theme_set(theme_light())

# 2. DATA INGESTION AND WRANGLING

#Coffee_Shop_Sales_Dataset <- read_csv("Coffee_Shop_Sales_Dataset.xlsx - Sheet1.csv")

sales_clean <- Coffee_Shop_Sales_Dataset %>%
  
  # 1. Rename columns for R (replace spaces with underscores)
  rename_with(~ str_replace_all(., " ", "_")) %>%
  
  # 2. Convert 'DATE' to an actual Date object
  mutate(DATE = ymd(DATE)) %>%
  
  # 3. Create time-based features for analysis
  mutate(
    Month = month(DATE, label = TRUE, abbr = FALSE),
    Year = year(DATE)
  ) %>%
  
  # 4. Convert key location and product variables to factors
  mutate(
    City = factor(City),
    Branch = factor(Branch),
    PRODUCT_CATEGORY = factor(PRODUCT_CATEGORY)
  )

print("Data Structure and First 6 Rows (Sales Clean)")
glimpse(sales_clean) 
head(sales_clean)


# 3. CORE EXPLORATORY VISUALIZATIONS

# Finding 1: Time Series - Monthly Revenue Trend (Across all years)
# Aggregate data by month
monthly_revenue <- sales_clean %>%
  group_by(Month) %>%
  summarise(Total_Revenue = sum(REVENUE))

plot_monthly_revenue <- ggplot(monthly_revenue, aes(x = Month, y = Total_Revenue, group = 1)) +
  geom_line(color = "#0072B2", linewidth = 1) +
  geom_point(color = "#0072B2", size = 2) +
  labs(
    title = "1. Monthly Revenue Trend (Key Seasonal Patterns)",
    x = "Month",
    y = "Total Revenue ($)"
  )
print(plot_monthly_revenue)


# Finding 2: Geographic Comparison - Profit Margin by City
# Aggregate data by city
city_profit <- sales_clean %>%
  group_by(City) %>%
  # NOTE: Column name is PROFIT_MARGIN after the rename_with() step
  summarise(Total_Profit = sum(PROFIT_MARGIN)) %>%
  arrange(desc(Total_Profit)) # Sort for clear comparison

plot_city_profit <- ggplot(city_profit, aes(x = reorder(City, Total_Profit), y = Total_Profit)) +
  geom_col(fill = "#D55E00") +
  coord_flip() + # Flip coordinates for readability
  labs(
    title = "2. Total Profit Margin by City (Geographic Performance)",
    x = "City",
    y = "Total Profit Margin ($)"
  )
print(plot_city_profit)


# Finding 3: Product Analysis - Total Revenue by Product Category
# Aggregate data by product category
product_revenue <- sales_clean %>%
  group_by(PRODUCT_CATEGORY) %>%
  summarise(Total_Revenue = sum(REVENUE)) %>%
  arrange(desc(Total_Revenue))

plot_product_revenue <- ggplot(product_revenue, aes(x = reorder(PRODUCT_CATEGORY, Total_Revenue), y = Total_Revenue)) +
  geom_col(fill = "#CC79A7") +
  coord_flip() +
  labs(
    title = "3. Revenue Breakdown by Product Category",
    x = "Product Category",
    y = "Total Revenue ($)"
  )
print(plot_product_revenue)


# Finding 4: Price vs. Volume - Relationship between Price and Quantity Sold
# NOTE: Column names are UNIT_PRICE and QTY_SOLD after the rename_with() step
plot_price_vs_qty <- ggplot(sales_clean, aes(x = UNIT_PRICE, y = QTY_SOLD)) +
  geom_point(alpha = 0.5, color = "#009E73") +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Add a linear trend line
  labs(
    title = "4. Relationship between Unit Price and Quantity Sold",
    x = "Unit Price ($)",
    y = "Quantity Sold"
  )
print(plot_price_vs_qty)


# Finding 5: Outlier Check - Revenue Distribution by City
plot_revenue_boxplot <- ggplot(sales_clean, aes(x = City, y = REVENUE)) +
  geom_boxplot(fill = "#E69F00", alpha = 0.7) +
  labs(
    title = "5. Revenue Distribution (and Outliers) by City",
    x = "City",
    y = "Transaction Revenue ($)"
  )
print(plot_revenue_boxplot)
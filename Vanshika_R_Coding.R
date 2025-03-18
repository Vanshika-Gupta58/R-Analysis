library(tidyverse)
library(ggplot2)
library(dplyr)

#Load dataset
Sales_Data <- read.csv("C:/Users/Vanshika Gupta/Desktop/online sale dataset.csv")
View(Sales_Data)

#View first few rows
View(head(Sales_Data))

#view last 10 rows
View(tail(Sales_Data,10))

#view structure
str(Sales_Data)

#summary statistics
print(summary(Sales_Data))

#check for missing values
colSums(is.na(Sales_Data))

#remove duplicate rows, if any
Sales_Data_Clean<- Sales_Data%>% distinct()
View(Sales_Data_Clean)

# Top 5 Categories by Sales
top_categories <- Sales_Data %>%
  group_by(Category) %>%
  summarise(TotalSales = sum(Sales)) %>%
  arrange(desc(TotalSales)) %>%
  head(5)

View(top_categories)


# Calculate return rates by category
return_rates <- Sales_Data %>%
  group_by(Category, ReturnStatus) %>%
  summarise(Count = n()) %>%
  mutate(ReturnRate = Count / sum(Count)) %>%
  filter(ReturnStatus == "Returned")

View(return_rates)


# Calculate average shipping cost per category
average_shipping_cost <- Sales_Data %>%
  group_by(Category) %>%
  summarise(AverageShippingCost = mean(ShippingCost, na.rm = TRUE))

View(average_shipping_cost)


# Sales Trends Over Time
sales_trends <- Sales_Data %>%
  group_by(InvoiceDate) %>%
  summarise(DailySales = sum(Sales))
View(sales_trends)


# Sales Channel Performance: Compare online vs in-store sales
channel_performance <- Sales_Data %>%
  group_by(SalesChannel) %>%
  summarise(TotalSales = sum(Quantity * UnitPrice, na.rm = TRUE),
            TotalTransactions = n())

View(channel_performance)



# Discount Impact: Sales volume and revenue with discounts
discount_impact <- Sales_Data %>%
  filter(Discount > 0) %>%
  group_by(Category) %>%
  summarise(TotalDiscountedSales = sum(Quantity),
            TotalRevenueWithDiscounts = sum((Quantity * UnitPrice) * (1 - Discount), na.rm = TRUE))

View(discount_impact)


# Top 5 Countries by Total Sales
top_countries <- Sales_Data %>%
  group_by(Country) %>%
  summarise(TotalSales = sum(Sales)) %>%
  arrange(desc(TotalSales)) %>%
  head(5)

View(top_countries)


#Faceted Scatter Plot: Quantity vs. Sales by Category
ggplot(Sales_Data, aes(x = Quantity, y = Sales, color = Category)) +
  geom_point() +
  facet_wrap(~ Category) +
  labs(title = "Scatter Plot of Quantity vs. Sales by Category", x = "Quantity", y = "Sales") +
  theme_minimal()


#Histogram
ggplot(Sales_Data, aes(x = Sales)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "blue") +
  labs(title = "Sales Distribution Histogram", x = "Sales", y = "Frequency") +
  theme_minimal()

# Return Rate by Product Category
return_rate_by_category <- Sales_Data %>%
  group_by(Category) %>%
  summarise(ReturnCount = sum(ReturnStatus == "Returned"),
            TotalCount = n(),
            ReturnRate = ReturnCount / TotalCount) %>%
  arrange(desc(ReturnRate))

ggplot(return_rate_by_category, aes(x = reorder(Category, ReturnRate), y = ReturnRate)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Return Rate by Product Category", x = "Product Category", y = "Return Rate") +
  theme_minimal()

# Sales Distribution by Country
sales_by_country <- Sales_Data %>%
  group_by(Country) %>%
  summarise(TotalSales = sum(Quantity * UnitPrice, na.rm = TRUE)) %>%
  arrange(desc(TotalSales))

ggplot(sales_by_country, aes(x = reorder(Country, TotalSales), y = TotalSales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Total Sales by Country", x = "Country", y = "Total Sales") +
  theme_minimal()

#Density Plot: Sales Distribution by Category
ggplot(Sales_Data, aes(x = Sales, fill = Category)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Sales by Category", x = "Sales", y = "Density") +
  theme_minimal()


#Box Plot- Category-wise Sales Distribution
ggplot(Sales_Data, aes(x = Category, y = Sales)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Category-wise Sales Distribution", x = "Category", y = "Sales") +
  theme_minimal()

# Bar plot for Top Countries
ggplot(top_countries, aes(x = reorder(Country, TotalSales), y = TotalSales)) +
  geom_bar(stat = "identity", fill = "green") +
  coord_flip() +
  labs(title = "Top 5 Countries by Sales", x = "Country", y = "Sales") +
  theme_minimal()

#Pie chart - Category Sales Contribution
category_sales <- Sales_Data %>%
  group_by(Category) %>%
  summarise(TotalSales = sum(Sales))

ggplot(category_sales, aes(x = "", y = TotalSales, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Category Sales Distribution") +
  theme_minimal()


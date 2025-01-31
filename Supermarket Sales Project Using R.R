# Supermarket Sales Project Using R .......

# Import Libraries.....

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

data <- readxl::read_xlsx("E:/مشاريعي/Supermarket Sales Project Using R/Supermarket Sales.xlsx")

# Clean the data .....

data <- data %>%
  mutate( Date = as.Date(Date, format = "%m-%d-%y"),
         Time = hms::as_hms(Time),
         Month = factor(Month, levels = 1:12, labels = month.name),
         Year = as.factor(Year)
         )

# Summary statistics .....

summary_stats <- data %>%
  summarise(Total_Sales = sum(Total),
            Total_Transactions = n(),
            Average_Rating = mean(Rating, na.rm = TRUE),
            Total_Units_Sold = sum(Quantity)
            )

print(summary_stats)


# Visualizations of Data .....

# 1. Total sales by month (Bar Chart)

sales_by_month <- data %>%
  group_by(Month) %>%
  summarise(Total_Sales = sum(Total))

ggplot(sales_by_month, aes(x = Month, y = Total_Sales, fill = Month)) + 
   geom_bar(stat = "identity") + 
   scale_y_continuous(labels = dollar) + 
   labs(title = "Total Sales by Month", x = "Month", y = "Total Sales") + 
   theme_minimal() + 
   theme(legend.position = "none")


# 2. Distribution of Total Sales (Histogram chart)

ggplot(data, aes(x = Total)) + 
   geom_histogram(bins = 30, fill = "orange", color = "white") +
   labs(title = "Distribution of Total Sales", x = "Total Sales", y = "Frequency")


# 3. Total sales by product line (Bar Chart)

sales_by_product <- data %>% 
  group_by(`Product line`) %>%
  summarise(Total_Sales = sum(Total))

ggplot(sales_by_product, aes(x = reorder(`Product line`, Total_Sales), y = Total_Sales)) + 
   geom_bar(stat="identity", fill="steelblue") + 
   labs(title="Total Sales by Product Line", x = "Product Line", y = "Total Sales") +
   theme(legend.position = "none")


# 4. Sales distribution by gender (Bar Chart)

sales_by_gender <- data %>%
  group_by(Gender) %>%
  summarise(Total_Sales = sum(Total))

ggplot(sales_by_gender, aes(x = Gender, y = Total_Sales, fill = Gender)) + 
   geom_bar(stat = "identity") +
   scale_y_continuous(labels = dollar) +
   labs(title = "Total Sales by Gender", x = "Gender", y = "Total Sales") +
   theme_minimal() +
   theme(legend.position = "none")


# 5. Average rating by product line (Bar Chart)

rating_by_product <- data %>%
  group_by(`Product line`) %>%
  summarise(Average_Rating = mean(Rating, na.rm = TRUE))

ggplot(rating_by_product, aes(x = reorder(`Product line`, -Average_Rating), y = Average_Rating, fill = `Product line`)) +
   geom_bar(stat = "identity") + 
   labs(title = "Average Rating by Product Line", x = "Product Line", y = "Average Rating") +
   theme_minimal() +
   theme(legend.position = "none")


# 6. Total sales over time (Line Chart)

sales_over_time <- data %>%
  group_by(Date) %>%
  summarise(Total_Sales = sum(Total))

ggplot(sales_over_time, aes(x = Date, y = Total_Sales)) +
   geom_line(color = "blue") +
   scale_y_continuous(labels = dollar) +
   labs(title = "Total Sales Over Time", x = "Date", y = "Total Sales") +
   theme_minimal()


# 7. sales by customer type (Pie Chart)

sales_by_customer <- data %>%
  group_by(`Customer type`) %>%
  summarise(Total_Sales = sum(Total))

ggplot(sales_by_customer, aes(x = "", y = Total_Sales, fill = `Customer type`)) +
   geom_bar(width = 1, stat = "identity") +
   coord_polar(theta = "y") +
   labs(title = "Sales Distribution by Customer Type") +
   theme_void()


# 8. Total Sales per Month,Year (Line Chart)

data$Month_Year <- format(as.Date(data$Date), "%Y-%m")

monthly_sales <- data %>%
  group_by(Month_Year) %>%
  summarise(Total_Sales = sum(Total))

ggplot(monthly_sales, aes(x = Month_Year, y = Total_Sales)) +
   geom_line(group = 1, color = "blue") +
   labs(title = "Monthly Sales Trend", x = "Month-Year", y = "Total Sales") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load the cleaned house price data
house_prices <- read_csv("Clean Data/house_prices_all_years.csv")

head(house_prices)

# Calculate average price per county, per year
avg_price_by_year <- house_prices %>%
  group_by(county, year) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")

print(avg_price_by_year)

# Create the line chart
line_chart <- ggplot(avg_price_by_year, aes(x = year, y = avg_price, color = county)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 2021:2025) +
  labs(
    title = "Average House Price Trend 2021-2025: Norfolk vs Suffolk",
    x = "Year",
    y = "Average Price (£)",
    color = "County"
  ) +
  theme_minimal()

print(line_chart)

# Save the chart as an image
ggsave("Charts (Visualizations)/house_price_linechart.png", line_chart, width = 8, height = 6, dpi = 300)

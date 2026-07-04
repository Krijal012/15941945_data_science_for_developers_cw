# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load the cleaned house price data
house_prices <- read_csv("Clean Data/house_prices_all_years.csv")

head(house_prices)

# Calculate the average price for each county
avg_price_by_county <- house_prices %>%
  group_by(county) %>%
  summarise(avg_price = mean(price, na.rm = TRUE))

print(avg_price_by_county)

# Create the bar chart
bar_chart <- ggplot(avg_price_by_county, aes(x = county, y = avg_price, fill = county)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = round(avg_price, 0)), vjust = -0.5) +   # show the number above each bar
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Average House Price: Norfolk vs Suffolk (2021-2025)",
    x = "County",
    y = "Average Price (£)"
  ) +
  theme_minimal()

print(bar_chart)

# Save the chart as an image
ggsave("Charts (Visualizations)/house_price_barchart.png", bar_chart, width = 8, height = 6, dpi = 300)

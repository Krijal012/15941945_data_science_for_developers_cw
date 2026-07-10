# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load both cleaned datasets
house_prices <- read_csv("Clean Data/house_prices_all_years.csv")
broadband <- read_csv("Clean Data/broadband_clean.csv")

# Calculate average house price per county PER YEAR
avg_price_year <- house_prices %>%
  group_by(county, year) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")

print(avg_price_year)

# Calculate average download speed per county (one value per county)
avg_speed <- broadband %>%
  group_by(county) %>%
  summarise(avg_download = mean(avg_download, na.rm = TRUE))

print(avg_speed)

# Join - each year gets that county's download speed
combined <- avg_price_year %>%
  left_join(avg_speed, by = "county")

print(combined)  

# Run the linear regression model
model <- lm(avg_price ~ avg_download, data = combined)

summary(model)

# Create a scatter plot with the regression line
scatter_plot <- ggplot(combined, aes(x = avg_download, y = avg_price, color = county)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", aes(group = 1)) +
  labs(
    title = "House Price vs Download Speed",
    x = "Average Download Speed (Mbit/s)",
    y = "Average House Price (£)"
  ) +
  theme_minimal()

print(scatter_plot)

# Save the chart
ggsave("Charts (Visualizations)/lm_houseprice_vs_downloadspeed.png", scatter_plot, width = 8, height = 6, dpi = 300)

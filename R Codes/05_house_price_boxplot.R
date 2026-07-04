# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load the cleaned house price data
house_prices <- read_csv("Clean Data/house_prices_all_years.csv")

head(house_prices)

# Filter to one year (let's pick 2024)
prices_2024 <- house_prices %>%
  filter(year == 2024)

nrow(prices_2024)

# Calculate distribution stats (mean, median, SD)
distribution_stats <- prices_2024 %>%
  group_by(county) %>%
  summarise(
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    sd_price = sd(price, na.rm = TRUE)
  )

print(distribution_stats)

# Create the box plot
box_plot <- ggplot(prices_2024, aes(x = county, y = price, fill = county)) +
  geom_boxplot(outlier.alpha = 0.2) +
  scale_y_continuous(labels = scales::comma, limits = c(0, quantile(prices_2024$price, 0.95, na.rm = TRUE))) +
  labs(
    title = "House Price Distribution 2024: Norfolk vs Suffolk",
    x = "County",
    y = "Price (£)"
  ) +
  theme_minimal()

print(box_plot)

# Save the chart as an image
ggsave("Charts (Visualizations)/house_price_boxplot_2024.png", box_plot, width = 8, height = 6, dpi = 300)

# Save the distribution stats as a table too (useful for your report)
write_csv(distribution_stats, "Clean Data/house_price_distribution_stats_2024.csv")

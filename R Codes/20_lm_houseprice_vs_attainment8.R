# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load datasets
house_prices <- read_csv("Clean Data/house_prices_all_years.csv")
education <- read_csv("Clean Data/education_clean.csv")

# Average house price per county per year
avg_price_year <- house_prices %>%
  group_by(county, year) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")

print(avg_price_year)

# Convert academic year (e.g. "2024-2025") to a single matching year
education <- education %>%
  mutate(match_year = as.numeric(sub(".*-", "", year)))   # takes the second year from "2024-2025"

head(education)

# Average Attainment 8 score per county per matching year
avg_att8_year <- education %>%
  group_by(county, match_year) %>%
  summarise(avg_att8 = mean(att8_score, na.rm = TRUE), .groups = "drop") %>%
  rename(year = match_year)

print(avg_att8_year)

# Join house prices with Attainment 8 scores
combined <- avg_price_year %>%
  inner_join(avg_att8_year, by = c("county", "year"))

print(combined)

# Run the linear regression model
model <- lm(avg_price ~ avg_att8, data = combined)

summary(model)

# Create scatter plot with regression line
scatter_plot <- ggplot(combined, aes(x = avg_att8, y = avg_price, color = county)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", aes(group = 1)) +
  labs(
    title = "House Price vs Average Attainment 8 Score",
    x = "Average Attainment 8 Score",
    y = "Average House Price (£)"
  ) +
  theme_minimal()

print(scatter_plot)

# Save the chart
ggsave("Charts (Visualizations)/lm_houseprice_vs_attainment8.png", scatter_plot, width = 8, height = 6, dpi = 300)
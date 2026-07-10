# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load datasets
house_prices <- read_csv("Clean Data/house_prices_all_years.csv")
crime <- read_csv("Clean Data/crime_clean.csv")

# Average house price per county per year
avg_price_year <- house_prices %>%
  group_by(county, year) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")

print(avg_price_year)

# County population (same as before)
county_population <- tribble(
  ~county,    ~population,
  "NORFOLK",  914500,
  "SUFFOLK",  762000
)

# Filter to drug offences, extract year
drugs <- crime %>%
  filter(crime_type == "Drugs") %>%
  mutate(year = as.numeric(substr(month, 1, 4)))

# Count drug offences per county per year, calculate rate per 100k
drug_rate_year <- drugs %>%
  group_by(county, year) %>%
  summarise(n_offences = n(), .groups = "drop") %>%
  left_join(county_population, by = "county") %>%
  mutate(drug_rate_per_100k = (n_offences / population) * 100000)

print(drug_rate_year)

# Join house prices with drug offence rate
combined <- avg_price_year %>%
  inner_join(drug_rate_year, by = c("county", "year"))

print(combined)   

# Run the linear regression model
model <- lm(avg_price ~ drug_rate_per_100k, data = combined)

summary(model)

# Create scatter plot with regression line
scatter_plot <- ggplot(combined, aes(x = drug_rate_per_100k, y = avg_price, color = county)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", aes(group = 1)) +
  labs(
    title = "House Price vs Drug Offence Rate",
    x = "Drug Offence Rate per 100,000 People",
    y = "Average House Price (£)"
  ) +
  theme_minimal()

print(scatter_plot)

# Save the chart
ggsave("Charts (Visualizations)/lm_houseprice_vs_drugoffence.png", scatter_plot, width = 8, height = 6, dpi = 300)

# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load datasets
education <- read_csv("Clean Data/education_clean.csv")
crime <- read_csv("Clean Data/crime_clean.csv")

# Average Attainment 8 per county per year
avg_att8_year <- education %>%
  mutate(year = as.numeric(sub(".*-", "", year))) %>%
  group_by(county, year) %>%
  summarise(avg_att8 = mean(att8_score, na.rm = TRUE), .groups = "drop")

print(avg_att8_year)

# County population (same as before)
county_population <- tribble(
  ~county,    ~population,
  "NORFOLK",  914500,
  "SUFFOLK",  762000
)

# Filter to drug offences, calculate rate per 100k per year
drug_rate_year <- crime %>%
  filter(crime_type == "Drugs") %>%
  mutate(year = as.numeric(substr(month, 1, 4))) %>%
  group_by(county, year) %>%
  summarise(n_offences = n(), .groups = "drop") %>%
  left_join(county_population, by = "county") %>%
  mutate(drug_rate_per_100k = (n_offences / population) * 100000)

print(drug_rate_year)

# Join Attainment 8 with drug offence rate
combined <- avg_att8_year %>%
  inner_join(drug_rate_year, by = c("county", "year"))

print(combined)

# Run the linear regression model
model <- lm(avg_att8 ~ drug_rate_per_100k, data = combined)

summary(model)

# Create scatter plot with regression line
scatter_plot <- ggplot(combined, aes(x = drug_rate_per_100k, y = avg_att8, color = county)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", aes(group = 1)) +
  labs(
    title = "Attainment 8 Score vs Drug Offence Rate",
    x = "Drug Offence Rate per 100,000 People",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal()

print(scatter_plot)

# Save the chart
ggsave("Charts (Visualizations)/lm_attainment8_vs_drugoffence.png", scatter_plot, width = 8, height = 6, dpi = 300)

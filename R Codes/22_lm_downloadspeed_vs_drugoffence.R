# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load datasets
broadband <- read_csv("Clean Data/broadband_clean.csv")
crime <- read_csv("Clean Data/crime_clean.csv")

# Average download speed per county (one value per county)
avg_speed <- broadband %>%
  group_by(county) %>%
  summarise(avg_download = mean(avg_download, na.rm = TRUE))

print(avg_speed)

# County population
county_population <- tribble(
  ~county,    ~population,
  "NORFOLK",  914500,
  "SUFFOLK",  762000
)

# Drug offence rate per county per year
drug_rate_year <- crime %>%
  filter(crime_type == "Drugs") %>%
  mutate(year = as.numeric(substr(month, 1, 4))) %>%
  group_by(county, year) %>%
  summarise(n_offences = n(), .groups = "drop") %>%
  left_join(county_population, by = "county") %>%
  mutate(drug_rate_per_100k = (n_offences / population) * 100000)

print(drug_rate_year)

# Join - each year gets that county's download speed
combined <- drug_rate_year %>%
  left_join(avg_speed, by = "county")

print(combined)

# Run the linear regression model
model <- lm(drug_rate_per_100k ~ avg_download, data = combined)

summary(model)

# Create scatter plot with regression line
scatter_plot <- ggplot(combined, aes(x = avg_download, y = drug_rate_per_100k, color = county)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", aes(group = 1)) +
  labs(
    title = "Drug Offence Rate vs Average Download Speed",
    x = "Average Download Speed (Mbit/s)",
    y = "Drug Offence Rate per 100,000 People"
  ) +
  theme_minimal()

print(scatter_plot)

# Save the chart
ggsave("Charts (Visualizations)/lm_downloadspeed_vs_drugoffence.png", scatter_plot, width = 8, height = 6, dpi = 300)
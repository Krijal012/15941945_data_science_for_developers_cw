# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load the cleaned crime data
crime <- read_csv("Clean Data/crime_clean.csv")

# Check exact spelling of drug offences in your data
unique(crime$crime_type)

# County-level population (Norfolk + Suffolk totals)
county_population <- tribble(
  ~county,    ~population,
  "NORFOLK",  914500,
  "SUFFOLK",  762000
)

print(county_population)

# Filter to drug offences only
drugs <- crime %>%
  filter(crime_type == "Drugs")

nrow(drugs)

# Extract year from month (format "2023-05")
drugs <- drugs %>%
  mutate(year = as.numeric(substr(month, 1, 4)))

# Check what years are actually available
unique(drugs$year)

# Count drug offences per county per year
yearly_counts <- drugs %>%
  group_by(county, year) %>%
  summarise(n_offences = n(), .groups = "drop")

print(yearly_counts)

# Calculate rate per 100,000 people
yearly_rate <- yearly_counts %>%
  left_join(county_population, by = "county") %>%
  mutate(rate_per_100k = round((n_offences / population) * 100000, 1))

print(yearly_rate)

# Create the line chart
line_chart <- ggplot(yearly_rate, aes(x = year, y = rate_per_100k, color = county)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2023:2026) +
  labs(
    title = "Drug Offence Rate per 100,000 People (2023-2026): Norfolk vs Suffolk",
    x = "Year",
    y = "Rate per 100,000 People",
    color = "County"
  ) +
  theme_minimal()

print(line_chart)

# Save the chart
ggsave("Charts (Visualizations)/crime_linechart_drugs.png", line_chart, width = 8, height = 6, dpi = 300)

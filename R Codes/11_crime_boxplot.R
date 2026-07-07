# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load the cleaned crime data
crime <- read_csv("Clean Data/crime_clean.csv")

head(crime)

# See what crime types are available
unique(crime$crime_type)

# Extract year from the month column (format is usually "2024-03")
crime <- crime %>%
  mutate(year = as.numeric(substr(month, 1, 4)))

head(crime)

# Filter to ONE crime type and ONE year
crime_filtered <- crime %>%
  filter(crime_type == "Vehicle crime", year == 2024)

nrow(crime_filtered)

# Count how many crimes happened per district, per month
crime_counts <- crime_filtered %>%
  group_by(county, district, month) %>%
  summarise(n_crimes = n(), .groups = "drop")

head(crime_counts)
nrow(crime_counts)

# Create the box plot
box_plot <- ggplot(crime_counts, aes(x = county, y = n_crimes, fill = county)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(
    title = "Vehicle Crime Distribution (2024): Norfolk vs Suffolk",
    x = "County",
    y = "Number of Crimes per District per Month"
  ) +
  theme_minimal()

print(box_plot)

# Save the chart
ggsave("Charts (Visualizations)/crime_boxplot_2024.png", box_plot, width = 8, height = 6, dpi = 300)
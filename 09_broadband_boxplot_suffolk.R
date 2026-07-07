# Load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# Load the cleaned broadband data
broadband <- read_csv("Clean Data/broadband_clean.csv")

# Extract the district (same pattern as before)
broadband <- broadband %>%
  mutate(district = str_extract(postcode, "^[A-Z]{1,2}[0-9]{1,2}"))

# Filter to Suffolk only
suffolk_broadband <- broadband %>%
  filter(county == "SUFFOLK")

nrow(suffolk_broadband)

# Check district counts
district_counts <- suffolk_broadband %>%
  count(district) %>%
  arrange(desc(n))

print(district_counts)
nrow(district_counts)

# Keep only districts with enough postcodes
top_districts <- district_counts %>%
  filter(n >= 5) %>%
  pull(district)

suffolk_filtered <- suffolk_broadband %>%
  filter(district %in% top_districts)

length(unique(suffolk_filtered$district))
nrow(suffolk_filtered)

# Create the box plot
box_plot <- ggplot(suffolk_filtered, aes(x = district, y = avg_download)) +
  geom_boxplot(fill = "darkorange", outlier.alpha = 0.3) +
  labs(
    title = "Average Download Speed by District - Suffolk",
    x = "District",
    y = "Average Download Speed (Mbit/s)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(box_plot)

# Save the chart
ggsave("Charts (Visualizations)/broadband_boxplot_suffolk.png", box_plot, width = 10, height = 6, dpi = 300)
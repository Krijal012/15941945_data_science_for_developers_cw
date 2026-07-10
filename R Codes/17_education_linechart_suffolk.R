# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load the cleaned education data
education <- read_csv("Clean Data/education_clean.csv")

# Filter to Suffolk only
suffolk_edu <- education %>%
  filter(county == "SUFFOLK")

nrow(suffolk_edu)

# Check how many schools per town
town_counts <- suffolk_edu %>%
  count(town) %>%
  arrange(desc(n))

print(town_counts)
nrow(town_counts)

# Keep only towns with enough schools
top_towns <- town_counts %>%
  filter(n >= 8) %>%
  pull(town)

suffolk_filtered <- suffolk_edu %>%
  filter(town %in% top_towns)

length(unique(suffolk_filtered$town))

# Calculate average Attainment 8 score per town, per year
suffolk_avg <- suffolk_filtered %>%
  group_by(town, year) %>%
  summarise(avg_att8 = mean(att8_score, na.rm = TRUE), .groups = "drop")

print(suffolk_avg)

# Create the line chart
line_chart <- ggplot(suffolk_avg, aes(x = year, y = avg_att8, color = town, group = town)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average Attainment 8 Score by Town - Suffolk (2021-2025)",
    x = "Academic Year",
    y = "Average Attainment 8 Score",
    color = "Town"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(line_chart)

# Save the chart
ggsave("Charts (Visualizations)/education_linechart_suffolk.png", line_chart, width = 10, height = 6, dpi = 300)

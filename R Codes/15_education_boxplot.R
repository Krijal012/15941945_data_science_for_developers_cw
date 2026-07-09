# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load the cleaned education data
education <- read_csv("Clean Data/education_clean.csv")

head(education)

# Check what years are available
unique(education$year)

# Filter to one chosen year (let's use 2024-2025)
education_filtered <- education %>%
  filter(year == "2024-2025")

nrow(education_filtered)

# Calculate distribution stats (useful for your report)
distribution_stats <- education_filtered %>%
  group_by(county) %>%
  summarise(
    mean_att8 = mean(att8_score, na.rm = TRUE),
    median_att8 = median(att8_score, na.rm = TRUE),
    sd_att8 = sd(att8_score, na.rm = TRUE)
  )

print(distribution_stats)

# Create the box plot
box_plot <- ggplot(education_filtered, aes(x = county, y = att8_score, fill = county)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(
    title = "Attainment 8 Score Distribution 2024-2025: Norfolk vs Suffolk",
    x = "County",
    y = "Attainment 8 Score"
  ) +
  theme_minimal()

print(box_plot)

# Save the chart
ggsave("Charts (Visualizations)/education_boxplot_2024_2025.png", box_plot, width = 8, height = 6, dpi = 300)

# Save the distribution stats too
write_csv(distribution_stats, "Clean Data/education_distribution_stats_2024_2025.csv")

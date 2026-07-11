# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load datasets
broadband <- read_csv("Clean Data/broadband_clean.csv")
education <- read_csv("Clean Data/education_clean.csv")

# Average download speed per county (one value per county)
avg_speed <- broadband %>%
  group_by(county) %>%
  summarise(avg_download = mean(avg_download, na.rm = TRUE))

print(avg_speed)

# Average Attainment 8 per county per year
avg_att8_year <- education %>%
  mutate(year = as.numeric(sub(".*-", "", year))) %>%
  group_by(county, year) %>%
  summarise(avg_att8 = mean(att8_score, na.rm = TRUE), .groups = "drop")

print(avg_att8_year)

# Join - each year gets that county's download speed
combined <- avg_att8_year %>%
  left_join(avg_speed, by = "county")

print(combined)

# Run the linear regression model
model <- lm(avg_att8 ~ avg_download, data = combined)

summary(model)

# Create scatter plot with regression line
scatter_plot <- ggplot(combined, aes(x = avg_download, y = avg_att8, color = county)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", aes(group = 1)) +
  labs(
    title = "Attainment 8 Score vs Average Download Speed",
    x = "Average Download Speed (Mbit/s)",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal()

print(scatter_plot)

# Save the chart
ggsave("Charts (Visualizations)/lm_downloadspeed_vs_attainment8.png", scatter_plot, width = 8, height = 6, dpi = 300)

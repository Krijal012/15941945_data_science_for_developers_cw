# Load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the cleaned broadband data
broadband <- read_csv("Clean Data/broadband_clean.csv")

head(broadband)

# Calculate average AND max download speed per county
speed_stats <- broadband %>%
  group_by(county) %>%
  summarise(
    avg_speed = mean(avg_download, na.rm = TRUE),
    max_speed = max(max_download, na.rm = TRUE)
  )

print(speed_stats)

# Reshape the data so both stats can appear as grouped bars
speed_long <- speed_stats %>%
  pivot_longer(cols = c(avg_speed, max_speed),
               names_to = "speed_type",
               values_to = "speed_value")

print(speed_long)

# Create the grouped bar chart
stat_bar_chart <- ggplot(speed_long, aes(x = county, y = speed_value, fill = speed_type)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = round(speed_value, 1)),
            position = position_dodge(width = 0.6), vjust = -0.5) +
  labs(
    title = "Average vs Maximum Download Speed: Norfolk vs Suffolk",
    x = "County",
    y = "Speed (Mbit/s)",
    fill = "Speed Type"
  ) +
  scale_fill_manual(values = c("avg_speed" = "steelblue", "max_speed" = "darkorange"),
                    labels = c("Average Speed", "Maximum Speed")) +
  theme_minimal()

print(stat_bar_chart)

# Save the chart
ggsave("Charts (Visualizations)/broadband_statbarchart.png", stat_bar_chart, width = 8, height = 6, dpi = 300)

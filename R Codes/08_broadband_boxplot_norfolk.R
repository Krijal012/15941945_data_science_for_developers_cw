# Load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# Load the cleaned broadband data
broadband <- read_csv("Clean Data/broadband_clean.csv")

head(broadband)

# Check what the postcode actually looks like
head(broadband$postcode, 10)

# STEP 4: Extract the district using a pattern instead of splitting on space
# UK postcode district = letters + first number block (e.g. NR1, NR14, IP4)
broadband <- broadband %>%
  mutate(district = str_extract(postcode, "^[A-Z]{1,2}[0-9]{1,2}"))

# Check it worked
head(broadband$postcode, 10)
head(broadband$district, 10)

# STEP 5: Filter to Norfolk only
norfolk_broadband <- broadband %>%
  filter(county == "NORFOLK")

nrow(norfolk_broadband)

# Check how many districts we have and their counts
district_counts <- norfolk_broadband %>%
  count(district) %>%
  arrange(desc(n))

print(district_counts)
nrow(district_counts)   # how many unique districts total

# Keep only districts with a reasonable sample size
top_districts <- district_counts %>%
  filter(n >= 5) %>%
  pull(district)

norfolk_filtered <- norfolk_broadband %>%
  filter(district %in% top_districts)

length(unique(norfolk_filtered$district))
nrow(norfolk_filtered)

# Create the box plot
box_plot <- ggplot(norfolk_filtered, aes(x = district, y = avg_download)) +
  geom_boxplot(fill = "steelblue", outlier.alpha = 0.3) +
  labs(
    title = "Average Download Speed by District - Norfolk",
    x = "District",
    y = "Average Download Speed (Mbit/s)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(box_plot)

# Save the chart
ggsave("Charts (Visualizations)/broadband_boxplot_norfolk.png", box_plot, width = 10, height = 6, dpi = 300)

# Load libraries
library(readr)
library(dplyr)

# Load the cleaned crime data
crime <- read_csv("Clean Data/crime_clean.csv")

# Check the crime types and districts available
unique(crime$crime_type)
unique(crime$district)

# Population lookup table for each district
district_population <- tribble(
  ~district,                          ~population,
  "Breckland",                        145700,
  "Broadland",                        129000,
  "Great Yarmouth",                   99000,
  "King's Lynn and West Norfolk",     152800,
  "North Norfolk",                    103900,
  "Norwich",                          144000,
  "South Norfolk",                    140300,
  "Babergh",                          92700,
  "East Suffolk",                     250000,
  "Ipswich",                          139000,
  "Mid Suffolk",                      102500,
  "West Suffolk",                     180000
)

print(district_population)

# Filter crime data to Robbery only
robbery <- crime %>%
  filter(crime_type == "Robbery")

nrow(robbery)

# Count robberies per district
robbery_counts <- robbery %>%
  group_by(district) %>%
  summarise(n_robberies = n())

print(robbery_counts)

# Join with population and calculate rate per 100,000
robbery_rate <- robbery_counts %>%
  left_join(district_population, by = "district") %>%
  mutate(rate_per_100k = round((n_robberies / population) * 100000, 1)) %>%
  filter(!is.na(rate_per_100k))

print(robbery_rate)

# Create the labelled pie chart
png("Charts (Visualizations)/crime_piechart_robbery.png", width = 900, height = 700)

pie(robbery_rate$rate_per_100k,
    labels = paste0(robbery_rate$district, "\n", robbery_rate$rate_per_100k),
    main = "Robbery Rate per 100,000 People by District",
    col = rainbow(nrow(robbery_rate)),
    cex = 0.8)

dev.off()

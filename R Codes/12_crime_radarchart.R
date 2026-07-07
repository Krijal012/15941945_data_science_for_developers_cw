# ============================================
# STEP 1: Load libraries
# ============================================
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(fmsb)

# ============================================
# STEP 2: Load population data
# ============================================
population <- read_csv("Raw Data/Population2011_1656567141570.csv")

population_clean <- population %>%
  mutate(
    Population = as.numeric(gsub(",", "", Population)),
    area = str_extract(Postcode, "^[A-Z]+")
  ) %>%
  filter(area %in% c("NR", "IP")) %>%
  mutate(county = ifelse(area == "NR", "NORFOLK", "SUFFOLK")) %>%
  group_by(county) %>%
  summarise(total_population = sum(Population, na.rm = TRUE))

print(population_clean)

# ============================================
# STEP 3: Load crime data and filter
# ============================================
crime <- read_csv("Clean Data/crime_clean.csv")

# Check what month format looks like
head(unique(crime$month))

vehicle_crime <- crime %>%
  filter(crime_type == "Vehicle crime", month >= "2024-05", month <= "2025-04")

nrow(vehicle_crime)

# ============================================
# STEP 4: Count crimes per county per month
# ============================================
monthly_counts <- vehicle_crime %>%
  group_by(county, month) %>%
  summarise(n_crimes = n(), .groups = "drop")

print(monthly_counts)

# CHECK: does every month have BOTH counties? (should be 24 rows total: 12 months x 2 counties)
nrow(monthly_counts)

# ============================================
# STEP 5: Calculate rate per 100,000 people
# ============================================
monthly_rate <- monthly_counts %>%
  left_join(population_clean, by = "county") %>%
  mutate(rate_per_100k = round((n_crimes / total_population) * 100000, 2))

print(monthly_rate)

# ============================================
# STEP 6: Reshape into wide format, fill any missing months with 0
# ============================================
radar_data <- monthly_rate %>%
  select(county, month, rate_per_100k) %>%
  pivot_wider(names_from = month, values_from = rate_per_100k, values_fill = 0) %>%
  arrange(county)

print(radar_data)   # check no NA values appear here

# ============================================
# STEP 7: Prepare data structure fmsb needs
# ============================================
radar_df <- as.data.frame(radar_data[, -1])
rownames(radar_df) <- radar_data$county

max_val <- ceiling(max(radar_df, na.rm = TRUE) * 1.2)
min_val <- 0

radar_final <- rbind(rep(max_val, ncol(radar_df)), rep(min_val, ncol(radar_df)), radar_df)

# ============================================
# STEP 8: SHOW the chart on screen first (to check it looks right)
# ============================================
radarchart(radar_final,
           pcol = c("steelblue", "darkorange"),
           plwd = 2,
           title = "Vehicle Crime Rate per 100,000 People (May 2024 - April 2025)")
legend("topright", legend = rownames(radar_df), col = c("steelblue", "darkorange"), lty = 1, lwd = 2)

# ============================================
# STEP 9: NOW save it to a file (only after confirming it looks right above)
# ============================================
png("Charts (Visualizations)/crime_radarchart_vehicle.png", width = 800, height = 800)
radarchart(radar_final,
           pcol = c("steelblue", "darkorange"),
           plwd = 2,
           title = "Vehicle Crime Rate per 100,000 People (May 2024 - April 2025)")
legend("topright", legend = rownames(radar_df), col = c("steelblue", "darkorange"), lty = 1, lwd = 2)
dev.off()
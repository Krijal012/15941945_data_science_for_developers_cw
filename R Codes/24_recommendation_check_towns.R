# ============================================
# STEP 1: Load libraries
# ============================================
library(readr)
library(dplyr)

# ============================================
# STEP 2: Load all cleaned datasets
# ============================================
house_prices <- read_csv("Clean Data/house_prices_all_years.csv")
education <- read_csv("Clean Data/education_clean.csv")
broadband <- read_csv("Clean Data/broadband_clean.csv")
crime <- read_csv("Clean Data/crime_clean.csv")

# ============================================
# STEP 3: Average house price per town (most recent year, 2025)
# ============================================
town_price <- house_prices %>%
  filter(year == 2025) %>%
  group_by(town, county) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")

head(town_price)
nrow(town_price)

# ============================================
# STEP 4: Average Attainment 8 per town (most recent year, 2024-2025)
# ============================================
town_school <- education %>%
  filter(year == "2024-2025") %>%
  group_by(town, county) %>%
  summarise(avg_att8 = mean(att8_score, na.rm = TRUE), .groups = "drop")

head(town_school)
nrow(town_school)

# ============================================
# STEP 5: Join house price + school data by town
# (only keeps towns present in BOTH datasets)
# ============================================
towns_combined <- town_price %>%
  inner_join(town_school, by = c("town", "county"))

nrow(towns_combined)
head(towns_combined)

# ============================================
# STEP 6: County-level download speed (applied to all towns in that county)
# ============================================
county_speed <- broadband %>%
  group_by(county) %>%
  summarise(avg_download = mean(avg_download, na.rm = TRUE))

print(county_speed)

# ============================================
# STEP 7: County-level crime rate (2025, all crime types combined, per 100k)
# ============================================
county_population <- tribble(
  ~county,    ~population,
  "NORFOLK",  914500,
  "SUFFOLK",  762000
)

county_crime <- crime %>%
  mutate(year = as.numeric(substr(month, 1, 4))) %>%
  filter(year == 2025) %>%
  group_by(county) %>%
  summarise(n_crimes = n()) %>%
  left_join(county_population, by = "county") %>%
  mutate(crime_rate_per_100k = (n_crimes / population) * 100000)

print(county_crime)

# ============================================
# STEP 8: Add download speed and crime rate to every town
# (based on which county the town belongs to)
# ============================================
towns_full <- towns_combined %>%
  left_join(county_speed, by = "county") %>%
  left_join(county_crime %>% select(county, crime_rate_per_100k), by = "county")

head(towns_full)

# ============================================
# STEP 9: Normalize each metric to a 0-100 scale
# Higher = better for all 4 final scores
# ============================================
normalize <- function(x, reverse = FALSE) {
  score <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
  if (reverse) score <- 100 - score   # flip so LOWER original value = HIGHER score
  return(score)
}

towns_scored <- towns_full %>%
  mutate(
    price_score = normalize(avg_price, reverse = TRUE),     # lower price = better = higher score
    school_score = normalize(avg_att8, reverse = FALSE),    # higher score = better
    speed_score = normalize(avg_download, reverse = FALSE), # higher speed = better
    crime_score = normalize(crime_rate_per_100k, reverse = TRUE)  # lower crime = better
  )

# ============================================
# STEP 10: Calculate overall score (simple average of the 4 scores)
# ============================================
towns_scored <- towns_scored %>%
  mutate(overall_score = round((price_score + school_score + speed_score + crime_score) / 4, 1))

# ============================================
# STEP 11: Rank towns and get the TOP 10
# ============================================
top10_towns <- towns_scored %>%
  arrange(desc(overall_score)) %>%
  select(town, county, avg_price, avg_att8, avg_download, crime_rate_per_100k, overall_score) %>%
  head(10)

print(top10_towns)

# ============================================
# STEP 12: Save the final table
# ============================================
write_csv(top10_towns, "Clean Data/top10_towns_recommendation.csv")

list.files("Clean Data")
# Load libraries
library(readr)
library(dplyr)

# Define column names (raw files have no headers)
col_names <- c("id", "price", "date", "postcode", "property_type",
               "old_new", "duration", "paon", "saon", "street",
               "locality", "town", "district", "county",
               "category", "status")

# Clean 2021
data_2021 <- read_csv("Raw Data/pp-2021.csv", col_names = col_names)

data_clean_2021 <- data_2021 %>%
  filter(county == "NORFOLK" | county == "SUFFOLK") %>%
  mutate(year = as.numeric(substr(date, 1, 4))) %>%
  select(price, town, county, year)

nrow(data_clean_2021)
write_csv(data_clean_2021, "Clean Data/house_prices_2021_clean.csv")

# Clean 2022
data_2022 <- read_csv("Raw Data/pp-2022.csv", col_names = col_names)

data_clean_2022 <- data_2022 %>%
  filter(county == "NORFOLK" | county == "SUFFOLK") %>%
  mutate(year = as.numeric(substr(date, 1, 4))) %>%
  select(price, town, county, year)

nrow(data_clean_2022)
write_csv(data_clean_2022, "Clean Data/house_prices_2022_clean.csv")

# Clean 2023
data_2023 <- read_csv("Raw Data/pp-2023.csv", col_names = col_names)

data_clean_2023 <- data_2023 %>%
  filter(county == "NORFOLK" | county == "SUFFOLK") %>%
  mutate(year = as.numeric(substr(date, 1, 4))) %>%
  select(price, town, county, year)

nrow(data_clean_2023)
write_csv(data_clean_2023, "Clean Data/house_prices_2023_clean.csv")

# Clean 2024
data_2024 <- read_csv("Raw Data/pp-2024.csv", col_names = col_names)

data_clean_2024 <- data_2024 %>%
  filter(county == "NORFOLK" | county == "SUFFOLK") %>%
  mutate(year = as.numeric(substr(date, 1, 4))) %>%
  select(price, town, county, year)

nrow(data_clean_2024)
write_csv(data_clean_2024, "Clean Data/house_prices_2024_clean.csv")

# Clean 2025
data_2025 <- read_csv("Raw Data/pp-2025.csv", col_names = col_names)

data_clean_2025 <- data_2025 %>%
  filter(county == "NORFOLK" | county == "SUFFOLK") %>%
  mutate(year = as.numeric(substr(date, 1, 4))) %>%
  select(price, town, county, year)

nrow(data_clean_2025)
write_csv(data_clean_2025, "Clean Data/house_prices_2025_clean.csv")

# Combine all 5 years into one file
house_prices_all <- bind_rows(
  data_clean_2021,
  data_clean_2022,
  data_clean_2023,
  data_clean_2024,
  data_clean_2025
)

nrow(house_prices_all)
head(house_prices_all)
table(house_prices_all$year, house_prices_all$county)  # counts per year/county

write_csv(house_prices_all, "Clean Data/house_prices_all_years.csv")

# Confirm everything saved
list.files("Clean Data")

# ============================================
# STEP 1: Install and load required libraries
# ============================================
install.packages(c("DBI", "RSQLite", "readr", "dplyr"))

library(DBI)
library(RSQLite)
library(readr)
library(dplyr)

# ============================================
# STEP 2: Load all cleaned datasets
# ============================================
house <- read_csv("Clean Data/house_prices_all_years.csv", show_col_types = FALSE)
broadband <- read_csv("Clean Data/broadband_clean.csv", show_col_types = FALSE)
crime <- read_csv("Clean Data/crime_clean.csv", show_col_types = FALSE)
education <- read_csv("Clean Data/education_clean.csv", show_col_types = FALSE)
population <- read_csv("Clean Data/population_clean.csv", show_col_types = FALSE)

# ============================================
# STEP 3: Create the County table (reference table for 3NF)
# ============================================
county <- data.frame(
  county_id = c(1, 2),
  county_name = c("NORFOLK", "SUFFOLK")
)

print(county)

# ============================================
# STEP 4: Create the Town table (linked to County)
# Built from unique towns found in house price + education data
# ============================================
town <- house %>%
  distinct(town, county) %>%
  left_join(county, by = c("county" = "county_name")) %>%
  select(town, county_id) %>%
  distinct() %>%
  mutate(town_id = row_number()) %>%
  select(town_id, town, county_id)

head(town)
nrow(town)

# ============================================
# STEP 5: Clean HousePrices table (linked to Town, not repeating county name)
# ============================================
house_clean <- house %>%
  left_join(town, by = "town") %>%
  select(price, town_id, year)

head(house_clean)

# ============================================
# STEP 6: Clean Broadband table (linked to County, since data is county-level)
# ============================================
broadband_clean <- broadband %>%
  left_join(county, by = c("county" = "county_name")) %>%
  select(postcode, postcode_area, avg_download, max_download, county_id)

head(broadband_clean)

# ============================================
# STEP 7: Clean Crime table (linked to County)
# ============================================
crime_clean <- crime %>%
  left_join(county, by = c("county" = "county_name")) %>%
  select(month, district, crime_type, county_id)

head(crime_clean)

# ============================================
# STEP 8: Clean Education table (linked to Town)
# ============================================
education_clean <- education %>%
  left_join(town, by = "town") %>%
  select(URN, att8_score, year, town_id)

head(education_clean)

# ============================================
# STEP 9: Clean Population table (linked to County)
# ============================================
population_clean <- population %>%
  left_join(county, by = c("county" = "county_name")) %>%
  select(total_population, county_id)

head(population_clean)

# ============================================
# STEP 10: Create the SQLite database and write all tables
# ============================================
con <- dbConnect(SQLite(), "Clean Data/norfolk_suffolk_database.db")

dbWriteTable(con, "County", county, overwrite = TRUE)
dbWriteTable(con, "Town", town, overwrite = TRUE)
dbWriteTable(con, "HousePrices", house_clean, overwrite = TRUE)
dbWriteTable(con, "Broadband", broadband_clean, overwrite = TRUE)
dbWriteTable(con, "Crime", crime_clean, overwrite = TRUE)
dbWriteTable(con, "Education", education_clean, overwrite = TRUE)
dbWriteTable(con, "Population", population_clean, overwrite = TRUE)

# ============================================
# STEP 11: Verify everything was written correctly
# ============================================
dbListTables(con)

dbGetQuery(con, "SELECT * FROM County")
dbGetQuery(con, "SELECT * FROM Town LIMIT 10")
dbGetQuery(con, "SELECT * FROM HousePrices LIMIT 10")
dbGetQuery(con, "SELECT * FROM Broadband LIMIT 10")
dbGetQuery(con, "SELECT * FROM Crime LIMIT 10")
dbGetQuery(con, "SELECT * FROM Education LIMIT 10")
dbGetQuery(con, "SELECT * FROM Population LIMIT 10")

dbListFields(con, "HousePrices")
dbListFields(con, "Broadband")
dbListFields(con, "Crime")
dbListFields(con, "Education")
dbListFields(con, "Population")

# ============================================
# STEP 12: Disconnect
# ============================================
dbDisconnect(con)
# Load libraries
library(readr)
library(dplyr)

# Find every crime CSV file automatically
crime_files <- list.files(
  path = "Raw Data/crime",
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Check it found all 72 files
length(crime_files)
head(crime_files)

# Read all 72 files and stack them into one big table
crime_raw <- crime_files %>%
  lapply(read_csv) %>%
  bind_rows()

# Check it worked
nrow(crime_raw)
head(crime_raw)

# Keep only the columns we need
crime_selected <- crime_raw %>%
  select(month = Month, lsoa_name = `LSOA name`, crime_type = `Crime type`)

# Extract district name and assign county
crime_clean <- crime_selected %>%
  mutate(
    district = sub(" [0-9A-Z]+$", "", lsoa_name),
    county = case_when(
      district %in% c("Breckland", "Broadland", "Great Yarmouth",
                      "King's Lynn and West Norfolk", "North Norfolk",
                      "Norwich", "South Norfolk") ~ "NORFOLK",
      TRUE ~ "SUFFOLK"
    )
  ) %>%
  select(month, district, county, crime_type)

# Check the result
head(crime_clean)
nrow(crime_clean)
table(crime_clean$county)   # should show both counties with reasonable counts


# Save the cleaned file
write_csv(crime_clean, "Clean Data/crime_clean.csv")

list.files("Clean Data")

nrow(crime_clean)
table(crime_clean$county)
head(crime_clean)
file.exists("Clean Data/crime_clean.csv")

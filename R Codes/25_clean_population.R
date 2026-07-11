# Load libraries
library(readr)
library(dplyr)
library(stringr)

# Load the raw population data
population <- read_csv("Raw Data/Population2011_1656567141570.csv")

head(population)

# Convert population text (e.g. "5,453") to numbers,
population_clean <- population %>%
  mutate(
    Population = as.numeric(gsub(",", "", Population)),
    area = str_extract(Postcode, "^[A-Z]+")
  ) %>%
  filter(area %in% c("NR", "IP")) %>%
  mutate(county = ifelse(area == "NR", "NORFOLK", "SUFFOLK"))

head(population_clean)

# Sum population by county to get total figures
population_by_county <- population_clean %>%
  group_by(county) %>%
  summarise(total_population = sum(Population, na.rm = TRUE))

print(population_by_county)

# Save the cleaned population data
write_csv(population_by_county, "Clean Data/population_clean.csv")

list.files("Clean Data")
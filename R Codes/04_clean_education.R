# Load libraries
library(readr)
library(dplyr)

# Find all score files and school information files
score_files <- list.files(
  "Raw Data/education",
  pattern = "ks4final.csv$",
  recursive = TRUE,
  full.names = TRUE
)

info_files <- list.files(
  "Raw Data/education",
  pattern = "school_information.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Check that all files are found
length(score_files)   # Should be 8
length(info_files)    # Should be 8

# Read and combine score files
scores_raw <- score_files %>%
  lapply(function(f) {
    df <- read_csv(f, col_types = cols(.default = "c"))
    
    df %>%
      mutate(source_file = f) %>%
      select(URN, LEA, ATT8SCR, source_file)
  }) %>%
  bind_rows()

# Read and combine school information
info_raw <- info_files %>%
  lapply(function(f) {
    read_csv(f, col_types = cols(.default = "c")) %>%
      select(URN, TOWN)
  }) %>%
  bind_rows()

# Remove duplicate schools
info_selected <- info_raw %>%
  rename(town = TOWN) %>%
  distinct(URN, .keep_all = TRUE)

# Clean score information
scores_selected <- scores_raw %>%
  mutate(
    year = sub(".*education/([0-9]{4}-[0-9]{4}).*", "\\1", source_file),
    county = if_else(LEA == "926", "NORFOLK", "SUFFOLK"),
    att8_score = suppressWarnings(as.numeric(ATT8SCR))
  ) %>%
  select(URN, county, year, att8_score)

# Join and clean data
education_clean <- scores_selected %>%
  left_join(info_selected, by = "URN") %>%
  filter(!is.na(att8_score)) %>%
  distinct(URN, year, .keep_all = TRUE) %>%   # Removes any accidental duplicates
  select(URN, town, county, year, att8_score)

# Check the results
head(education_clean)

nrow(education_clean)

table(education_clean$county, education_clean$year)

# Check for duplicate URN-Year combinations
education_clean %>%
  count(URN, year) %>%
  filter(n > 1)

# Save cleaned dataset
write_csv(education_clean, "Clean Data/education_clean.csv")

# Confirm file exists
list.files("Clean Data")

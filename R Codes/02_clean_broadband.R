# Load libraries
library(readr)
library(dplyr)

# Read the broadband file
broadband_raw <- read_csv("Raw Data/201805_fixed_pc_performance_r03.csv")

# Look at the column names to confirm what we have
colnames(broadband_raw)

# Keep only the columns we need
broadband_selected <- broadband_raw %>%
  select(
    postcode,
    postcode_area = `postcode area`,
    avg_download = `Average download speed (Mbit/s)`,
    max_download = `Maximum download speed (Mbit/s)`
  )

# Filter to Norfolk (NR) and Suffolk (IP) postcodes only
broadband_clean <- broadband_selected %>%
  filter(postcode_area == "NR" | postcode_area == "IP") %>%
  mutate(county = ifelse(postcode_area == "NR", "NORFOLK", "SUFFOLK"))

# Check how many rows we have now
nrow(broadband_clean)
head(broadband_clean)

# Save the cleaned file
write_csv(broadband_clean, "Clean Data/broadband_clean.csv")

list.files("Clean Data")

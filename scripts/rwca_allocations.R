library(readxl)
library(dplyr)
library(readr)

data_loc <- "/cloud/project/data" 

# Read the prices data and process it
prices_fred <- read_excel(file.path(data_loc, "prices_fred.xls"), sheet = "FRED Graph") %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year) %>%
  summarise(cpi = mean(cpi, na.rm = TRUE)) %>%
  filter(year > 1990, year < 2019)

# Temporarily save the processed data
tempfile1 <- tempfile(fileext = ".rds")
saveRDS(prices_fred, tempfile1)

# Read the title1 funding data and merge with the prices data
title1_funding <- read_excel(file.path(data_loc, "title1.xls"), sheet = "Sheet1") %>%
  left_join(readRDS(tempfile1), by = "year") %>%
  mutate(title1_funding = title1_funding * 251.10142 / cpi) %>%
  select(-cpi)

# Generate funding variables
title1_funding <- title1_funding %>%
  mutate(funding_1991_1995 = if_else(year < 1996, title1_funding, NA_real_),
         funding_1996_2000 = if_else(year > 1995 & year < 2001, title1_funding, NA_real_),
         # Continue for other funding periods
  ) %>%
  group_by(cityfip) %>%
  summarise(across(starts_with("funding"), sum, na.rm = TRUE))

# Save the aggregated funding data
write_csv(title1_funding, file.path(data_loc, "interim/agg_title1_funding_by_city.csv"))

# Read state-year AIDS cases data from CDC's AtlasPlus and Surveillance Reports
atlas_data <- read_csv(file.path(data_loc, "AtlasPlusTableData_state.csv")) %>%
  rename(statefip = fips)

# Temporarily save the processed data
tempfile2 <- tempfile(fileext = ".rds")
saveRDS(atlas_data, tempfile2)

# Append with data from surveillance reports and perform further processing
surv_report_data <- read_excel(file.path(data_loc, "surv_report_data.xlsx"), sheet = "Sheet1")

atlas_data <- read_csv(file.path(data_loc, "AtlasPlusTableData_state.csv"), col_types = cols(year = col_double(), fips = col_character(), cases = col_double())) %>%
  rename(statefip = fips)

combined_data <- bind_rows(surv_report_data, atlas_data)

write_csv(combined_data, file.path(data_loc, "interim/aids_state_year.csv"))

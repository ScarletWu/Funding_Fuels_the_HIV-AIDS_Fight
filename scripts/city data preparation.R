library(dplyr)
library(readr)
library(readxl)
library(haven)

data_loc <- "/project/data" 


# 1994 AIDS cases
aids_cases_1994 <- read_delim(file.path(data_loc, "aids_cases_rwca_1994.txt"), delim = ",") %>%
  filter(!is.na(locationcode)) %>%
  select(cases, locationcode, location) %>%
  rename(cityfip = locationcode, aids_reported_by_1995 = cases)
write_csv(aids_cases_1994, file.path(data_loc, "cleaned/aids_cases_rwca_1994.csv"))

# 1995 AIDS cases
aids_cases_1995 <- read_delim(file.path(data_loc, "aids_cases_rwca_1995.txt"), delim = ",") %>%
  filter(!is.na(locationcode)) %>%
  select(cases, locationcode, location) %>%
  rename(cityfip = locationcode, aids_reported_in_1995 = cases)
write_csv(aids_cases_1995, file.path(data_loc, "cleaned/aids_cases_rwca_1995.csv"))

title1_info <- read_excel(file.path(data_loc, "t1years.xlsx"), sheet = "Sheet1") %>%
  filter(city != "Honolulu, HI", city != "San Juan, PR") %>%
  mutate(state_postal = substr(city, nchar(city)-1, nchar(city)),
         state_postal = if_else(state_postal == "M.", "NM", state_postal),
         cityfip = as.integer(cityfip)) %>%
  left_join(read_csv(file.path(data_loc, "cleaned/aids_cases_rwca_1995.csv")), by = "cityfip") %>%
  left_join(read_csv(file.path(data_loc, "cleaned/aids_cases_rwca_1994.csv")), by = "cityfip") %>%
  mutate(aids_ever_1995_imp = round(aids_reported_by_1995 + 0.25 * aids_reported_in_1995),
         rank_1995 = row_number()) %>%
  select(cityfip, year_rwca_status, aids_ever_1995_imp, rank_1995, state_postal)
write_csv(title1_info, file.path(data_loc, "/cleaned/t1years.csv"))


# AIDS cases reported by year
aids_cases_reported <- read_delim(file.path(data_loc, "/aids_cases_rep_year.txt"), delim = ",") %>%
  mutate(aids_rep = cases,
         year = as.integer(yearreportedcode),
         cityfip = as.integer(locationcode)) %>%
  filter(!is.na(cityfip)) %>%
  select(aids_rep, cityfip, year)
write_csv(aids_cases_reported, file.path(data_loc, "/cleaned/aids_cases_reported.csv"))

# AIDS cases diagnosed by year
aids_cases_diagnosed <- read_delim(file.path(data_loc, "/aids_cases_diag_year.txt"), delim = ",") %>%
  mutate(aids_diag_apids = cases,
         year = as.integer(yeardiagnosedcode),
         cityfip = as.integer(locationcode)) %>%
  filter(!is.na(cityfip)) %>%
  select(aids_diag_apids, cityfip, year)
write_csv(aids_cases_diagnosed, file.path(data_loc, "/cleaned/aids_cases_diagnosed.csv"))

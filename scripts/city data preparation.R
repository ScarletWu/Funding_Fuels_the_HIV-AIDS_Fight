library(dplyr)
library(readr)
library(readxl)
library(haven)

data_loc <- "/cloud/project/data" 


# 1994 AIDS cases
aids_cases_1994 <- read_tsv(file.path(data_loc, "aids_cases_rwca_1994.txt")) %>%
  filter(!is.na(`Location Code`)) %>%
  select(Cases, `Location Code`, Location) %>%
  rename(cityfip = `Location Code`, aids_reported_by_1995 = Cases)
write_csv(aids_cases_1994, file.path(data_loc, "cleaned/aids_cases_rwca_1994.csv"))

# 1995 AIDS cases
aids_cases_1995 <- read_tsv(file.path(data_loc, "aids_cases_rwca_1995.txt")) %>%
  filter(!is.na(`Location Code`)) %>%
  select(Cases, `Location Code`, Location) %>%
  rename(cityfip = `Location Code`, aids_reported_in_1995 = Cases)
write_csv(aids_cases_1995, file.path(data_loc, "cleaned/aids_cases_rwca_1995.csv"))

title1_info <- read_excel(file.path(data_loc, "t1years.xlsx"), sheet = "Sheet1") %>%
  filter(city != "Honolulu, HI", city != "San Juan, PR") %>%
  mutate(
    state_postal = substr(city, nchar(city)-1, nchar(city)),
    state_postal = if_else(state_postal == "M.", "NM", state_postal),
    cityfip = as.character(cityfip)  # Convert cityfip to character
  ) %>%
  left_join(read_csv(file.path(data_loc, "cleaned/aids_cases_rwca_1995.csv"), col_types = cols(cityfip = col_character())), by = "cityfip") %>%
  left_join(read_csv(file.path(data_loc, "cleaned/aids_cases_rwca_1994.csv"), col_types = cols(cityfip = col_character())), by = "cityfip") %>%
  mutate(
    aids_ever_1995_imp = round(aids_reported_by_1995 + 0.25 * aids_reported_in_1995),
    rank_1995 = row_number()
  ) %>%
  select(cityfip, year_rwca_status, aids_ever_1995_imp, rank_1995, state_postal)

write_csv(title1_info, file.path(data_loc, "cleaned/t1years.csv"))


# AIDS cases reported by year
aids_cases_reported <- read_tsv(file.path(data_loc, "aids_cases_rep_year.txt"), show_col_types = FALSE) %>%
  select(-Notes, `Year Reported Code`, Location, `Location Code`, Cases) %>%
  rename(year = `Year Reported Code`, 
         cityfip = `Location Code`, 
         aids_rep = Cases) %>%
  mutate(year = as.integer(year),
         cityfip = as.character(cityfip), # Assuming cityfip should be treated as a string due to leading zeros
         aids_rep = as.integer(aids_rep))
write_csv(aids_cases_reported, file.path(data_loc, "cleaned/aids_cases_reported.csv"))

# AIDS cases diagnosed by year
aids_cases_diagnosed <- read_tsv(file.path(data_loc, "aids_cases_diag_year.txt"), show_col_types = FALSE) %>%
  mutate(
    aids_diag_apids = as.integer(Cases),
    year = as.integer(`Year Diagnosed Code`),
    cityfip = as.character(`Location Code`)  # Keeping as character to preserve any leading zeros
  ) %>%
  filter(!is.na(cityfip)) %>%
  select(aids_diag_apids, cityfip, year)
write_csv(aids_cases_diagnosed, file.path(data_loc, "cleaned/aids_cases_diagnosed.csv"))

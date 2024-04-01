library(dplyr)
library(haven)

data_loc <- "path_to_your_data_folder" # Update with actual data location

# Reading initial dataset
seer_info <- read_dta(file.path(data_loc, "interim/seer_info_by_city.dta"))

# Merge function to simplify merging process
merge_data <- function(data, path, type = "inner", by = c("year", "cityfip")) {
  file_path <- file.path(data_loc, path)
  merge_func <- match.fun(paste0(type, "_join"))
  data <- merge_func(data, read_dta(file_path), by = by)
  if ("_merge" %in% names(data)) data <- data %>% select(-_merge) # Drop _merge if present
  data
}

# Sequential merges
files_to_merge <- c("interim/seer_info_by_city_hisp.dta", "interim/vs_city_year.dta", 
                    "interim/laus_city_data.dta", "interim/t1years.dta", 
                    "interim/aids_cases_reported.dta", "interim/aids_cases_diagnosed.dta", 
                    "interim/cdc_city_aids_prev", "interim/cdc_city_aids_diagnosis", 
                    "interim/rwca_non_city", "interim/agg_title1_funding_by_city.dta", 
                    "interim/state_year_controls.dta", "interim/vs_city_age_adjusted.dta", 
                    "interim/ann_years_lost_aids.dta")

# Adjust merge type and by columns as needed per file
for (file in files_to_merge) {
  seer_info <- merge_data(seer_info, file, type = "inner")
}

# Generating new variables and replacing missing values
seer_info <- seer_info %>%
  mutate(across(starts_with("funding_"), ~replace(., is.na(.), 0)),
         aids_dths_all_100k = aids_dths_all / pop_all * 100000,
         laids_dths_all_100k = log(aids_dths_all_100k),
         within30 = aids_ever_1995_imp < 7500 & aids_ever_1995_imp > 750,
         # Add more conditions and variables as needed
  ) %>%
  arrange(cityfip, year)

# Save processed data
write_dta(seer_info, file.path(data_loc, "cleaned/rwca_sample.dta"))

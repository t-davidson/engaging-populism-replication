# This script compiles data at the country level
# Includes updated data collected for final version
# 2/11/24

# Three data sources:
# World Bank (for all countries except Northern Ireland)
# Northern Ireland (limited data compiled manually from UK ONS)
# Eurobarometer survey question

library(tidyverse)
library(janitor)
library(readxl)

#############################
## Reading World Bank data ##
#############################

data <- read_csv("../data/country_data/world_bank_macroeconomic_data.csv") %>%
    select(-`Series Code`, -`Country Name`)

data <- data %>%
    rename(country = `Country Code`) %>%
    mutate(across(starts_with("20"), ~as.numeric(.x)))

# Pivot the year columns to a long format using names_pattern to extract the year
data_long <- data %>%
    pivot_longer(
        cols = starts_with("20"), # Selects all columns that start with "20"
        names_to = "year",
        names_pattern = ".*(\\d{4}).*", # Extracts the four-digit year from the column name
        values_to = "value"
    ) %>%
    pivot_wider(
        names_from = `Series Name`,
        values_from = value,
        names_prefix = "series_"
    ) %>%
    mutate(year = as.integer(str_extract(year, "\\d{4}"))) %>%
    relocate(country, year)

# Convert all column names to lowercase snake_case
data_long <- data_long %>%
    clean_names(case = "snake") %>%
    rename_with(~str_remove(.x, "series_"))

#############################
## Reading N.Ireland Data  ##
#############################

nir <- read_excel("../data/country_data/northern_ireland_macroeconomic_data.xls")
nir <- nir[1:13,] # Dropping rows w/o data

nir$country <- "NIR"
nir$year <- nir$year %>% as.numeric()

# Selecting columns and using same names as above
nir <- nir %>% select(country, year, gdp_per_capita_constant_2015_us = `CPI adjusted`, gdp_per_capita_growth_annual_percent = `Change in 2015 GBP (%)`, unemployment_total_percent_of_total_labor_force_modeled_ilo_estimate = `Unemployment rate`, population_total = Population)

# Ensuring all numeric
nir <- nir %>%
    mutate(across(-country, ~as.numeric(as.character(.x))))

# Merging with World Bank data (note that most columns are missing so using full join)
merged <- full_join(data_long, nir)

#############################
## Reading Eurobarometer   ##
#############################

eurobarometer <- read.csv("../data/eurobarometer_data/Eurobarometer.csv")

# Rename columns for consistency and clarity
eurobarometer <- rename(eurobarometer,
                             year = Year,
                             ebq1_1 = EBQ1_1,
                             country = country_right) %>%
    select(country, year, ebq1_1)

merged <- merged %>% left_join(eurobarometer, by = c("country", "year"))

#############################
##   Creating country lags  #
#############################

merged <- merged %>% group_by(country) %>%
    mutate(refugee_change = refugee_population_by_country_or_territory_of_asylum -
               lag(refugee_population_by_country_or_territory_of_asylum)) %>% ungroup()

#############################
##   Writing the results   ##
#############################

write_csv(merged, "../data/country_data/merged_country_data.csv")

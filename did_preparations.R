library(tidyr)
library(did)
library(readxl)

# Reading the data to R -------------------------------------------------------

df <- read_rds("data/combined.rds")

# Data preparation for the d-i-d analysis --------------------------------------

# Selecting the countries of interest

df_did <- df |>
  filter(country %in% c("Australia", "Canada", "South Korea", "Germany", "Japan", "United Kingdom",
                        "New Zealand", "United States", "Austria", "Israel", "Slovak Republic", "Czechia", "Hungary"))


View(df_did)

# Creating a new variable to indicate treatment and control

df_did <- df_did |>
  mutate(treatment = ifelse(country %in% c("Australia", "Canada", "South Korea", "Germany", "Japan", "United Kingdom"), 1, 0))

# Adding control variables to the data -----------------------------------------

# generic gender equality index

equality <- read_excel("WBL2024-1-0-Historical-Panel-Data.xlsx", 
           sheet = "WBL Panel 2024",
           range = cell_cols("A:G"),
           col_names = TRUE)
           
equality <- equality |>
  rename(country = "Economy",
         year = "Report Year",
         equality_index = "WBL INDEX") |>
  select(country, year, equality_index) |>
  filter(country %in% c("Australia", "Canada", "South Korea", "Germany", "Japan", "United Kingdom",
                        "New Zealand", "United States", "Austria", "Israel", "Slovak Republic", "Czechia", "Hungary")) |>
  mutate(country = ifelse(country == "Korea, Rep.", "South Korea", country))

equality$year <- as.numeric(equality$year)


df_did <- df_did |>
  left_join(equality, by = c("country", "year"))
           
# GDP per capita

# Gini index

# Comparing treatment and control conditions -----------------------------------



# Creating a new variable to indicate to indicate the treatment year individually for each country

df_did <- df_did |>
  mutate(treatment_year = case_when(
    country == "Australia" ~ year - 2014,
    country == "Canada" ~ year - 2020,
    country == "South Korea" ~ year - 2003,
    country == "Germany" ~ year - 2008,
    country == "Japan" ~ year - 2012,
    country == "United Kingdom" ~ year - 2003,
    country == "New Zealand" ~ year - 2000,
    country == "United States" ~ year - 2000,
    country == "Austria" ~ year - 2000,
    country

?case_when

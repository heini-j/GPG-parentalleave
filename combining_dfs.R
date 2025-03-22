library(dplyr)
library(readr)
library(tidyr)

# Load the data

df_parentalleave <- read_rds("data/parental_leave.rds")
df_gwg <- read_rds("data/gwg_clean.rds")

# Exploring the data to find out a good sample to work with -------------------

# finding out how many different countries are in the sample

df_parentalleave |>
  select(country) |>
  distinct() |>
  count()

# 190 different countries

df_gwg |>
  select(country) |>
  distinct() |>
  count()

# oecd dataset has 50 different countries

# finding out which countries are covered in both datasets

country_list <- df_parentalleave |>
  select(country) |>
  distinct() |>
  filter(country %in% df_gwg$country)

summarise(country_list, n())

# 47 shared countries

# filtering the datasets to only include the shared countries

df_parentalleave_common <- df_parentalleave |>
  filter(country %in% country_list$country)

df_gwg_common <- df_gwg |>
  filter(country %in% country_list$country)

# checking which years are covered by the datasets

df_parentalleave_common |>
  summarise(min(year), max(year))

# wbl has data from 1971 to 2024

df_gwg_common |>
  summarise(min(year), max(year))

# oecd has data from 1970 to 2023

# filtering the datasets to only include the years that are covered by both datasets

df_parentalleave_common <- df_parentalleave_common |>
  filter(year >= 1971 & year <= 2023)

df_gwg_common <- df_gwg_common |>
  filter(year >= 1971 & year <= 2023)

# Combining the dataframes --------------------------------------------------

# reshaping the oecd dataset to wide format

gwg_wide <- df_gwg_common |>
  pivot_wider(names_from = gwg_type, values_from = gwg)

# merging the dataframes

df_combined <- df_parentalleave_common |>
  left_join(gwg_wide, by = c("country", "year"))

View(df_combined)


# Data exploration to find a suitable sample  ----------------------------------------


# COmbining the paternity leave and fathers' share of shared leave to form a complete picture of the parental leave

df_combined <- df_combined |>
  mutate(paternity_total = paternityleave_length + shared_length_father)

# doing the same for maternity leaves

df_combined <- df_combined |>
  mutate(maternity_total = maternityleave_length + shared_length_mother)

# Renaming the wage gap columns to make them more understandable

df_combined <- df_combined |>
  rename(
   gwg_median = "MEDIAN",
   gwg_d1 = "D1",
   gwg_d9 = "D9")


# Saving the combined data --------------------------------------------------

write_rds(df_combined, "data/combined.rds")



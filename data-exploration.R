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

# oecd dataset has 48 different countries

# finding out which countries are covered in both datasets

country_list <- df_parentalleave |>
  select(country) |>
  distinct() |>
  filter(country %in% df_gwg$country)

summarise(country_list, n())

# 44 shared countries

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

# oecd has data from 2005 to 2023

# Discovering paternity leave lenghts ----------------------------------------

# Cut-off at 5 years prior and after 

# Which countries had paternity leave of at least 14 days in 2018 

df_parentalleave_common |>
  filter(year == 2018 & paternityleave_length >= 14)|>
  summarise(n())

# 17 countries

# cut-off at 2015

country_list <- df_parentalleave_common |>
  filter(year == 2015 & paternityleave_length >= 14)

country_list |> summarise(n())

# 16 countries -> only a difference of 1 so this could be a better cut-off point to allow for data points after


# Combining the dataframes --------------------------------------------------

# How many zeroes in the gwg column?

df_gwg_common |>
  filter(gwg == 0) |>
  count()

# 34 zeroes, 39 NAs

# turning the gwg dataframe to the wide format

gwg_wide <- df_gwg_common |>
  pivot_wider(names_from = gwg_type, values_from = gwg)

# how many zeroes in the new columns

gwg_wide |>
  filter(D9 == 0) |>
  count()
 
# MEDIAN: 4 NAs , 0 zeroes
# D1 43 NAs, 26 zeroes
# D9 28 NAs, 8 zeroes

# merging the dataframes

df_combined <- df_parentalleave_common |>
  left_join(gwg_wide, by = c("country", "year", "country_code"))

View(df_combined)


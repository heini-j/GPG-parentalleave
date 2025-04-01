library(dplyr)
library(readr)
library(tidyr)

# Load the dataframes

df_parentalleave <- read_rds("data/parental_leave.rds")
df_gwg <- read_rds("data/gwg_clean.rds")

# Exploring the data to find out a good sample to work with -------------------

# finding out how many different countries are in the sample

df_parentalleave |>
  select(country) |>
  distinct() |>
  count()

# 190 different countries

country_list <- df_gwg |>
  select(country) |>
  distinct()

View(country_list)

# oecd dataset has 50 different countries (includes also EU, OECD, and other aggregates)

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


# Combining the paternity leave and fathers' share of shared leave to form a complete picture of the parental leave

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


# Adding coveriates -----------------------------------------------------------

# general gender equality index

equality <- read_excel("WBL2024-1-0-Historical-Panel-Data.xlsx", 
                       sheet = "WBL Panel 2024",
                       range = cell_cols("A:G"),
                       col_names = TRUE)

# transforming the data to match with the main dataframe

equality <- equality |>
  rename(country = "Economy",
         year = "Report Year",
         equality_index = "WBL INDEX") |>
  select(country, year, equality_index) |>
  mutate(country = ifelse(country == "Korea, Rep.", "South Korea", country))

equality$year <- as.numeric(equality$year)

# Adding to the main dataframe

df_combined <- df_combined |>
  left_join(equality, by = c("country", "year"))

df_combined |>
  count(country, year) |>
  filter(n > 1)

# GDP per capita & Gini index

gdp <- read_csv("oecd_gdp.csv")

View(gdp)

gdp <- gdp |> 
  select("Reference area", "TIME_PERIOD", "OBS_VALUE", "Transaction") |>
  filter(Transaction == "Gross domestic product") |>
  rename(gdp = "OBS_VALUE")

gini <- read_csv("oecd_gini.csv")

View(gini)

gini <- gini |> 
  select("Reference area", "TIME_PERIOD", "OBS_VALUE") |>
  rename(gini = "OBS_VALUE")

gdp <- gdp |>
  left_join(gini, by = c("Reference area", "TIME_PERIOD")) |>
  rename(country = "Reference area",
         year = "TIME_PERIOD")

gdp <- gdp |>
  mutate(country = ifelse(country == "Korea", "South Korea", country))

# Adding the values to the main dataframe

df_combined <- df_combined |>
  left_join(gdp, by = c("country", "year"))


df_combined |>
  count(country, year) |>
  filter(n > 1)

# women's labour force participation

lf_participation <- read_csv("Labor force participation rate, total (% of total population ages 15-64) (modeled ILO estimate).csv")


unique(lf_participation$"Disaggregation")

View(lf_participation)

lf_participation <- lf_participation |>
  select("Country Name", "Year", "Value", "Disaggregation") |>
  filter(Disaggregation == "female, Modeled, 15+") |>
  rename(country = "Country Name",
         year = "Year",
         lf_participation = "Value") |>
  mutate(country = ifelse(country == "Korea, Rep.", "South Korea", country))

# Adding to the main dataframe


df_combined <- df_combined |>
  left_join(lf_participation, by = c("country", "year"))


View(df_combined)


# Saving the combined data --------------------------------------------------

write_rds(df_combined, "data/combined.rds")



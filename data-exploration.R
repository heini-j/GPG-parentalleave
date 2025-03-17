library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggrepel)

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

# For a 3-year follow-up, a country has to have at least a 14-day paternity leave in 2020

df_combined |> 
  group_by(country) |> 
  filter(any(year == 2020 & paternity_total >= 14)) |>
  summarise(n = n_distinct(country))

# 27 countries out of 47 fulfill this condition

# Plotting the paternity leave lengths in all the 47 countries for grouping
selection_countries <- df_combined |>
  ggplot(aes(x = year, y = paternity_total, color = country)) +
  geom_line() +
  facet_wrap(~country, scales = "free_y") +
  geom_text_repel(data = df_combined |> filter(year == 2020), 
                  aes(label = paternity_total), 
                  hjust = 0, # Position labels slightly to the right
                  nudge_x = 0.2, # Move labels slightly away from the last point
                  direction = "y", # Avoid overlapping
                  size = 2,
                  max.overlaps = 15) +
  scale_x_continuous(breaks = seq(1971, 2020, by = 5)) +
  labs(title = "Paternity leave lengths in the sample countries",
       x = "Year",
       y = "Length of paternity leave (days)",
       color = "Country") +
  theme_minimal()+
  theme(legend.position = "none")

ggsave("plots/selected.png", selection_countries, width = 20, height = 20)

# creating 4 groups of countries based on the changes in paternity leave length - All countries had a length of 0 at some point, 
# but some have several increases. We label those as "high with increase" in comparison to those that only went from 0 directly to a higher number
# Low = less than 14 days all years up until 2020

df_combined <- df_combined |>
  mutate(paternityleave_group = case_when(
    country %in% c("Australia",  "Bulgaria", "Canada", "Cyprus", "Denmark", "Estonia", "Germany", "Ireland", "Italy", "Japan", "Lithuania", "Poland", "Luxembourg", "South Korea", "United Kingdom") ~ "Increase from zero",
    country %in% c("Belgium", "Croatia", "Finland", "France", "Iceland", "Norway", "Portugal", "Romania", "Slovenia", "Spain", "Sweden") ~ "High with increase", 
    country %in% c("Argentina", "Brazil", "Chile", "Colombia", "Czechia", "Greece", "Hungary", "Latvia", "Malta", "Mexico", "Netherlands", "Peru", "Türkiye") ~ "Constant low",
    country %in% c("Austria", "Costa Rica", "India", "Israel", "New Zealand", "Slovak Republic", "Switzerland" , "United States") ~ "Constant zero")) 

# Adding numerical values to the groups

df_combined <- df_combined |>
  mutate(paternityleave_group = factor(paternityleave_group, levels = c("Constant zero", "Constant low", "Increase from zero", "High with increase")))

# Plotting the paternity leave lengths for the selected countries


length_by_group <- df_combined |>
  filter(year >=1971 & year <= 2020) |>
  ggplot(aes(x = year, y = paternity_total, color = country)) +
  geom_line() +
  facet_wrap(~paternityleave_group, scales = "free_y") +
  geom_text_repel(data = df_combined |> filter(year == 2020), 
                  aes(label = country), 
                  hjust = 0, # Position labels slightly to the right
                  nudge_x = 0.2, # Move labels slightly away from the last point
                  direction = "y", # Avoid overlapping
                  size = 2,
                  max.overlaps = 15) +
  scale_x_continuous(breaks = seq(1971, 2020, by = 5)) +
  labs(title = "Paternity leave lengths",
       x = "Year",
       y = "Length of paternity leave (days)",
       color = "Country") +
  theme_minimal()+
  theme(legend.position = "none")

# Saving the plot

ggsave("plots/length_by_group.png", length_by_group, width = 20, height = 20)

# Renaming some columns for clearer read

df_combined <- df_combined |>
  rename(
    gwg_median = "MEDIAN",
    gwg_d1 = "D1",
    gwg_d9 = "D9")
    
# Missing variables -----------------------------------------------------------

# Checking the column wise missing values in the combined dataset

df_missing <- df_combined |>
  summarise_all(~sum(is.na(.)))

View(df_missing)

# A lot of missing values in the GWG columns -> analysing for systematic errors

df_missing <- df_combined |>
  group_by(country) |>
  summarise(missing_median = sum(is.na(gwg_median)),
            missing_d1 = sum(is.na(gwg_d1)),
            missing_d9 = sum(is.na(gwg_d9))) |>
  arrange(desc(missing_median))

# some countries are missing a lot of values; deletion depends on the time period of the missing values

# Plotting the missing values against years to find systematic errors

df_missing |>
  ggplot(aes(x = country, y = missing_median)) +
  geom_point() +
  geom_point(aes(y = missing_d1), color = "red") +
  geom_point(aes(y = missing_d9), color = "blue") +
  labs(title = "Missing values in the GWG columns",
       x = "Country",
       y = "Number of missing values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plotting missing values per year

df_missing_year <- df_combined |>
  group_by(year, country) |>
  summarise(missing_median = sum(is.na(gwg_median)),
            missing_d1 = sum(is.na(gwg_d1)),
            missing_d9 = sum(is.na(gwg_d9)))


df_missing_year |>
  ggplot(aes(x = year, y = missing_median)) +
  geom_point(size = 1)+
  facet_wrap(~country) +
  labs(title = "Missing values in the GWG columns",
       x = "Year",
       y = "Number of missing values") +
  theme_minimal()

# a lot of missing values before 2010; basically gwg info from more than half of the countries missing

# Countries that have least/longest period of no missing values:~
# ~2000: Austria, Belgium, Canada, Czechia, Denmark, Germany, Hungary, Israel, New Zealand, Norway, Poland, Slovak, Sweden, Switzerland
# Longer: Australia, Finland, Japan, UK, US
# To be removed: Turkey, Luxembourg, Argentina, india, Croatia, Bulgaria, Romania

# Removing the countries with a long period of missing values

df_combined <- df_combined |>
  filter(!country %in% c("Türkiye", "Luxembourg", "Argentina", "India", "Croatia", "Bulgaria", "Romania"))

# Saving the combined data --------------------------------------------------

write_rds(df_combined, "data/combined.rds")


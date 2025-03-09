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

# filtering the datasets to only include the years that are covered by both datasets

df_parentalleave_common <- df_parentalleave_common |>
  filter(year >= 2005 & year <= 2023)

# Discovering paternity leave lengths ----------------------------------------

# Plotting the leave lengths in the 44 countries to see how they have changed over time

df_filtered <- df_parentalleave_common |> 
  group_by(country) |> 
  filter(any(year == 2020 & paternityleave_length >= 14)) |>
  filter(year >= 2008 & year <= 2020)

filtered_countries <- unique(df_filtered$country)

# 18 countriess left after filtering

selection_countries <- df_filtered |>
  ggplot(aes(x = year, y = paternityleave_length, color = country)) +
  geom_line() +
  facet_wrap(~country) +
  geom_text_repel(data = df_filtered |> filter(year == 2020), 
                  aes(label = country), 
                  hjust = 0, # Position labels slightly to the right
                  nudge_x = 0.2, # Move labels slightly away from the last point
                  direction = "y", # Avoid overlapping
                  size = 2,
                  max.overlaps = 15) +
  scale_x_continuous(breaks = seq(2005, 2020, by = 1)) +
  labs(title = "Paternity leave lengths in 18 countries",
       x = "Year",
       y = "Length of paternity leave (days)",
       color = "Country") +
  theme_minimal()+
  theme(legend.position = "none")

ggsave("plots/selected.png", selection_countries, width = 20, height = 20)

# Here, we can see that for the purpose of this study, Australia, Bulgaria, Cyprus,
# Finland, Ireland, Lithuania, Luxembourg, Poland, Portugal, Slovenia and Spain
# had an increase of at least 14 days in the relevant time period

# Of these, the increase was from 0 to 14 days in Australia, Bulgaria, Cyprus, Ireland, Lithuania (to 30 days),
# Poland

# Belgium, Denmark, Estonia, Iceland, Romania, Sweden and the UK had no changes to the paternity leave (>= 14 days) in this time period

# Leave length was 0 throughout the time period in Austria, Canada, Costa Rica, Croatia, Germany,
# Israel, Japan, New Zealand, Norway, Slovak Republic, Switzerland and the US

# leave length was less than 14 d in 2020 in Argentina, Brazil, Chile, Colombia, Czechia, France (decrease from 15), Greece,
# HUngary, Italy, Latvia, Malta, Maxico, Netherlands & Türkiye

# creating 4 groups of countries based on the changes in paternity leave length

df_parentalleave_common <- df_parentalleave_common |>
  mutate(paternityleave_group = case_when(
    country %in% c("Australia", "Bulgaria", "Cyprus", "Ireland", "Lithuania", "Poland", "Luxembourg") ~ "Increase from 0",
    country %in% c("Belgium", "Denmark", "Estonia", "Iceland", "Romania", "Sweden", "United Kingdom") ~ "Constant high",
    country %in% c("Finland", "Portugal", "Slovenia", "Spain") ~ "High with increase", 
    country %in% c("Argentina", "Brazil", "Chile", "Colombia", "Czechia", "France", "Greece", "Hungary", "Italy", "Latvia", "Malta", "Mexico", "Netherlands", "Türkiye") ~ "Constant low",
    country %in% c("Austria", "Canada", "Costa Rica", "Croatia", "Germany", "Israel", "Japan", "New Zealand", "Norway", "Slovak Republic", "Switzerland" , "United States") ~ "Constant zero")) 

df_parentalleave_common |>
  ggplot(aes(x = year, y = paternityleave_length, color = country)) +
  geom_line() +
  facet_wrap(~paternityleave_group, scales = "free_y") +
  geom_text_repel(data = df_parentalleave_common |> filter(year == 2023), 
                  aes(label = country), 
                  hjust = 0, # Position labels slightly to the right
                  nudge_x = 0.2, # Move labels slightly away from the last point
                  direction = "y", # Avoid overlapping
                  size = 2,
                  max.overlaps = 15) +
  scale_x_continuous(breaks = seq(2005, 2023, by = 2)) +
  labs(title = "Paternity leave lengths",
       x = "Year",
       y = "Length of paternity leave (days)",
       color = "Country") +
  theme_minimal()+
  theme(legend.position = "none")

# Cut-off at 4 years prior and after 

# Which countries had a change in the length of paternity leave between 2009 and 2019?

df_parentalleave_common |> 
  filter(year %in% c(2009, 2019)) |> # Keep only relevant years
  group_by(country) |> # Group by country to check both years
  filter(any(year == 2019 & paternityleave_length >= 7) & 
           any(year == 2009 & paternityleave_length < 7)) |> 
  summarise(n = n_distinct(country))


df_parentalleave_common |>
  filter(year == 2009 & paternityleave_length >= 14) |>
  group_by(country) |>
  summarise(n = n_distinct(country))

# 17 countries

# cut-off at 2015

country_list <- df_parentalleave_common |>
  filter(year == 2015 & paternityleave_length >= 14)

country_list |> summarise(n())

# 16 countries -> only a difference of 1 so this could be a better cut-off point to allow for data points after


# Combining the dataframes --------------------------------------------------


# merging the dataframes

df_combined <- df_parentalleave_common |>
  left_join(gwg_wide, by = c("country", "year"))

View(df_combined)

# Saving the combined data --------------------------------------------------

write_rds(df_combined, "data/combined.rds")


library(tidyr)
library(dplyr)
library(did)
library(readr)
library(readxl)
library(ggplot2)
library(ggrepel)
install.packages("fixest")
library(fixest)


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

df_did <- df_did |>
  mutate(treatment = as.factor(treatment))

# Adding control variables to the data -----------------------------------------

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
  filter(country %in% c("Australia", "Canada", "Korea, Rep.", "Germany", "Japan", "United Kingdom",
                        "New Zealand", "United States", "Austria", "Israel", "Slovak Republic", "Czechia", "Hungary")) |>
  mutate(country = ifelse(country == "Korea, Rep.", "South Korea", country))

equality$year <- as.numeric(equality$year)

# Adding to the main dataframe

df_did <- df_did |>
  left_join(equality, by = c("country", "year"))
           
# GDP per capita & Gini index

gdp <- read_csv("oecd_gdp.csv")

View(gdp)

gdp <- gdp |> 
  select("Reference area", "TIME_PERIOD", "OBS_VALUE") |>
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

df_did <- df_did |>
  left_join(gdp, by = c("country", "year"))

# Comparing treatment and control conditions -----------------------------------

# testing parallel trends assumption

summary_did <- df_did |> 
  filter(year < 2012) |>
  group_by(treatment, country) |>
  summarize(
    mean_gdp = mean(gdp, na.rm = TRUE),
    mean_gender_equality = mean(equality_index, na.rm = TRUE),
    mean_gini = mean(gini, na.rm = TRUE),
    mean_wage_gap = mean(gwg_median, na.rm = TRUE),
    mean_d1 = mean(gwg_d1, na.rm = TRUE),
    mean_d9 = mean(gwg_d9, na.rm = TRUE)
  )

View(summary_did)

# plotting the gwg median values per treatment group and year

df_did |>
  filter(year >= 1995, year <= 2003) |>
  #filter(!(country %in% c("Slovak Republic", "Israel"))) |>
  ggplot(aes(x = year, y = gwg_median, color = treatment)) +
  geom_point(size = 1, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.7) +
  # geom_text_repel(
  #   data = df_did |> filter(year == 2003),  # Label only the last year
  #   aes(label = country),
  #   hjust = 1, nudge_x = 0.5, size = 3
  # ) +
  labs(title = "X") +
  theme_minimal()

?geom_smooth


# finding the amount of missing values in the gdp variable

df_did |> 
  summarise(missing_gdp = sum(is.na(gdp)))

# Creating a new variable to indicate the treatment year individually for each country


df_did <- df_did |>
  mutate(treatment_year = case_when(
    country == "Australia" ~ 2014,
    country == "Canada" ~ 2020,
    country == "South Korea" ~ 2003,
    country == "Germany" ~ 2008,
    country == "Japan" ~ 2012,
    country == "United Kingdom" ~ 2003,
    treatment == 0 ~ 0))

View(df_did)

# Creating a relative time variable to account for years before and after the treatment

df_did <- df_did |>
  mutate(relative_time = case_when(
    treatment == 1 ~ year - treatment_year,
    treatment == 0 ~ NA))

# Filtering the data to include only the years around the treatment year

df_did_sample <- df_did |>
  filter((relative_time >= -3 & relative_time <= 3) | is.na(relative_time)) |>
  filter(year > 1999)

# Staggered did analysis ------------------------------------------------------

df_did_sample$country <- as.numeric(as.factor(df_did_sample$country))


# locating the missing data in the sample df

df_did_sample |> 
  summarise(missing_gdp = sum(is.na(year)))



att_gt_results <- att_gt(
  yname = "gwg_median",           
  tname = "year",              
  idname = "country",          
  gname = "treatment_year",
  data = df_did_sample,
  panel = FALSE,
  control_group = "notyettreated"
)

ggdid(att_gt_results)

twfe_model <- feols(gwg_median ~ i(relative_time, treatment, ref = -1) | country + year, data = df_did_sample)
summary(twfe_model)

iplot(twfe_model)

table(df_did_sample$treatment_year)

colSums(is.na(df_did))

?att_gt

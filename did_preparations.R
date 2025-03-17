library(tidyr)
library(did)
library(readxl)
library(ggplot2)
library(ggrepel)

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

# generic gender equality index

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
  filter(country %in% c("Australia", "Canada", "South Korea", "Germany", "Japan", "United Kingdom",
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

# Adding the values to the main dataframe

df_did <- df_did |>
  left_join(gdp, by = c("country", "year"))

# Comparing treatment and control conditions -----------------------------------

# testing parallel trends assumption

summary_did <- df_did |> 
  filter(year < 2012) |>
  group_by(treatment) |>
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
  filter(year < 2012) |>
  ggplot(aes(x = year, y = gwg_median, color = treatment, group = country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
  geom_text_repel(
    data = df_did |> filter(year == 2011),  # Label only the last year
    aes(label = country),
    hjust = 1, nudge_x = 0.5, size = 3
  ) +
  labs(title = "X") +
  theme_minimal()



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

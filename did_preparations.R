library(tidyr)
library(dplyr)
library(did)
library(readr)
library(readxl)
library(ggplot2)
library(ggrepel)
library(fixest)
library(showtext)


# Enable showtext
showtext_auto()

# Add a Google Font 
font_add_google("Lato", "lato")

# Reading the data to R -------------------------------------------------------

df <- read_rds("data/combined_clean.rds")

# Data preparation for the statistical analysis, 14 days cut-off  --------------------------------------

# Selecting the countries of interest

df_analysis<- df |>
  filter(country %in% c("Australia", "Canada", "South Korea", "Germany", "Japan", "United Kingdom",
                        "New Zealand", "United States", "Israel", "Slovak Republic", "Hungary"))


# Creating a new variable to indicate treatment and control

df_analysis <- df_analysis |>
  mutate(treatment = ifelse(country %in% c("Australia", "Canada", "South Korea", "Germany", "Japan", "United Kingdom"), 1, 0))

df_analysis <- df_analysis |>
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

df_analysis <- df_analysis |>
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

df_analysis <- df_analysis |>
  left_join(gdp, by = c("country", "year"))

# adding the treatment year

df_analysis <- df_analysis |>
  mutate(treatment_year = case_when(
    country == "Australia" ~ 2014,
    country == "Canada" ~ 2020,
    country == "South Korea" ~ 2003,
    country == "Germany" ~ 2008,
    country == "Japan" ~ 2012,
    country == "United Kingdom" ~ 2003,
    treatment == 0 ~ 0))

# creating variables for event time relative to treatment year and a dummy for post-treatment years

df_analysis <- df_analysis |>
  mutate(
    event_time = ifelse(treatment_year > 0, year - treatment_year, treatment_year),
    treated_post = ifelse(year >= treatment_year & treatment_year > 0, 1, 0)
  )

write_rds(df_analysis, "data/df_analysis.rds")

# Comparing treatment and control conditions -----------------------------------

# Summary statistics

summary_stats <- df_analysis |> 
  group_by(country) |>
  summarise(
    mean_gdp = mean(gdp, na.rm = TRUE))

# testing parallel trends assumption, from 1995 to first treatment year

summary_did <- df_analysis |> 
  filter(year >= 1995, year < 2003) |>
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

# gini index only available for Canada for the early years. Groups pretty equal in 

# Plotting the parallel trends assumption

df_analysis |>
  filter(year >= 1995, year < 2003) |>
  ggplot(aes(x = year, y = gwg_median, colour = treatment)) +
  geom_point(size = 1, alpha = 0.8) +
  geom_smooth(method = "lm") +
  scale_colour_paletteer_d("lisa::BridgetRiley") +
  labs(title = NULL,
       x = NULL,
       y = "Median gender wage gap (%)",
       colour = "Treatment condition") +
  theme_minimal(base_family = "lato", base_size = 30)+
  theme(legend.position = "bottom")


df_analysis |>
  filter(year >= 1995, year < 2003) |>
  ggplot(aes(x = year, y = gwg_d1, colour = treatment)) +
  geom_point(size = 1, alpha = 0.8) +
  geom_smooth(method = "lm") +
  scale_colour_paletteer_d("lisa::BridgetRiley") +
  labs(title = NULL,
       x = NULL,
       y = "1st decile gender wage gap (%)",
       colour = "Treatment condition") +
  theme_minimal(base_family = "lato", base_size = 30)+
  theme(legend.position = "bottom")

df_analysis |>
  filter(year >= 1995, year < 2003) |>
  ggplot(aes(x = year, y = gwg_d9, colour = treatment)) +
  geom_point(size = 1, alpha = 0.8) +
  geom_smooth(method = "lm") +
  scale_colour_paletteer_d("lisa::BridgetRiley") +
  labs(title = NULL,
       x = NULL,
       y = "Median gender wage gap (%)",
       colour = "Treatment condition") +
  theme_minimal(base_family = "lato", base_size = 30)+
  theme(legend.position = "bottom")

# Running the TWFE analysis ---------------------------------------------------

df_analysis <- df_analysis |>
  filter(event_time >= -5)


model <- feols(gwg_median ~ treated_post | country + year, data = df_analysis, cluster = "country")
summary(model)

# event study

model_event <- feols(gwg_median ~ i(event_time, ref = -1) | country + year, data = df_analysis, cluster = "country")
summary(model_event)
iplot(model_event)

# Two way fixed effects model --------------------------------------------------

df_did_sample <- df_did |>
  filter(year > 1995)

twfe_event_study <- feols(gwg_median ~ i(event_time, ref = -1) | country + year, data = df_did_sample)
summary(twfe_event_study)

iplot(twfe_event_study)


# Callaway & Sant Anna Staggered DID -------------------------------------------

df_did_sample$country <- as.numeric(as.factor(df_did_sample$country))

att_gt_est <- att_gt(yname = "gwg_median",  # Outcome variable
                     tname = "year",             # Time variable
                     idname = "country",         # Unit ID (Country)
                     gname = "treatment_year",   # First year of treatment
                     data = df_did_sample,
                     control_group="notyettreated"
                     )
summary(att_gt_est)

agg_effects <- aggte(att_gt_est, type = "group", na.rm = TRUE)
summary(agg_effects)

ggdid(att_gt_est)








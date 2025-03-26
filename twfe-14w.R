library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(fixest)
library(showtext)
library(ggfixest)


# Enable showtext
showtext_auto()

# Add a Google Font 
font_add_google("Lato", "lato")

# Reading the data to R -------------------------------------------------------

df <- read_rds("data/combined.rds")

# Data preparation ------------------------------------------------------------

# Filtering the data to only include the countries that are in the treatment and control groups

df_analysis<- df |>
  filter(country %in% c("Belgium", "France", "South Korea", "Norway", "Japan", "Sweden",
                        "New Zealand", "United States", "Israel", "Slovak Republic", "Hungary"))


# Creating a new variable to indicate treatment and control

df_analysis <- df_analysis |>
  mutate(treatment = ifelse(country %in% c("Belgium", "France", "South Korea", "Norway", "Japan", "Sweden"), 1, 0))

df_analysis <- df_analysis |>
  mutate(treatment = as.factor(treatment))

# adding the treatment year

df_analysis <- df_analysis |>
  mutate(treatment_year = case_when(
    country == "Belgium" ~ 2004,
    country == "France" ~ 2016,
    country == "South Korea" ~ 2003,
    country == "Norway" ~ 2015,
    country == "Japan" ~ 2012,
    country == "Sweden" ~ 2017,
    treatment == 0 ~ 0))

# creating variables for event time relative to treatment year and a dummy for post-treatment years

df_analysis <- df_analysis |>
  mutate(
    event_time = ifelse(treatment_year > 0, year - treatment_year, treatment_year),
    treated_post = ifelse(year >= treatment_year & treatment_year > 0, 1, 0))

df_analysis <- df_analysis |>
  filter(event_time >= -7)

# Summary statistics ----------------------------------------------------------

summary_stats <- df_analysis |> 
  filter(year >= 1995, year < 2003) |>
  group_by(treatment) |>
  summarize(
    mean_gdp = mean(gdp, na.rm = TRUE),
    mean_gender_equality = mean(equality_index, na.rm = TRUE),
    mean_gini = mean(gini, na.rm = TRUE),
    mean_lf_participation = mean(lf_participation, na.rm = TRUE),
    mean_wage_gap = mean(gwg_median, na.rm = TRUE),
    mean_d1 = mean(gwg_d1, na.rm = TRUE),
    mean_d9 = mean(gwg_d9, na.rm = TRUE)
  )

View(summary_stats)

# Event studies ---------------------------------------

# Event study: median

event_median_14w <- feols(gwg_median ~ i(event_time, ref = -1) | country + year, data = df_analysis, cluster = "country")
events_median <-  summary(event_median_14w)

# plotting the results

ggiplot(event_median_14w) +
  xlab("Years from treatment") +
  labs(title = "Event study without controls") +
  theme_minimal(base_family = "lato", base_size = 30)

# Event study: 1st decile

event_d1_14w <- feols(gwg_d1 ~ i(event_time, ref = -1) | country + year, data = df_analysis, cluster = "country")
events_d1 <-  summary(event_d1_14w)

# plotting the results

ggiplot(event_d1_14w) +
  xlab("Years from treatment") +
  labs(title = "Event study without controls") +
  theme_minimal(base_family = "lato", base_size = 30)


# Event study: 9th decile

event_d9_14w <- feols(gwg_d9 ~ i(event_time, ref = -1) | country + year, data = df_analysis, cluster = "country")
events_d9 <-  summary(event_d9_14w)

# plotting the results

ggiplot(event_d9_14w) +
  xlab("Years from treatment") +
  labs(title = "Event study without controls") +
  theme_minimal(base_family = "lato", base_size = 30)


# TWFE with covariates --------------------------------------------------------

# median

twfe_median <- feols(gwg_median ~ treated_post + equality_index + gdp + gini + lf_participation | country, 
                     data = df_analysis, cluster = "country")
summary(twfe_median)

# 1st decile

twfe_d1 <- feols(gwg_d1 ~ treated_post + equality_index + gdp + gini + lf_participation | country, 
                 data = df_analysis, cluster = "country")
summary(twfe_d1)


# 9th decile

twfe_d9 <- feols(gwg_d9 ~ treated_post + equality_index + gdp + gini + lf_participation | country, 
                 data = df_analysis, cluster = "country")
summary(twfe_d9)

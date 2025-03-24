library(tidyr)
library(dplyr)
library(did)
library(readr)
library(readxl)
library(ggplot2)
library(ggrepel)
library(fixest)
library(showtext)
library(ggfixest)


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

# Running the TWFE analysis, 14 day cut-off  ---------------------------------------------------

df_analysis <- df_analysis |>
  filter(event_time >= -5)

# Median

model <- feols(gwg_median ~ treated_post | country + year, data = df_analysis, cluster = "country")
feols1 <- summary(model) # not significant, p <.1

# with controls

model_controls <- feols(gwg_median ~ treated_post + equality_index + gdp + gini | country + year, data = df_analysis, cluster = "country")
feols1_controls <- summary(model_controls) # equality and gini significant only at p<.1

# d1

model_d1 <- feols(gwg_d1 ~ treated_post | country + year, data = df_analysis, cluster = "country")
summary(model_d1) # significant at p<..05 level!

# with controls

model_d1_controls <- feols(gwg_d1 ~ treated_post + equality_index + gdp + gini | country + year, data = df_analysis, cluster = "country")
summary(model_d1_controls) # gini significant at p<.05 level, no other significance

# d9

model_d9 <- feols(gwg_d9 ~ treated_post | country + year, data = df_analysis, cluster = "country")
summary(model_d9) # not significant

# with controls

model_d9_controls <- feols(gwg_d9 ~ treated_post + equality_index + gdp + gini | country + year, data = df_analysis, cluster = "country")
summary(model_d9_controls) # no significance


# Event studies ----------------------------------------------------------------

# Median

model_event <- feols(gwg_median ~ i(event_time, ref = -1) | country + year, data = df_analysis, cluster = "country")
events1 <-  summary(model_event)


model_nocontrols <- ggiplot(model_event) +
  xlab("Years from treatment") +
  labs(title = "Event study without controls") +
  theme_minimal(base_family = "lato", base_size = 30)

# with controls

model_event_controls <- feols(gwg_median ~ i(event_time, ref = -1) + equality_index + gdp + gini | country + year, data = df_analysis, cluster = "country")
events_controls <-  summary(model_event_controls)

ggiplot(model_event_controls, pt.join = TRUE) +
  xlab("Years from treatment") +
  theme_minimal(base_family = "lato", base_size = 30)


# d1

model_event_d1 <- feols(gwg_d1 ~ i(event_time, ref = -1) | country + year, data = df_analysis, cluster = "country")
summary(model_event_d1)

# with controls

model_event_d1_controls <- feols(gwg_d1 ~ i(event_time, ref = -1) + equality_index + gdp + gini | country + year, data = df_analysis, cluster = "country")
summary(model_event_d1_controls)  # gini gains significance

# d9

model_event_d9 <- feols(gwg_d9 ~ i(event_time, ref = -1) | country + year, data = df_analysis, cluster = "country")
summary(model_event_d9)

# with controls

model_event_d9_controls <- feols(gwg_d9 ~ i(event_time, ref = -1) + equality_index + gdp + gini | country + year, data = df_analysis, cluster = "country")
summary(model_event_d9_controls) # parallel assumptions doesnt hold

# TWFE, 14 week cut-off ---------------------------------------------------

# Creating a new variable to indicate treatment and control for 14-week cut-off







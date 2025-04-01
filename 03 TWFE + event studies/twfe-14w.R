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
    country == "Norway" ~ 2020,
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


# Event studies ---------------------------------------

# Event study: median

event_median_14w <- feols(gwg_median ~ i(event_time, ref = -1) | country + year, data = df_analysis, cluster = "country")
events_median <-  summary(event_median_14w)

# plotting the results

p1 <- ggiplot(event_median_14w, geom_style = 'ribbon', pt.pch = NA, col = '#2BAA92FF') +
  xlab("Years since policy change") +
  ylab("Estimate") +
  labs(title = "(i) Effect on median gender wage gap") +
  theme_minimal(base_family = "lato", base_size = 30)

# Event study: 1st decile

event_d1_14w <- feols(gwg_d1 ~ i(event_time, ref = -1) | country + year, data = df_analysis, cluster = "country")
events_d1 <-  summary(event_d1_14w)

# plotting the results

p2 <- ggiplot(event_d1_14w, geom_style = 'ribbon', pt.pch = NA, col = '#2BAA92FF') +
  xlab("Years since policy change") +
  ylab("Estimate") +
  labs(title = "(ii) Effect on gender wage gap, 1st decile") +
  theme_minimal(base_family = "lato", base_size = 30)


# Event study: 9th decile

event_d9_14w <- feols(gwg_d9 ~ i(event_time, ref = -1) | country + year, data = df_analysis, cluster = "country")
events_d9 <-  summary(event_d9_14w)

# plotting the results

p3 <- ggiplot(event_d9_14w, geom_style = 'ribbon', pt.pch = NA, col = '#2BAA92FF') +
  xlab("Years since policy change") +
  ylab("Estimate") +
  labs(title = "(iii) Effect on gender wage gap, 9th decile") +
  theme_minimal(base_family = "lato", base_size = 30)

# Combining the plots

event_plots_14w <- p1 + p2 + p3 +
  plot_layout(ncol = 1, axes = "collect")

ggsave("plots/event_plots_14w.png", event_plots_14w, width = 5, height = 8, dpi = 300)


# TWFE with covariates, fixed for country or country + year --------------------------------------------------------

# median - fixed for country

twfe_median_c <- feols(gwg_median ~ treated_post + equality_index + gdp + gini + lf_participation | country,
                       data = df_analysis, cluster = "country")


# fixed for year + country

twfe_median_y <- feols(gwg_median ~ treated_post + equality_index + gdp + gini + lf_participation | country + year, 
                     data = df_analysis, cluster = "country")


# 1st decile, fixed for country 

twfe_d1_c <- feols(gwg_d1 ~ treated_post + equality_index + gdp + gini + lf_participation | country, 
                 data = df_analysis, cluster = "country")

# fixed for year + country

twfe_d1_y <- feols(gwg_d1 ~ treated_post + equality_index + gdp + gini + lf_participation | country + year, 
                 data = df_analysis, cluster = "country")


# 9th decile, fixed for country

twfe_d9_c <- feols(gwg_d9 ~ treated_post + equality_index + gdp + gini + lf_participation | country, 
                 data = df_analysis, cluster = "country")

# fixed for year + country

twfe_d9_y <- feols(gwg_d9 ~ treated_post + equality_index + gdp + gini + lf_participation | country + year, 
                 data = df_analysis, cluster = "country")


# Exporting the results --------------------------------------------------------

combined_table <- etable(twfe_median_c, twfe_median_y, twfe_d1_c, twfe_d1_y, twfe_d9_c, twfe_d9_y,
                         digits = 3)


write_excel_csv(combined_table, "tables/results_14w.csv")

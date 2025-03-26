library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(fixest)
library(showtext)
library(ggfixest)
library(patchwork)


# Enable showtext
showtext_auto()

# Add a Google Font 
font_add_google("Lato", "lato")

# Reading the data to R -------------------------------------------------------

df <- read_rds("data/combined.rds")

# Data preparation ------------------------------------------------------------

# Filtering the data to only include the countries that are in the treatment and control groups

df_analysis<- df |>
  filter(country %in% c("Australia", "Canada", "South Korea", "Germany", "Japan", "United Kingdom",
                        "New Zealand", "United States", "Israel", "Slovak Republic", "Hungary"))


# Creating a new variable to indicate treatment and control

df_analysis <- df_analysis |>
  mutate(treatment = ifelse(country %in% c("Australia", "Canada", "South Korea", "Germany", "Japan", "United Kingdom"), 1, 0))

df_analysis <- df_analysis |>
  mutate(treatment = as.factor(treatment))

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

event_median_14d <- feols(gwg_median ~ i(event_time, ref = -1) | country + year, data = df_analysis, cluster = "country")

event_median_14d_sa <- feols(gwg_median ~ sunab(treatment_year, year) | country + year, data = df_analysis, cluster = "country")
summary(event_median_14d_sa)


ggiplot(
  list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
  main = 'Staggered treatment', ref.line = -1, pt.join = TRUE
)
events_median <-  summary(event_median_14d)

# plotting the results

p1 <- ggiplot(event_median_14d, geom_style = 'ribbon', pt.pch = NA, col = '#2BAA92FF') +
  xlab("Years since policy change") +
  ylab("Estimate") +
  labs(title = "(i) Effect on median gender wage gap") +
  theme_minimal(base_family = "lato", base_size = 30)

ggiplot(
  list('TWFE' = event_median_14d, 'Sun & Abraham (2020)' = event_median_14d_sa),
  main = 'Staggered treatment', ref.line = -1, pt.join = TRUE
)

# Event study: 1st decile

event_d1_14d <- feols(gwg_d1 ~ i(event_time, ref = -1) | country + year, data = df_analysis, cluster = "country")
events_d1 <-  summary(event_d1_14d)

# plotting the results

p2 <- ggiplot(event_d1_14d, geom_style = 'ribbon', pt.pch = NA, col = '#2BAA92FF') +
  xlab("Years since policy change") +
  ylab("Estimate") +
  labs(title = "(ii) Effect on gender wage gap, 1st decile") +
  theme_minimal(base_family = "lato", base_size = 30)


# Event study: 9th decile

event_d9_14d <- feols(gwg_d9 ~ i(event_time, ref = -1) | country + year, data = df_analysis, cluster = "country")
events_d9 <-  summary(event_d9_14d)

# plotting the results

p3 <- ggiplot(event_d9_14d, geom_style = 'ribbon', pt.pch = NA, col = '#2BAA92FF') +
  xlab("Years since policy change") +
  ylab("Estimate") +
  labs(title = "(iii) Effect on gender wage gap, 9th decile") +
  theme_minimal(base_family = "lato", base_size = 30)

event_plots_14d <- p1 + p2 + p3 +
  plot_layout(ncol = 1, axes = "collect")

ggsave("plots/event_plots_14d.png", event_plots_14d, width = 5, height = 8, dpi = 300)



# TWFE with covariates --------------------------------------------------------

# median

twfe_median <- feols(gwg_median ~ treated_post + equality_index + gdp + gini + lf_participation | country + year, 
      data = df_analysis, cluster = "country")
summary_median <- esttable(twfe_median)

write_excel_csv(summary_median, "twfe_median_14d.csv")



# 1st decile

twfe_d1 <- feols(gwg_d1 ~ treated_post + equality_index + gdp + gini + lf_participation | country + year, 
      data = df_analysis, cluster = "country")
summary_d1 <- etable(twfe_d1)

?etable

write_excel_csv(summary_d1, "twfe_d1_14d.csv")

# 9th decile

twfe_d9 <- feols(gwg_d9 ~ treated_post + equality_index + gdp + gini + lf_participation | country + year, 
      data = df_analysis, cluster = "country")
summary_d9 <- etable(twfe_d9)

write_excel_csv(summary_d9, "twfe_d9_14d.csv")


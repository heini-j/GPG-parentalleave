library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(showtext)
library(patchwork)

# Enable showtext
showtext_auto()

# Add a Google Font 
font_add_google("Lato", "lato")

# Reading data to R ---------------------------------------------------------------

df <- read_rds("data/combined.rds")

# Creating a separate df for the plots and summary statistics -----------------------

# sample countries

df_plots <- df |>
  filter(country %in% c("Australia", "Belgium", "France", "Canada", "South Korea", 
                        "Germany", "Japan", "Norway", "Sweden", "United Kingdom",
                        "New Zealand", "United States", "Israel", "Slovak Republic", "Hungary"))

# treatment 1

df_plots <- df_plots |>
  mutate(treatment1 = case_when(country %in% c("Australia", "Canada", "South Korea", "Germany", "Japan", "United Kingdom") ~ 1,
                                country %in% c("New Zealand", "United States", "Israel", "Slovak Republic", "Hungary") ~ 0))

# treatment 2

df_plots <- df_plots |>
  mutate(treatment2 = case_when(
    country %in% c("Belgium", "France", "South Korea", "Norway", "Japan", "Sweden") ~ 1,
    country %in% c("New Zealand", "United States", "Israel", "Slovak Republic", "Hungary") ~ 0))

# Plots for descriptive statistics, covariates ---------------------------------------------------------------

# gdp

gdp1 <- df_plots |>
  filter(year >= 1995) |>
  filter(treatment1 == 1 | treatment1 == 0) |>
  ggplot(aes(x = year, y = log(gdp), color = as.factor(treatment1), group = country)) +
  geom_line() +
  geom_text_repel(data = df_plots |> 
                  filter(treatment1 == 1 | treatment1 == 0) |>
                  filter(year == 2023),
                  aes(label = country),
                  family = "lato",
                  size = 8,
                  max.overlaps = Inf,
                  nudge_x = 2,
                  box.padding = 1,
                  direction = "y")+
  labs(title = "GDP over time",
       x = "Year",
       y = "log of GDP",
       color = "Treatment group") +
  scale_color_manual(values = c("1" = "#d3295e", 
                                "0" = "#2BAA92FF")) +
  theme_minimal(base_family = "lato", base_size = 30)

gdp2 <- df_plots |>
  filter(year >= 1995) |>
  filter(treatment2 == 1 | treatment2 == 0) |>
  ggplot(aes(x = year, y = log(gdp), color = as.factor(treatment2), group = country)) +
  geom_line() +
  geom_text_repel(data = df_plots |> 
                    filter(treatment2 == 1 | treatment2 == 0) |>
                    filter(year == 2023),
                  aes(label = country),
                  family = "lato",
                  size = 8,
                  max.overlaps = Inf,
                  nudge_x = 2,
                  box.padding = 1,
                  direction = "y")+
  labs(title = "GDP over time, condition 2",
       x = "Year",
       y = "log of GDP",
       color = "Treatment group") +
  scale_color_manual(values = c("1" = "#d3295e", 
                                "0" = "#2BAA92FF")) +
  theme_minimal(base_family = "lato", base_size = 30)

# combining to one plot

gdp_both <- gdp1 + gdp2 +
  plot_layout(guides = "collect", axes = "collect") &
  theme(legend.position = "bottom")

# saving the plot

ggsave("plots/gdp_both.png", gdp_both, width = 10, height = 7, dpi = 300)

# Gini coefficient

gini1 <- df_plots |>
  filter(year >= 2002) |>
  filter(treatment1 == 1 | treatment1 == 0) |>
  ggplot(aes(x = year, y = gini, color = as.factor(treatment1), group = country)) +
  geom_line() +
  geom_text_repel(data = df_plots |> 
                    drop_na(gini) |>
                  filter(treatment1 == 1 | treatment1 == 0) |>
                  filter(year == 2020),
                  aes(label = country),
                  family = "lato",
                  size = 8,
                  max.overlaps = Inf,
                  nudge_x = 2,
                  box.padding = 1,
                  direction = "y")+
  labs(title = "Income equality, condition 1",
       x = "Year",
       y = "Gini coefficient",
       color = "Treatment group") +
  scale_color_manual(values = c("1" = "#d3295e", 
                                "0" = "#2BAA92FF")) +
  theme_minimal(base_family = "lato", base_size = 30)


gini2 <- df_plots |>
  filter(year >= 2002) |>
  filter(treatment2 == 1 | treatment2 == 0) |>
  ggplot(aes(x = year, y = gini, color = as.factor(treatment2), group = country)) +
  geom_line() +
  geom_text_repel(data = df_plots |> 
                    filter(treatment2 == 1 | treatment2 == 0) |>
                    filter(year == 2020),
                  aes(label = country),
                  family = "lato",
                  size = 8,
                  max.overlaps = Inf,
                  nudge_x = 2,
                  box.padding = 1,
                  direction = "y")+
  labs(title = "Income equality, condition 2",
       x = "Year",
       y = "Gini coefficient",
       color = "Treatment group") +
  scale_color_manual(values = c("1" = "#d3295e", 
                                "0" = "#2BAA92FF")) +
  theme_minimal(base_family = "lato", base_size = 30)

gini_both <- gini1 + gini2 +
  plot_layout(guides = "collect", axes = "collect") &
  theme(legend.position = "bottom")

ggsave("plots/gini_both.png", gini_both, width = 10, height = 7, dpi = 300)

# women's labour force participation 

lf_participation1 <- df_plots |>
  filter(year >= 1995) |>
  filter(treatment1 == 1 | treatment1 == 0) |>
  ggplot(aes(x = year, y = lf_participation, color = as.factor(treatment1), group = country)) +
  geom_line() +
  geom_text_repel(data = df_plots |> 
                    filter(treatment1 == 1 | treatment1 == 0) |>
                    filter(year == 2023),
                  aes(label = country),
                  family = "lato",
                  size = 8,
                  max.overlaps = Inf,
                  nudge_x = 2,
                  box.padding = 1,
                  direction = "y")+
  labs(title = "Women's labour force participation, condition 1",
       x = "Year",
       y = "% of women",
       color = "Treatment group") +
  scale_y_continuous(limits = c(40, 70))+
  scale_color_manual(values = c("1" = "#d3295e", 
                                "0" = "#2BAA92FF")) +
  theme_minimal(base_family = "lato", base_size = 30)


lf_participation2 <- df_plots |>
  filter(year >= 1995) |>
  filter(treatment2 == 1 | treatment2 == 0) |>
  ggplot(aes(x = year, y = lf_participation, color = as.factor(treatment2), group = country)) +
  geom_line() +
  geom_text_repel(data = df_plots |> 
                    filter(treatment2 == 1 | treatment2 == 0) |>
                    filter(year == 2023),
                  aes(label = country),
                  family = "lato",
                  size = 8,
                  max.overlaps = Inf,
                  nudge_x = 2,
                  box.padding = 1,
                  direction = "y")+
  labs(title = "Women's labour force participation, condition 2",
       x = "Year",
       y = "% of women",
       color = "Treatment group")  +
  scale_y_continuous(limits = c(40, 70))+
  scale_color_manual(values = c("1" = "#d3295e", 
                                "0" = "#2BAA92FF")) +
  theme_minimal(base_family = "lato", base_size = 30)

lf_both <- lf_participation1 + lf_participation2 +
  plot_layout(guides = "collect", axes = "collect") &
  theme(legend.position = "bottom")

ggsave("plots/lf_both.png", lf_both, width = 10, height = 7, dpi = 300)

# Gender equality index

equality1 <- df_plots |>
  filter(year >= 1995) |>
  filter(treatment1 == 1 | treatment1 == 0) |>
  ggplot(aes(x = year, y = equality_index, color = as.factor(treatment1), group = country)) +
  geom_line() +
  geom_text_repel(data = df_plots |> 
                    filter(treatment1 == 1 | treatment1 == 0) |>
                    filter(year == 2023),
                  aes(label = country),
                  family = "lato",
                  size = 8,
                  max.overlaps = Inf,
                  nudge_x = 2,
                  box.padding = 1,
                  direction = "y")+
  labs(title = "Gender equality, condition 1",
       x = "Year",
       y = "WBL Index",
       color = "Treatment group") +
  scale_color_manual(values = c("1" = "#d3295e", 
                                "0" = "#2BAA92FF")) +
  theme_minimal(base_family = "lato", base_size = 30) +
  theme(legend.position = "bottom")


equality2 <- df_plots |>
  filter(year >= 1995) |>
  filter(treatment2 == 1 | treatment2 == 0) |>
  ggplot(aes(x = year, y = equality_index, color = as.factor(treatment2), group = country)) +
  geom_line() +
  geom_text_repel(data = df_plots |> 
                    filter(treatment2 == 1 | treatment2 == 0) |>
                    filter(year == 2023),
                  aes(label = country),
                  family = "lato",
                  size = 8,
                  max.overlaps = Inf,
                  nudge_x = 2,
                  box.padding = 1,
                  direction = "y")+
  labs(title = "Gender equality, condition 2",
       x = "Year",
       y = "WBL Index",
       color = "Treatment group")  +
  scale_color_manual(values = c("1" = "#d3295e", 
                                "0" = "#2BAA92FF")) +
  theme_minimal(base_family = "lato", base_size = 30)+
  theme(legend.position = "bottom")

equality_both <- (equality1 + equality2) +
  plot_layout(guides = "collect", axes = "collect") &
  theme(legend.position = "bottom")

ggsave("plots/equality_both.png", equality_both, width = 10, height = 7, dpi = 300)

# Dependent variables ---------------------------------------------------------------

# Median GWG

gwg1 <- df_plots |>
  filter(year >= 1995) |>
  filter(treatment1 == 1 | treatment1 == 0) |>
  ggplot(aes(x = year, y = gwg_median, color = as.factor(treatment1), group = country)) +
  geom_line() +
  geom_text_repel(data = df_plots |> 
                    filter(treatment1 == 1 | treatment1 == 0) |>
                    filter(year == 2023),
                  aes(label = country),
                  family = "lato",
                  size = 8,
                  max.overlaps = Inf,
                  nudge_x = 2,
                  box.padding = 1,
                  direction = "y")+
  labs(title = "GWG, median income, condition 1",
       x = "Year",
       y = "Gender wage gap, %",
       color = "Treatment group") +
  scale_color_manual(values = c("1" = "#d3295e", 
                                "0" = "#2BAA92FF")) +
  theme_minimal(base_family = "lato", base_size = 30)+
  theme(legend.position = "bottom")


gwg2 <- df_plots |>
  filter(year >= 1995) |>
  filter(treatment2 == 1 | treatment2 == 0) |>
  ggplot(aes(x = year, y = gwg_median, color = as.factor(treatment2), group = country)) +
  geom_line() +
  geom_text_repel(data = df_plots |> 
                    filter(treatment2 == 1 | treatment2 == 0) |>
                    filter(year == 2022),
                  aes(label = country),
                  family = "lato",
                  size = 8,
                  max.overlaps = Inf,
                  nudge_x = 2,
                  box.padding = 1,
                  direction = "y")+
  labs(title = "GWG, median income, condition 2",
       x = "Year",
       y = "Gender wage gap, %",
       color = "Treatment group")  +
  scale_color_manual(values = c("1" = "#d3295e", 
                                "0" = "#2BAA92FF")) +
  theme_minimal(base_family = "lato", base_size = 30) +
  theme(legend.position = "bottom")

gwg_both <- gwg1 + gwg2 +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("plots/gwg_both.png", gwg_both, width = 10, height = 7, dpi = 300)

# D1

d1_1 <- df_plots |>
  filter(year >= 1995) |>
  filter(treatment1 == 1 | treatment1 == 0) |>
  ggplot(aes(x = year, y = gwg_d1, color = as.factor(treatment1), group = country)) +
  geom_line() +
  geom_text_repel(data = df_plots |> 
                    filter(treatment1 == 1 | treatment1 == 0) |>
                    filter(year == 2023),
                  aes(label = country),
                  family = "lato",
                  size = 8,
                  max.overlaps = Inf,
                  nudge_x = 2,
                  box.padding = 1,
                  direction = "y")+
  labs(title = "GWG, D1 income, condition 1",
       x = "Year",
       y = "Gender wage gap, %",
       color = "Treatment group") +
  scale_color_manual(values = c("1" = "#d3295e", 
                                "0" = "#2BAA92FF")) +
  theme_minimal(base_family = "lato", base_size = 30)+
  theme(legend.position = "bottom")


d1_2 <- df_plots |>
  filter(year >= 1995) |>
  filter(treatment2 == 1 | treatment2 == 0) |>
  ggplot(aes(x = year, y = gwg_d1, color = as.factor(treatment2), group = country)) +
  geom_line() +
  geom_text_repel(data = df_plots |> 
                    filter(treatment2 == 1 | treatment2 == 0) |>
                    filter(year == 2022),
                  aes(label = country),
                  family = "lato",
                  size = 8,
                  max.overlaps = Inf,
                  nudge_x = 2,
                  box.padding = 1,
                  direction = "y")+
  labs(title = "GWG, D1 income, condition 2",
       x = "Year",
       y = "Gender wage gap, %",
       color = "Treatment group")  +
  scale_color_manual(values = c("1" = "#d3295e", 
                                "0" = "#2BAA92FF")) +
  theme_minimal(base_family = "lato", base_size = 30)+
  theme(legend.position = "bottom")

d1_both <- (d1_1 + d1_2) +
  plot_layout(guides = "collect", axes = "collect") &
  theme(legend.position = "bottom")

ggsave("plots/d1_both.png", d1_both, width = 10, height = 7, dpi = 300)

# D9

d9_1 <- df_plots |>
  filter(year >= 1995) |>
  filter(treatment1 == 1 | treatment1 == 0) |>
  ggplot(aes(x = year, y = gwg_d9, color = as.factor(treatment1), group = country)) +
  geom_line() +
  geom_text_repel(data = df_plots |> 
                    filter(treatment1 == 1 | treatment1 == 0) |>
                    filter(year == 2022),
                  aes(label = country),
                  family = "lato",
                  size = 8,
                  max.overlaps = Inf,
                  nudge_x = 2,
                  box.padding = 1,
                  direction = "y")+
  labs(title = "GWG, D9 income, condition 1",
       x = "Year",
       y = "Gender wage gap, %",
       color = "Treatment group") +
  scale_color_manual(values = c("1" = "#d3295e", 
                                "0" = "#2BAA92FF")) +
  scale_y_continuous(limits = c(0, 50)) +
  theme_minimal(base_family = "lato", base_size = 30)+
  theme(legend.position = "bottom")


d9_2 <- df_plots |>
  filter(year >= 1995) |>
  filter(treatment2 == 1 | treatment2 == 0) |>
  ggplot(aes(x = year, y = gwg_d9, color = as.factor(treatment2), group = country)) +
  geom_line() +
  geom_text_repel(data = df_plots |> 
                    filter(treatment2 == 1 | treatment2 == 0) |>
                    filter(year == 2022),
                  aes(label = country),
                  family = "lato",
                  size = 8,
                  max.overlaps = Inf,
                  nudge_x = 2,
                  box.padding = 1,
                  direction = "y")+
  labs(title = "GWG, D9 income, condition 2",
       x = "Year",
       y = "Gender wage gap, %",
       color = "Treatment group")  +
  scale_color_manual(values = c("1" = "#d3295e", 
                                "0" = "#2BAA92")) +
  scale_y_continuous(limits = c(0, 50)) +
  theme_minimal(base_family = "lato", base_size = 30) +
  theme(legend.position = "bottom")

d9_both <- (d9_1 + d9_2) +
  plot_layout(guides = "collect", axes = "collect") &
  theme(legend.position = "bottom")

ggsave("plots/d9_both.png", d9_both, width = 10, height = 7, dpi = 300)


# Summary statistics table ---------------------------------------------------------------

summary_stats_1 <- df_plots |> 
  filter(year >= 1995, year < 2003) |>
  filter(treatment1 == 1 | treatment1 == 0) |>
  group_by(treatment1) |>
  summarise(
    "Median GWG" = mean(gwg_median, na.rm = TRUE),
    "sd1" = sd(gwg_median, na.rm = TRUE),
    "D1 GWG" = mean(gwg_d1, na.rm = TRUE),
    "sd2" = sd(gwg_d1, na.rm = TRUE),
    "D9 GWG" = mean(gwg_d9, na.rm = TRUE),
    "sd3" = sd(gwg_d9, na.rm = TRUE),
    "GDP" = mean(gdp),
    "sd4" = sd(gdp),
    "Gender equality index" = mean(equality_index),
    "sd5" = sd(equality_index),
    "Gini coefficient" = mean(gini, na.rm = TRUE),
    "sd6" = sd(gini, na.rm = TRUE),
    "Women's labour force participation" = mean(lf_participation),
    "sd7" = sd(lf_participation)
  ) |>
  rename("Treatment" = treatment1)

View(summary_stats_1)

summary_stats_2 <- df_plots |> 
  filter(year >= 1995, year < 2003) |>
  filter(treatment2 == 1) |>
  group_by(treatment2) |>
  summarise(
    "Median GWG" = mean(gwg_median, na.rm = TRUE),
    "sd1" = sd(gwg_median, na.rm = TRUE),
    "D1 GWG" = mean(gwg_d1, na.rm = TRUE),
    "sd2" = sd(gwg_d1, na.rm = TRUE),
    "D9 GWG" = mean(gwg_d9, na.rm = TRUE),
    "sd3" = sd(gwg_d9, na.rm = TRUE),
    "GDP" = mean(gdp),
    "sd4" = sd(gdp),
    "Gender equality index" = mean(equality_index),
    "sd5" = sd(equality_index),
    "Gini coefficient" = mean(gini, na.rm = TRUE),
    "sd6" = sd(gini, na.rm = TRUE),
    "Women's labour force participation" = mean(lf_participation),
    "sd7" = sd(lf_participation)
  ) |>
  rename("Treatment" = treatment2)

# Combine the two tables

summary_stats <- rbind(summary_stats_1, summary_stats_2)

# Exporting the table

write_excel_csv(summary_stats, "tables/summary_stats.csv")

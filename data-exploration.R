library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)

# Reading the data -----------------------------------------------------------

df <- read_rds("data/combined.rds")

# Exploratory data analysis ---------------------------------------------------------

# plotting the average gwg median value for the sample countries

p_median <- df |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 1, alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Median",
       x = NULL,
       y = "Gender pay gap (%)") +
  theme_minimal()

p_d1 <- df |>
  ggplot(aes(x = year, y = gwg_d1)) +
  geom_point(size = 1, alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "1st decile",
       x = NULL,
       y = "Gender pay gap (%)") +
  theme_minimal()

p_d9 <- df |>
  ggplot(aes(x = year, y = gwg_d9)) +
  geom_point(size = 1, alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "9th decile",
       x = NULL,
       y = "Gender pay gap (%)") +
  theme_minimal()


p_median + p_d1 + p_d9 +
  plot_annotation(title = 'Gender pay gap in the sample countries') +
  plot_layout(axes = "collect")


# Finding a fitting sample of countries ----------------------------------------

# Which countries fulfill the conditions: cut-off at >14 days of leave, with minimum 3-year follow-up

country_list <- df |> 
  group_by(country) |> 
  filter(any(year == 2020 & paternity_total >= 14)) |>
  summarise(n = n_distinct(country))

country_list$country

# 27 countries out of 47 fulfill this condition

# Plotting the paternity leave lengths in all the 47 countries for grouping
selection_countries <- df |>
  ggplot(aes(x = year, y = paternity_total, color = country)) +
  geom_line() +
  facet_wrap(~country, scales = "free_y") +
  geom_text_repel(data = df_combined |> filter(year == 2023), 
                  aes(label = paternity_total), 
                  hjust = 0, # Position labels slightly to the right
                  nudge_x = 0.2, # Move labels slightly away from the last point
                  direction = "y", # Avoid overlapping
                  size = 2,
                  max.overlaps = 15) +
  scale_x_continuous(breaks = seq(1971, 2023, by = 5)) +
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

df <- df |>
  mutate(paternityleave_group = case_when(
    country %in% c("Australia",  "Bulgaria", "Canada", "Cyprus", "Germany", "Ireland", "Italy", "Japan", "Lithuania", "Poland", "Luxembourg", "South Korea", "United Kingdom") ~ "Increase from zero",
    country %in% c("Belgium", "Croatia", "Denmark", "Estonia", "Finland", "France", "Iceland", "Norway", "Portugal", "Romania", "Slovenia", "Spain", "Sweden") ~ "Multiple increases", 
    country %in% c("Argentina", "Brazil", "Chile", "Colombia", "Czechia", "Greece", "Hungary", "Latvia", "Malta", "Mexico", "Netherlands", "Peru", "Türkiye") ~ "Constant low",
    country %in% c("Austria", "Costa Rica", "India", "Israel", "New Zealand", "Slovak Republic", "Switzerland" , "United States") ~ "Constant zero")) 

# Adding numerical values to the groups

df <- df |>
  mutate(paternityleave_group = factor(paternityleave_group, levels = c("Constant zero", "Constant low", "Increase from zero", "Multiple increases")))

# Plotting the paternity leave lengths for the selected countries

length_by_group <- df |>
  filter(year <= 2020) |>
  ggplot(aes(x = year, y = paternity_total, color = country)) +
  geom_line() +
  facet_wrap(~paternityleave_group, scales = "free_y") +
  geom_hline(yintercept = 14, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "black") +
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

# Missing variables -----------------------------------------------------------

# Checking the column wise missing values in the combined dataset

df_missing <- df |>
  summarise_all(~sum(is.na(.)))

View(df_missing)

# A lot of missing values in the GWG columns -> analysing for systematic errors

df_missing <- df |>
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

df_missing_year <- df |>
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

df <- df |>
  filter(!country %in% c("Türkiye", "Luxembourg", "Argentina", "India", "Croatia", "Bulgaria", "Romania"))


# Saving the cleaned data -----------------------------------------------------

write_rds(df, "data/combined_clean.rds")


# Summary plots ----------------------------------------------------------------

# plotting the median pay gap by year

# Making a scatter plot that shows the relationship between paternity leave and maternity leave

ggplot(df, aes(x = paternity_total, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Relationship between paternity leave and GPG",
       x = "Paternity leave",
       y = "Median pay gap") +
  theme_minimal()

# for different groups of countries

gpg_per_year <- ggplot(df, aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.7) +
  facet_grid(~paternityleave_group, scales = "free_x") +
  labs(title = "Median gender pay gap",
       x = "Year",
       y = "Gender pay gap (%)") +
  theme_minimal()

# plotting the 1st decile pay gap by year

gpg_d1 <- ggplot(df, aes(x = year, y = gwg_d1)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.7) +
  facet_grid(~paternityleave_group, scales = "free_x") +
  labs(title = "D1 gender pay gap",
       x = "Year",
       y = "Gender pay gap (%)") +
  theme_minimal()

# plotting the 9th decile pay gap by year

gpg_d9 <- ggplot(df, aes(x = year, y = gwg_d9)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.7) +
  facet_grid(~paternityleave_group, scales = "free_x") +
  labs(title = "D9 gender pay gap",
       x = "Year",
       y = "Gender pay gap (%)") +
  theme_minimal()

# Saving the plot

ggsave("plots/median_pay_gap_by_year.png", gpg_per_year)


# Relationship between maternity and paternity leaves -> not a very good visualisation

ggplot(df, aes(x = paternity_total, y = maternity_total)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  facet_wrap(~paternityleave_group, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between paternity leave and GPG",
       x = "Paternity leave",
       y = "Median pay gap") +
  theme_minimal()

library(readr)
library(ggplot2)

# Loading the data -----------------------------------------------------------

df <- read_rds("data/combined.rds")


# Plotting -------------------------------------------------------------------

# Making a scatter plot that shows the relationship between paternity leave and maternity leave

ggplot(df, aes(x = paternity_total, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  labs(title = "Relationship between paternity leave and GPG",
       x = "Paternity leave",
       y = "Median pay gap") +
  theme_minimal()

# relationship between paternity and maternity leaves

df <- df |>
  mutate(maternity_total = maternityleave_length + shared_length_mother)

ggplot(df, aes(x = paternity_total, y = maternity_total)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  facet_wrap(~paternityleave_group, scales = "free_y") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between paternity leave and GPG",
       x = "Paternity leave",
       y = "Median pay gap") +
  theme_minimal()

# paternityleave and median pay gap with a linear regression line

ggplot(df, aes(x = paternityleave_length, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between paternity leave and GPG",
       x = "Paternity leave",
       y = "Median pay gap") +
  theme_minimal()

# promising as line is negative, however, the relationship doesnt seem very clear

# plotting the median pay gap by year

gpg_per_year <- ggplot(df, aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.7) +
  facet_wrap(~paternityleave_group, scales = "free") +
  labs(title = "Median pay gap by year",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()


# Saving the plot

ggsave("plots/median_pay_gap_by_year.png", gpg_per_year)

# test-case: australia - 14 days of paternity leave in 2014. Plotting the development of the gwg variables per year

df |>
  filter(country == "Australia") |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Australia",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

df |>
  filter(country == "Germany") |>
  filter(year > 1990) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Germany",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# no data on italy!!

df |>
  filter(country == "Italy") |>
  # filter(year > 1990) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2001, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Italy",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

df |>
  filter(country == "Japan") |>
  filter(year > 1975) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Japan",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

df |>
  filter(country == "Poland") |>
  filter(year > 1990) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Poland",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

df |>
  filter(country == "Slovenia") |>
  filter(year > 1990) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2003, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Slovenia",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# not enough data for slovenia :(

df |>
  filter(country == "Portugal") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Portugal",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()
)

# quite few data points for portugal

df |>
  filter(country == "Spain") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Poland",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# also for spain maybe too few data points

df |>
  filter(country == "United Kingdom") |>
  filter(year > 1990) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2003, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in UK",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

df |>
  filter(country == "Sweden") |>
  filter(year > 1995) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 1996, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Sweden",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# for sweden, enough data only for the last change in 2017

df |>
  filter(country == "Denmark") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  #geom_vline(xintercept = 1986, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Denmark",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Data available for denmark for change in 2017

df |>
  filter(country == "Norway") |>
  filter(year > 1995) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  #geom_vline(xintercept = 1996, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Norway",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

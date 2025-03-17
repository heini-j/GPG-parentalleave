library(readr)
library(ggplot2)

# Loading the data -----------------------------------------------------------

df <- read_rds("data/combined.rds")

df |>
  group_by(country)|>
  select(paternityleave_group) |>
  filter(paternityleave_group == "High with increase")|>
  print(country)


# Plotting -------------------------------------------------------------------

# Making a scatter plot that shows the relationship between paternity leave and maternity leave

ggplot(df, aes(x = paternity_total, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
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
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Relationship between paternity leave and GPG",
       x = "Paternity leave",
       y = "Median pay gap") +
  theme_minimal()

# promising as line is negative, however, the relationship doesnt seem very clear

# plotting the median pay gap by year

gpg_per_year <- ggplot(df, aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.7) +
  facet_grid(~paternityleave_group, shrink = TRUE) +
  labs(title = "Median pay gap by year",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

?facet_grid

# Saving the plot

ggsave("plots/median_pay_gap_by_year.png", gpg_per_year)

# Country-wise analysis ------------------------------------------------------

# Zero to >= 14 days ---------------------------------------------------------

# AUstralia

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

# Canada

df |>
  filter(country == "Canada") |>
  filter(year > 1995) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Canada",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Cyprus

df |>
  filter(country == "Cyprus") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Cyprus",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Denmark

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

# Estonia


df |>
  filter(country == "Estonia") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  #geom_vline(xintercept = 2003, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Estonia",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Germany

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

# Ireland

df |>
  filter(country == "Ireland") |>
  filter(year > 1990) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Ireland",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Italy

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

# Japan

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

# Lithuania

df |>
  filter(country == "Lithuania") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Lithuania",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Poland

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

# UK

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

# Prior high, with increase --------------------------------------------------

# Belgium

df |>
  filter(country == "Belgium") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  #geom_vline(xintercept = 1999, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Belgium",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Finland

df |>
  filter(country == "Finland") |>
  filter(year > 1975) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  #geom_vline(xintercept = 1979, linetype = "dashed", color = "black") + # 14 days
  geom_vline(xintercept = 1994, linetype = "dashed", color = "black") + # 21 days
  geom_vline(xintercept = 2004, linetype = "dashed", color = "black") + # 35 days
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black") + # 76 days
  labs(title = "Median pay gap in Finland",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# France

df |>
  filter(country == "France") |>
  filter(year > 1990) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  # geom_vline(xintercept = 2003, linetype = "dashed", color = "black") + # 15 days
  geom_vline(xintercept = 2016, linetype = "dashed", color = "black") + # added 180 of shared days
  labs(title = "Median pay gap in France",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Iceland


df |>
  filter(country == "Iceland") |>
  filter(year > 1990) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 1999, linetype = "dashed", color = "black") + # 14 days
  geom_vline(xintercept = 2004, linetype = "dashed", color = "black") + # 90 days
  labs(title = "Median pay gap in Iceland",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Norway

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

# Portugal

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

# Slovenia

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

# Spain

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

# Sweden

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


# Looking for control group --------------------------------------------------

# Constant zero group --------------------------------------------------------

# Austria


# compared to Germany?

df |>
  filter(country == "Austria") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Austria",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Switzerland

df |>
  filter(country == "Switzerland") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Switzerland",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# compared to canada

df |>
  filter(country == "United States") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in US",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# compared to australia?

df |>
  filter(country == "New Zealand") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in NZ",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()


# Netherlands

df |>
  filter(country =="Netherlands") |>
  filter(year > 2015) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  #geom_vline(xintercept = 2003, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Netherlands",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Israel

df |>
  filter(country =="Israel") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2003, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Israel",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Costa Rica

df |>
  filter(country =="Costa Rica") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Costa Rica",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Slovak

df |>
  filter(country =="Slovak Republic") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in SR",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()


# Brazil

df |>
  filter(country =="Brazil") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Brazil",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Chile

df |>
  filter(country =="Chile") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Chile",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Colombia

df |>
  filter(country =="Colombia") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Colombia",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Czechia

df |>
  filter(country =="Czechia") |>
  filter(year > 1999) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Czech Republic",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Greece

df |>
  filter(country =="Greece") |>
  filter(year > 2000) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Greece",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Hungary

df |>
  filter(country =="Hungary") |>
  filter(year > 1990) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Hungary",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Latvia

df |>
  filter(country =="Latvia") |>
  filter(year > 2010) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Latvia",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Malta

df |>
  filter(country =="Malta") |>
  filter(year > 2010) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Malta",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Mexico

df |>
  filter(country =="Mexico") |>
  filter(year > 1990) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Mexico",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Netherlands

df |>
  filter(country =="Netherlands") |>
  filter(year > 2015) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Netherlands",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

# Peru

df |>
  filter(country =="Peru") |>
  filter(year > 2010) |>
  ggplot(aes(x = year, y = gwg_median)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_point(aes(y = gwg_d1), color = "red") +
  geom_point(aes(y = gwg_d9), color = "blue") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(title = "Median pay gap in Peru",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()

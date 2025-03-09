library(readr)
library(ggplot2)

# Loading the data -----------------------------------------------------------

df <- read_rds("data/combined.rds")


# Plotting -------------------------------------------------------------------

# Making a scatter plot that shows the relationship between paternity leave and maternity leave

ggplot(df, aes(x = paternityleave_length, y = MEDIAN)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  labs(title = "Relationship between paternity leave and GPG",
       x = "Paternity leave",
       y = "Median pay gap") +
  theme_minimal()



ggplot(df, aes(x = paternityleave_length, y = maternityleave_length)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  facet_wrap(~paternityleave_group, scales = "free_y") +
  labs(title = "Relationship between paternity leave and GPG",
       x = "Paternity leave",
       y = "Median pay gap") +
  theme_minimal()


# Adding a trend line to the plot

ggplot(df, aes(x = paternityleave_length, y = MEDIAN)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between paternity leave and GPG",
       x = "Paternity leave",
       y = "Median pay gap") +
  theme_minimal()

# plotting the median pay gap by year

ggplot(df, aes(x = year, y = MEDIAN)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.7) +
  facet_wrap(~paternityleave_group, scales = "free_y") +
  labs(title = "Median pay gap by year",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()


ggplot(df, aes(x = year, y = MEDIAN)) +
  geom_point(size = 0.7, position = "jitter", alpha = 0.3) +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.7) +
  facet_wrap(~paternityleave_group, scales = "free_y") +
  labs(title = "Median pay gap by year",
       x = "Year",
       y = "Median pay gap") +
  theme_minimal()
?geom_smooth

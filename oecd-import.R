library(readr)
library(dplyr)

# Load the data to R ---------------------------------------------------------

df_gwg <- read_csv("OECD.ELS.SAE,DSD_EARNINGS@GENDER_WAGE_GAP,+all.csv")

View(df_gwg)

names(df_gwg)

# Cleaning the data for easier use ------------------------------------------

# selecting the columns to be used
df_gwg <- df_gwg |>
  select(REF_AREA, "Reference area", "AGGREGATION_OPERATION", "TIME_PERIOD", "OBS_VALUE")

# renaming to match with the wbl dataset

df_gwg_clean <- df_gwg |>
  rename(
    country = "Reference area",
    country_code = "REF_AREA",
    year = "TIME_PERIOD",
    gwg = "OBS_VALUE",
    gwg_type = "AGGREGATION_OPERATION")


View(df_gwg_clean)

# Saving the cleaned data --------------------------------------------------

write_rds(df_gwg_clean, "data/gwg_clean.rds")

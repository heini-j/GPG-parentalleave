library(readxl)
library(readr)
library(dplyr)


# Importing from excel file -----------------------------------------------

# Importing the right sheet from the excel file and specifying the column types

df_parental_leave <- read_excel("WBL2024-1-0-Historical-Panel-Data.xlsx", 
                                sheet = "Parenthood",
                                col_types = c("text", "text", "text", "text", "text", 
                                              "numeric", "numeric", "text", "logical", "text",
                                              "numeric", "text", "logical", "text", "text",
                                              "logical", "text", "numeric", "text", "logical",
                                              "text", "numeric", "numeric", "numeric","text",
                                              "logical", "text"))

# checking the names of the columns
names(df_parental_leave)

# checking the first rows of the data

df_parental_leave |>
  head()

# checking the dataframe

df_parental_leave |>
  View()

# Renaming columns ----------------------------------------------------------------

# Although clear, some columns have names that are too long for practical use, so they are renamed
df_parental_leave_clean <- df_parental_leave |>
  rename(
    country = "Economy",
    country_code = "Economy Code",
    ISO_code = "ISO Code",
    region = "Region",
    income_group = "Income Group",
    year = "Report Year",
    parenthood_score = "PARENTHOOD SCORE",
    maternityleave_minimum = "Is paid leave of at least 14 weeks available to mothers?",
    maternityleave_log = "Score...9",
    maternity_law = "Legal Basis...10",
    maternityleave_length = "Length of paid maternity leave",
    maternityleave_benefits = "Does the government administer 100 percent of maternity leave benefits?",
    maternityleave_benefits_log = "Score...13",
    maternitityleave_benefits_law = "Legal Basis...14",
    paternity_leave = "Is there paid leave available to fathers?",
    paternityleave_log = "Score...16",
    paternity_law = "Legal Basis...17",
    paternityleave_length = "Length of paid paternity leave",
    parentalleave = "Is there paid parental leave?",
    parentalleave_log = "Score...20",
    parentalleave_law = "Legal Basis...21",
    shared_length = "Shared days",
    shared_length_mother = "Days for the mother",
    shared_length_father = "Days for the father",
    dismissal_prohibited = "Is dismissal of pregnant workers prohibited?",
    dismissal_prohibited_log = "Score...26",
    dismissal_prohibited_law = "Legal Basis...27"
  )

View(df_parental_leave_clean)

# Improving readability -----------------------------------------------------------

# changing "false" and "true" to 0 and 1 for easier use in further analysis
df_parental_leave_clean <-
  df_parental_leave_clean |>
  mutate(
    maternityleave_log = as.numeric(maternityleave_log),
    maternityleave_benefits_log = as.numeric(maternityleave_benefits_log),
    paternityleave_log = as.numeric(paternityleave_log),
    parentalleave_log = as.numeric(parentalleave_log),
    dismissal_prohibited_log = as.numeric(dismissal_prohibited_log)
  )

# Removing unnecessary columns -----------------------------------------------------

df_wbl <- df_parental_leave_clean |>
  select(
    country,
    region,
    income_group,
    year,
    parenthood_score,
    maternityleave_log,
    maternityleave_length,
    maternityleave_benefits_log,
    paternityleave_log,
    paternityleave_length,
    parentalleave_log,
    shared_length,
    shared_length_mother,
    shared_length_father,
    dismissal_prohibited_log
  )

# Saving the cleaned data ----------------------------------------------------------

write_rds(df_wbl, "data/parental_leave.rds")

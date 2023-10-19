library(readxl)
library(dplyr)
library(ggplot2)
library(zscorer)

# test data  - bmi_data <- readxl::read_excel('test_data/test-data.xlsx')

add_bmiz <- function(data, wt, ht, age, wt_units = c('kg', 'lb'), ht_units = c('cm', 'm', 'in', 'ft'), sex = 2, dob = NULL, date_assessed = NULL) {
  df <- data %>%
    mutate(
      wt_kg = weight_in_kg(wt, wt_units),
      ht_cm = height_in_cm(ht, ht_units),
      bmi = wt_kg / (ht_cm / 100)^2,
      age_days = sapply(age, age_in_days, dob = dob, date_assessed= date_assessed),
      sex_1M2F = align_sex_coding(sex)$sex
    )

  # Add labels to columns
  attr(df$wt_kg, "label") <- "Weight (kg)"
  attr(df$ht_cm, "label") <- "Height (cm)"
  attr(df$bmi, "label") <- "BMI"
  attr(df$age_days, "label") <- "Age (days)"
  attr(df$sex_1M2F, "label") <- "Sex (1 = Male, 2 = Female)"

  df <- addWGSR(data = df, sex = 'sex_1M2F', firstPart = 'wt_kg', secondPart = 'ht_cm', thirdPart = 'age_days', index = 'bfa', output = 'bmiAgeZ', digits = 2) %>%
    rename('bmiz' = bmiAgeZ)

  return(df)
}

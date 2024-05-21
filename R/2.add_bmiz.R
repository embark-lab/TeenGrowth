

#' Calculate BMI-Z scores for a dataset.
#'
#' This function takes a dataset and calculates BMI-Z scores.
#'
#' @param data A data frame containing weight, height, age, and sex information.
#' @param wt A vector representing weight.
#' @param ht A vector representing height.
#' @param age A vector representing age.
#' @param wt_units A vector of weight units ('kg' or 'lb').
#' @param ht_units A vector of height units ('cm', 'm', 'in', 'ft').
#' @param sex A vector representing sex (default set to 2 = female)
#' @param dob Date of birth (optional).
#' @param date_assessed Date of assessment (optional).
#'
#' @return A data frame with BMI-Z scores added as 'bmiz' column.
#'
#' @examples
#' data <- data.frame(wt = c(70, 65), ht = c(175, 160), age = c(25, 30), sex = c('Male', 'Female'))
#' add_bmiz(data, wt = "wt", ht = "ht", age = "age", wt_units = "kg", ht_units = "cm", dob = NULL, date_assessed = NULL)
#' @import dplyr
#' @import zscorer
#' @export

bmiz_zscorer <- function(data, wt, ht, age, wt_units = 'kg', ht_units = 'cm', sex = 2, dob = NULL, date_assessed = NULL) {
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


#' Calculate BMI-Z scores for a dataset.
#'
#' This function takes a dataset and calculates BMI-Z scores.
#'
#' @param data A data frame containing weight, height, age, and sex information.
#' @param wt A vector representing weight (optional).
#' @param ht A vector representing height (optional).
#' @param age A vector representing age.
#' @param bmi A vector representing BMI (optional).
#' @param wt_units A vector of weight units ('kg' or 'lb').
#' @param ht_units A vector of height units ('cm', 'm', 'in', 'ft').
#' @param sex A vector representing sex (default set to 2 = female).
#' @param dob Date of birth (optional).
#' @param date_assessed Date of assessment (optional).
#' @param data_source Data source ('cdc' or 'wgsr') - Default to 'cdc'
#' @return A data frame with BMI-Z scores added as 'bmiz' column.
#'
#' @examples
#' data <- data.frame(wt = c(70, 65), ht = c(175, 160), age = c(25, 30), sex = c('Male', 'Female'))
#' calculate_bmiz(data, wt = "wt", ht = "ht", age = "age", wt_units = "kg", ht_units = "cm", dob = NULL, date_assessed = NULL)
#' calculate_bmiz(data, bmi = "bmi", age = "age", dob = NULL, date_assessed = NULL)
#' @import dplyr
#' @export

add_bmiz <- function(data, wt = NULL, ht = NULL, age, bmi = NULL, wt_units = 'kg', data_source = 'cdc', ht_units = 'cm', sex = 2, dob = NULL, date_assessed = NULL) {
  df <- data %>%
    mutate(
      wt_kg = if (!is.null(wt)) weight_in_kg(wt, wt_units) else NULL,
      ht_cm = if (!is.null(ht)) height_in_cm(ht, ht_units) else NULL,
      bmi = if (!is.null(bmi)) bmi else if (!is.null(wt) & !is.null(ht)) wt_kg / (ht_cm / 100)^2 else NULL,
      age_days = sapply(age, age_in_days, dob = dob, date_assessed = date_assessed),
      sex_1M2F = align_sex_coding(sex)$sex
    )

  # Add labels to columns
  attr(df$wt_kg, "label") <- "Weight (kg)"
  attr(df$ht_cm, "label") <- "Height (cm)"
  attr(df$bmi, "label") <- "BMI"
  attr(df$age_days, "label") <- "Age (days)"
  attr(df$sex_1M2F, "label") <- "Sex (1 = Male, 2 = Female)"

  # Calculate BMI-Z scores using bmiz_lookup
  df <- df %>%
    rowwise() %>%
    mutate (bmiz = bmiz_lookup(bmi = bmi, age = {{age}}, sex = sex_1M2F, dob = {{dob}},data_source = {{data_source}}, date_assessed = {{date_assessed}})) |>
    ungroup()

  return(df)
}



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
#' @return A data frame with BMI-Z scores added as 'bmiz' column.
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
#' @param data_source Data source ('cdc' or 'wgsr') - Default to 'cdc'.
#' @return A data frame with BMI-Z scores added as the 'bmiz' column.
#' @import dplyr
#' @import distributional
#' @importFrom magrittr %>%
#' @export

add_bmiz <- function(data, wt = NULL, ht = NULL, age = NULL, bmi = NULL,
                     wt_units = 'kg', ht_units = 'cm', sex = 2,
                     dob = NULL, date_assessed = NULL, data_source = 'cdc') {
  # Check if age_days exists
  age_days_exists <- "age_days" %in% names(data)

  df <- data %>%
    mutate(
      # Convert weight to kg
      wt_kg = if (!is.null(wt)) weight_in_kg(.data[[wt]], wt_units) else NULL,
      # Convert height to cm
      ht_cm = if (!is.null(ht)) height_in_cm(.data[[ht]], ht_units) else NULL,
      # Calculate BMI if not provided
      bmi = if (!is.null(bmi)) .data[[bmi]] else if (!is.null(wt) && !is.null(ht)) wt_kg / (ht_cm / 100)^2 else NULL,
      # Calculate age in days
      age_days = if (age_days_exists) {
        .data[["age_days"]] # Use existing age_days column
      } else if (!is.null(dob) && !is.null(date_assessed)) {
        as.numeric(difftime(as.Date(.data[[date_assessed]]), as.Date(.data[[dob]]), units = 'days'))
      }  else if (!is.null(age)) {
        sapply(.data[[age]], function(x) {
          age_in_days(
            age = x)
        })
      }

      else {
        NA_real_
      },
      # Align sex coding
      sex_1M2F = align_sex_coding(.data[[sex]])$sex
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
    mutate (bmiz = bmiz_lookup(bmi = bmi,
                               age = age_days,
                               sex = sex_1M2F,
                               dob = {{dob}},
                               data_source = {{data_source}},
                               age_unit = 'days',
                               date_assessed = {{date_assessed}})) |>
    ungroup()

  return(df)
}

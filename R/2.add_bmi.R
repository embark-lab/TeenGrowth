#' Add BMI to Data Frame
#'
#' This function adds a BMI column to a data frame based on age, sex, and normalized BMI (BMI percentile or BMI z-score).
#'
#' @param data A data frame containing the necessary columns.
#' @param age A column in `data` representing the age of individuals.
#' @param normed_bmi A column in `data` representing the normalized BMI (either BMI percentile or BMI z-score).
#' @param data_source A string specifying the data source, either 'cdc' or 'wgsr'. Default is 'cdc'.
#' @param type A string specifying the type of normalized BMI, either 'bmiz' for BMI z-score or 'pct' for BMI percentile. Default is 'bmiz'.
#' @param sex A column in `data` representing the sex of individuals, where 1 = Male and 2 = Female. Default is 2 (Female).
#' @param dob (Optional) A column in `data` representing the date of birth.
#' @param date_assessed (Optional) A column in `data` representing the date of assessment.
#' @param age_unit (Optional) A string specifying the unit of the age column.
#' @import dplyr
#' @return A data frame with an added BMI column.
#' @export


add_bmi <- function(data,
                    age,
                    normed_bmi,
                    data_source = 'cdc',
                    type = 'bmiz',
                    sex = 2,
                    dob = NULL,
                    date_assessed = NULL,
                    age_unit = NULL) {

  df <- data %>%
    mutate(sex_1M2F = align_sex_coding(sex)$sex)

  # Calculate BMI for each row
  df <- df %>%
    rowwise() %>%
    mutate(bmi = bmi_lookup(
      data_point = {{normed_bmi}},
      age = {{age}},
      sex = sex_1M2F,
      type = {{type}},
      dob = {{dob}},
      date_assessed = {{date_assessed}},
      age_unit = {{age_unit}}
    )) %>%
    ungroup()

  # Add labels to columns
  attr(df$bmi, "label") <- "BMI"
  attr(df$sex_1M2F, "label") <- "Sex (1 = Male, 2 = Female)"

  return(df)
}



#' Add BMI to data frame
#'  @param data Data frame
#'  @param age Age column
#'  @param normed_bmi Normalized BMI column (either bmi percentile or bmiz score)
#'  @param data_source Data source ('cdc' or 'wgsr') - Default to 'cdc'
#'  @param type 'bmiz' or 'pct' - Default to 'bmiz'
#'  @param sex sex - 1 = Male; 2 = Female - Default to Female
#'  @param dob Date of birth column (optional)
#'  @param date_assessed Date assessed column (optional)
#'  @param age_unit Unit of age column (optional)
#'  @import dplyr
#'  @return data frame with BMI column added
#'  @export

add_bmi <- function(data, age, normed_bmi, data_source = 'cdc', type = 'bmiz', sex = 2, dob = NULL, date_assessed = NULL, age_unit = NULL) {
  df <- data %>%
    mutate(sex_1M2F = align_sex_coding(sex)$sex)

  # Calculate BMI for each row

  df <- df %>%
    rowwise() %>%
    mutate (bmi = bmi_lookup(data_point = {{normed_bmi}}, age = {{age}}, sex = sex_1M2F, type = {{type}}, dob = {{dob}}, date_assessed = {{date_assessed}}, age_unit = {{age_unit}})) |>
    ungroup()

  # Add labels to columns
  attr(df$bmi, "label") <- "BMI"
  attr(df$sex_1M2F, "label") <- "Sex (1 = Male, 2 = Female)"

  return(df)
}


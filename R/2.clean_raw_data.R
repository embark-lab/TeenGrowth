# create a function that intakes raw data and makes columns for :
# 1. id
# 2. agemos
# 3. sex
# 4. adult_height
# 5. bmi
# 6. bmiz

#' Clean BMI data
#' This function intakes raw data and creates columns for id, agemos, sex, adult height, BMI, and BMIZ. It handles various formats for sex and height/weight units, calculates age in months, and manages missing columns.
#'
#' @param data A data frame containing the raw data.
#' @param id_col_name The name of the column containing unique identifiers. Defaults to NULL.
#' @param age_col_name The name of the column containing age values. Defaults to NULL.
#' @param dob_col_name The name of the column containing date of birth. Defaults to NULL.
#' @param date_assessed_col_name The name of the column containing the date of assessment. Defaults to NULL.
#' @param age_unit The unit of age values ('days', 'months', or 'years'). Defaults to NULL.
#' @param sex_col_name The name of the column containing sex values. Defaults to NULL.
#' @param ht_col_name The name of the column containing height values. Defaults to NULL.
#' @param ht_unit The unit of height values ('cm' or 'inches'). Defaults to 'cm'.
#' @param wt_col_name The name of the column containing weight values. Defaults to NULL.
#' @param wt_unit The unit of weight values ('kg' or 'lbs'). Defaults to 'kg'.
#' @param bmi_col_name The name of the column containing BMI values. Defaults to NULL.
#' @param bmiz_col_name The name of the column containing BMIZ values. Defaults to NULL.
#' @param pct_col_name The name of the column containing percentile values. Defaults to NULL.
#' @param data_source The source of the data ('cdc' or other). Defaults to 'cdc'.
#' @param adult_height_col_name The name of the column containing adult height values. Defaults to NULL.
#'
#' @return A data frame with columns for id, agemos, sex, adult_height, bmi, and bmiz.
#'
#' @examples
#' raw_data <- data.frame(id = 1:5, sex = c("M", "F", "M", "F", "M"), age = c(16, 17, 18, 19, 20), height = c(160, 165, 170, 175, 180), weight = c(60, 65, 70, 75, 80))
#' clean_data(raw_data, id_col_name = "id", age_col_name = "age", sex_col_name = "sex", ht_col_name = "height", wt_col_name = "weight")
#'
#' @export
#'
clean_data <- function(data,
                           id_col_name = NULL,
                           age_col_name = NULL,
                           dob_col_name = NULL,
                           date_assessed_col_name = NULL,
                           age_unit = NULL,
                           sex_col_name = NULL,
                           ht_col_name = NULL,
                           ht_unit = 'cm',
                           wt_col_name = NULL,
                           wt_unit = 'kg',
                           bmi_col_name = NULL,
                           bmiz_col_name = NULL,
                           pct_col_name = NULL,
                           data_source = 'cdc',
                           adult_height_col_name = NULL,
                           ed_aoo_col_name = NULL) {
# if id column is NULL, create a new id column and make the ID = 1
  if (is.null(id_col_name)) {
    data$id <- 1
  } else {
    data$id <- data[[id_col_name]]
  }
  vectorized_age_in_months <- Vectorize(age_in_months, vectorize.args = c("age"))
  # if age column is NULL, create a new age column
  if (is.null(age_col_name) & is.null(dob_col_name) & is.null(date_assessed_col_name)) {
    stop('age column is required') }
  if (!is.null(dob_col_name) & !is.null(date_assessed_col_name)) {
    data$age_days <- as.numeric(lubridate::difftime(data[[date_assessed_col_name]], data[[dob_col_name]], units = 'days'))
    age_col_name <- 'age_days' }
  # Convert age column to months
  data <- data %>%
    mutate(agemos = vectorized_age_in_months(!!sym(age_col_name), age_unit = age_unit))
  # make an age column that is in years
  data$age_years <- data$agemos / 12

# create sex column and align
# if sex column is null - assign all sex to female and make a note of this
if (is.null(sex_col_name)) {
  data$sex <- 2
  sex_col_name <- 'sex'
  # add a message that sex is specified as female
  message("Sex column was not provided. All sex values have been set to female.")
}
data <- data |>
  mutate(sex = align_sex_coding(sex)[[sex_col_name]])

# manage the adult_height column. if adult height column is provided, use this column and rename it 'adult_height' -- if it is not provided and ht_col_name is provided, make adult height = the most recent height measurement (oldest age) IF the age is > 14 years for females (sex == 2) or 16 years for males (sex ==1)
# Handle adult height column

if (!is.null(adult_height_col_name)) {
  data <- data %>%
    mutate(adult_height = data[[adult_height_col_name]])
} else if (!is.null(ht_col_name)) {
  data <- data %>%
    group_by(id) %>%
    arrange(desc(agemos)) %>%
    mutate(adult_height = ifelse(
      (sex == 2 & agemos / 12 > 14) | (sex == 1 & agemos / 12 > 16),
      !!sym(ht_col_name),
      NA_real_
    )) %>%
    fill(adult_height, .direction = "down") %>%
    ungroup()
  message("Adult height was not provided -- Adult height has been set to the oldest height obtained after age 14 for girls and after age 16 for boys.")

}
else {
  # make the adult height column = NA
  data$adult_height_cm <- NA_real_
  data$adult_height_in <- NA_real_
  message("Neither height nor an adult height column were provided. Adult height has been set to NA.")
}
# make the adult height column in cm if it is not already
# if the adult height column exists -- make it in cm
data$adult_height_cm <- height_in_cm(data$adult_height, ht_unit)
# make an adult height in inches column for show - convert adult height to inches
data$adult_height_in <- data$adult_height_cm / 2.54

# manage the height and weight columns, if they exist -- make sure they are in cm and kg
# Handle height and weight columns
if (!is.null(ht_col_name)) {
  data$height_cm <- height_in_cm(data[[ht_col_name]], ht_unit)
  data$height_in <- data$height_cm / 2.54
}

# if ht_col_name doesn't exist, make height_cm = NA and height_in = NA
if (is.null(ht_col_name)) {
  data$height_cm <- NA_real_
  data$height_in <- NA_real_
  message("Height column was not provided. Height has been set to NA.")
}

if (!is.null(wt_col_name)) {
  data$weight_kg <- weight_in_kg(data[[wt_col_name]], wt_unit)
  data$weight_lb <- data$weight_kg / 0.45359237
}

# if wt_col_name doesn't exist, make weight_kg = NA and weight_lb = NA
if (is.null(wt_col_name)) {
  data$weight_kg <- NA_real_
  data$weight_lb <- NA_real_
  message("Weight column was not provided. Weight has been set to NA.")
}

# if bmi column is provided, use this column and rename it 'bmi' -- if it is not provided, calculate bmi from height and weight columns
# Handle BMI column
if (!is.null(bmi_col_name)) {
  data$bmi <- data[[bmi_col_name]]
} else if (!is.null(ht_col_name) & !is.null(wt_col_name)) {
  data$bmi <- calculate_bmi(weight = data$weight_kg,
                            weight_unit = 'kg',
                            height = data$height_cm,
                            height_unit = 'cm')
}
# if bmiz column is provided and data$bmi still doesn't exist -- use this column to with age and sex to back out to bmi values
  else if (!is.null(bmiz_col_name)) {
  # vectorize the bmi_lookup function
  vectorized_bmi_lookup <- Vectorize(bmi_lookup, vectorize.args = c("data_point", "age", "sex"))
  data$bmi <- vectorized_bmi_lookup(data_point = data[[bmiz_col_name]], age = data$agemos, sex = data$sex, type = 'bmiz', data_source = data_source)
}
  else if (!is.null(pct_col_name)) {
  # vectorize the bmi_lookup function
  vectorized_bmi_lookup <- Vectorize(bmi_lookup, vectorize.args = c("data_point", "age", "sex"))
  data$bmi <- vectorized_bmi_lookup(data_point = data[[pct_col_name]], age = data$agemos, sex =
          data$sex, type = 'pct', data_source = data_source)
  }
  else {
    # stop and return message that bmi, bmiz, height and weight, or pct columns are required
    stop('Not enough information to calculate bmi:
         one of the following is required: bmi, bmiz, percentiles, or height and weight.
         Please specify the column names in the function call if these columns do exist')
  }
# do the same thing for looking up bmiz
  if (!is.null(bmiz_col_name)) {
  data$bmiz <- data[[bmiz_col_name]]
}
  else {
  vectorized_bmiz_lookup <- Vectorize(bmiz_lookup, vectorize.args = c("bmi", "age", "sex"))
  data$bmiz <- vectorized_bmiz_lookup(bmi = data$bmi, age = data$agemos, sex = data$sex, data_source = data_source)
  }

# convert ed_aao to months if ed_aao_col_name is provided
if (!is.null(ed_aoo_col_name)) {
data <- data %>%
  mutate(ed_aoo = vectorized_age_in_months(!!sym(ed_aoo_col_name), age_unit = age_unit))
} else {
  data$ed_aoo <- NA_real_
  message("No eating disorder age of onset column was provided. Eating disorder age of onset has been set to NA -- can add this information later for plotting individuals")
}
# put all columns in the right order as noted at the top
data <- data %>% select (id,
                         sex,
                         age_years,
                         agemos,
                         height_in,
                         height_cm,
                         weight_lb,
                         weight_kg,
                         adult_height_in,
                         adult_height_cm,
                         bmi,
                         bmiz,
                         ed_aoo)
return(data)
}



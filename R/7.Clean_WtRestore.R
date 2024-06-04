

# Helper function to convert height to the desired unit
convert_height <- function(height, from_unit, to_unit) {
  if (is.null(height)) return(NA)
  if (from_unit == to_unit) return(height)
  if (from_unit == 'cm' && to_unit == 'in') return(height * 0.393701)
  if (from_unit == 'in' && to_unit == 'cm') return(height * 2.54)
  stop("Invalid height units")
}

# Helper function to convert weight to the desired unit
convert_weight <- function(weight, from_unit, to_unit) {
  if (is.null(weight)) return(NA)
  if (from_unit == to_unit) return(weight)
  if (from_unit == 'kg' && to_unit == 'lbs') return(weight * 2.20462)
  if (from_unit == 'lbs' && to_unit == 'kg') return(weight / 2.20462)
  stop("Invalid weight units")
}


#' Clean Treatment Data
#' @param filtered_data A data frame containing the filtered data for a single participant.
#' @param age_col_name The name of the column containing the age of the participant
#' @param date_assessed_col_name The name of the column containing the date the data was assessed
#' @param ht_col_name The name of the column containing the height of the participant
#' @param wt_col_name The name of the column containing the weight of the participant
#' @param age_unit The unit of the age column (e.g., years, months, days)
#' @param ht_unit The unit of the height column (e.g., cm, in)
#' @param wt_unit The unit of the weight column (e.g., kg, lbs)
#' @param bmi_col_name The name of the column containing the BMI of the participant
#' @param bmiz_col_name The name of the column containing the BMI z-score of the participant
#' @param pct_col_name The name of the column containing the percentile of the participant
#' @param data_source The source of the data (e.g., cdc, who)
#' @param dob The date of birth of the participant
#' @param sex Sex
#' @param current_ht The current height of the participant
#' @param age_current_ht The age at which the current height was measured
#' @param adult_ht The adult height of the participant
#' @param age_adult_ht The age at which the adult height was measured
#' @param ed_aoo The age of onset of the eating disorder
#' @param tx_start_date The start date of the treatment
#' @return A cleaned and processed data frame with the age in years, expected
#' BMI, and expected weight, formatted appropriately.
#' @import dplyr
#' @import lubridate
#' @import rlang
#' @export


# Main function
tx_plot_clean <- function(filtered_data,
                          age_col_name = NULL,
                          date_assessed_col_name = NULL,
                          ht_col_name = NULL,
                          wt_col_name = NULL,
                          age_unit = NULL,
                          ht_unit = 'cm',
                          wt_unit = 'kg',
                          bmi_col_name = NULL,
                          bmiz_col_name = NULL,
                          pct_col_name = NULL,
                          data_source = 'cdc',
                          dob = NULL,
                          sex = NULL,
                          current_ht = NULL,
                          age_current_ht = NULL,
                          adult_ht = NULL,
                          age_adult_ht = NULL,
                          ed_aoo = NULL,
                          tx_start_date = NULL,
                          intake_wt = NULL) {

  # Ensure required inputs are provided
  if (is.null(dob)) stop("Date of birth is required. Please provide a date of birth")
  if (is.null(tx_start_date)) stop("Treatment start date is required. Please provide a treatment start date")

  # Convert dob and tx_start_date to date format
  dob <- ymd(dob)
  tx_start_date <- ymd(tx_start_date)
  sex <- sex

  # Calculate tx_start_age_days
  tx_start_age_days <- as.numeric(difftime(tx_start_date, dob, units = "days"))

  # Vectorize age_in_days function
  vectorized_age_in_days <- Vectorize(age_in_days, vectorize.args = c("age", "dob", "date_assessed"))

  # Create static characteristics list
  static_data <- list(
    dob = dob,
    sex = sex,
    tx_start_date = tx_start_date,
    tx_start_age_days = tx_start_age_days
  )

  # Calculate eating disorder age of onset date if provided
  if (!is.null(ed_aoo)) {
    ed_aoo_days <- vectorized_age_in_days(ed_aoo, dob = dob)
    static_data$ed_onset_date <- dob + days(as.integer(ed_aoo_days))
    static_data$ed_aoo_days <- ed_aoo_days
  }

  # Create dynamic data frame
  dynamic_data <- filtered_data

  # Manage age -- convert to days based on dob and age unit
  if (!is.null(age_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(age_days = vectorized_age_in_days(!!sym(age_col_name), age_unit = age_unit, dob = dob))
  }

  print(dynamic_data)

  # Manage date assessed -- convert to date and ensure correct format using lubridate
  if (!is.null(date_assessed_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(date_assessed = ymd(!!sym(date_assessed_col_name)),
             age_days = vectorized_age_in_days(dob = dob, date_assessed = ymd(!!sym(date_assessed_col_name))))
  } else if (!is.null(age_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(date_assessed = dob + days(as.integer(age_days)))
  }

  print(dynamic_data)

  # Convert height columns to both cm and in
  if (!is.null(ht_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(height_in = convert_height(!!sym(ht_col_name), ht_unit, 'in'),
             height_cm = convert_height(!!sym(ht_col_name), ht_unit, 'cm'))
  }
  print(dynamic_data)
  # if adult_ht is provided -- convert to in and store
  if (!is.null(adult_ht)) {
    static_data$adult_ht_in <- convert_height(adult_ht, ht_unit, 'in')
  }

  # Calculate adult height if not provided
  if (!is.null(ht_col_name) && is.null(adult_ht)) {
    adult_ht <- dynamic_data %>%
      filter(age_days >= ifelse(static_data$sex == 2, 14*365.25, 16*365.25)) %>%
      arrange(desc(age_days)) %>%
      slice(1) %>%
      pull(height_cm)

    if (length(adult_ht) == 0) adult_ht <- NA
    static_data$adult_ht_in <- convert_height(adult_ht, 'cm', 'in')
  }

  # Calculate age at adult height
  if (!is.null(age_adult_ht)) {
    static_data$age_adult_ht_days <- vectorized_age_in_days(age_adult_ht, dob = dob)
  } else if (!is.null(ht_col_name) && !is.null(adult_ht)) {
    age_adult_ht_days <- dynamic_data %>%
      filter(abs(height_cm - convert_height(static_data$adult_ht_in, 'in', 'cm')) <= 2) %>%
      summarize(min_age_days = min(age_days, na.rm = TRUE)) %>%
      pull(min_age_days)
    if (length(age_adult_ht_days) == 0) age_adult_ht_days <- Inf
    static_data$age_adult_ht_days <- age_adult_ht_days
  }

  # Calculate age at current height
  if (!is.null(age_current_ht)) {
    static_data$age_current_ht_days <- vectorized_age_in_days(age_current_ht, dob = dob)
  } else if (!is.null(ht_col_name) && !is.null(current_ht)) {
    age_current_ht_days <- dynamic_data %>%
      filter(abs(height_cm - convert_height(current_ht, ht_unit, 'cm')) <= 2) %>%
      summarize(min_age_days = min(age_days, na.rm = TRUE)) %>%
      pull(min_age_days)
    if (length(age_current_ht_days) == 0) age_current_ht_days <- Inf
    static_data$age_current_ht_days <- age_current_ht_days
  }

  # Determine the prediction height and type
  if (!is.null(static_data$adult_ht_in) && !is.na(static_data$adult_ht_in)) {
    static_data$prediction_ht_in <- static_data$adult_ht_in
    static_data$ht_type <- "adult"
  } else {
    static_data$prediction_ht_in <- static_data$current_ht_in
    static_data$ht_type <- "current"
  }

  # Debugging information
  print(paste("Calculated adult height (in):", static_data$adult_ht_in))
  print(paste("Age at adult height (days):", static_data$age_adult_ht_days))
  print(paste("Age at current height (days):", static_data$age_current_ht_days))
  print(paste("Prediction height (in):", static_data$prediction_ht_in))
  print(paste("Height type:", static_data$ht_type))

  # Filter data based on age at heights
  if (!is.null(static_data$age_adult_ht_days) && static_data$age_adult_ht_days != Inf) {
    dynamic_data <- dynamic_data %>%
      filter(age_days >= static_data$age_adult_ht_days)
  } else if (!is.null(static_data$age_current_ht_days) && static_data$age_current_ht_days != Inf) {
    dynamic_data <- dynamic_data %>%
      filter(age_days >= static_data$age_current_ht_days)
  }

  # If weight is provided, calculate weight in lbs and kgs from wt_col_name and wt_unit
  if (!is.null(wt_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(weight_lbs = convert_weight(!!sym(wt_col_name), wt_unit, 'lbs'),
             weight_kgs = convert_weight(!!sym(wt_col_name), wt_unit, 'kg'))
  }

  # If weight is not provided but BMI is, calculate weight in both lbs and kgs from BMI and prediction height
  else if (is.null(wt_col_name) && !is.null(bmi_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(weight_lbs = (!!sym(bmi_col_name)) * (static_data$prediction_ht_in^2) / 703,
             weight_kgs = (!!sym(bmi_col_name)) * (convert_height(static_data$prediction_ht_in, 'in', 'cm')^2) / 10000)
  }

  # If only BMIZ is provided, first lookup BMI based on age and sex
  else if (is.null(wt_col_name) && is.null(bmi_col_name) && !is.null(bmiz_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(agemos = age_days / 30.4375) # Convert days to months
    # Here should be the bmi_lookup part, assuming the bmi_lookup is a function
    # vectorized_bmi_lookup <- Vectorize(bmi_lookup, vectorize.args = c("data_point", "age", "sex"))
    dynamic_data <- dynamic_data %>%
      mutate(bmi = vectorized_bmi_lookup(data_point = !!sym(bmiz_col_name),
                                         age = agemos,
                                         sex = sex,
                                         type = 'bmiz',
                                         data_source = data_source)) %>%
      mutate(weight_lbs = bmi * (static_data$prediction_ht_in^2) / 703,
             weight_kgs = bmi * (convert_height(static_data$prediction_ht_in, 'in', 'cm')^2) / 10000)
  }

  # If only PCT is provided, first lookup BMI based on age and sex
  else if (is.null(wt_col_name) && is.null(bmi_col_name) && is.null(bmiz_col_name) && !is.null(pct_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(agemos = age_days / 30.4375) # Convert days to months
    # Here should be the bmi_lookup part, assuming the bmi_lookup is a function
    # vectorized_bmi_lookup <- Vectorize(bmi_lookup, vectorize.args = c("data_point", "age", "sex"))
    dynamic_data <- dynamic_data %>%
      mutate(bmi = vectorized_bmi_lookup(data_point = !!sym(pct_col_name),
                                         age = agemos,
                                         sex = sex,
                                         type = 'pct',
                                         data_source = data_source)) %>%
      mutate(weight_lbs = bmi * (static_data$prediction_ht_in^2) / 703,
             weight_kgs = bmi * (convert_height(static_data$prediction_ht_in, 'in', 'cm')^2) / 10000)
  }
  else {
    stop("No weight, bmi, bmiz, or pct data provided")
  }

  # Select and retain only the necessary columns in dynamic data
  dynamic_data <- dynamic_data %>%
    select(date_assessed, age_days, weight_lbs, weight_kgs)

  # If intake weight is provided, add it to the static data
  if (!is.null(intake_wt)) {
    static_data$intake_wt_lbs <- convert_weight(intake_wt, wt_unit, 'lbs')
    static_data$intake_wt_kgs <- convert_weight(intake_wt, wt_unit, 'kg')
  }

  # If intake weight is not provided, determine the weight that was obtained closest in time to the treatment start date
  else {
    closest_row <- dynamic_data %>%
      filter(abs(age_days - tx_start_age_days) == min(abs(age_days - tx_start_age_days))) %>%
      slice(1)
    static_data$intake_wt_lbs <- closest_row$weight_lbs
    static_data$intake_wt_kgs <- closest_row$weight_kgs

    # Leave a message if the intake weight is not provided that it is being estimated by the closest weight to the treatment start date
    message("Intake weight not provided. Estimating intake weight as the closest weight to the treatment start date.")
  }

  # Return a list containing both static data and dynamic data
  return(list(static_data = static_data, dynamic_data = dynamic_data))
}



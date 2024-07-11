

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
  if (from_unit == 'kg' && to_unit == 'lb') return(weight * 2.20462)
  if (from_unit == 'lb' && to_unit == 'kg') return(weight / 2.20462)
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
#' @param wt_unit The unit of the weight column (e.g., kg, lb)
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
#' @param intake_wt The intake weight of the participant
#' @return A cleaned and processed data frame with the age in years, expected
#' BMI, and expected weight, formatted appropriately.
#' @import dplyr
#' @import lubridate
#' @import rlang
#' @export

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

  if (is.null(dob)) stop("Date of birth is required. Please provide a date of birth")
  if (is.null(tx_start_date)) stop("Treatment start date is required. Please provide a treatment start date")

  dob <- ymd(dob)
  tx_start_date <- ymd(tx_start_date)

  tx_start_age_days <- as.numeric(difftime(tx_start_date, dob, units = "days"))

  vectorized_age_in_days <- Vectorize(age_in_days, vectorize.args = c("age", "dob", "date_assessed"))
  vectorized_bmi_lookup <- Vectorize(bmi_lookup, vectorize.args = c("data_point", "age", "sex"))

  static_data <- list(
    dob = dob,
    sex = sex,
    tx_start_date = tx_start_date,
    tx_start_age_days = tx_start_age_days
  )

  if (!is.null(ed_aoo)) {
    ed_aoo_days <- vectorized_age_in_days(ed_aoo, dob = dob)
    static_data$ed_onset_date <- dob + days(as.integer(ed_aoo_days))
    static_data$ed_aoo_days <- ed_aoo_days
  }

  dynamic_data <- filtered_data

  if (!is.null(age_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(age_days = vectorized_age_in_days(!!sym(age_col_name), age_unit = age_unit, dob = dob))
  }

  if (!is.null(date_assessed_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(date_assessed = ymd(!!sym(date_assessed_col_name)),
             age_days = vectorized_age_in_days(dob = dob, date_assessed = ymd(!!sym(date_assessed_col_name))))
  } else if (!is.null(age_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(date_assessed = dob + days(as.integer(age_days)))
  }

  if (!is.null(ht_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(height_in = convert_height(!!sym(ht_col_name), ht_unit, 'in'),
             height_cm = convert_height(!!sym(ht_col_name), ht_unit, 'cm'))
  }

  if (!is.null(adult_ht)) {
    static_data$adult_ht_in <- convert_height(adult_ht, ht_unit, 'in')
  }

  if (!is.null(ht_col_name) && is.null(adult_ht)) {
    adult_ht <- dynamic_data %>%
      filter(age_days >= ifelse(static_data$sex == 2, 14*365.25, 16*365.25)) %>%
      arrange(desc(age_days)) %>%
      slice(1) %>%
      pull(height_cm)

    if (length(adult_ht) == 0) adult_ht <- NA
    static_data$adult_ht_in <- convert_height(adult_ht, 'cm', 'in')
  }

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

  if (!is.null(static_data$adult_ht_in) && !is.na(static_data$adult_ht_in)) {
    static_data$prediction_ht_in <- static_data$adult_ht_in
    static_data$ht_type <- "adult"
  } else {
    static_data$prediction_ht_in <- static_data$current_ht_in
    static_data$ht_type <- "current"
  }

  if (!is.null(wt_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(weight_lbs = convert_weight(!!sym(wt_col_name), wt_unit, 'lb'),
             weight_kgs = convert_weight(!!sym(wt_col_name), wt_unit, 'kg'))
  } else if (is.null(wt_col_name) && !is.null(bmi_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(weight_lbs = (!!sym(bmi_col_name)) * (static_data$prediction_ht_in^2) / 703,
             weight_kgs = (!!sym(bmi_col_name)) * (convert_height(static_data$prediction_ht_in, 'in', 'cm')^2) / 10000)
  } else if (is.null(wt_col_name) && is.null(bmi_col_name) && !is.null(bmiz_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(agemos = age_days / 30.4375)

    dynamic_data$bmi <- vectorized_bmi_lookup(data_point = dynamic_data[[bmiz_col_name]], age = dynamic_data$agemos, sex = static_data$sex, type = 'bmiz', age_unit = 'months', data_source = 'cdc')

      dynamic_data <- dynamic_data %>%
      mutate(weight_lbs = bmi * (static_data$prediction_ht_in^2) / 703,
             weight_kgs = bmi * (convert_height(static_data$prediction_ht_in, 'in', 'cm')^2) / 10000)
  }
  else if (is.null(wt_col_name) && is.null(bmi_col_name) && is.null(bmiz_col_name) && !is.null(pct_col_name)) {
    dynamic_data <- dynamic_data %>%
      mutate(agemos = age_days / 30.4375) |>
      mutate(bmi = vectorized_bmi_lookup(data_point = !!sym(pct_col_name),
                                         age = agemos,
                                         age_unit = 'months',
                                         sex = sex,
                                         type = 'pct',
                                         data_source = data_source)) %>%
      mutate(weight_lbs = bmi * (static_data$prediction_ht_in^2) / 703,
             weight_kgs = bmi * (convert_height(static_data$prediction_ht_in, 'in', 'cm')^2) / 10000)
  } else {
    stop("No weight, bmi, bmiz, or pct data provided")
  }

  dynamic_data <- dynamic_data %>%
    select(date_assessed, age_days, weight_lbs, weight_kgs)

  if (!is.null(intake_wt)) {
    static_data$intake_wt_lbs <- convert_weight(intake_wt, wt_unit, 'lb')
    static_data$intake_wt_kgs <- convert_weight(intake_wt, wt_unit, 'kg')
  } else {
    closest_row <- dynamic_data %>%
      filter(abs(age_days - tx_start_age_days) == min(abs(age_days - tx_start_age_days), na.rm = TRUE)) %>%
      slice(1)
    static_data$intake_wt_lbs <- closest_row$weight_lbs
    static_data$intake_wt_kgs <- closest_row$weight_kgs

    message("Intake weight not provided. Estimating intake weight as the closest weight to the treatment start date.")
  }

  return(list(static_data = static_data, dynamic_data = dynamic_data))
}

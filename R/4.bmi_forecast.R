#' Cull Demographics Data
#'
#' Align participant, sex coding, and add adult height and sex columns, if available.
#'
#' @param data A data frame containing demographic information.
#' @param participant (Optional) Column name for participant IDs.
#' @param sex (Optional) Column name for sex.
#' @param adult_height (Optional) Column name for adult height.
#' @return A data frame with aligned demographics data.
#' @export
#'
cull_demos <- function(data, id = NULL, sex = NULL, adult_height = NULL) {
  if (!missing(id) && id %in% names(data)) {
    data <- data |> mutate(id = .data[[id]])
  } else if ('id' %in% names(data)) {
    data$id <- data$id
  } else {
    data <- data |> mutate(id = 1)
  }

  if (!missing(sex) && sex %in% names(data)) {
    data <- data |> mutate(sex = .data[[sex]])
  } else if ('sex' %in% names(data)) {
    data <- data
  } else {
    data <- data |> mutate(sex = 2)
  }

  if (!missing(adult_height) && adult_height %in% names(data)) {
    data <- data |> mutate(adult_height = .data[[adult_height]])
  } else {
    data <- data |> mutate(adult_height = NA)
  }

  df1_demos <- align_sex_coding_in_df(data = data, sex_column = 'sex') |>
    select(id, sex, adult_height) |>
    unique()

  return(df1_demos)
}

#' Generate BMI Cutoffs by Participant
#'
#' Generate BMI cutoff data for each participant in the data frame.
#'
#' @param data A data frame containing participant information.
#' @return A data frame with BMI cutoff data for each participant.
#' @export
cutoffs_by_participant <- function(data) {
  results_list <- list()

  for (i in 1:nrow(data)) {
    id <- data$id[i]
    sex <- data$sex[i]
    adult_height <- data$adult_height[i]

    cutoff_data <- cutoff_bmi_by_age_data(sex = sex, adult_height = adult_height)
    cutoff_data <- cutoff_data %>%
      mutate(id = id)

    results_list[[i]] <- cutoff_data
  }

  result <- bind_rows(results_list)
  return(result)
}


# make a function that will take a bmiz forecast dataframe and return formatted intervals
# Function to convert hilo objects to character strings
hilo_to_char <- function(hilo_obj) {
  lower <- hilo_obj$lower
  upper <- hilo_obj$upper
  paste0("[", lower, ", ", upper, "]")
}
#' Process BMIZ Forecast Data Frame
#'
#' Convert `hilo` objects to character strings, pivot longer, and format intervals.
#'
#' @param df A BMIZ forecast data frame.
#' @import tidyr
#' @import dplyr
#' @return A processed data frame with formatted intervals.
#' @export
process_bmiz_forecast <- function(df, lower_margin = lower_margin, upper_margin = upper_margin, central_value = central_value) {
  User_Model <- paste0('(-', lower_margin,', ',central_value, ', ',upper_margin,')')
  df <- df %>%
    mutate(
      `95%` = sapply(`95%`, hilo_to_char),
      `99%` = sapply(`99%`, hilo_to_char)
    )

  df_long <- df %>%
    tidyr::pivot_longer(
      cols = c(`95%`, `99%`, !!User_Model),
      names_to = "interval_type",
      values_to = "range"
    ) %>%
    filter(range != "[NA, NA]")

  df_long <- df_long %>%
    tidyr::separate(
      col = range,
      into = c("lower", "upper"),
      sep = ", ",
      remove = TRUE
    ) %>%
    mutate(
      lower_eBMIz = as.numeric(sub("\\[", "", lower)),
      upper_eBMIz = as.numeric(sub("\\]", "", upper))
    ) |>
    rename(eBMIz = .mean) |>
    select(-c(lower, upper))

  return(df_long)
}


#' Apply BMI Lookup
#'
#' Apply the BMI lookup function to a data frame.
#'
#' @param data A data frame containing BMI data.
#' @param data_point_col Column name for data points.
#' @param age_col Column name for age.
#' @param data_source Data source for BMI lookup (default: 'cdc').
#' @param type Type of BMI data (default: 'bmiz').
#' @return A vector of BMI values.
#' @export
apply_bmi_lookup <- function(data, data_point_col, age_col = agemos, data_source = 'cdc', type = 'bmiz') {
  data_point_col <- enquo(data_point_col)
  age_col <- enquo(age_col)

  data %>%
    rowwise() %>%
    mutate(
      bmi_value = bmi_lookup(
        data_point = !!data_point_col,
        type = type,
        data_source = data_source,
        sex = sex,
        age = !!age_col
      )
    ) %>%
    pull(bmi_value)
}


# lookup eBMI values

#' Add eBMI to Data Frame
#'
#' Add estimated BMI (eBMI) and corresponding weights to the data frame.
#'
#' @param data A data frame containing BMI data.
#' @param adult_height Column name for adult height.
#' @return A data frame with added eBMI and weight columns.
#' @export
add_eBMI_to_df <- function(data, adult_height = adult_height) {
  data <- data |>
    mutate(eBMI = apply_bmi_lookup(data, data_point_col = eBMIz),
           lower_eBMI = apply_bmi_lookup(data, data_point_col = lower_eBMIz),
           upper_eBMI = apply_bmi_lookup(data, data_point_col = upper_eBMIz)) |>
    mutate(eWeight = solve_for_weight(bmi = eBMI, height = adult_height),
           lower_eWeight = solve_for_weight(bmi = lower_eBMI, height = adult_height),
           upper_eWeight = solve_for_weight(bmi = upper_eBMI, height = adult_height)) |>
    # make eWeight, lower_eWeight, and upper_eWeight NA for agemos < 14*12
    mutate(eWeight = ifelse(agemos < 14*12, NA, eWeight),
           lower_eWeight = ifelse(agemos < 14*12, NA, lower_eWeight),
           upper_eWeight = ifelse(agemos < 14*12, NA, upper_eWeight))

  return(data)
}



#' Make Full BMI Data Frame
#'
#' Create a full BMI data frame by processing forecasts, generating cutoffs, and adding eBMI values.
#'
#' @param data A data frame containing BMI data.
#' @param id Column name for IDs (default: 'id').
#' @param adult_height Column name for adult height (default: 'adult_height').
#' @import tidyr
#' @import dplyr
#' @return A full BMI data frame with processed forecasts and cutoffs.
#' @export
#'
make_full_bmi_df <- function(data, id = 'id', adult_height = 'adult_height',
                             lower_margin = 0.5, upper_margin = 0.5, central_value = 'mean') {
  df_long <- process_bmiz_forecast(make_bmiz_forecast(data,
                                                      id = id,
                                                      lower_margin = lower_margin,
                                                      upper_margin = upper_margin,
                                                      central_value = central_value),
                                                      lower_margin = lower_margin,
                                                      upper_margin = upper_margin,
                                                      central_value = central_value)
  cutoff_data <- cutoffs_by_participant(cull_demos(data, adult_height = adult_height))
  full_df <- merge(cutoff_data, df_long, by = c('id', 'agemos'))
  full_df <- add_eBMI_to_df(full_df, adult_height = adult_height)

  return(full_df)
}

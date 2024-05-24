#' Cull Demographics Data
#'
#' Align participant, sex coding, and add adult height and sex columns, if available.
#'
#' @param cleaned_data A cleaned data frame containing demographic information.
#' @return A data frame with aligned demographics data.
#' @export
#'
cull_demos <- function(data) {
  df1_demos <- data |>
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
process_bmiz_forecast <- function(df,
                                  lower_margin = lower_margin,
                                  upper_margin = upper_margin,
                                  central_value = central_value) {
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
#' @param data A cleaned data frame containing BMI data.
#' @param id Column name for IDs (default: 'id').
#' @param adult_height Column name for adult height (default: 'adult_height').
#' @import tidyr
#' @import dplyr
#' @return A full BMI data frame with processed forecasts and cutoffs.
#' @export
#'
make_full_bmi_df <- function(data,
                             id = 'id',
                             adult_height = 'adult_height',
                             lower_margin = 0.5,
                             upper_margin = 0.5,
                             central_value = 'mean') {
  df_long <- process_bmiz_forecast(make_bmiz_forecast(data = data,
                                                      lower_margin = lower_margin,
                                                      upper_margin = upper_margin,
                                                      central_value = central_value),
                                                      lower_margin = lower_margin,
                                                      upper_margin = upper_margin,
                                                      central_value = central_value)
  cutoff_data <- cutoffs_by_participant(cull_demos(data))
  full_df <- full_join(cutoff_data, df_long, by = c('id', 'agemos'))
  full_df <- add_eBMI_to_df(data = full_df, adult_height = adult_height)

  return(full_df)
}

# clean and return the full BMI data frame
#' Clean and Process Data
#'
#' @param data A data frame containing BMI data.
#' @return A full BMI data frame with processed forecasts and cutoffs.
#' @export
#' @import dplyr
#' @import tidyr
#'

clean_and_process <- function(data,
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
                              lower_margin = 0.5,
                              upper_margin = 0.5,
                              central_value = 'mean') {
  clean_data <- clean_data(data,
                           id_col_name = id_col_name,
                           age_col_name = age_col_name,
                           dob_col_name = dob_col_name,
                           date_assessed_col_name = date_assessed_col_name,
                           age_unit = age_unit,
                           sex_col_name = sex_col_name,
                           ht_col_name = ht_col_name,
                           ht_unit = ht_unit,
                           wt_col_name = wt_col_name,
                           wt_unit = wt_unit,
                           bmi_col_name = bmi_col_name,
                           bmiz_col_name = bmiz_col_name,
                           pct_col_name = pct_col_name,
                           data_source = data_source,
                           adult_height_col_name = adult_height_col_name)
  full_df <- make_full_bmi_df(clean_data,
                              lower_margin = lower_margin,
                              upper_margin = upper_margin,
                              central_value = central_value)
  return(full_df)
}
